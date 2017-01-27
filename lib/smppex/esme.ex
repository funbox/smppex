defmodule SMPPEX.ESME do
  @moduledoc """
  Module for implementing custom SMPP ESME entities.

  `SMPPEX.ESME` represents a `GenServer` process which spawns and interacts with `SMPPEX.Session`
  `ranch_protocol`. The session is spawned under control of `ranch` supervision tree.
  The session makes all requests to the ESME process *syncronously* (via `GenServer.call`),
  while the ESME process makes only *asyncronous*(via `GenServer.cast`) requests to the session.

  This is made intentionally since this allows:
  * to avoid any kind of deadlocks while the session and the ESME process interact actively;
  * to control incoming SMPP message rate to avoid overflooding;
  * not to lose any control over connection because of the asyncronous nature of TCP implementation in OTP.

  To implement an ESME entitiy, one should implement several callbacks (`SMPPEX.ESME` behaviour).
  The most proper way to do it is to `use` `SMPPEX.ESME`:

  ```
  defmodule MyESME do
    use SMPPEX.ESME

    # ...Callback implementation

  end
  ```

  In this case all callbacks have reasonable defaults.
  """

  alias :erlang, as: Erlang

  alias SMPPEX.ClientPool
  alias SMPPEX.ESME
  alias SMPPEX.Pdu
  alias SMPPEX.PduStorage
  alias SMPPEX.Session
  alias SMPPEX.SMPPTimers

  use GenServer
  require Logger

  defstruct [
    :client_pool,
    :smpp_session,
    :module,
    :module_state,
    :pdu_storage,
    :timers,
    :response_limit,
    :time,
    :timer_resolution,
    :tick_timer_ref
  ]

  @default_timeout 5000
  @default_enquire_link_limit 30000
  @default_enquire_link_resp_limit 30000
  @default_inactivity_limit :infinity
  @default_response_limit 60000

  @default_timer_resolution 100

  @default_call_timeout 5000

  @default_transport :ranch_tcp

  @default_pool_size 2

  @type state :: term
  @type request :: term

  @doc """
  Invoked when the ESME is started after connection to SMSC successfully established.

  `args` argument is taken directly from `start_link` call, which does not return until `init` finishes.
  The return value should be either `{:ok, state}`, then ESME will successfully start and returned state will
  be later passed to the other callbacks, or `{:stop, reason}`, then ESME `GenServer` will stop
  with the returned reason.

  """

  @callback init(args :: term) :: {:ok, state} | {:stop, reason :: term}


  @doc """
  Invoked when the ESME receives an incoming PDU (which is not a response PDU).

  The returned value is used as the new state.
  """

  @callback handle_pdu(pdu :: Pdu.t, state) :: state

  @doc """
  Invoked when the ESME receives a response to a previously sent PDU.

  `pdu` argument contains the received response PDU, `original_pdu` contains
  the previously sent pdu for which the handled response is received.

  The returned value is used as the new state.
  """
  @callback handle_resp(pdu :: Pdu.t, original_pdu :: Pdu.t, state) :: state

  @doc """
  Invoked when the ESME does not receive a response to a previously sent PDU
  for the specified timeout.

  `pdu` argument contains the PDU for which no response was received. If the response
  will be received later it will be dropped (with an `info` log message).

  The returned value is used as the new state.
  """
  @callback handle_resp_timeout(pdu :: Pdu.t, state) :: state

  @doc """
  Invoked when the SMPP session successfully sent PDU to transport or failed to do this.

  `pdu` argument contains the PDU for which send status is reported. `send_pdu_result` can be
  either `:ok` or `{:error, reason}`.

  The returned value is used as the new state.
  """
  @callback handle_send_pdu_result(pdu :: Pdu.t, send_pdu_result :: SMPPEX.SMPPHandler.send_pdu_result, state) :: state

  @doc """
  Invoked when the SMPP session is about to stop.

  The returned value is ignored.
  """
  @callback handle_stop(state) :: any

  @doc """
  Invoked for handling `call/3` calls.

  The callback is called syncronously for handling.

  The returned values have the same meaning as in `GenServer` `handle_call` callback
  (but note that only two kinds of responses are possible). In case of delaying a reply (`{:noreply, state}` callback result)
  it can be later send using `GenServer.reply(from, reply)`

  """
  @callback handle_call(request, from :: GenServer.from, state) :: {:reply, reply :: term, state} | {:noreply, state}

  @doc """
  Invoked for handling `cast/2` calls.

  The callback is called asyncronously.

  The returned value is used as the new state.
  """
  @callback handle_cast(request, state) :: state

  @doc """
  Invoked for handling generic messages sent to the ESME process.

  The returned value is used as the new state.
  """
  @callback handle_info(request, state) :: state

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour SMPPEX.ESME

      @doc false
      def init(args) do
        {:ok, args}
      end

      @doc false
      def handle_pdu(_pdu, state), do: state

      @doc false
      def handle_resp(_pdu, _original_pdu, state), do: state

      @doc false
      def handle_resp_timeout(_pdu, state), do: state

      @doc false
      def handle_send_pdu_result(_pdu, _result, state), do: state

      @doc false
      def handle_stop(_state), do: nil

      @doc false
      def handle_call(_request, _from, state), do: {:reply, :ok, state}

      @doc false
      def handle_cast(_request, state), do: state

      @doc false
      def handle_info(_request, state), do: state

      defoverridable [
        init: 1,
        handle_pdu: 2,
        handle_resp: 3,
        handle_resp_timeout: 2,
        handle_send_pdu_result: 3,
        handle_stop: 1,
        handle_call: 3,
        handle_cast: 2,
        handle_info: 2
      ]
    end
  end

  # Public interface

  @spec start_link(host :: term, port :: non_neg_integer, {module, args :: term}, opts :: Keyword.t) :: GenServer.on_start

  @doc """
  Starts ESME entitiy.

  The function does not return until ESME successfully connects to the specified
  `host` and `port` and initializes or fails.

  `module` is the callback module which should implement `SMPPEX.ESME` behaviour.
  `args` is the argument passed to the `init` callback.
  `opts` is a keyword list of different options:
  * `:transport` is Ranch transport used for TCP connection: either `ranch_tcp` (the default) or
  `ranch_ssl`;
  * `:gen_server_opts` is a list of options passed directly to the underlying `GenServer.start_link` call,
  the default is `[]`;
  * `:timeout` is timeout for the whole connect and initialization process. The default is #{@default_timeout} ms;
  * `:esme_opts` is a keyword list of ESME options:
      - `:timer_resolution` is interval of internal `ticks` on which time related events happen, like checking timeouts
      for pdus, checking SMPP timers, etc. The default is #{@default_timer_resolution} ms;
      - `:enquire_link_limit` is value for enquire_link SMPP timer, i.e. the interval of SMPP session inactivity after which
      enquire_link PDU is send to "ping" the connetion. The default value is #{@default_enquire_link_limit} ms;
      - `:enquire_link_resp_limit` is the maximum time for which ESME waits for enquire_link PDU response. If the
      response is not received within this interval of time and no activity from the peer occurs, the session is then considered
      dead and the ESME stops. The default value is #{@default_enquire_link_resp_limit} ms;
      - `:inactivity_limit` is the maximum time for which the peer is allowed not to send PDUs (which are not response PDUs).
      If no such PDUs are received within this interval of time, ESME stops. The default is #{@default_inactivity_limit} ms;
      - `:response_limit` is the maximum time to wait for a response for a previously sent PDU. If the response is
      not received within this interval, `handle_resp_timeout` callback is triggered for the original pdu. If the response
      is received later, it is discarded. The default value is #{@default_response_limit} ms.
  If `:esme_opts` list of options is ommited, all options take their default values.

  The whole `opts` argument may also be ommited in order to start ESME with the defaults.

  The returned value is either `{:ok, pid}` or `{:error, reason}`.
  """
  def start_link(host, port, {module, args}, opts \\ []) do
    transport = Keyword.get(opts, :transport, @default_transport)
    gen_server_opts = Keyword.get(opts, :gen_server_opts, [])
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    esme_opts = Keyword.get(opts, :esme_opts, [])

    GenServer.start_link(
      __MODULE__,
      [
        convert_host(host),
        port,
        {module, args},
        transport,
        timeout,
        esme_opts
      ],
      gen_server_opts
    )
  end

  @spec send_pdu(esme :: pid, pdu :: Pdu.t) :: :ok

  @doc """
  Sends outcoming PDU from the ESME.

  The whole command is sent to the ESME asyncronously. The further lifecycle of the PDU
  can be traced through callbacks.
  """
  def send_pdu(esme, pdu) do
    GenServer.cast(esme, {:send_pdu, pdu})
  end

  @spec reply(esme :: pid, pdu :: Pdu.t, reply_pdu :: Pdu.t) :: :ok

  @doc """
  Sends reply to previously received PDU from the ESME.

  The whole command is sent to the ESME asyncronously. The further lifecycle of the response PDU
  can be traced through callbacks.
  """
  def reply(esme, pdu, reply_pdu) do
    GenServer.cast(esme, {:reply, pdu, reply_pdu})
  end

  @spec stop(esme :: pid) :: :ok

  @doc """
  Stops ESME asyncronously.

  The very moment of the SMPP session termination can be traced via `handle_stop` callback.
  """
  def stop(esme) do
    GenServer.cast(esme, :stop)
  end

  @spec call(esme ::pid, arg :: term, timeout) :: term

  @doc """
  Makes a syncronous call to ESME.

  The call is handled by `handle_call/3` ESME callback.
  """
  def call(esme, request, timeout \\ @default_call_timeout) do
    GenServer.call(esme, {:call, request}, timeout)
  end

  @spec cast(pid, term) :: :ok

  @doc """
  Makes an asyncronous call to ESME.

  The call is handled by `handle_cast/2` ESME callback.
  """
  def cast(esme, request) do
    GenServer.cast(esme, {:cast, request})
  end

  @spec with_session(esme :: pid, (smpp_session :: pid -> any)) :: :ok

  @doc """
  Asyncronously executes the passed lambda passing SMPP session(`SMPPEX.Session`) to it directly.

  This function can be used for uncommon cases like sending PDUs bypassing timers or
  sequence_number assignment.
  """
  def with_session(esme, fun) do
    GenServer.cast(esme, {:with_session, fun})
  end

  @spec handle_pdu(pid, Pdu.t) :: :ok

  @doc false
  def handle_pdu(esme, pdu) do
    GenServer.call(esme, {:handle_pdu, pdu})
  end

  @spec handle_stop(pid) :: :ok

  @doc false
  def handle_stop(esme) do
    GenServer.call(esme, :handle_stop)
  end

  @type send_pdu_result :: :ok | {:error, term}
  @spec handle_send_pdu_result(pid, Pdu.t, send_pdu_result) :: :ok

  @doc false
  def handle_send_pdu_result(esme, pdu, send_pdu_result) do
    GenServer.call(esme, {:handle_send_pdu_result, pdu, send_pdu_result})
  end

  # GenServer callbacks

  @doc false
  def init([host, port, mod_with_args, transport, timeout, esme_opts]) do

    esme = self()
    handler = fn(ref, _socket, _transport, session) ->
      Kernel.send esme, {ref, session}
      {:ok, SMPPEX.ESME.SMPPHandler.new(esme)}
    end

    pool_size = Keyword.get(esme_opts, :pool_size, @default_pool_size)
    case start_session(handler, host, port, transport, timeout, pool_size) do
      {:ok, pool, session} ->
        init_esme(mod_with_args, pool, session, esme_opts)
      {:error, reason} -> {:stop, reason}
    end
  end

  @doc false
  def handle_call({:handle_pdu, pdu}, _from, st) do
    case Pdu.resp?(pdu) do
      true -> do_handle_resp(pdu, st)
      false -> do_handle_pdu(pdu, st)
    end
  end

  def handle_call(:handle_stop, _from, st) do
    do_handle_stop(st)
  end

  def handle_call({:handle_send_pdu_result, pdu, send_pdu_result}, _from, st) do
    do_handle_send_pdu_result(pdu, send_pdu_result, st)
  end

  def handle_call({:call, request}, from, st) do
    case st.module.handle_call(request, from, st.module_state) do
      {:reply, reply, new_module_state} ->
        new_st = %ESME{st | module_state: new_module_state}
        {:reply, reply, new_st}
      {:noreply, new_module_state} ->
        new_st = %ESME{st | module_state: new_module_state}
        {:noreply, new_st}
    end
  end

  @doc false
  def handle_cast({:send_pdu, pdu}, st) do
    new_st = do_send_pdu(pdu, st)
    {:noreply, new_st}
  end

  def handle_cast({:reply, pdu, reply_pdu}, st) do
    new_st = do_reply(pdu, reply_pdu, st)
    {:noreply, new_st}
  end

  def handle_cast({:with_session, fun}, st) do
    fun.(st.smpp_session)
    {:noreply, st}
  end

  def handle_cast(:stop, st) do
    Session.stop(st.smpp_session)
    {:noreply, st}
  end

  def handle_cast({:cast, request}, st) do
    new_module_state = st.module.handle_cast(request, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    {:noreply, new_st}
  end

  @doc false
  def handle_info({:timeout, _timer_ref, :emit_tick}, st) do
    new_tick_timer_ref = Erlang.start_timer(st.timer_resolution, self(), :emit_tick)
    Erlang.cancel_timer(st.tick_timer_ref)
    Kernel.send self(), {:tick, SMPPEX.Time.monotonic}
    {:noreply, %ESME{st | tick_timer_ref: new_tick_timer_ref}}
  end

  def handle_info({:tick, time}, st) do
    do_handle_tick(time, st)
  end

  def handle_info(request, st) do
    new_module_state = st.module.handle_info(request, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    {:noreply, new_st}
  end

  # Private functions
  defp start_session(handler, host, port, transport, timeout, pool_size) do
    case transport.connect(host, port, [:binary, {:packet, 0}, {:active, :once}], timeout) do
      {:ok, socket} ->
        pool = ClientPool.start(handler, pool_size, transport, timeout)
        ClientPool.start_session(pool, socket)
        ref = ClientPool.ref(pool)
        receive do
          {^ref, session} ->
            {:ok, pool, session}
        after timeout ->
          {:error, :session_init_timeout}
        end
      {:error, _} = err -> err
    end
  end

  defp init_esme({module, args}, pool, session, esme_opts) do

    case module.init(args) do
      {:ok, state} ->
        timer_resolution = Keyword.get(esme_opts, :timer_resolution, @default_timer_resolution)
        timer_ref = Erlang.start_timer(timer_resolution, self(), :emit_tick)

        enquire_link_limit = Keyword.get(esme_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(esme_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(esme_opts, :inactivity_limit, @default_inactivity_limit)

        time = SMPPEX.Time.monotonic

        timers = SMPPTimers.new(
          time,
          :infinity,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )


        pdu_storage_pid = case Keyword.get(esme_opts, :pdu_storage_pid, nil) do
          nil ->
              {:ok, pid} = PduStorage.start_link()
              pid
          pid -> pid
        end
        response_limit = Keyword.get(esme_opts, :response_limit, @default_response_limit)

        {:ok, %ESME{
          client_pool: pool,
          smpp_session: session,
          module: module,
          module_state: state,
          pdu_storage: pdu_storage_pid,
          timers: timers,
          response_limit: response_limit,
          time: time,
          timer_resolution: timer_resolution,
          tick_timer_ref: timer_ref
        }}
      {:stop, _} = stop ->
        ClientPool.stop(pool)
        stop
    end
  end

  defp do_handle_pdu(pdu, st) do
    new_module_state = st.module.handle_pdu(pdu, st.module_state)
    new_timers = SMPPTimers.handle_peer_transaction(st.timers, st.time)
    {:reply, :ok, %ESME{st | module_state: new_module_state, timers: new_timers}}
  end

  defp do_handle_resp(pdu, st) do
    sequence_number = Pdu.sequence_number(pdu)
    new_timers = SMPPTimers.handle_peer_action(st.timers, st.time)
    new_st = %ESME{st | timers: new_timers}

    case PduStorage.fetch(st.pdu_storage, sequence_number) do
      [] ->
        # don't drop response pdu for sequence numbers which not recognized
        # with_session maybe in use
        # just return back a nil for original_pdu, and let the client handle it, with some pattern matching
        do_handle_resp_for_pdu(pdu, nil, new_st)
      [original_pdu] ->
        do_handle_resp_for_pdu(pdu, original_pdu, new_st)
    end
  end

  defp do_handle_resp_for_pdu(pdu, original_pdu, st) do
    new_module_state = st.module.handle_resp(pdu, original_pdu, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    case Pdu.bind_resp?(pdu) do
      true -> do_handle_bind_resp(pdu, new_st)
      false -> {:reply, :ok, new_st}
    end
  end

  defp do_handle_bind_resp(pdu, st) do
    case Pdu.success_resp?(pdu) do
      true ->
        new_timers = SMPPTimers.handle_bind(st.timers, st.time)
        new_st = %ESME{st | timers: new_timers}
        {:reply, :ok, new_st}
      false ->
        Logger.info("esme #{inspect self()}, bind failed with status #{Pdu.command_status(pdu)}, stopping")
        Session.stop(st.smpp_session)
        {:reply, :ok, st}
    end
  end

  defp do_handle_stop(st) do
    _ = st.module.handle_stop(st.module_state)
    ClientPool.stop(st.client_pool)
    {:stop, :normal, :ok, st}
  end

  defp do_handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    {:reply, :ok, new_st}
  end

  defp do_handle_tick(time, st) do
    expired_pdus = PduStorage.fetch_expired(st.pdu_storage, time)
    new_st = do_handle_expired_pdus(expired_pdus, st)
    do_handle_timers(time, new_st)
  end

  defp do_handle_expired_pdus([], st), do: st
  defp do_handle_expired_pdus([pdu | pdus], st) do
    new_module_state = st.module.handle_resp_timeout(pdu, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    do_handle_expired_pdus(pdus, new_st)
  end

  defp do_handle_timers(time, st) do
    case SMPPTimers.handle_tick(st.timers, time) do
      {:ok, new_timers} ->
        new_st = %ESME{st | timers: new_timers, time: time}
        {:noreply, new_st}
      {:stop, reason} ->
        Logger.info("esme #{inspect self()}, being stopped by timers(#{reason})")
        Session.stop(st.smpp_session)
        {:noreply, st}
      {:enquire_link, new_timers} ->
        new_st = %ESME{st | timers: new_timers, time: time}
        do_send_enquire_link(new_st)
    end
  end

  defp do_send_enquire_link(st) do
    enquire_link = SMPPEX.Pdu.Factory.enquire_link
    new_st = do_send_pdu(enquire_link, st)
    {:noreply, new_st}
  end

  defp do_send_pdu(pdu, st) do

    pdu = case pdu.sequence_number do
      0 -> %Pdu{pdu | sequence_number: PduStorage.reserve_sequence_number(st.pdu_storage)}
      _ -> pdu
    end

    true = PduStorage.store(st.pdu_storage, pdu, st.time + st.response_limit)
    Session.send_pdu(st.smpp_session, pdu)

    st
  end

  defp do_reply(pdu, reply_pdu, st) do
    new_reply_pdu = %Pdu{reply_pdu | sequence_number: pdu.sequence_number}
    Session.send_pdu(st.smpp_session, new_reply_pdu)
    st
  end

  defp convert_host(host) when is_binary(host), do: to_char_list(host)
  defp convert_host(host), do: host


end
