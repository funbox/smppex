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
    :timers,
    :pdus,
    :response_limit,
    :sequence_number,
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
  Invoked when the ESME fails to parse a pdu.

  The returned value can either be :ok or {:stop, reason}.
  """
  @callback handle_parse_error(reason :: term, state) :: SMPPEX.SMPPHandler.handle_pdu_result

  @doc """
  Invoked when the SMPP session is about to stop.

  `lost_pdus` contains sent PDUs which have not received resps (and will never
   receive since the session terminates).

  `reason` contains one of the following:
  * `:custom` -- session manually stopped by call to `ESME.stop`;
  * `{:parse_error, error}` -- error in parsing incoming SMPP packet occured;
  * `:socket_closed` -- peer closed socket;
  * `{:socket_error, error}` -- socket error occured;
  * `{:timers, reason}` -- session closed by timers;
  * `{:smpp_session_exit, reason}` -- SMPP session exited with reason `reason`. This will only be received if ESME traps exits. Otherwise, the ESME will exit too;
  * `{:unrecognized_message, message}` -- SMPP session received an unknown message.

  The return value is `{stop_reason, new_state}`. The session GenServer will stop
  with `stop_reason`.
  """
  @callback handle_stop(reason :: term, lost_pdus :: [Pdu.t], state) :: {exit_reason :: term, state}

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

  @doc """
  Invoked when ESME was stopped abnormally and wasn't able to
  handle sent PDUs without resps in `handle_stop`.

  Since the ESME is already stopped, the callback does not receive state,
  but the former pid of ESME, the reason of its termination and unconfirmed PDUs.

  The returned value is ignored.
  """
  @callback handle_lost_pdus(esme :: pid, reason :: term, pdus :: [Pdu.t]) :: any

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour SMPPEX.ESME

      require Logger

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
      def handle_parse_error(reason, state), do: {:stop, reason, state}

      @doc false
      def handle_stop(reason, lost_pdus, state) do
        Logger.info("esme #{inspect self()} is stopping, reason: #{inspect reason}, lost_pdus: #{inspect lost_pdus}")
        {:normal, state}
      end

      @doc false
      def handle_call(_request, _from, state), do: {:reply, :ok, state}

      @doc false
      def handle_cast(_request, state), do: state

      @doc false
      def handle_info(_request, state), do: state

      @doc false
      def handle_lost_pdus(pid, reason, lost_pdus) do
        Logger.info("mc_conn #{pid} stopped with reason: #{inspect reason}, lost_pdus: #{inspect lost_pdus}")
      end

      defoverridable [
        init: 1,
        handle_pdu: 2,
        handle_resp: 3,
        handle_resp_timeout: 2,
        handle_send_pdu_result: 3,
        handle_parse_error: 2,
        handle_stop: 3,
        handle_call: 3,
        handle_cast: 2,
        handle_info: 2,
        handle_lost_pdus: 3
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
      [convert_host(host), port, {module, args}, transport, timeout, esme_opts],
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

  @spec handle_stop(pid, reason :: term) :: :ok

  @doc false
  def handle_stop(esme, reason) do
    GenServer.call(esme, {:handle_stop, reason})
  end

  @type send_pdu_result :: :ok | {:error, term}
  @spec handle_send_pdu_result(pid, Pdu.t, send_pdu_result) :: :ok

  @doc false
  def handle_send_pdu_result(esme, pdu, send_pdu_result) do
    GenServer.call(esme, {:handle_send_pdu_result, pdu, send_pdu_result})
  end

  @spec handle_parse_error(pid, reason :: term) :: :ok | {:error, term}

  @doc false
  def handle_parse_error(esme, reason) do
    GenServer.call(esme, {:handle_parse_error, reason})
  end

  # GenServer callbacks

  @doc false
  def init([host, port, mod_with_args, transport, timeout, esme_opts]) do
    esme = self()
    handler = fn(ref, _socket, _transport, session) ->
      Process.link(esme)
      Kernel.send esme, {ref, session}
      {:ok, SMPPEX.ESME.SMPPHandler.new(esme)}
    end

    case start_session(handler, host, port, transport, timeout) do
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

  def handle_call({:handle_stop, reason}, _from, st) do
    {reason, new_st} = do_handle_stop(reason, st)
    {:stop, reason, :ok, new_st}
  end

  def handle_call({:handle_send_pdu_result, pdu, send_pdu_result}, _from, st) do
    do_handle_send_pdu_result(pdu, send_pdu_result, st)
  end

  def handle_call({:handle_parse_error, reason}, _from, st) do
    do_handle_parse_error(reason, st)
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
    Session.stop(st.smpp_session, :custom)
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
    smpp_session = st.smpp_session
    case request do
      {:EXIT, ^smpp_session, session_reason} ->
        {reason, new_st} = do_handle_stop({:smpp_session_exit, session_reason}, st)
        {:stop, reason, new_st}
      _ ->
        new_module_state = st.module.handle_info(request, st.module_state)
        new_st = %ESME{st | module_state: new_module_state}
        {:noreply, new_st}
      end
  end

  # Private functions

  defp start_session(handler, host, port, transport, timeout) do
    case transport.connect(host, port, [:binary, {:packet, 0}, {:active, :once}], timeout) do
      {:ok, socket} ->
        pool = ClientPool.start(handler, 2, transport, timeout)
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

        {:ok, pdu_storage} = PduStorage.start(&module.handle_lost_pdus/3)
        response_limit = Keyword.get(esme_opts, :response_limit, @default_response_limit)

        {:ok, %ESME{
          client_pool: pool,
          smpp_session: session,
          module: module,
          module_state: state,
          timers: timers,
          pdus: pdu_storage,
          response_limit: response_limit,
          sequence_number: 0,
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
    case PduStorage.fetch(st.pdus, sequence_number) do
      [] ->
        Logger.info("esme #{inspect self()}, resp for unknown pdu(sequence_number: #{sequence_number}), dropping")
        {:reply, :ok, new_st}
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
        Session.stop(st.smpp_session, :bind_failed)
        {:reply, :ok, st}
    end
  end

  defp do_handle_stop(reason, st) do
    lost_pdus = PduStorage.fetch_all(st.pdus)

    Process.unlink(st.smpp_session)
    ClientPool.stop(st.client_pool)

    # TODO: remove legacy implementation handling in future versions
    module = st.module

    result = try do
      module.handle_stop(st.module_state)
      :legacy_impl
    rescue
      e in UndefinedFunctionError -> {:new_impl, e}
    end

    case result do
      :legacy_impl ->
        Logger.warn("Implementing #{st.module}.handle_stop(st) is deprecated, implement #{st.module}.handle_stop(reason, lost_pdus, st) instead")
        {:normal, st}
      {:new_impl, %UndefinedFunctionError{arity: 1, function: :handle_stop, module: ^module}} ->
        {exit_reason, new_module_state} = module.handle_stop(reason, lost_pdus, st.module_state)
        {exit_reason, %ESME{st | module_state: new_module_state}}
      {:new_impl, other_exception} ->
        raise other_exception
    end
  end

  defp do_handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    new_st = %ESME{st | module_state: new_module_state}
    {:reply, :ok, new_st}
  end

  defp do_handle_parse_error(reason, st) do
    case st.module.handle_parse_error(reason, st.module_state) do
      {:ok, new_module_state} ->
        new_st = %ESME{st | module_state: new_module_state}
        {:reply, :ok, new_st}
      {:stop, reason, new_module_state} ->
        {:stop, reason, :ok, %ESME{st | module_state: new_module_state}}
    end
  end

  defp do_handle_tick(time, st) do
    expired_pdus = PduStorage.fetch_expired(st.pdus, time)
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
        Session.stop(st.smpp_session, {:timers, reason})
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
    sequence_number = st.sequence_number + 1
    new_pdu = %Pdu{pdu | sequence_number: sequence_number}
    true = PduStorage.store(st.pdus, new_pdu, st.time + st.response_limit)
    Session.send_pdu(st.smpp_session, new_pdu)
    new_st = %ESME{st | sequence_number: sequence_number}
    new_st
  end

  defp do_reply(pdu, reply_pdu, st) do
    new_reply_pdu = %Pdu{reply_pdu | sequence_number: pdu.sequence_number}
    Session.send_pdu(st.smpp_session, new_reply_pdu)
    st
  end

  defp convert_host(host) when is_binary(host), do: to_char_list(host)
  defp convert_host(host), do: host

end
