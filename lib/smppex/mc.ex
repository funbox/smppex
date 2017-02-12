defmodule SMPPEX.MC do
  @moduledoc """
  Module for implementing custom SMPP MC entities.

  In general, an SMPP MC entity represents a TCP server, which accepts connections
  and handles them. The TCP server is represented by a Ranch listener started
  by `start/2` call. On new connection the listener spawns `SMPPEX.Session` process
  coupled with `SMPPEX.MC` `GenServer` handler. The session interacts with the socket
  while the MC handler keeps state and does actual PDU handling. One also interacts with
  MC handler to send PDUs, PDU replies, etc.

  The session makes all requests to the MC handler process *syncronously* (via `GenServer.call`),
  while the MC handler process makes only *asyncronous*(via `GenServer.cast`) requests to the session.

  This is made intentionally since this allows:
  * to avoid any kind of deadlocks while the session and the MC handler process interact actively;
  * to control incoming SMPP message rate to avoid overflooding;
  * not to lose any control over connection because of the asyncronous nature of TCP implementation in OTP.

  To implement an MC entitiy, one should implement several callbacks for MC handler processes
  (`SMPPEX.MC` behaviour). The most proper way to do it is to `use` `SMPPEX.MC`:

  ```
  defmodule MyMC do
    use SMPPEX.MC

    # ...Callback implementation

  end
  ```

  In this case all callbacks have reasonable defaults.

  Note that `SMPPEX.MC` does not have a `start_link` method since `SMPPEX.MC` instances (handler processes)
  are launched when a Ranch listener created by `start/2` receives a new incoming connection.
  """

  alias :erlang, as: Erlang
  alias :ranch, as: Ranch

  alias SMPPEX.MC
  alias SMPPEX.Pdu
  alias SMPPEX.PduStorage
  alias SMPPEX.Session
  alias SMPPEX.SMPPTimers

  use GenServer
  require Logger

  defstruct [
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

  @default_enquire_link_limit         30000
  @default_enquire_link_resp_limit    30000
  @default_session_init_limit         10000
  @default_inactivity_limit           :infinity
  @default_response_limit             60000

  @default_timer_resolution           100

  @default_call_timeout               5000

  @type state :: term
  @type request :: term
  @type socket :: port | :ssl.sslsocket
  @type transport :: module

  @doc """
  Invoked when the listener has accepted a connection and tries to created an `SMPPEX.MC`.
  The Ranch acceptor handling the connection is busy till the function returns.

  `args` argument is taken directly from `start/2` call. `socket` and `transport` arguments
  are Ranch socket and transport respectively. They can be used, for example, to inspect
  peer address, etc.
  The return value should be either `{:ok, state}`, then MC handler will successfully start and returned
  state will be later passed to the other callbacks, or `{:stop, reason}`, then MC handler `GenServer` will stop
  and the connection closed.

  """

  @callback init(socket, transport, args :: term) :: {:ok, state} | {:stop, reason :: term}

  @doc """
  Invoked when the MC handler receives an incoming PDU (which is not a response PDU).

  The returned value is used as the new state.
  """
  @callback handle_pdu(pdu :: Pdu.t, state) :: state

  @doc """
  Invoked when the MC handler receives a response to a previously sent PDU.

  `pdu` argument contains the received response PDU, `original_pdu` contains
  the previously sent pdu for which the handled response is received.

  The returned value is used as the new state.
  """
  @callback handle_resp(pdu :: Pdu.t, original_pdu :: Pdu.t, state) :: state

  @doc """
  Invoked when the MC handler does not receive a response to a previously sent PDU
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
  Invoked for handling generic messages sent to the MC handler process.

  The returned value is used as the new state.
  """
  @callback handle_info(request, state) :: state

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour SMPPEX.MC

      @doc false
      def init(_socket, _transport, args) do
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
        init: 3,
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

  @default_transport :ranch_tcp
  @default_acceptor_count 50

  @spec start({module, args :: term}, opts :: Keyword.t) :: {:ok, listener_ref :: Ranch.ref} | {:error, reason :: term}

  @doc """
  Starts listener for MC entitiy.

  `module` is the callback module which should implement `SMPPEX.MC` behaviour.
  `args` is the argument passed to the `init` callback.
  `opts` is a keyword list of different options:
  * `:transport` is Ranch transport used for TCP connections: either `ranch_tcp` (the default) or
  `ranch_ssl`;
  * `:transport_opts` is a list of Ranch transport options. The major option is `{:port, port}`. The port is
  set to `0` by default, which means that the listener will accept connections on a random free port.
  * `:acceptor_count` is the number of Ranch listener acceptors, #{@default_acceptor_count} by default.
  * `:gen_server_opts` is a list of options passed directly to the underlying `GenServer.start_link` call,
  the default is `[]`;
  * `:mc_opts` is a keyword list of MC options:
      - `:timer_resolution` is interval of internal `ticks` on which time related events happen, like checking timeouts
      for pdus, checking SMPP timers, etc. The default is #{@default_timer_resolution} ms;
      - `:session_init_limit` is the maximum time for which the MC handler waits an incoming bind request.
      If no bind request is received within this interval of time, MC handler stops.
      The default value is #{@default_session_init_limit} ms;
      - `:enquire_link_limit` is value for enquire_link SMPP timer, i.e. the interval of SMPP session inactivity after which
      enquire_link PDU is send to "ping" the connetion. The default value is #{@default_enquire_link_limit} ms;
      - `:enquire_link_resp_limit` is the maximum time for which MC handler waits for enquire_link PDU response. If the
      response is not received within this interval of time and no activity from the peer occurs, the session is then considered
      dead and the MC handler stops. The default value is #{@default_enquire_link_resp_limit} ms;
      - `:inactivity_limit` is the maximum time for which the peer is allowed not to send PDUs (which are not response PDUs).
      If no such PDUs are received within this interval of time, MC handler stops. The default is #{@default_inactivity_limit} ms;
      - `:response_limit` is the maximum time to wait for a response for a previously sent PDU. If the response is
      not received within this interval, `handle_resp_timeout` callback is triggered for the original pdu. If the response
      is received later, it is discarded. The default value is #{@default_response_limit} ms.
  If `:mc_opts` list of options is ommited, all options take their default values.

  The returned value is either `{:ok, ref}` or `{:error, reason}`. The `ref` can be later used
  to stop the whole MC listener and all sessions received by it.
  """
  def start({_module, _args} = mod_with_args, opts \\ []) do

    acceptor_count = Keyword.get(opts, :acceptor_count, @default_acceptor_count)
    transport = Keyword.get(opts, :transport, @default_transport)
    transport_opts = Keyword.get(opts, :transport_opts, [{:port, 0}])
    mc_opts = Keyword.get(opts, :mc_opts, [])

    handler = fn(ref, socket, transport, session) ->
      case start_mc(mod_with_args, ref, socket, transport, session, mc_opts) do
        {:ok,  mc} -> {:ok, SMPPEX.MC.SMPPHandler.new(mc)}
        {:error, _} = error -> error
      end
    end
    ref = make_ref()
    case Ranch.start_listener(ref, acceptor_count, transport, transport_opts, Session, [handler: handler]) do
      {:error, _} = error -> error
      {:ok, _, _} -> {:ok, ref}
      {:ok, _} -> {:ok, ref}
    end
  end

  @spec stop(Ranch.ref) :: :ok

  @doc """
  Stops MC listener and all its sessions.

  The very moment of the SMPP session termination can be traced via `handle_stop` callback.
  """
  def stop(mc_server) do
    Ranch.stop_listener(mc_server)
  end

  @spec send_pdu(mc :: pid, pdu :: Pdu.t) :: :ok

  @doc """
  Sends outcoming PDU from the MC handler.

  The whole command is sent to the MC handler asyncronously. The further lifecycle of the PDU
  can be traced through callbacks.
  """
  def send_pdu(mc, pdu) do
    GenServer.cast(mc, {:send_pdu, pdu})
  end

  @spec reply(mc :: pid, pdu :: Pdu.t, reply_pdu :: Pdu.t) :: :ok

  @doc """
  Sends reply to previously received PDU from the MC handler.

  The whole command is sent to the MC handler asyncronously. The further lifecycle of the response PDU
  can be traced through callbacks.
  """
  def reply(mc, pdu, reply_pdu) do
    GenServer.cast(mc, {:reply, pdu, reply_pdu})
  end

  @spec stop_session(pid) :: :ok

  @doc """
  Stops MC handler asyncronously.

  The very moment of the SMPP session termination can be traced via `handle_stop` callback.
  """
  def stop_session(mc) do
    GenServer.cast(mc, :stop)
  end

  @spec call(pid, term, timeout) :: term

  @doc """
  Makes a syncronous call to MC handler.

  The call is handled by `handle_call/3` MC callback.
  """
  def call(mc, request, timeout \\ @default_call_timeout) do
    GenServer.call(mc, {:call, request}, timeout)
  end

  @spec cast(pid, term) :: :ok

  @doc """
  Makes an asyncronous call to MC handler.

  The call is handled by `handle_cast/2` MC callback.
  """
  def cast(mc, request) do
    GenServer.cast(mc, {:cast, request})
  end

  @spec handle_pdu(pid, Pdu.t) :: :ok

  @doc false
  def handle_pdu(mc, pdu) do
    GenServer.call(mc, {:handle_pdu, pdu})
  end

  @spec handle_stop(pid) :: :ok

  @doc false
  def handle_stop(mc) do
    GenServer.call(mc, :handle_stop)
  end

  @type send_pdu_result :: :ok | {:error, term}
  @spec handle_send_pdu_result(pid, Pdu.t, send_pdu_result) :: :ok

  @doc false
  def handle_send_pdu_result(mc, pdu, send_pdu_result) do
    GenServer.call(mc, {:handle_send_pdu_result, pdu, send_pdu_result})
  end

  # GenServer callbacks

  def init([{module, args}, mc_opts, _ref, socket, transport, session]) do
    case module.init(socket, transport, args) do
      {:ok, state} ->
        timer_resolution = Keyword.get(mc_opts, :timer_resolution, @default_timer_resolution)
        timer_ref = Erlang.start_timer(timer_resolution, self(), :emit_tick)

        enquire_link_limit = Keyword.get(mc_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(mc_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(mc_opts, :inactivity_limit, @default_inactivity_limit)
        session_init_limit = Keyword.get(mc_opts, :session_init_limit, @default_session_init_limit)

        time = SMPPEX.Time.monotonic

        timers = SMPPTimers.new(
          time,
          session_init_limit,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )

        pdu_storage_pid = case Keyword.get(mc_opts, :pdu_storage_pid, nil) do
          nil ->
            {:ok, pid} = PduStorage.start_link()
            pid
          pid -> pid
        end

        response_limit = Keyword.get(mc_opts, :response_limit, @default_response_limit)

        {:ok, %MC{
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
        stop
    end
  end

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
        new_st = %MC{st | module_state: new_module_state}
        {:reply, reply, new_st}
      {:noreply, new_module_state} ->
        new_st = %MC{st | module_state: new_module_state}
        {:noreply, new_st}
    end
  end

  def handle_cast({:send_pdu, pdu}, st) do
    new_st = do_send_pdu(pdu, st)
    {:noreply, new_st}
  end

  def handle_cast({:reply, pdu, reply_pdu}, st) do
    new_st = do_reply(pdu, reply_pdu, st)
    {:noreply, new_st}
  end

  def handle_cast(:stop, st) do
    Session.stop(st.smpp_session)
    {:noreply, st}
  end

  def handle_cast({:cast, request}, st) do
    new_module_state = st.module.handle_cast(request, st.module_state)
    new_st = %MC{st | module_state: new_module_state}
    {:noreply, new_st}
  end

  def handle_info({:timeout, _timer_ref, :emit_tick}, st) do
    new_tick_timer_ref = Erlang.start_timer(st.timer_resolution, self(), :emit_tick)
    Erlang.cancel_timer(st.tick_timer_ref)
    Kernel.send self(), {:tick, SMPPEX.Time.monotonic}
    {:noreply, %MC{st | tick_timer_ref: new_tick_timer_ref}}
  end

  def handle_info({:tick, time}, st) do
    do_handle_tick(time, st)
  end

  def handle_info(request, st) do
    new_module_state = st.module.handle_info(request, st.module_state)
    new_st = %MC{st | module_state: new_module_state}
    {:noreply, new_st}
  end

  # Private functions

  defp start_mc(mod_with_args, ref, socket, transport, session, opts) do
    gen_server_opts = Keyword.get(opts, :gen_server_opts, [])
    mc_opts = Keyword.get(opts, :mc_opts, [])
    GenServer.start_link(
      __MODULE__,
      [mod_with_args, mc_opts, ref, socket, transport, session],
      gen_server_opts
    )
  end

  defp do_handle_pdu(pdu, st) do
    new_module_state = st.module.handle_pdu(pdu, st.module_state)
    new_timers = SMPPTimers.handle_peer_transaction(st.timers, st.time)
    new_st = %MC{st | module_state: new_module_state, timers: new_timers}
    if Pdu.bind?(pdu) do
      do_handle_bind(new_st)
    else
      {:reply, :ok, new_st}
    end
  end

  defp do_handle_resp(pdu, st) do
    sequence_number = Pdu.sequence_number(pdu)
    new_timers = SMPPTimers.handle_peer_action(st.timers, st.time)
    new_st = %MC{st | timers: new_timers}
    case PduStorage.fetch(st.pdu_storage, sequence_number) do
      [] ->
        Logger.info("mc #{inspect self()}, resp for unknown pdu(sequence_number: #{sequence_number}), dropping")
        {:reply, :ok, new_st}
      [original_pdu] ->
        do_handle_resp_for_pdu(pdu, original_pdu, new_st)
    end
  end

  defp do_handle_resp_for_pdu(pdu, original_pdu, st) do
    new_module_state = st.module.handle_resp(pdu, original_pdu, st.module_state)
    new_st = %MC{st | module_state: new_module_state}
    {:reply, :ok, new_st}
  end

  defp do_handle_bind(st) do
    new_timers = SMPPTimers.handle_bind(st.timers, st.time)
    new_st = %MC{st | timers: new_timers}
    {:reply, :ok, new_st}
  end

  defp do_handle_stop(st) do
    _ = st.module.handle_stop(st.module_state)
    {:stop, :normal, :ok, st}
  end

  defp do_handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    new_st = %MC{st | module_state: new_module_state}
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
    new_st = %MC{st | module_state: new_module_state}
    do_handle_expired_pdus(pdus, new_st)
  end

  defp do_handle_timers(time, st) do
    case SMPPTimers.handle_tick(st.timers, time) do
      {:ok, new_timers} ->
        new_st = %MC{st | timers: new_timers, time: time}
        {:noreply, new_st}
      {:stop, reason} ->
        Logger.info("mc #{inspect self()}, being stopped by timers(#{reason})")
        Session.stop(st.smpp_session)
        {:noreply, st}
      {:enquire_link, new_timers} ->
        new_st = %MC{st | timers: new_timers, time: time}
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

end
