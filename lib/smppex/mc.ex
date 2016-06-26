defmodule SMPPEX.MC do

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
  @default_session_init_limit 10000
  @default_inactivity_limit :infinity
  @default_response_limit 60000

  @default_timer_resolution 100

  @default_call_timeout 5000

  @type state :: term
  @type request :: term
  @type socket :: port | :ssl.sslsocket
  @type transport :: module

  @callback init(socket, transport, args :: term) :: {:ok, state} | {:close, reason :: term}

  @callback handle_pdu(Pdu.t, state) :: state

  @callback handle_resp(Pdu.t, Pdu.t, state) :: state

  @callback handle_resp_timeout(Pdu.t, state) :: state

  @callback handle_send_pdu_result(Pdu.t, SMPPEX.SMPPHandler.send_pdu_result, state) :: state

  @callback handle_stop(state) :: any

  @callback handle_call(request, GenServer.from, state) :: {:reply, reply :: term, state} | {:noreply, state}

  @callback handle_cast(request, state) :: state

  @callback handle_info(request, state) :: state

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour SMPPEX.MC

      def init(_socket, _transport, args) do
        {:ok, args}
      end

      def handle_pdu(_pdu, state), do: state

      def handle_resp(_pdu, _original_pdu, state), do: state

      def handle_resp_timeout(_pdu, state), do: state

      def handle_send_pdu_result(_pdu, _result, state), do: state

      def handle_stop(_state), do: nil

      def handle_call(_request, _from, state), do: {:reply, :ok, state}

      def handle_cast(_request, state), do: state

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

  def start({_module, _args} = mod_with_args, opts \\ []) do
    acceptor_count = Keyword.get(opts, :acceptor_count, @default_acceptor_count)
    transport = Keyword.get(opts, :transport, @default_transport)
    transport_opts = Keyword.get(opts, :transport_opts, [{:port, 0}])
    handler = fn(ref, socket, transport, session) ->
      case start_mc(mod_with_args, ref, socket, transport, session, opts) do
        {:ok,  mc} -> {:ok, SMPPEX.MC.SMPPHandler.new(mc)}
        {:error, _} = error -> error
      end
    end
    ref = make_ref
    case Ranch.start_listener(ref, acceptor_count, transport, transport_opts, Session, [handler: handler]) do
      {:error, _} = error -> error
      {:ok, _, _} -> {:ok, ref}
      {:ok, _} -> {:ok, ref}
    end
  end

  def stop(mc_server) do
    Ranch.stop_listener(mc_server)
  end

  def send_pdu(mc, pdu) do
    GenServer.cast(mc, {:send_pdu, pdu})
  end

  def reply(mc, pdu, reply_pdu) do
    GenServer.cast(mc, {:reply, pdu, reply_pdu})
  end

  def stop_session(mc) do
    GenServer.cast(mc, :stop)
  end

  def handle_pdu(mc, pdu) do
    GenServer.call(mc, {:handle_pdu, pdu})
  end

  def handle_stop(mc) do
    GenServer.call(mc, :handle_stop)
  end

  def handle_send_pdu_result(mc, pdu, send_pdu_result) do
    GenServer.call(mc, {:handle_send_pdu_result, pdu, send_pdu_result})
  end

  def call(mc, request, timeout \\ @default_call_timeout) do
    GenServer.call(mc, {:call, request}, timeout)
  end

  def cast(mc, request) do
    GenServer.cast(mc, {:cast, request})
  end

  # GenServer callbacks

  def init([{module, args}, mc_opts, _ref, socket, transport, session]) do
    case module.init(socket, transport, args) do
      {:ok, state} ->
        timer_resolution = Keyword.get(mc_opts, :timer_resolution, @default_timer_resolution)
        timer_ref = Erlang.start_timer(timer_resolution, self, :emit_tick)

        enquire_link_limit = Keyword.get(mc_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(mc_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(mc_opts, :inactivity_limit, @default_inactivity_limit)
        session_init_limit = Keyword.get(mc_opts, :session_init_limit, @default_session_init_limit)

        time = Erlang.system_time(:milli_seconds)

        timers = SMPPTimers.new(
          time,
          session_init_limit,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )

        {:ok, pdu_storage} = PduStorage.start_link
        response_limit = Keyword.get(mc_opts, :response_limit, @default_response_limit)

        {:ok, %MC{
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
        new_st = %MC{ st | module_state: new_module_state }
        {:reply, reply, new_st}
      {:noreply, new_module_state} ->
        new_st = %MC{ st | module_state: new_module_state }
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
    new_st = %MC{ st | module_state: new_module_state }
    {:noreply, new_st}
  end

  def handle_info({:timeout, _timer_ref, :emit_tick}, st) do
    new_tick_timer_ref = Erlang.start_timer(st.timer_resolution, self, :emit_tick)
    Erlang.cancel_timer(st.tick_timer_ref)
    Kernel.send self, {:tick, Erlang.system_time(:milli_seconds)}
    {:noreply, %MC{ st | tick_timer_ref: new_tick_timer_ref }}
  end

  def handle_info({:tick, time}, st) do
    do_handle_tick(time, st)
  end

  def handle_info(request, st) do
    new_module_state = st.module.handle_info(request, st.module_state)
    new_st = %MC{ st | module_state: new_module_state }
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
    new_st = %MC{ st | module_state: new_module_state, timers: new_timers }
    if Pdu.bind?(pdu) do
      do_handle_bind(new_st)
    else
      {:reply, :ok, new_st}
    end
  end

  defp do_handle_resp(pdu, st) do
    sequence_number = Pdu.sequence_number(pdu)
    new_timers = SMPPTimers.handle_peer_action(st.timers, st.time)
    new_st = %MC{ st | timers: new_timers }
    case PduStorage.fetch(st.pdus, sequence_number) do
      [] ->
        Logger.info("mc #{inspect self}, resp for unknown pdu(sequence_number: #{sequence_number}), dropping")
        {:reply, :ok, new_st}
      [original_pdu] ->
        do_handle_resp_for_pdu(pdu, original_pdu, new_st)
    end
  end

  defp do_handle_resp_for_pdu(pdu, original_pdu, st) do
    new_module_state = st.module.handle_resp(pdu, original_pdu, st.module_state)
    new_st = %MC{ st | module_state: new_module_state }
    {:reply, :ok, new_st}
  end

  defp do_handle_bind(st) do
    new_timers = SMPPTimers.handle_bind(st.timers, st.time)
    new_st = %MC{ st | timers: new_timers }
    {:reply, :ok, new_st}
  end

  defp do_handle_stop(st) do
    _ = st.module.handle_stop(st.module_state)
    {:stop, :normal, :ok, st}
  end

  defp do_handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    new_st = %MC{ st | module_state: new_module_state }
    {:reply, :ok, new_st}
  end

  defp do_handle_tick(time, st) do
    expired_pdus = PduStorage.fetch_expired(st.pdus, time)
    new_st = do_handle_expired_pdus(expired_pdus, st)
    do_handle_timers(time, new_st)
  end

  defp do_handle_expired_pdus([], st), do: st
  defp do_handle_expired_pdus([pdu | pdus], st) do
    new_module_state = st.module.handle_resp_timeout(pdu, st.module_state)
    new_st = %MC{ st | module_state: new_module_state }
    do_handle_expired_pdus(pdus, new_st)
  end

  defp do_handle_timers(time, st) do
    case SMPPTimers.handle_tick(st.timers, time) do
      {:ok, new_timers} ->
        new_st = %MC{ st | timers: new_timers, time: time }
        {:noreply, new_st}
      {:stop, reason} ->
        Logger.info("mc #{inspect self}, being stopped by timers(#{reason})")
        Session.stop(st.smpp_session)
        {:noreply, st}
      {:enquire_link, new_timers} ->
        new_st = %MC{ st | timers: new_timers, time: time }
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
    new_pdu = %Pdu{ pdu | sequence_number: sequence_number}
    true = PduStorage.store(st.pdus, new_pdu, st.time + st.response_limit)
    Session.send_pdu(st.smpp_session, new_pdu)
    new_st = %MC{ st | sequence_number: sequence_number}
    new_st
  end

  defp do_reply(pdu, reply_pdu, st) do
    new_reply_pdu = %Pdu{ reply_pdu | sequence_number: pdu.sequence_number }
    Session.send_pdu(st.smpp_session, new_reply_pdu)
    st
  end

end

