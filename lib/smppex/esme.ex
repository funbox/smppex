defmodule SMPPEX.ESME.Session do
  defstruct [
    :esme
  ]
end

defimpl SMPPEX.SMPPHandler, for: SMPPEX.ESME.Session do

  alias SMPPEX.ESME.Session, as: ESMESession
  alias SMPPEX.ESME, as: ESME

  require Logger

  def after_init(_session) do
  end

  def handle_parse_error(session, error) do
    Logger.info("esme #{session.esme}, parse error: #{error}, stopping")
  end

  def handle_pdu(session, {:unparsed_pdu, raw_pdu, error}) do
    Logger.info("esme #{session.esme}, unknown pdu: #{inspect raw_pdu}(#{error}), stopping")
    :stop
  end

  def handle_pdu(session, {:pdu, pdu}) do
    :ok = ESME.handle_pdu(session.esme, pdu)
    :ok
  end

  def handle_socket_closed(session) do
    Logger.info("esme #{session.esme}, socket closed, stopping")
  end

  def handle_socket_error(session, reason) do
    Logger.info("esme #{session.esme}, socket error #{reason}, stopping")
  end

  def handle_stop(session) do
    :ok = ESME.handle_stop(session.esme)
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    :ok = ESME.handle_send_pdu_result(session.esme, pdu, send_pdu_result)
    session
  end
end

defmodule SMPPEX.ESME do

  alias SMPPEX.ESME
  alias SMPPEX.ESME.Session, as: ESMESession
  alias SMPPEX.Pdu
  alias SMPPEX.PduStorage
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
    :bound,
    :sequence_number,
    :time
  ]

  @default_timeout 5000
  @default_enquire_link_limit 30000
  @default_enquire_link_resp_limit 30000
  @default_inactivity_limit :infinity
  @default_response_limit 60000

  @default_timer_resolution 100

  @type state :: any
  @type args :: any
  @type reason :: any

  @callback init(args) :: {:ok, state} | {:close, reason}

  @callback handle_pdu(Pdu.t, state) :: state

  @callback handle_resp(Pdu.t, Pdu.t, state) :: state

  @callback handle_resp_timeout(Pdu.t, state) :: state

  @callback handle_send_pdu_result(Pdu.t, SMPPEX.SMPPHandler.send_pdu_result, state) :: state

  @callback handle_close(state) :: any

  def start_link(host, port, {module, args}, opts \\ []) do
    transport = Keyword.get(opts, :transport, :tcp)
    gen_server_opts = Keyword.get(opts, :gen_server_opts, [])
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    esme_opts = Keyword.get(opts, :esme_opts, [])
    GenServer.start_link(
      __MODULE__,
      [host, port, {module, args}, ranch_transport(transport), timeout, esme_opts],
      gen_server_opts
    )
  end

  def send_pdu(esme, pdu) do
    GenServer.cast(esme, {:send_pdu, pdu})
  end

  def reply(esme, pdu, reply_pdu) do
    GenServer.cast(esme, {:reply, pdu, reply_pdu})
  end

  def handle_pdu(esme, pdu) do
    GenServer.call(esme, {:handle_pdu, pdu})
  end

  def handle_stop(esme) do
    GenServer.call(esme, :handle_stop)
  end

  def handle_send_pdu_result(esme, pdu, send_pdu_result) do
    GenServer.call(esme, {:handle_send_pdu_result, pdu, send_pdu_result})
  end

  def init([host, port, mod_with_args, transport, timeout, esme_opts]) do
    esme = self
    handler = fn(ref, _socket, _transport, session) ->
      Kernel.send esme, {ref, session}
      %ESMESession{
        esme: esme
      }
    end

    case start_session(handler, host, port, transport, timeout) do
      {:ok, pool, session} ->
        init_esme(mod_with_args, pool, session, esme_opts)
      {:error, reason} -> {:stop, reason}
    end
  end

  def handle_call({:handle_pdu, pdu}, _from, st) do
    case resp?(pdu) do
      true -> do_handle_pdu(pdu, st)
      false -> do_handle_resp(pdu, st)
    end
  end

  def handle_call(:handle_stop, _from, st) do
    do_handle_stop(st)
  end

  def handle_call({:handle_send_pdu_result, pdu, send_pdu_result}, _from, st) do
    do_handle_send_pdu_result(pdu, send_pdu_result, st)
  end

  def handle_cast({:send_pdu, pdu}, st) do
    new_st = do_send_pdu(pdu, st)
    {:noreply, new_st}
  end

  def handle_cast({:reply, pdu, reply_pdu}, st) do
    new_st = do_reply(pdu, reply_pdu, st)
    {:noreply, new_st}
  end

  def handle_info({:tick, time}, st) do
    do_handle_tick(time, st)
  end

  defp start_session(handler, host, port, transport, timeout) do
    case transport.connect(host, port, timeout) do
      {:ok, socket} ->
        pool = ClientPool.start(handler, 1, transport, timeout)
        ClientPool.start_session(pool, socket)
        ref = ClientPool.ref(pool)
        receive do
          {^ref, session} -> {:ok, pool, session}
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
        SMPPEX.Timer.start_link(self, timer_resolution)

        enquire_link_limit = Keyword.get(esme_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(esme_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(esme_opts, :inactivity_limit, @default_inactivity_limit)

        time = :erlang.system_time(:milli_seconds)

        timers = SMPPTimers.new(
          time,
          :infinity,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )

        {:ok, pdu_storage} = PduStorage.start_link
        response_limit = Keyword.get(esme_opts, :response_limit, @default_response_limit)

        {:ok, %ESME{
          client_pool: pool,
          smpp_session: session,
          module: module,
          module_state: state,
          timers: timers,
          pdus: pdu_storage,
          response_limit: response_limit,
          bound: false,
          sequence_number: 1,
          time: time
        }}
      {:stop, _} = stop ->
        ClientPool.stop(pool)
        stop
    end
  end

  defp ranch_transport(:tcp), do: :ranch_tcp
  defp ranch_transport(:ssl), do: :ranch_ssl

  defp resp?(pdu), do: Pdu.command_id(pdu) > 0x80000000

  def do_handle_pdu(pdu, st) do
    new_module_state = st.module.handle_pdu(pdu, st.module_state)
    new_timers = SMPPTimers.handle_peer_transaction(st.timers, st.time)
    {:reply, :ok, %ESME{ st | module_state: new_module_state, timers: new_timers }}
  end

  def do_handle_resp(pdu, st) do
    sequence_number = Pdu.sequence_number(pdu)
    new_timers = SMPPTimers.handle_peer_action(st.timers, st.time)
    new_st = %ESME{ st | timers: new_timers }
    case PduStorage.fetch(st.pdus, sequence_number) do
      [] ->
        Logger.info("esme #{self}, resp for unknown pdu(sequence_number: #{sequence_number}), dropping")
        {:reply, :ok, new_st}
      [original_pdu] ->
        do_handle_resp_for_pdu(pdu, original_pdu, new_st)
    end
  end

  def do_handle_resp_for_pdu(pdu, original_pdu, st) do
    new_module_state = st.module.handle_resp(pdu, original_pdu, st.module_state)
    new_st = %ESME{ st | module_state: new_module_state }
    case bind_resp?(pdu) do
      true -> do_handle_bind_resp(pdu, new_st)
      false -> {:reply, :ok, new_st}
    end
  end

  defp do_handle_bind_resp(pdu, st) do
    case success_resp?(pdu) do
      true ->
        new_timers = SMPPTimers.handle_bind(st.timers, st.time)
        new_st = %ESME{ st | timers: new_timers, bound: true }
        {:reply, :ok, new_st}
      false ->
        Logger.info("esme #{self}, bind failed with status #{Pdu.command_status(pdu)}, stopping")
        Session.stop(st.session)
    end
  end

  defp success_resp?(pdu) do
    Pdu.command_status(pdu) == 0
  end

  defp bind_resp?(pdu) do
    command_id = Pdu.command_id(pdu)
    {:ok, command_name} = CommandNames.name_by_id(command_id)
    command_name == :bind_receiver_resp or command_name == :bind_transmitter_resp or command_name == :bind_transceiver_resp
  end

  defp do_handle_stop(st) do
    _ = st.module.handle_stop(st.module_state)
    ClientPool.stop(st.pool)
    {:stop, :stop, :ok, st}
  end

  defp do_handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    new_st = %ESME{ st | module_state: new_module_state }
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
    new_st = %ESME{ st | module_state: new_module_state }
    do_handle_expired_pdus(pdus, new_st)
  end

  defp do_handle_timers(time, st) do
    case SMPPTimers.handle_tick(st.timers, time) do
      {:ok, new_timers} ->
        new_st = %ESME{ timers: new_timers, time: time }
        {:noreply, new_st}
      {:stop, reason} ->
        Logger.info("esme #{self}, being stopped by timers(#{reason})")
        Session.stop(st.session)
      {:enquire_link, new_timers} ->
        new_st = %ESME{ timers: new_timers, time: time }
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
    true = PduStorage.store(st.pdus, pdu, st.time + st.response_limit)
    Session.send_pdu(st.session, new_pdu)
    new_st = %ESME{ st | sequence_number: sequence_number}
    new_st
  end

  defp do_reply(pdu, reply_pdu, st) do
    new_reply_pdu = %Pdu{ reply_pdu | sequence_number: pdu.sequence_number }
    Session.send_pdu(st.session, new_reply_pdu)
    st
  end

end
