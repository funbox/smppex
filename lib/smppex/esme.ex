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
    ESME.handle_pdu(session.esme, pdu)
    :ok
  end

  def handle_socket_closed(session) do
    Logger.info("esme #{session.esme}, socket closed, stopping")
  end

  def handle_socket_error(session, reason) do
    Logger.info("esme #{session.esme}, socket error #{reason}, stopping")
  end

  def handle_stop(session) do
    ESME.handle_stop(session.esme)
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    ESME.handle_send_pdu_result(session.esme, pdu, send_pdu_result)
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

  defstruct [
    :client_pool,
    :smpp_session,
    :module,
    :module_state,
    :timers,
    :pdus,
    :response_limit,
    :bound,
    :sequence_number
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

        timers = SMPPTimers.new(
          :erlang.system_time(:milli_seconds),
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
          sequence_number: 1
        }}
      {:stop, _} = stop ->
        ClientPool.stop(pool)
        stop
    end
  end

  defp ranch_transport(:tcp), do: :ranch_tcp
  defp ranch_transport(:ssl), do: :ranch_ssl

  def handle_pdu(esme, pdu) do
  end

  def handle_stop(esme) do
  end

  def handle_send_pdu_result(esme, pdu, send_pdu_result) do
  end

end
