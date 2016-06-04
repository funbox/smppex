defmodule SMPPEX.ESME.Session do
  defstruct [
    :esme_pid
  ]
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

  @type state :: any
  @type args :: any
  @type reason :: any

  @callback init(args) :: {:ok, state} | {:close, reason}

  @callback handle_pdu(Pdu.t, state) :: state

  @callback handle_resp(Pdu.t, Pdu.t, state) :: state

  @callback handle_resp_timeout(Pdu.t, state) :: state

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
    esme_pid = self
    handler = fn(ref, _socket, _transport, session) ->
      Kernel.send esme_pid, {ref, session}
      %ESMESession{
        esme_pid: esme_pid
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
        enquire_link_limit = Keyword.get(esme_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(esme_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(esme_opts, :inactivity_limit, @default_inactivity_limit)
        response_limit = Keyword.get(esme_opts, :response_limit, @default_response_limit)

        timers = SMPPTimers.new(
          :erlang.system_time(:milli_seconds),
          :infinity,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )

        {:ok, pdu_storage} = PduStorage.start_link

        %ESME{
          client_pool: pool,
          smpp_session: session,
          module: module,
          module_state: state,
          timers: timers,
          pdus: pdu_storage,
          response_limit: response_limit,
          bound: false,
          sequence_number: 1
        }
      {:stop, _} = stop ->
        ClientPool.stop(pool)
        stop
    end
  end

  defp ranch_transport(:tcp), do: :ranch_tcp
  defp ranch_transport(:ssl), do: :ranch_ssl



end
