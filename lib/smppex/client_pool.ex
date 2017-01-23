defmodule SMPPEX.ClientPool do
  @moduledoc false

  alias :erlang, as: Erlang
  alias :ranch, as: Ranch
  alias :ranch_server, as: RanchServer
  alias :ranch_conns_sup, as: RanchConnsSup

  @default_capacity 500
  @default_transport :ranch_tcp
  @default_timeout 5000

  @type session :: term
  @type handler_result :: {:ok, session} | {:error, reason :: term}
  @type handler :: (Ranch.ref, term, module, pid -> handler_result)

  @type pool :: {pid, Ranch.ref, module}

  @spec start(handler, non_neg_integer, module, non_neg_integer) :: pool

  def start(handler, capacity \\ @default_capacity, transport \\ @default_transport, ack_timeout \\ @default_timeout) do
    ref = make_ref()
    RanchServer.set_new_listener_opts(ref, capacity, [{:handler, handler}])
    {:ok, pid} = RanchConnsSup.start_link(ref, :worker, :brutal_kill, transport, ack_timeout, SMPPEX.Session)
    {pid, ref, transport}
  end

  @spec stop(pool) :: :ok

  def stop({pid, ref, _transport}) do
    Erlang.unlink(pid)
    Erlang.exit(pid, :shutdown)
    RanchServer.cleanup_listener_opts(ref)
  end

  @spec start_session(pool, port) :: :ok

  def start_session({pid, _ref, transport}, socket) do
    transport.controlling_process(socket, pid)
    RanchConnsSup.start_protocol(pid, socket)
  end

  @spec ref(pool) :: Ranch.ref

  def ref({_pid, ref, _transport}), do: ref

end
