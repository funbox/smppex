defmodule SMPPEX.ClientPool do

  alias :erlang, as: Erlang
  alias :ranch_server, as: RanchServer
  alias :ranch_conns_sup, as: RanchConnsSup

  @default_capacity 500
  @default_transport :ranch_tcp
  @default_timeout 5000

  def start(handler, capacity \\ @default_capacity, transport \\ @default_transport, ack_timeout \\ @default_timeout) do
    ref = make_ref
    RanchServer.set_new_listener_opts(ref, capacity, [{:handler, handler}])
    {:ok, pid} = RanchConnsSup.start_link(ref, :worker, :brutal_kill, transport, ack_timeout, SMPPEX.Session)
    {pid, ref, transport}
  end

  def stop({pid, ref, _transport}) do
    Erlang.unlink(pid)
    Erlang.exit(pid, :shutdown)
    RanchServer.cleanup_listener_opts(ref)
  end

  def start_session({pid, _ref, transport}, socket) do
    transport.controlling_process(socket, pid)
    RanchConnsSup.start_protocol(pid, socket)
  end

  def ref({_pid, ref, _transport}), do: ref

end
