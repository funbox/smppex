defmodule SMPPEX.ClientPool do

  @default_capacity 500
  @default_transport :ranch_tcp
  @default_timeout 5000

  def start(handler, capacity \\ @default_capacity, transport \\ @default_transport, ack_timeout \\ @default_timeout) do
    ref = make_ref
    :ranch_server.set_new_listener_opts(ref, capacity, [{:handler, handler}])
    {:ok, pid} = :ranch_conns_sup.start_link(ref, :worker, :brutal_kill, transport, ack_timeout, SMPPEX.Session)
    {pid, ref}
  end

  def stop({pid, ref}) do
    :erlang.unlink(pid)
    :erlang.exit(pid, :shutdown)
    :ranch_server.cleanup_listener_opts(ref)
  end

  def start_session({pid, _ref}, socket) do
    :ranch_tcp.controlling_process(socket, pid)
    :ranch_conns_sup.start_protocol(pid, socket)
  end

end
