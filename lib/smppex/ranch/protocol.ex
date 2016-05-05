defmodule SMPPEX.Ranch.Protocol do

  @behaviour :ranch_protocol
  use GenServer

  # Application.start(:ranch, :permanent)
  # :ranch_server.set_new_listener_opts(:foo, 400, [:foo, :bar])
  # :ranch_conns_sup.start_link(:foo, :worker, :brutal_kill, :ranch_tcp, 5000, SMPPEX.Ranch.Protocol)
  # {:ok, sock} = :gen_tcp.connect('rubybox.ru', 80, [])
  # :ranch_tcp.controlling_process(sock, s)
  # :ranch_conns_sup.start_protocol(s, sock)

  def start_link(ref, socket, transport, opts) do
	:proc_lib.start_link(__MODULE__, :init, [ref, socket, transport, opts])
  end

  def init(ref, socket, transport, opts) do
    :ok = :proc_lib.init_ack({:ok, self})
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    IO.puts "Entering SMPPEX.Ranch.Protocol loop with opts #{inspect opts}"
    :gen_server.enter_loop(__MODULE__, [], [])
  end

end


