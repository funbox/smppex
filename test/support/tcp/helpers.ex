defmodule Support.TCP.Helpers do

  def find_free_port do
    {:ok, socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(socket)
    :ok = :gen_tcp.close(socket)
    port # assume no one will immediately take this port
  end

end
