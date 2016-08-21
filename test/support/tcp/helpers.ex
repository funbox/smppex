defmodule Support.TCP.Helpers do
  @moduledoc false

  alias :gen_tcp, as: GenTCP
  alias :inet, as: INET

  def find_free_port do
    {:ok, socket} = GenTCP.listen(0, [])
    {:ok, port} = INET.port(socket)
    :ok = GenTCP.close(socket)
    port # assume no one will immediately take this port
  end

end
