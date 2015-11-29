defprotocol SMPPEX.TCP.ClientHandler do

  @spec init(h) :: h when h: var
  def init(handler)

  @spec accept(h) :: h when h: var
  def accept(handler)
end

defmodule SMPPEX.TCP.Server do

  alias SMPPEX.TCP.ClientHandler
  use GenServer

  @spec start_link(any, list) :: GenServer.on_start

  def start_link(client_handler, opts \\ []) do
    GenServer.start_link(__MODULE__, [client_handler], opts)
  end

  # GenServer

  def init([client_handler]) do
    schedule_accept
    {:ok, ClientHandler.init(client_handler)}
  end

  def handle_cast(:accept, client_handler) do
    schedule_accept
    new_client_handler = ClientHandler.accept(client_handler)
    {:noreply, new_client_handler}
  end

  # Private

  defp schedule_accept do
    GenServer.cast(self, :accept)
  end

end

