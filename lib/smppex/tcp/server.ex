defprotocol SMPPEX.TCP.ClientHandler do
  def init(handler)
  def accept(handler)
end

defmodule SMPPEX.TCP.Server do

  alias SMPPEX.TCP.ClientHandler
  use GenServer

  def start_link(client_handler, opts \\ []) do
    GenServer.start_link(__MODULE__, [client_handler], opts)
  end

  def init([client_handler]) do
    schedule_accept
    {:ok, ClientHandler.init(client_handler)}
  end

  defp schedule_accept do
    GenServer.cast(self, :accept)
  end

  def handle_cast(:accept, client_handler) do
    schedule_accept
    new_client_handler = ClientHandler.accept(client_handler)
    {:noreply, new_client_handler}
  end
end

