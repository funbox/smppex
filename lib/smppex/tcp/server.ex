defmodule SMPPEX.TCP.Server do

  alias SMPPEX.TCP.Listener
  use GenServer

  @spec start_link(Listener.t, list) :: GenServer.on_start

  def start_link(client_handler, opts \\ []) do
    GenServer.start_link(__MODULE__, [client_handler], opts)
  end

  # GenServer

  @spec init([Listener.t]) :: {:ok, Listener.t}

  def init([client_handler]) do
    schedule_accept
    {:ok, Listener.init(client_handler)}
  end

  @spec handle_cast(:accept, Listener.t) :: {:noreply, Listener.t}

  def handle_cast(:accept, client_handler) do
    schedule_accept
    new_client_handler = Listener.accept(client_handler)
    {:noreply, new_client_handler}
  end

  # Private

  defp schedule_accept do
    GenServer.cast(self, :accept)
  end

end

