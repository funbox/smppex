defmodule SMPPEX.TCP.Server do

  alias SMPPEX.TCP.Listener
  use GenServer

  @spec start_link(Listener.t, list) :: GenServer.on_start

  def start_link(listener, opts \\ []) do
    GenServer.start_link(__MODULE__, [listener], opts)
  end

  # GenServer

  @spec init([Listener.t]) :: {:ok, Listener.t}

  def init([listener]) do
    schedule_accept
    {:ok, Listener.init(listener)}
  end

  @spec handle_cast(:accept, Listener.t) :: {:noreply, Listener.t}

  def handle_cast(:accept, listener) do
    schedule_accept
    new_listener = Listener.accept(listener)
    {:noreply, new_listener}
  end

  # Private

  defp schedule_accept do
    GenServer.cast(self, :accept)
  end

end

