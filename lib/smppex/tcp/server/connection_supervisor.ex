defmodule SMPPEX.TCP.Server.ConnectionSupervisor do
  use Supervisor

  @spec start_link(atom) :: Supervisor.on_start

  def start_link(name) do
    Supervisor.start_link(__MODULE__, [], [name: name])
  end

  def init([]) do
    supervise([], strategy: :one_for_one)
  end

end
