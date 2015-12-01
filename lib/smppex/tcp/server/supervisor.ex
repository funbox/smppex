defmodule SMPPEX.TCP.Server.Supervisor do
  use Supervisor


  def start_link(name, listen_opts, connection_handler) do
    Supervisor.start_link(__MODULE__, [name, listen_opts, connection_handler])
  end

  def init([name, listen_opts, connection_handler]) do
    listener = SMPPEX.TCP.Listener.new(listen_opts, client_handler(client_supervisor_name(name), connection_handler))

    specs = [
      supervisor(Task.Supervisor, [[name: client_supervisor_name(name)]]),
      worker(SMPPEX.TCP.Server, [listener])
    ]

    supervise(specs, strategy: :one_for_one)
  end

  defp client_supervisor_name(name) do
    String.to_atom "#{name}.Clients"
  end

  defp client_handler(connections_supervisor, connection_handler) do
    fn(socket) ->
      start_connection(connections_supervisor, socket, connection_handler)
    end
  end

  defp start_connection(connections_supervisor, socket, connection_handler) do
    {:ok, _} = Supervisor.start_child(
      connections_supervisor,
      connection_spec(socket, connection_handler)
    )
  end

  defp connection_spec(socket, connection_handler) do
    worker(SMPPEX.TCP.Connection, [socket, connection_handler], restart: :temporary)
  end

end
