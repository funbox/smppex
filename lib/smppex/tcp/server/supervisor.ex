defmodule SMPPEX.TCP.Server.Supervisor do
  use Supervisor

  alias SMPPEX.TCP.Server
  alias SMPPEX.TCP.Listener

  @type connection_handler :: any

  @spec start_link(atom, Listener.listen_opts, connection_handler) :: Supervisor.on_start

  def start_link(name, listen_opts, connection_handler) do
    Supervisor.start_link(__MODULE__, [name, listen_opts, connection_handler], [name: name])
  end

  def stop(name) do
    case Process.whereis(name) do
      pid when is_pid(pid) -> Process.exit(pid, :normal)
      _ -> :not_found
    end
  end

  def init([name, listen_opts, connection_handler]) do
    listener = Listener.new(listen_opts, client_handler(client_supervisor_name(name), connection_handler))

    specs = [
      supervisor(SMPPEX.TCP.Server.ConnectionSupervisor, [client_supervisor_name(name)]),
      worker(Server, [listener, [name: server_name(name)]])
    ]

    supervise(specs, strategy: :one_for_one)
  end

  def client_supervisor_name(name) do
    String.to_atom "#{name}.Clients"
  end

  def server_name(name) do
    String.to_atom "#{name}.Server"
  end

  defp client_handler(connections_supervisor, connection_handler) do
    fn(socket) ->
      start_connection(connections_supervisor, socket, connection_handler)
    end
  end

  defp start_connection(connections_supervisor, socket, connection_handler) do
    spec = connection_spec(socket, connection_handler)
    {:ok, _} = Supervisor.start_child(
      connections_supervisor,
      spec
    )
  end

  defp connection_spec(socket, connection_handler) do
    worker(SMPPEX.TCP.Connection, [socket, connection_handler], restart: :temporary)
  end

end
