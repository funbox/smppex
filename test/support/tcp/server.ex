defmodule Support.TCP.Server do
  @moduledoc false

  alias :gen_tcp, as: GenTCP
  alias :timer, as: Timer
  alias Support.TCP.Server

  defstruct [
    :server_pid,
    :received_data_pid,
    :port
  ]

  def start_link do
    port = Support.TCP.Helpers.find_free_port()

    {:ok, received_data_pid} = Agent.start_link(fn -> %{data: <<>>, messages: []} end)

    server_pid = spawn_link(fn -> listen(port, received_data_pid) end)

    %Server{server_pid: server_pid, received_data_pid: received_data_pid, port: port}
  end

  def port(server), do: server.port

  def stop(server) do
    Kernel.send(server.server_pid, :tcp_close)
    Agent.stop(server.received_data_pid)
  end

  def send(server, data) do
    Kernel.send(server.server_pid, {:tcp_send, data})
  end

  def messages(server) do
    Agent.get(server.received_data_pid, fn received_data ->
      Enum.reverse(received_data.messages)
    end)
  end

  def received_data(server) do
    Agent.get(server.received_data_pid, fn received_data -> received_data.data end)
  end

  defp listen(port, received_data_pid) do
    {:ok, listen_sock} = GenTCP.listen(port, [:binary, {:active, true}])
    {:ok, sock} = GenTCP.accept(listen_sock)
    :ok = GenTCP.close(listen_sock)
    loop(received_data_pid, sock)
    wait_for_shutdown()
  end

  defp loop(received_data_pid, sock) do
    receive do
      {:tcp, ^sock, data} = message ->
        Agent.update(received_data_pid, fn received_data ->
          %{data: received_data.data <> data, messages: [message | received_data.messages]}
        end)

        loop(received_data_pid, sock)

      {:tcp_closed, ^sock} = message ->
        Agent.update(received_data_pid, fn received_data ->
          %{received_data | messages: [message | received_data.messages]}
        end)

        GenTCP.close(sock)

      {:tcp_error, ^sock, _reason} = message ->
        Agent.update(received_data_pid, fn received_data ->
          %{received_data | messages: [message | received_data.messages]}
        end)

        GenTCP.close(sock)

      {:tcp_send, data} ->
        GenTCP.send(sock, data)
        loop(received_data_pid, sock)

      :tcp_close ->
        GenTCP.close(sock)
    after
      1_000 ->
        GenTCP.close(sock)
    end
  end

  defp wait_for_shutdown do
    Timer.sleep(1_000)
  end
end
