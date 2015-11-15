defmodule SMPPEX.TCP.ServerTest do

  use ExUnit.Case
  alias SMPPEX.TCP.Server
  alias SMPPEX.TCP.Listener

  def stop_gen_server(pid) do
    try do
      :gen_server.stop(pid, :kill, 1)
    catch
      :exit, _ -> :nop
    end
  end

  def find_free_port do
    {:ok, socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(socket)
    :ok = :gen_tcp.close(socket)
    port # assume no one will immediately take this port
  end

  test "accepting connections" do

    {:ok, clients_agent} = Agent.start_link(fn -> [] end)

    client_handler = fn(client) ->
      Agent.update(clients_agent, fn(cs) -> [client | cs]  end)
      {:ok, spawn(fn -> :nop end)}
    end

    port = find_free_port

    listener = Listener.new({port, []}, client_handler)

    {:ok, server} = Server.start_link(listener)

    {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, 0}])
    :timer.sleep(100)
    :ok = :gen_tcp.close(socket)

    clients = Agent.get(clients_agent, fn(st) -> st end)

    assert [_] = clients

    [client] = clients

    assert is_port(client)

    Agent.stop(clients_agent)
    stop_gen_server(server)
  end

end


