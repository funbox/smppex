defmodule SMPPEX.TCP.ServerTest do

  use ExUnit.Case
  alias SMPPEX.TCP.Server
  alias SMPPEX.TCP.Listener

  alias Support.TCP.Helpers, as: TCPHelpers
  alias Support.GenServerHelpers

  test "accepting connections" do

    {:ok, clients_agent} = Agent.start_link(fn -> [] end)

    client_handler = fn(client) ->
      Agent.update(clients_agent, fn(cs) -> [client | cs]  end)
      {:ok, spawn(fn -> :nop end)}
    end

    port = TCPHelpers.find_free_port

    listener = Listener.new({port, []}, client_handler)

    {:ok, server} = Server.start_link(listener)

    {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, 0}])
    :timer.sleep(10)
    :ok = :gen_tcp.close(socket)

    clients = Agent.get(clients_agent, fn(st) -> st end)

    assert [_] = clients

    [client] = clients

    assert is_port(client)

    Agent.stop(clients_agent)
    GenServerHelpers.stop_gen_server(server)
  end

end


