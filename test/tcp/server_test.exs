defmodule SMPPEX.TCP.ServerTest do

  use ExUnit.Case
  alias SMPPEX.TCP.Server

  defmodule ListenerSpy do
    defstruct listener: nil, spy_agent: nil
    alias ListenerSpy
    alias SMPPEX.TCP.Listener

    def new(opts, handler) do
      {:ok, spy_agent} = Agent.start_link(fn -> {0, []} end)
      %ListenerSpy{ listener: Listener.new(opts, handler), spy_agent: spy_agent }
    end

    def stop(listener_spy) do
      Agent.stop(listener_spy.spy_agent)
    end
  end

  defimpl SMPPEX.TCP.ClientHandler, for: ListenerSpy do

    alias SMPPEX.TCP.ClientHandler

    def init(listener_spy) do
      listener = ClientHandler.init(listener_spy.listener)
      Agent.update(listener_spy.spy_agent, fn({_, events}) -> {listener.port, events} end)
      %ListenerSpy{ listener_spy | listener: listener }
    end

    def accept(listener_spy) do
      listener = ClientHandler.accept(listener_spy.listener)
      Agent.update(listener_spy.spy_agent, fn({port, events}) -> {port, [:accept | events]} end)
      %ListenerSpy{ listener_spy | listener: listener }
    end

  end

  test "accepting connections" do

    {:ok, clients_agent} = Agent.start_link(fn -> [] end)

    client_handler = fn(client) ->
      Agent.update(clients_agent, fn(cs) -> [client | cs]  end)
      {:ok, spawn(fn -> :nop end)}
    end

    listener_spy = ListenerSpy.new({0, []}, client_handler)

    {:ok, server} = Server.start_link(listener_spy)

    {port, _} = Agent.get(listener_spy.spy_agent, fn(st) -> st end)

    {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, 0}])
    :timer.sleep(100)
    :ok = :gen_tcp.close(socket)

    {_, accepts} = Agent.get(listener_spy.spy_agent, fn(st) -> st end)

    assert accepts == [:accept]

    clients = Agent.get(clients_agent, fn(st) -> st end)

    assert [_] = clients

    [client] = clients

    assert is_port(client)

    try do
      :gen_server.stop(server, :kill, 1)
    catch
      :exit, _ -> :nop
    end
    Agent.stop(clients_agent)
    ListenerSpy.stop(listener_spy)
  end

end


