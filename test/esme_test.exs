defmodule SMPPEX.ESMETest do
  use ExUnit.Case

  alias :gen_tcp, as: GenTCP
  alias :timer, as: Timer

  alias Support.TCP.Server
  alias Support.Session, as: SupportSession
  alias SMPPEX.ESME

  test "start_link" do
    server = Server.start_link()
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn -> [] end)
    handler = fn {:init, _socket, _transport}, st -> {:ok, st} end

    assert {:ok, _} =
             ESME.start_link(
               {127, 0, 0, 1},
               Server.port(server),
               {SupportSession, {pid, handler}}
             )
  end

  test "start_link by hostname" do
    server = Server.start_link()
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn -> [] end)
    handler = fn {:init, _socket, _transport}, st -> {:ok, st} end

    assert {:ok, _} =
             ESME.start_link('localhost', Server.port(server), {SupportSession, {pid, handler}})
  end

  test "start_link by hostname as a string" do
    server = Server.start_link()
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn -> [] end)
    handler = fn {:init, _socket, _transport}, st -> {:ok, st} end

    assert {:ok, _} =
             ESME.start_link("localhost", Server.port(server), {SupportSession, {pid, handler}})
  end

  test "start_link when MC is down" do
    server = Server.start_link()
    port = Server.port(server)
    {:ok, sock} = GenTCP.connect('localhost', port, [])
    :ok = GenTCP.close(sock)

    {:ok, pid} = Agent.start_link(fn -> [] end)
    handler = fn {:init, _socket, _transport}, st -> {:ok, st} end

    Process.flag(:trap_exit, true)

    assert {:error, :econnrefused} =
             ESME.start_link("localhost", port, {SupportSession, {pid, handler}})
  end
end
