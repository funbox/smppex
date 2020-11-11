defmodule SMPPEX.Integration.SocketEdgeCasesTest do
  use ExUnit.Case

  alias Support.SSL.MC
  alias Support.SSL.ESME
  alias Support.TCP.Helpers

  test "denying mc" do
    Process.flag(:trap_exit, true)

    port = Helpers.find_free_port()
    {:ok, ref} = MC.start(port, "localhost.crt", false)
    {:ok, pid} = ESME.start_link(port)

    receive do
      {:EXIT, ^pid, :socket_closed} -> :ok
      {:EXIT, ^pid, {:socket_error, :closed}} -> :ok
    after
      1000 ->
        assert false
    end

    MC.stop(ref)
  end

  test "socket closed before esme finishes initialization" do
    Process.flag(:trap_exit, true)

    port = Helpers.find_free_port()
    {:ok, ref} = MC.start(port, "localhost.crt", false)
    {:ok, pid} = ESME.start_link(port, 100)

    receive do
      {:EXIT, ^pid, :socket_closed} -> :ok
      {:EXIT, ^pid, {:socket_error, :closed}} -> :ok
    after
      1000 ->
        assert false
    end

    MC.stop(ref)
  end
end
