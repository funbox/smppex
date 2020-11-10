defmodule SMPPEX.Integration.SSLTest do
  use ExUnit.Case

  alias Support.SSL.MC
  alias Support.SSL.ESME
  alias Support.TCP.Helpers
  alias SMPPEX.Pdu

  test "pdu exchange" do
    port = Helpers.find_free_port()
    {:ok, ref} = MC.start(port, "localhost.crt")
    {:ok, _pid} = ESME.start_link(port)

    receive do
      {bind_resp, bind} ->
        assert :bind_transceiver_resp == Pdu.command_name(bind_resp)
        assert :bind_transceiver == Pdu.command_name(bind)
    after
      1000 ->
        assert false
    end

    MC.stop(ref)
  end

  test "ssl handshake fail" do
    otp_release = :erlang.system_info(:otp_release)

    case :string.to_integer(otp_release) do
      {n, ''} when n >= 20 ->
        port = Helpers.find_free_port()
        {:ok, ref} = MC.start(port, "badhost.crt")
        {:error, {:tls_alert, _}} = ESME.start_link(port)
        MC.stop(ref)

      _ ->
        assert true
    end
  end

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
end
