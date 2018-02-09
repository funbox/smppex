defmodule SMPPEX.Integration.SSLTest do
  use ExUnit.Case

  alias Support.SSL.MC
  alias Support.SSL.ESME
  alias Support.TCP.Helpers
  alias SMPPEX.Pdu

  test "pdu exchange" do
    port = Helpers.find_free_port()
    {:ok, ref} = MC.start(port)
    {:ok, _pid} = ESME.start_link(port)

    receive do
      {bind_resp, bind} ->
        assert :bind_transceiver_resp == Pdu.command_name(bind_resp)
        assert :bind_transceiver == Pdu.command_name(bind)
    after 1000 ->
      assert false
    end

    MC.stop(ref)
  end

end
