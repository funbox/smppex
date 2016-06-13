defmodule SMPPEX.Pdu.PPTest do
  use ExUnit.Case

  alias SMPPEX.Pdu.PP
  alias SMPPEX.Pdu.Factory

  test "format" do
    pdu = Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    assert pdu |> PP.format |> :erlang.iolist_to_binary
  end

end


