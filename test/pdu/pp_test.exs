defmodule SMPPEX.Pdu.PPTest do
  alias :erlang, as: Erlang

  alias SMPPEX.Pdu.PP
  alias SMPPEX.Pdu.Factory

  use ExUnit.Case

  test "format" do
    pdu = Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    assert pdu |> PP.format |> Erlang.iolist_to_binary
  end

end


