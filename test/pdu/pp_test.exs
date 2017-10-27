defmodule SMPPEX.Pdu.PPTest do
  alias :erlang, as: Erlang

  alias SMPPEX.Pdu.PP
  alias SMPPEX.Pdu.Factory

  use ExUnit.Case

  test "format" do
    pdu = Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")

    # Just check that the function does not fail and that its result is printable
    assert pdu |> PP.format() |> Erlang.iolist_to_binary()
  end

  test "format: error status and tlvs" do
    pdu = SMPPEX.Pdu.new({0x01, 0x01, 0x01}, %{}, %{0x0005 => 1, 0x2222 => "val"})

    # Just check that the function does not fail and that its result is printable
    assert pdu |> PP.format() |> Erlang.iolist_to_binary()
  end
end
