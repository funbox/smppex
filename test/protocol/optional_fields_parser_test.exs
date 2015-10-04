defmodule SMPPEX.Protocol.OptionalFieldsParserTest do
  use AssertHelpers
  use HexHelpers

  import SMPPEX.Protocol.OptionalFieldsParser

  test "parse" do
    data = <<00, 01, 00, 03, ?f, ?o, ?o, 01, 01, 00, 03, ?b, ?a, ?r>>
    assert {:ok, %{1 => "foo", 257 => "bar"}} == parse(data)
  end

end
