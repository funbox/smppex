defmodule SMPPEX.Protocol.OptionalFieldsParserTest do
  use ExUnit.Case
  use HexHelpers

  import SMPPEX.Protocol.OptionalFieldsParser

  test "parse unknown" do
    data = <<00, 01, 00, 03, ?f, ?o, ?o, 01, 01, 00, 03, ?b, ?a, ?r>>
    assert {:ok, %{1 => "foo", 257 => "bar"}} == parse(data)
  end

  test "parse known valid" do
    data = <<00, 05, 00, 01, 123>>
    assert {:ok, %{5 => 123}} == parse(data)

    data = <<00, 0x1D, 00, 03, ?a, ?b, 0>>
    assert {:ok, %{0x1D => "ab"}} == parse(data)

    data = <<0x02, 0x03, 00, 03, ?a, ?b, ?c>>
    assert {:ok, %{0x0203 => "abc"}} == parse(data)
  end

  test "parse known invalid" do
    data = <<00, 05, 00, 02, 123, 123>>
    assert {:error, _} = parse(data)

    data = <<00, 0x1D, 00, 03, ?a, ?b, ?c>>
    assert {:error, _} = parse(data)

    data = <<0x02, 0x03, 00, 01, ?a >>
    assert {:error, _} = parse(data)
  end
end
