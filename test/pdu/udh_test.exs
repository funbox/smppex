defmodule SMPPEX.Pdu.UDHTest do
  use ExUnit.Case
  alias SMPPEX.Pdu.UDH
  alias SMPPEX.Pdu

  test "has_udh?" do
    pdu = Pdu.new({1,0,1}, %{esm_class: 0}, %{})
    refute UDH.has_udh?(pdu)

    pdu = Pdu.new({1,0,1}, %{esm_class: 0b01000000}, %{})
    assert UDH.has_udh?(pdu)
  end

  test "extract" do
    data = <<5, 0, 3, 197, 3, 3, "message">>
    assert UDH.extract(data) == {:ok, [{0, <<197, 3, 3>>}], "message"}

    data = <<0x0B, 0x05, 0x04, 0x06, 0x2d, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>
    assert UDH.extract(data) == {:ok, [{0x05, <<0x06, 0x2d, 0x00, 0x00>>}, {0x00, <<0x01, 0x02, 0x01>> }], "message"}

    data = <<0x10, "short">>
    assert {:error, _} = UDH.extract(data)

    data = <<0x06, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>
    assert {:error, _} = UDH.extract(data)

    data = <<5, 0, 4, 197, 3, 3, "message">>
    assert {:error, _} = UDH.extract(data)
  end

  test "add" do
    ies = [{0x05, <<0x06, 0x2d, 0x00, 0x00>>}, {0x00, <<0x01, 0x02, 0x01>> }]
    assert UDH.add(ies, "message") == {:ok, <<0x0B, 0x05, 0x04, 0x06, 0x2d, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>}

    ies = [{0, 123}]
    assert {:error, _} = UDH.add(ies, "message")

    ies = [{345, "ie"}]
    assert {:error, _} = UDH.add(ies, "message")

    ies = [{-1, "ie"}]
    assert {:error, _} = UDH.add(ies, "message")

    ies = [{0, <<1 :: integer-size(2040)>>}]
    assert {:error, _} = UDH.add(ies, "message")
  end

end

