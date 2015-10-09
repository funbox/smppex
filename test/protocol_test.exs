defmodule SMPPEX.ProtocolTest do
  use ExUnit.Case

  import SMPPEX.Protocol
  alias SMPPEX.InPdu

  test "parse: insufficient data" do
    assert parse(<<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>) == {:ok, nil, <<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>}
    assert parse(<<1,2,3>>) == {:ok, nil, <<1,2,3>>}
  end

  test "parse: bad command_length" do
    assert {:error, _} = parse(<<00, 00, 00, 0x0F,   00, 00, 00, 00,   00, 00, 00, 00,   00, 00, 00, 00>>)
  end

  test "parse: bad command_id" do
    parse_result = parse(<<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    assert {:ok, _, _} = parse_result

    {:ok, pdu, tail} = parse_result

    assert tail == <<0xAA, 0xBB, 0xCC>>
    assert InPdu.command_id(pdu) == 0x80003302
    assert InPdu.command_name(pdu) == nil
    assert InPdu.command_status(pdu) == 0
    assert InPdu.sequence_number(pdu) == 1
    assert InPdu.valid?(pdu) == false
  end

  test "parse: bind_transmitter_resp" do
    parse_result = parse(<<00, 00, 00, 0x11,   0x80, 00, 00, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0x00, 0xAA, 0xBB, 0xCC>>)

    assert {:ok, _, _} = parse_result

    {:ok, pdu, tail} = parse_result

    assert tail == <<0xAA, 0xBB, 0xCC>>
    assert InPdu.command_id(pdu) == 0x80000002
    assert InPdu.command_name(pdu) == :bind_transmitter_resp
    assert InPdu.command_status(pdu) == 0
    assert InPdu.sequence_number(pdu) == 1
    assert InPdu.valid?(pdu) == true
    assert InPdu.get_field(pdu, :system_id) == ""
  end

  test "parse: bind_transmitter" do

    data = <<
      0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
      0x74, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x6f, 0x33, 0x00, 0x59, 0x37, 0x6c, 0x48, 0x7a, 0x76, 0x46,
      0x6a, 0x00, 0x63, 0x6f, 0x6d, 0x6d, 0x00, 0x7b, 0x01, 0x02, 0x72, 0x61, 0x6e, 0x67, 0x65, 0x00
    >>

    parse_result = parse(data)

    assert {:ok, _, _} = parse_result

    {:ok, pdu, _} = parse_result

    assert InPdu.command_id(pdu) == 0x00000002
    assert InPdu.command_name(pdu) == :bind_transmitter
    assert InPdu.command_status(pdu) == 0
    assert InPdu.sequence_number(pdu) == 1
    assert InPdu.valid?(pdu) == true

    assert InPdu.get_field(pdu, :system_id) == "test_mo3"
    assert InPdu.get_field(pdu, :password) == "Y7lHzvFj"
    assert InPdu.get_field(pdu, :system_type) == "comm"
    assert InPdu.get_field(pdu, :interface_version) == 123
    assert InPdu.get_field(pdu, :addr_ton) == 1
    assert InPdu.get_field(pdu, :addr_npi) == 2
    assert InPdu.get_field(pdu, :address_range) == "range"

  end

end

