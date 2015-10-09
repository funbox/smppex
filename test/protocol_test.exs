defmodule SMPPEX.ProtocolTest do
  use ExUnit.Case
  use HexHelpers

  import SMPPEX.Protocol
  alias SMPPEX.InPdu

  test "parse: insufficient data" do
    assert parse(<<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>) == {:ok, nil, <<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>}
    assert parse(<<1,2,3>>) == {:ok, nil, <<1,2,3>>}
  end

  test "parse: bad command_length" do
    assert {:error, _} = parse(from_hex "00 00 00 0F   00 00 00 00   00 00 00 00   00 00 00 00")
  end

  test "parse: bad command_id" do
    parse_result = parse(from_hex "00 00 00 10   80 00 33 02   00 00 00 00   00 00 00 01   AA BB CC")

    assert {:ok, _, _} = parse_result

    {:ok, pdu, tail} = parse_result

    assert tail == from_hex("AA BB CC")
    assert InPdu.command_id(pdu) == 0x80003302
    assert InPdu.command_name(pdu) == nil
    assert InPdu.command_status(pdu) == 0
    assert InPdu.sequence_number(pdu) == 1
    assert InPdu.valid?(pdu) == false
  end

  test "parse: bind_transmitter_resp" do
    parse_result = parse(from_hex "00 00 00 11   80 00 00 02   00 00 00 00   00 00 00 01   00 AA BB CC")

    assert {:ok, _, _} = parse_result

    {:ok, pdu, tail} = parse_result

    assert tail == from_hex("AA BB CC")
    assert InPdu.command_id(pdu) == 0x80000002
    assert InPdu.command_name(pdu) == :bind_transmitter_resp
    assert InPdu.command_status(pdu) == 0
    assert InPdu.sequence_number(pdu) == 1
    assert InPdu.valid?(pdu) == true
    assert InPdu.get_field(pdu, :system_id) == ""
  end

  test "parse: bind_transmitter" do

    data =  """
      00 00 00 30 00 00 00 02 00 00 00 00 00 00 00 01
      74 65 73 74 5f 6d 6f 33 00 59 37 6c 48 7a 76 46
      6a 00 63 6f 6d 6d 00 7b 01 02 72 61 6e 67 65 00
    """

    parse_result = parse(from_hex data)

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

