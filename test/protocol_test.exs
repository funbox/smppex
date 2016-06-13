defmodule SMPPEX.ProtocolTest do
  use ExUnit.Case

  import SMPPEX.Protocol
  alias SMPPEX.Pdu
  alias SMPPEX.RawPdu

  test "parse: insufficient data" do
    assert parse(<<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>) == {:ok, nil, <<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5>>}
    assert parse(<<1,2,3>>) == {:ok, nil, <<1,2,3>>}
  end

  test "parse: bad command_length" do
    assert {:error, _} = parse(<<00, 00, 00, 0x0F,   00, 00, 00, 00,   00, 00, 00, 00,   00, 00, 00, 00>>)
  end

  test "parse: bad command_id" do
    parse_result = parse(<<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    assert {:ok, {:unparsed_pdu, %RawPdu{command_id: 2147496706, command_status: 0, sequence_number: 1}, _}, <<0xAA, 0xBB, 0xCC>>} = parse_result
  end

  test "parse: bind_transmitter_resp" do
    parse_result = parse(<<00, 00, 00, 0x11,   0x80, 00, 00, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0x00, 0xAA, 0xBB, 0xCC>>)

    assert {:ok, {:pdu, _}, _} = parse_result

    {:ok, {:pdu, pdu}, tail} = parse_result

    assert tail == <<0xAA, 0xBB, 0xCC>>
    assert Pdu.command_id(pdu) == 0x80000002
    assert Pdu.command_status(pdu) == 0
    assert Pdu.sequence_number(pdu) == 1
    assert Pdu.field(pdu, :system_id) == ""
  end

  test "parse: bind_transmitter_resp, unsuccessful" do
    parse_result = parse(<<0, 0, 0, 16, 128, 0, 0, 2, 0, 0, 0, 15, 0, 0, 0, 1>>)

    assert {:ok, {:pdu, pdu}, tail} = parse_result

    assert tail == <<>>
    assert Pdu.command_id(pdu) == 0x80000002
    assert Pdu.command_status(pdu) == 15
    assert Pdu.sequence_number(pdu) == 1
    assert Pdu.field(pdu, :system_id) == nil
  end

  test "parse: bind_transmitter" do

    data = <<
      0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
      0x74, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x6f, 0x33, 0x00, 0x59, 0x37, 0x6c, 0x48, 0x7a, 0x76, 0x46,
      0x6a, 0x00, 0x63, 0x6f, 0x6d, 0x6d, 0x00, 0x7b, 0x01, 0x02, 0x72, 0x61, 0x6e, 0x67, 0x65, 0x00
    >>

    parse_result = parse(data)

    assert {:ok, {:pdu, _}, _} = parse_result

    {:ok, {:pdu, pdu}, _} = parse_result

    assert Pdu.command_id(pdu) == 0x00000002
    assert Pdu.command_status(pdu) == 0
    assert Pdu.sequence_number(pdu) == 1

    assert Pdu.field(pdu, :system_id) == "test_mo3"
    assert Pdu.field(pdu, :password) == "Y7lHzvFj"
    assert Pdu.field(pdu, :system_type) == "comm"
    assert Pdu.field(pdu, :interface_version) == 123
    assert Pdu.field(pdu, :addr_ton) == 1
    assert Pdu.field(pdu, :addr_npi) == 2
    assert Pdu.field(pdu, :address_range) == "range"

  end

  test "build: bind_transmitter_resp" do
    data = <<00, 00, 00, 0x11,   0x80, 00, 00, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0x00>>

    header = {0x80000002, 0, 1}
    mandatory_fields = %{system_id: ""}
    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == build(pdu)
  end

  test "build: bind_transmitter_resp (unsuccessful)" do
    data = <<00, 00, 00, 0x10,   0x80, 00, 00, 0x02,   00, 00, 00, 0x0f,   00, 00, 00, 0x01>>

    header = {0x80000002, 0x0f, 1}
    mandatory_fields = %{system_id: "sid"}
    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == build(pdu)
  end

  test "build: bind_transmitter" do

    data = <<
      0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
      0x74, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x6f, 0x33, 0x00, 0x59, 0x37, 0x6c, 0x48, 0x7a, 0x76, 0x46,
      0x6a, 0x00, 0x63, 0x6f, 0x6d, 0x6d, 0x00, 0x7b, 0x01, 0x02, 0x72, 0x61, 0x6e, 0x67, 0x65, 0x00
    >>

    header = {0x00000002, 0, 1}
    mandatory_fields = %{
      system_id: "test_mo3",
      password: "Y7lHzvFj",
      system_type: "comm",
      interface_version: 123,
      addr_ton: 1,
      addr_npi: 2,
      address_range: "range",
    }
    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == build(pdu)

  end
end
