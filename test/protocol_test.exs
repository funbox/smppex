defmodule SMPPEX.ProtocolTest do
  use ExUnit.Case

  alias SMPPEX.Protocol
  alias SMPPEX.Pdu
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.RawPdu

  test "parse: insufficient data" do
    assert Protocol.parse(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5>>) ==
             {:ok, nil, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5>>}

    assert Protocol.parse(<<1, 2, 3>>) == {:ok, nil, <<1, 2, 3>>}
  end

  test "parse: bad command_length" do
    assert {:error, _} =
             Protocol.parse(<<00, 00, 00, 0x0F, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00>>)
  end

  test "parse: bad command_id" do
    parse_result =
      Protocol.parse(<<
        00,
        00,
        00,
        0x10,
        0x80,
        00,
        0x33,
        0x02,
        00,
        00,
        00,
        00,
        00,
        00,
        00,
        0x01,
        0xAA,
        0xBB,
        0xCC
      >>)

    assert {
             :ok,
             {
               :unparsed_pdu,
               %RawPdu{command_id: 2_147_496_706, command_status: 0, sequence_number: 1},
               _
             },
             <<0xAA, 0xBB, 0xCC>>
           } = parse_result
  end

  test "parse: bind_transmitter_resp" do
    parse_result =
      Protocol.parse(<<
        00,
        00,
        00,
        0x11,
        0x80,
        00,
        00,
        0x02,
        00,
        00,
        00,
        00,
        00,
        00,
        00,
        0x01,
        0x00,
        0xAA,
        0xBB,
        0xCC
      >>)

    assert {:ok, {:pdu, pdu}, tail} = parse_result

    assert tail == <<0xAA, 0xBB, 0xCC>>
    assert Pdu.command_id(pdu) == 0x80000002
    assert Pdu.command_status(pdu) == 0
    assert Pdu.sequence_number(pdu) == 1
    assert Pdu.field(pdu, :system_id) == ""
  end

  test "parse: bind_transmitter_resp, unsuccessful" do
    parse_result = Protocol.parse(<<0, 0, 0, 16, 128, 0, 0, 2, 0, 0, 0, 15, 0, 0, 0, 1>>)

    assert {:ok, {:pdu, pdu}, tail} = parse_result

    assert tail == <<>>
    assert Pdu.command_id(pdu) == 0x80000002
    assert Pdu.command_status(pdu) == 15
    assert Pdu.sequence_number(pdu) == 1
    assert Pdu.field(pdu, :system_id) == nil
  end

  test "parse: bind_transmitter" do
    data = <<
      0x00,
      0x00,
      0x00,
      0x30,
      0x00,
      0x00,
      0x00,
      0x02,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x01,
      0x74,
      0x65,
      0x73,
      0x74,
      0x5F,
      0x6D,
      0x6F,
      0x33,
      0x00,
      0x59,
      0x37,
      0x6C,
      0x48,
      0x7A,
      0x76,
      0x46,
      0x6A,
      0x00,
      0x63,
      0x6F,
      0x6D,
      0x6D,
      0x00,
      0x7B,
      0x01,
      0x02,
      0x72,
      0x61,
      0x6E,
      0x67,
      0x65,
      0x00
    >>

    parse_result = Protocol.parse(data)

    assert {:ok, {:pdu, pdu}, _} = parse_result

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

  test "parse: negative resp with nonempty body" do
    bin = <<
      0x00,
      0x00,
      0x00,
      0x21,
      0x80,
      0x00,
      0x00,
      0x04,
      0x00,
      0x00,
      0x00,
      0x0B,
      0x00,
      0x00,
      0x00,
      0x02,
      0x30,
      0x41,
      0x30,
      0x30,
      0x30,
      0x30,
      0x30,
      0x30,
      0x41,
      0x33,
      0x44,
      0x33,
      0x32,
      0x33,
      0x41,
      0x31,
      0x00
    >>

    assert {
             :ok,
             {:pdu,
              %SMPPEX.Pdu{command_id: 2_147_483_652, command_status: 11, sequence_number: 2}},
             ""
           } = Protocol.parse(bin)
  end

  test "parse: negative resp with nonempty malformed body" do
    bin = <<
      0x00,
      0x00,
      0x00,
      0x20,
      0x80,
      0x00,
      0x00,
      0x04,
      0x00,
      0x00,
      0x00,
      0x0B,
      0x00,
      0x00,
      0x00,
      0x02,
      0x30,
      0x41,
      0x30,
      0x30,
      0x30,
      0x30,
      0x30,
      0x30,
      0x41,
      0x33,
      0x44,
      0x33,
      0x32,
      0x33,
      0x41,
      0x31,
      0x00
    >>

    assert {
             :ok,
             {:pdu,
              %SMPPEX.Pdu{command_id: 2_147_483_652, command_status: 11, sequence_number: 2}},
             <<0>>
           } = Protocol.parse(bin)
  end

  test "parse: negative resp with empty body" do
    bin = <<
      0x00,
      0x00,
      0x00,
      0x10,
      0x80,
      0x00,
      0x00,
      0x04,
      0x00,
      0x00,
      0x00,
      0x0B,
      0x00,
      0x00,
      0x00,
      0x02
    >>

    assert {
             :ok,
             {:pdu,
              %SMPPEX.Pdu{command_id: 2_147_483_652, command_status: 11, sequence_number: 2}},
             <<>>
           } = Protocol.parse(bin)
  end

  test "build: bind_transmitter_resp" do
    data = <<00, 00, 00, 0x11, 0x80, 00, 00, 0x02, 00, 00, 00, 00, 00, 00, 00, 0x01, 0x00>>

    header = {0x80000002, 0, 1}
    mandatory_fields = %{system_id: ""}
    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == Protocol.build(pdu)
  end

  test "build: bind_transmitter_resp (unsuccessful)" do
    data = <<00, 00, 00, 0x10, 0x80, 00, 00, 0x02, 00, 00, 00, 0x0F, 00, 00, 00, 0x01>>

    header = {0x80000002, 0x0F, 1}
    mandatory_fields = %{system_id: "sid"}
    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == Protocol.build(pdu)
  end

  test "build: bind_transmitter" do
    data = <<
      0x00,
      0x00,
      0x00,
      0x30,
      0x00,
      0x00,
      0x00,
      0x02,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x01,
      0x74,
      0x65,
      0x73,
      0x74,
      0x5F,
      0x6D,
      0x6F,
      0x33,
      0x00,
      0x59,
      0x37,
      0x6C,
      0x48,
      0x7A,
      0x76,
      0x46,
      0x6A,
      0x00,
      0x63,
      0x6F,
      0x6D,
      0x6D,
      0x00,
      0x7B,
      0x01,
      0x02,
      0x72,
      0x61,
      0x6E,
      0x67,
      0x65,
      0x00
    >>

    header = {0x00000002, 0, 1}

    mandatory_fields = %{
      system_id: "test_mo3",
      password: "Y7lHzvFj",
      system_type: "comm",
      interface_version: 123,
      addr_ton: 1,
      addr_npi: 2,
      address_range: "range"
    }

    optional_fields = %{}
    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:ok, data} == Protocol.build(pdu)
  end

  test "build: delivery report" do
    data = <<
      0x00,
      0x00,
      0x00,
      0x33,
      0x00,
      0x00,
      0x00,
      0x05,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x01,
      0x02,
      0x31,
      0x32,
      0x33,
      0x00,
      0x04,
      0x05,
      0x34,
      0x35,
      0x36,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x1E,
      0x00,
      0x03,
      0x34,
      0x32,
      0x00,
      0x04,
      0x27,
      0x00,
      0x01,
      0x02
    >>

    message = ""
    # message_state_delivered
    message_state = 2
    source = {"123", 1, 2}
    dest = {"456", 4, 5}
    message_id = "42"

    pdu = SMPPEX.Pdu.Factory.delivery_report(message_id, source, dest, message, message_state)

    assert {:ok, data} == Protocol.build(pdu)
  end

  test "build failure (invalid optional field name)" do
    header = {0x00000002, 0, 1}

    mandatory_fields = %{
      system_id: "test_mo3",
      password: "Y7lHzvFj",
      system_type: "comm",
      interface_version: 123,
      addr_ton: 1,
      addr_npi: 2,
      address_range: "range"
    }

    optional_fields = %{
      invalid_field: "value"
    }

    pdu = Pdu.new(header, mandatory_fields, optional_fields)

    assert {:error, _} = Protocol.build(pdu)
  end

  test "build submit_sm with message_payload" do
    {:ok, command_id} = CommandNames.id_by_name(:submit_sm)

    pdu =
      Pdu.new(
        command_id,
        %{
          source_addr: "from",
          source_addr_ton: 5,
          source_addr_npi: 0,
          destination_addr: "to",
          dest_addr_ton: 1,
          dest_addr_npi: 1,
          short_message: "",
          registered_delivery: 0
        },
        %{
          message_payload: "message"
        }
      )

    assert {:ok, _} = Protocol.build(pdu)
  end
end
