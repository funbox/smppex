defmodule SMPPEX.Pdu.OserlTest do

  use ExUnit.Case

  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Oserl

  test "to" do
    pdu = Pdu.new(
      {1, 2, 3},
      %{short_message: "message"},
      %{0x0424 => "message payload", 0x1424 => "any tlv field"}
    )

    assert {1, 2, 3, fields} = Oserl.to(pdu)
    assert [{5156, "any tlv field"}, {:message_payload, 'message payload'}, {:short_message, 'message'}] == Enum.sort(fields)
  end

  test "from" do
    oserl_pdu = {1, 2, 3, [{:message_payload, 'message payload'}, {:short_message, 'message'}, {5156, "any tlv field"}]}

    pdu = Oserl.from(oserl_pdu)

    assert 1 == Pdu.command_id(pdu)
    assert 2 == Pdu.command_status(pdu)
    assert 3 == Pdu.sequence_number(pdu)
    assert "message" == Pdu.mandatory_field(pdu, :short_message)
    assert "message payload" == Pdu.optional_field(pdu, :message_payload)
    assert "any tlv field" == Pdu.optional_field(pdu, 0x1424)
  end
end
