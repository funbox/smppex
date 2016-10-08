defmodule SMPPEX.Pdu.OserlTest do

  use ExUnit.Case

  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Oserl

  test "to" do
    pdu = Pdu.new(
      {1, 2, 3},
      %{short_message: "message"},
      %{0x0424 => "message payload"}
    )

    assert {1, 2, 3, fields} = Oserl.to(pdu)
    assert [{:message_payload, "message payload"}, {:short_message, "message"}] == Enum.sort(fields)
  end

  test "from" do
    oserl_pdu = {1, 2, 3, [{:message_payload, "message payload"}, {:short_message, "message"}]}

    pdu = Oserl.from(oserl_pdu)

    assert 1 == Pdu.command_id(pdu)
    assert 2 == Pdu.command_status(pdu)
    assert 3 == Pdu.sequence_number(pdu)
    assert "message" == Pdu.mandatory_field(pdu, :short_message)
    assert "message payload" == Pdu.optional_field(pdu, :message_payload)

  end
end
