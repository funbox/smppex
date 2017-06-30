defmodule SMPPEX.PduTest do
  use ExUnit.Case

  doctest SMPPEX.Pdu, except: [new: 3]

  test "new" do
    assert %SMPPEX.Pdu{command_id: 1, command_status: 0, mandatory: %{}, optional: %{}, sequence_number: 0} = SMPPEX.Pdu.new(1)
    assert %SMPPEX.Pdu{command_id: 1, command_status: 0,
    mandatory: %{password: "pass", system_id: "sid"}, optional: %{},
    sequence_number: 123} = SMPPEX.Pdu.new({1, 0, 123}, %{system_id: "sid", password: "pass"}, %{})
  end

  test "setting optional field doesn't allow duplicates" do
    pdu = SMPPEX.Pdu.new({1, 1, 1}, %{}, %{1060 => "message1", message_payload: "message2"})
    pdu_with_changed_payload = SMPPEX.Pdu.set_optional_field(pdu, :message_payload, "message3")
    assert pdu_with_changed_payload.optional == %{message_payload: "message3"}
    pdu_with_changed_payload = SMPPEX.Pdu.set_optional_field(pdu, 1060, "message3")
    assert pdu_with_changed_payload.optional == %{1060 => "message3"}
  end

end

