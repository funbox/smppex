defmodule SMPPEX.PduTest do
  use ExUnit.Case

  doctest SMPPEX.Pdu, except: [new: 3]

  test "new" do
    assert %SMPPEX.Pdu{command_id: 1, command_status: 0, mandatory: %{}, optional: %{}, sequence_number: 0} = SMPPEX.Pdu.new(1)
    assert %SMPPEX.Pdu{command_id: 1, command_status: 0,
    mandatory: %{password: "pass", system_id: "sid"}, optional: %{},
    sequence_number: 123} = SMPPEX.Pdu.new({1, 0, 123}, %{system_id: "sid", password: "pass"}, %{})
  end

end

