defmodule SMPPEX.Pdu.FactoryTest do
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory
  alias SMPPEX.Pdu.MessageState

  use ExUnit.Case

  test "delivery_report_for_submit_sm" do
    submit_sm = Factory.submit_sm(
      {"from", 1, 2},
      {"to", 3, 4},
      "message",
      1
    )

    delivery_report = Factory.delivery_report_for_submit_sm(
      "message_id",
      submit_sm,
      "dlr message",
      :DELIVERED
    )

    [
      destination_addr: "from",
      dest_addr_ton: 1,
      dest_addr_npi: 2,
      source_addr: "to",
      source_addr_ton: 3,
      source_addr_npi: 4,
      short_message: "dlr message",
      message_state: MessageState.code_by_name(:DELIVERED),
      receipted_message_id: "message_id"
    ] |> Enum.each(fn({field, value}) ->
      assert Pdu.field(delivery_report, field) == value
    end)
  end
end
