defmodule SMPPEX.Pdu.FactoryTest do
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory
  alias SMPPEX.Pdu.MessageState

  use ExUnit.Case

  test "bind_transmitter" do
    assert %Pdu{} = Factory.bind_transmitter("system_id", "password", %{system_type: "type"})
  end

  test "bind_receiver" do
    assert %Pdu{} = Factory.bind_receiver("system_id", "password", %{system_type: "type"})
  end

  test "bind_transceiver" do
    assert %Pdu{} = Factory.bind_transceiver("system_id", "password", %{system_type: "type"})
  end

  test "bind_transmitter_resp" do
    assert %Pdu{} = Factory.bind_transmitter_resp(0, "smpp mc")
  end

  test "bind_receiver_resp" do
    assert %Pdu{} = Factory.bind_receiver_resp(0, "smpp mc")
  end

  test "bind_transceiver_resp" do
    assert %Pdu{} = Factory.bind_transceiver_resp(0, "smpp mc")
  end

  test "unbind" do
    assert %Pdu{} = Factory.unbind()
  end

  test "unbind_resp" do
    assert %Pdu{} = Factory.unbind_resp()
  end

  test "enquire_link" do
    assert %Pdu{} = Factory.enquire_link()
  end

  test "enquire_link_resp" do
    assert %Pdu{} = Factory.enquire_link_resp()
  end

  test "submit_sm" do
    assert %Pdu{} = Factory.submit_sm({"from", 0, 0}, {"to", 0, 0}, "hello", 1)
  end

  test "submit_sm_resp" do
    assert %Pdu{} = Factory.submit_sm_resp(0, "message_id")
    assert %Pdu{} = Factory.submit_sm_resp(1)
  end

  test "deliver_sm" do
    assert %Pdu{} = Factory.deliver_sm({"from", 0, 0}, {"to", 0, 0}, "hello")
  end

  test "deliver_sm_resp" do
    assert %Pdu{} = Factory.deliver_sm_resp(0)
  end

  test "delivery_report" do
    assert %Pdu{} = Factory.delivery_report("message_id", {"to", 0, 0}, {"from", 0, 0})

    assert %Pdu{} =
             Factory.delivery_report(
               "message_id",
               {"to", 0, 0},
               {"from", 0, 0},
               "hello",
               :UNDELIVERABLE
             )
  end

  test "delivery_report_for_submit_sm" do
    submit_sm =
      Factory.submit_sm(
        {"from", 1, 2},
        {"to", 3, 4},
        "message",
        1
      )

    delivery_report =
      Factory.delivery_report_for_submit_sm(
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
    ]
    |> Enum.each(fn {field, value} ->
      assert Pdu.field(delivery_report, field) == value
    end)
  end
end
