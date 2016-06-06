defmodule SMPPEX.ESMETest do
  use ExUnit.Case

  alias Support.TCP.Server
  alias Support.ESME, as: SupportESME
  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  setup do
    server = Server.start_link
    :timer.sleep(50)
    {st_store, esme} = SupportESME.start_link({127,0,0,1}, Server.port(server))

    {:ok, esme: esme, st_store: st_store, server: server}
  end

  test "start_link", context do

    server = Server.start_link
    :timer.sleep(50)
    assert {:ok, _} = SMPPEX.ESME.start_link({127,0,0,1}, Server.port(server), {SupportESME, context[:st_store]})

  end

  test "init", context do

    assert [{:init}] == SupportESME.callbacks_received(context[:st_store])
    assert context[:st_store] == ESME.call(context[:esme], fn(st) -> st end)

  end

  test "send_pdu", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    ESME.send_pdu(context[:esme], pdu)

    :timer.sleep(50)

    assert {:ok, {:pdu, pdu1}, _} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.mandatory_field(pdu, :system_id) == Pdu.mandatory_field(pdu1, :system_id)
    assert Pdu.mandatory_field(pdu, :password) == Pdu.mandatory_field(pdu1, :password)
  end

  test "send_pdu sequence_numbers", context do
    pdu1 = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu2 = SMPPEX.Pdu.Factory.bind_transceiver("system_id", "password")

    ESME.send_pdu(context[:esme], pdu1)
    ESME.send_pdu(context[:esme], pdu2)

    :timer.sleep(50)

    assert {:ok, {:pdu, pdu1r}, rest_data} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, pdu2r}, _} = rest_data |> SMPPEX.Protocol.parse


    assert Pdu.sequence_number(pdu1r) == 1
    assert Pdu.sequence_number(pdu2r) == 2
  end

  test "reply, reply sequence_number", context do
    pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123 }
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)
    Server.send(context[:server], pdu_data)

    :timer.sleep(50)

    assert [{:init}, {:handle_pdu, received_pdu}] = SupportESME.callbacks_received(context[:st_store])

    reply_pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0)

    ESME.reply(context[:esme], received_pdu, reply_pdu)

    :timer.sleep(50)

    assert {:ok, {:pdu, reply_received}, _} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.sequence_number(reply_received) == 123
  end

  test "stop", context do

    ESME.stop(context[:esme])

    :timer.sleep(50)

    assert [{:init}, {:handle_stop}] = SupportESME.callbacks_received(context[:st_store])
    refute Process.alive?(context[:esme])

  end

end

