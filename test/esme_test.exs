defmodule SMPPEX.ESMETest do
  use ExUnit.Case

  alias Support.TCP.Server
  alias Support.ESME, as: SupportESME
  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  setup do
    server = Server.start_link
    :timer.sleep(50)
    {callback_backup, esme} = SupportESME.start_link({127,0,0,1}, Server.port(server))

    {:ok, esme: esme, callback_backup: callback_backup, server: server}
  end

  test "start_link", context do

    server = Server.start_link
    :timer.sleep(50)
    {:ok, pid} = Agent.start_link(fn() -> [] end)

    assert {:ok, _} = ESME.start_link({127,0,0,1}, Server.port(server), {SupportESME, %{callbacks: [], callback_backup: pid}})

  end

  test "init", context do

    assert [{:init}] == SupportESME.callbacks_received(context[:esme])

  end

  test "init, stop from init", context do

    server = Server.start_link
    :timer.sleep(50)

    Process.flag(:trap_exit, true)

    assert {:error, :oops} == ESME.start_link({127,0,0,1}, Server.port(server), {Support.StoppingESME, :oops})

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

    assert [{:init}, {:handle_pdu, received_pdu}] = SupportESME.callbacks_received(context[:esme])

    reply_pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0)

    ESME.reply(context[:esme], received_pdu, reply_pdu)

    :timer.sleep(50)

    assert {:ok, {:pdu, reply_received}, _} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.sequence_number(reply_received) == 123
  end

  test "stop", context do

    ESME.stop(context[:esme])

    :timer.sleep(50)

    assert [{:init}, {:handle_stop}] = SupportESME.callbacks_received_backuped(context[:callback_backup])
    refute Process.alive?(context[:esme])

  end

  test "cast", context do
    ref = make_ref

    ESME.cast(context[:esme], ref)

    :timer.sleep(10)

    assert [{:init}, {:handle_cast, ref}] == SupportESME.callbacks_received(context[:esme])
  end

  test "call", context do
    ref = make_ref

    ESME.call(context[:esme], ref)
    assert [{:init}, {:handle_call, _, ^ref}] = SupportESME.callbacks_received(context[:esme])
  end

  test "info", context do
    ref = make_ref

    Kernel.send context[:esme], ref

    :timer.sleep(10)

    assert [{:init}, {:handle_info, ^ref}] = SupportESME.callbacks_received(context[:esme])
  end

end

