defmodule SMPPEX.TransportSessionTest do
  use ExUnit.Case

  alias :timer, as: Timer

  alias Support.TCP.Server
  alias Support.SMPPSession

  setup do
    server = Server.start_link()
    {:ok, pid} = Agent.start_link(fn -> [] end)
    Timer.sleep(50)
    session = SMPPSession.start_link({127, 0, 0, 1}, Server.port(server), pid)

    callbacks_received = fn ->
      Agent.get(pid, fn callbacks -> Enum.reverse(callbacks) end)
    end

    {:ok, session: session, server: server, callbacks_received: callbacks_received}
  end

  test "handle_parse_error", context do
    Process.flag(:trap_exit, true)

    Server.send(context[:server], <<
      00,
      00,
      00,
      0x0F,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00
    >>)

    Timer.sleep(50)

    assert [
             {:terminate, [{:parse_error, "Invalid PDU command_length 15"}]}
           ] = context[:callbacks_received].()
  end

  test "handle_pdu with valid pdu", context do
    {:ok, pdu_data} =
      SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))

    Server.send(context[:server], pdu_data)

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:pdu, _}]}
           ] = context[:callbacks_received].()
  end

  test "handle_pdu with unknown pdu", context do
    Server.send(context[:server], <<
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

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:unparsed_pdu, _, _}]}
           ] = context[:callbacks_received].()
  end

  test "handle_pdu returning stop", context do
    Process.flag(:trap_exit, true)

    SMPPSession.set_pdu_handler(context[:session], fn _ -> {:stop, :custom_stop, []} end)

    {:ok, pdu_data} =
      SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))

    Server.send(context[:server], pdu_data)

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:pdu, _}]},
             {:terminate, [:custom_stop]}
           ] = context[:callbacks_received].()

    assert {:tcp_closed, _} = Server.messages(context[:server]) |> Enum.reverse() |> hd
  end

  test "handle_pdu returning new session", context do
    SMPPSession.set_pdu_handler(context[:session], fn _ -> {:ok, []} end)

    {:ok, pdu_data} =
      SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))

    Server.send(context[:server], pdu_data)

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:pdu, _}]}
           ] = context[:callbacks_received].()
  end

  test "handle_pdu returning additional pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")

    SMPPSession.set_pdu_handler(context[:session], fn _ -> {:ok, [pdu_rx]} end)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)

    Server.send(context[:server], pdu_tx_data)

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:pdu, _}]},
             {:handle_send_pdu_result, [^pdu_rx, :ok]}
           ] = context[:callbacks_received].()

    assert pdu_rx_data == Server.received_data(context[:server])
  end

  test "handle_pdu returning additional pdus & stop", context do
    Process.flag(:trap_exit, true)

    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")

    SMPPSession.set_pdu_handler(context[:session], fn _ -> {:stop, :custom_stop, [pdu_rx]} end)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)

    Server.send(context[:server], pdu_tx_data)

    Timer.sleep(50)

    assert [
             {:handle_pdu, [{:pdu, _}]},
             {:handle_send_pdu_result, [^pdu_rx, :ok]},
             {:terminate, [:custom_stop]}
           ] = context[:callbacks_received].()

    assert pdu_rx_data == Server.received_data(context[:server])
    assert {:tcp_closed, _} = Server.messages(context[:server]) |> Enum.reverse() |> hd
  end

  test "handle_socket_closed", context do
    Process.flag(:trap_exit, true)

    Server.stop(context[:server])

    Timer.sleep(50)

    assert [
             {:handle_socket_closed, []},
             {:terminate, [:socket_closed]}
           ] == context[:callbacks_received].()
  end

  test "stop from handle_call", context do
    Process.flag(:trap_exit, true)

    context[:session] |> SMPPSession.stop(:some_reason)

    Timer.sleep(50)

    assert [{:tcp_closed, _}] = Server.messages(context[:server])
  end

  test "handle_send_pdu_result, single pdu", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    context[:session] |> SMPPSession.send_pdus([pdu])

    Timer.sleep(50)

    assert [
             {:handle_send_pdu_result, [pdu, :ok]}
           ] == context[:callbacks_received].()
  end

  test "handle_send_pdu_result, multiple pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")
    context[:session] |> SMPPSession.send_pdus([pdu_tx, pdu_rx])

    Timer.sleep(50)

    assert [
             {:handle_send_pdu_result, [pdu_tx, :ok]},
             {:handle_send_pdu_result, [pdu_rx, :ok]}
           ] == context[:callbacks_received].()
  end

  test "send_pdus as handle_call result", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    context[:session] |> SMPPSession.send_pdus([pdu])

    Timer.sleep(50)

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)
    assert pdu_data == Server.received_data(context[:server])
  end

  test "handle_pdu returning send_pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")
    context[:session] |> SMPPSession.send_pdus([pdu_tx, pdu_rx])

    Timer.sleep(50)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)
    assert pdu_tx_data <> pdu_rx_data == Server.received_data(context[:server])
  end

  test "reply", context do
    assert :test_reply == SMPPSession.test_reply(context[:session])
  end
end
