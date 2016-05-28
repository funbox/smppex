defmodule SMPPEX.SessionTest do
  use ExUnit.Case

  alias Support.TCP.Server
  alias Support.ClientPool
  alias Support.SMPPSession

  setup do
    {:ok, hello: "world"}
    server = Server.start_link
    client_pool = ClientPool.create
    ClientPool.connect(client_pool, {127,0,0,1}, Server.port(server))

    :timer.sleep(50)

    [session] = ClientPool.sessions(client_pool)

    {:ok, session: session, server: server}
  end

  test "connect & after_init", context do
    assert [{:after_init, []}] == SMPPSession.callbacks_received(context[:session])
  end

  test "handle_parse_error", context do
    Server.send(context[:server], <<00, 00, 00, 0x0F,   00, 00, 00, 00,   00, 00, 00, 00,   00, 00, 00, 00>>)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_parse_error, _},
      {:handle_stop, []}
    ] = SMPPSession.callbacks_received(context[:session])
  end

  test "handle_pdu with valid pdu", context do
    {:ok, pdu_data} = SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))
    Server.send(context[:server], pdu_data)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:pdu, _}]}
    ] = SMPPSession.callbacks_received(context[:session])
  end

  test "handle_pdu with unknown pdu", context do
    Server.send(context[:server], <<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:unparsed_pdu, _, _}]}
    ] = SMPPSession.callbacks_received(context[:session])
  end

  test "handle_pdu returning stop", context do
    SMPPSession.set_pdu_handler(context[:session], fn(_) -> :stop end)

    {:ok, pdu_data} = SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))
    Server.send(context[:server], pdu_data)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:pdu, _}]},
      {:handle_stop, []}
    ] = SMPPSession.callbacks_received(context[:session])
  end

  test "handle_pdu returning new session", context do
    SMPPSession.set_pdu_handler(context[:session], fn(_) -> {:ok, context[:session]} end)

    {:ok, pdu_data} = SMPPEX.Protocol.build(SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))
    Server.send(context[:server], pdu_data)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:pdu, _}]}
    ] = SMPPSession.callbacks_received(context[:session])
  end

  test "handle_pdu returning additional pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")

    SMPPSession.set_pdu_handler(context[:session], fn(_) -> {:ok, context[:session], [pdu_rx]} end)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)

    Server.send(context[:server], pdu_tx_data)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:pdu, _}]},
      {:handle_send_pdu_result, [pdu_rx, :ok]}
    ] = SMPPSession.callbacks_received(context[:session])

    assert pdu_rx_data == Server.received_data(context[:server])
  end

  test "handle_pdu returning additional pdus & stop", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")

    SMPPSession.set_pdu_handler(context[:session], fn(_) -> {:stop, context[:session], [pdu_rx]} end)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)

    Server.send(context[:server], pdu_tx_data)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_pdu, [{:pdu, _}]},
      {:handle_send_pdu_result, [pdu_rx, :ok]},
      {:handle_stop, []}
    ] = SMPPSession.callbacks_received(context[:session])

    assert pdu_rx_data == Server.received_data(context[:server])
  end

  test "handle_socket_closed", context do
    Server.stop(context[:server])

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_socket_closed, []},
      {:handle_stop, []}
    ] == SMPPSession.callbacks_received(context[:session])
  end

  test "stop & handle_stop", context do
    context[:session] |> SMPPSession.protocol |> SMPPEX.Session.stop

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_stop, []}
    ] == SMPPSession.callbacks_received(context[:session])

    assert [{:tcp_closed, _}] = Server.messages(context[:server])
  end

  test "handle_send_pdu_result, single pdu", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    context[:session] |> SMPPSession.protocol |> SMPPEX.Session.send_pdu(pdu)

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_send_pdu_result, [pdu, :ok]}
    ] == SMPPSession.callbacks_received(context[:session])
  end

  test "handle_send_pdu_result, multiple pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")
    context[:session] |> SMPPSession.protocol |> SMPPEX.Session.send_pdus([pdu_tx, pdu_rx])

    :timer.sleep(50)

    assert [
      {:after_init, []},
      {:handle_send_pdu_result, [pdu_tx, :ok]},
      {:handle_send_pdu_result, [pdu_rx, :ok]}
    ] == SMPPSession.callbacks_received(context[:session])
  end

  test "send_pdu", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    context[:session] |> SMPPSession.protocol |> SMPPEX.Session.send_pdu(pdu)

    :timer.sleep(50)

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)
    assert pdu_data == Server.received_data(context[:server])
  end

  test "send_pdus", context do
    pdu_tx = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu_rx = SMPPEX.Pdu.Factory.bind_receiver("system_id", "password")
    context[:session] |> SMPPSession.protocol |> SMPPEX.Session.send_pdus([pdu_tx, pdu_rx])

    :timer.sleep(50)

    {:ok, pdu_tx_data} = SMPPEX.Protocol.build(pdu_tx)
    {:ok, pdu_rx_data} = SMPPEX.Protocol.build(pdu_rx)
    assert pdu_tx_data <> pdu_rx_data == Server.received_data(context[:server])
  end
end
