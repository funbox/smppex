defmodule SMPPEX.ESMETest do
  use ExUnit.Case

  alias :gen_tcp, as: GenTCP
  alias :timer, as: Timer
  alias :sys, as: Sys

  alias SMPPEX.Protocol.CommandNames
  alias Support.TCP.Server
  alias Support.ESME, as: SupportESME
  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  setup do
    server = Server.start_link

    Timer.sleep(50)

    {:ok, callback_agent} = Agent.start_link(fn -> [] end)

    callbacks = fn ->
      Agent.get(
        callback_agent,
        &Enum.reverse(&1)
      )
    end

    esme_opts = [
      enquire_link_limit: 1000,
      enquire_link_resp_limit: 1000,
      inactivity_limit: 10000,
      response_limit: 2000,
      timer_resolution: 100000
    ]

    esme = fn(handler) ->
      SupportESME.start_link(
        {127,0,0,1},
        Server.port(server),
        callback_agent,
        handler,
        esme_opts
      )
    end

    {:ok, esme: esme, callbacks: callbacks, server: server}
  end

  test "start_link" do
    server = Server.start_link
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    handler = fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end
    assert {:ok, _} = ESME.start_link({127,0,0,1}, Server.port(server), {SupportESME, {pid, handler}})
  end

  test "start_link by hostname" do
    server = Server.start_link
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    handler = fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end
    assert {:ok, _} = ESME.start_link('localhost', Server.port(server), {SupportESME, {pid, handler}})
  end

  test "start_link by hostname as a string" do
    server = Server.start_link
    Timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    handler = fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end
    assert {:ok, _} = ESME.start_link("localhost", Server.port(server), {SupportESME, {pid, handler}})
  end

  test "start_link when MC is down" do
    server = Server.start_link
    port = Server.port(server)
    {:ok, sock} = GenTCP.connect('localhost', port, [])
    :ok = GenTCP.close(sock)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    handler = fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end

    Process.flag(:trap_exit, true)
    assert {:error, :econnrefused} = ESME.start_link("localhost", port, {SupportESME, {pid, handler}})
  end

  test "send_pdu", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    assert {:ok, {:pdu, pdu1}, _} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.mandatory_field(pdu, :system_id) == Pdu.mandatory_field(pdu1, :system_id)
    assert Pdu.mandatory_field(pdu, :password) == Pdu.mandatory_field(pdu1, :password)
  end

  test "send_pdu sequence_numbers", ctx do
    pdu1 = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu2 = SMPPEX.Pdu.Factory.bind_transceiver("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
    end)

    ESME.send_pdu(esme, pdu1)
    ESME.send_pdu(esme, pdu2)
    Timer.sleep(50)

    assert {:ok, {:pdu, pdu1r}, rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, pdu2r}, _} = rest_data |> SMPPEX.Protocol.parse
    assert Pdu.sequence_number(pdu1r) == 1
    assert Pdu.sequence_number(pdu2r) == 2
  end

  test "reply, reply sequence_number", ctx do
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123}
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_pdu, _pdu}, st -> {:ok, st}
    end)

    Server.send(ctx[:server], pdu_data)
    Timer.sleep(50)

    assert [{:init, _, _}, {:handle_pdu, received_pdu}] = ctx[:callbacks].()

    reply_pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0) |> Pdu.as_reply_to(received_pdu)
    ESME.send_pdu(esme, reply_pdu)
    Timer.sleep(50)

    assert {:ok, {:pdu, reply_received}, _} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.sequence_number(reply_received) == 123
  end

  test "stop", ctx do
    Process.flag(:trap_exit, true)

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:terminate, _reason, _lost_pdus}, _st -> nil
    end)

    Timer.sleep(50)

    assert :ok = ESME.stop(esme, :oops)

    assert [
      {:init, _, _},
      {:terminate, :oops, _lost_pdus}
    ] = ctx[:callbacks].()

    Timer.sleep(50)

    refute Process.alive?(esme)
  end

  test "cast", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_cast, _}, st -> {:noreply, st}
    end)

    ESME.cast(esme, ref)
    Timer.sleep(10)

    assert [{:init, _, _}, {:handle_cast, ^ref}] = ctx[:callbacks].()
  end

  test "cast with pdu", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_cast, ^ref}, st -> {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}
      {:handle_send_pdu_result, _, _}, st -> st
    end)

    assert :ok == ESME.cast(esme, ref)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.command_id(enquire_link_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "cast with stop", ctx do
    Process.flag(:trap_exit, true)

    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_cast, ^ref}, st -> {:stop, :ooops, st}
      {:terminate, _, _}, _ -> nil
    end)

    ESME.cast(esme, ref)

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_cast, ^ref},
      {:terminate, :ooops, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "call", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_call, ^ref, _from}, st -> {:reply, :got_it, st}
    end)

    assert :got_it == ESME.call(esme, ref)

    assert [{:init, _, _}, {:handle_call, ^ref, _from}] = ctx[:callbacks].()
  end

  test "call with delayed reply", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_call, ^ref, from}, st ->
        spawn(fn -> ESME.reply(from, :got_it) end)
        {:noreply, st}
    end)

    assert :got_it == ESME.call(esme, ref)
  end

  test "call with delayed reply and pdu", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_call, ^ref, from}, st ->
        spawn(fn -> ESME.reply(from, :got_it) end)
        {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}
      {:handle_send_pdu_result, _, _}, st -> st
    end)

    assert :got_it == ESME.call(esme, ref)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse

    assert Pdu.command_id(enquire_link_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "call with delayed reply and stop", ctx do
    Process.flag(:trap_exit, true)

    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_call, ^ref, from}, st ->
        spawn(fn -> Timer.sleep(20); ESME.reply(from, :got_it) end)
        {:stop, :ooops, st}
      {:terminate, _, _}, _ -> nil
    end)

    spawn_link(fn -> ESME.call(esme, ref) end)

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_call, ^ref, _from},
      {:terminate, :ooops, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "info", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_info, ^ref}, st -> {:noreply, st}
    end)

    Kernel.send esme, ref
    Timer.sleep(50)

    assert [{:init, _, _}, {:handle_info, ^ref}] = ctx[:callbacks].()
  end

  test "info with pdu", ctx do
    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_info, ^ref}, st -> {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}
      {:handle_send_pdu_result, _, _}, st -> st
    end)

    Kernel.send(esme, ref)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert Pdu.command_id(enquire_link_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "info with stop", ctx do
    Process.flag(:trap_exit, true)

    ref = make_ref()

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_info, ^ref}, st -> {:stop, :ooops, st}
      {:terminate, _, _}, _ -> nil
    end)

    Kernel.send(esme, ref)

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_info, ^ref},
      {:terminate, :ooops, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "init", ctx do
    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end)

    assert [{:init, _, _}] = ctx[:callbacks].()
  end

  test "init, stop from init" do
    server = Server.start_link
    Timer.sleep(50)

    Process.flag(:trap_exit, true)
    assert {:error, :oops} == ESME.start_link({127,0,0,1}, Server.port(server), {Support.StoppingESME, :oops})
  end

  test "handle_pdu with ok", ctx do
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123}
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_pdu, _pdu}, st -> {:ok, st}
    end)

    Server.send(ctx[:server], pdu_data)
    Timer.sleep(50)

    assert [{:init, _, _}, {:handle_pdu, received_pdu}] = ctx[:callbacks].()
    assert Pdu.mandatory_field(received_pdu, :system_id) == "system_id"
    assert Pdu.mandatory_field(received_pdu, :password) == "password"
    assert Pdu.sequence_number(received_pdu) == 123
  end

  test "handle_pdu with ok and pdus", ctx do
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123}
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_pdu, _pdu}, st -> {:ok, [SMPPEX.Pdu.Factory.bind_transmitter_resp(0)], st}
    end)

    Server.send(ctx[:server], pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_pdu, _},
      {:handle_send_pdu_result, _, _}
    ] = ctx[:callbacks].()

    assert {:ok, {:pdu, reply_pdu}, _rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse

    assert Pdu.command_id(reply_pdu) |> CommandNames.name_by_id == {:ok, :bind_transmitter_resp}
  end

  test "handle_pdu with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123}
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_pdu, _pdu}, st -> {:stop, :nopenope, st}
      {:terminate, _, _}, _st -> nil
    end)

    Server.send(ctx[:server], pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_pdu, _},
      {:terminate, :nopenope, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "handle_resp ok", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, received_reply_pdu, _}
    ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(received_reply_pdu, :system_id) == "sid"
  end

  test "handle_resp ok with pdus", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, received_reply_pdu, _},
      {:handle_send_pdu_result, _, :ok}
    ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(received_reply_pdu, :system_id) == "sid"

    assert {:ok, {:pdu, _}, rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = SMPPEX.Protocol.parse(rest_data)

    assert Pdu.command_id(enquire_link_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "handle_resp ok with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:stop, :nopenope, st}
      {:terminate, _, _}, _ -> nil
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, _, _},
      {:terminate, :nopenope, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "handle_resp (with additional submit_sm)", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    pdu = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.submit_sm_resp(0) | sequence_number: 2}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, bind_resp, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, submit_sm_resp, _}
    ] = ctx[:callbacks].()
    assert Pdu.command_id(bind_resp) |> CommandNames.name_by_id == {:ok, :bind_transmitter_resp}
    assert Pdu.command_id(submit_sm_resp) |> CommandNames.name_by_id == {:ok, :submit_sm_resp}
  end

  test "handle_resp with unknown resp", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 2}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
    ] = ctx[:callbacks].()
  end

  test "handle_resp_timeout with ok", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp_timeout, _pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})
    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)

    Timer.sleep(50)
    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp_timeout, [timeout_pdu]},
    ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"
  end

  test "handle_resp_timeout with ok and pdus", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp_timeout, _pdu}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})
    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)

    Timer.sleep(50)
    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp_timeout, [timeout_pdu]},
      {:handle_send_pdu_result, _, :ok},
    ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"

    assert {:ok, {:pdu, _}, rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = SMPPEX.Protocol.parse(rest_data)

    assert Pdu.command_id(enquire_link_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "handle_resp_timeout with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp_timeout, _pdu}, st -> {:stop, :nopenope, st}
      {:terminate, _, _}, _ -> nil
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})

    Timer.sleep(50)
    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp_timeout, [_timeout_pdu]},
      {:terminate, :nopenope, []},
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "handle_send_pdu_result", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "too_long_password")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, {:error, _}}
    ] = ctx[:callbacks].()
  end

  test "handle_unparsed_pdu with ok", ctx do
    Server.send(ctx[:server], <<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_unparsed_pdu, _pdu, _error}, st -> {:ok, st}
    end)

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_unparsed_pdu, _pdu, "Unknown command_id"},
    ] = ctx[:callbacks].()
  end

  test "handle_unparsed_pdu with ok and pdus", ctx do
    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_unparsed_pdu, _pdu, _error}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
    end)

    Server.send(ctx[:server], <<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    Timer.sleep(50)

    assert {:ok, {:pdu, reply_pdu}, _rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse

    assert Pdu.command_id(reply_pdu) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "handle_unparsed_pdu with ok and stop", ctx do
    Process.flag(:trap_exit, true)

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_unparsed_pdu, _pdu, _error}, st -> {:stop, :nopenope, st}
      {:terminate, _pdu, _los_pdus}, _st -> nil
    end)

    Server.send(ctx[:server], <<00, 00, 00, 0x10,   0x80, 00, 0x33, 0x02,   00, 00, 00, 00,   00, 00, 00, 0x01,   0xAA, 0xBB, 0xCC>>)

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_unparsed_pdu, _pdu, "Unknown command_id"},
      {:terminate, :nopenope, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "enquire_link by timeout", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 1050})
    Timer.sleep(50)

    assert {:ok, {:pdu, _}, rest_data} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, enquire_link}, _} = rest_data |> SMPPEX.Protocol.parse
    assert Pdu.command_id(enquire_link) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "enquire_link cancel by peer action", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      {:handle_pdu, _pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 950})
    Timer.sleep(50)

    action_pdu = %Pdu{SMPPEX.Pdu.Factory.enquire_link | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(ctx[:server], action_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 1050})
    Timer.sleep(50)

    assert {:ok, {:pdu, _bind_pdu}, <<>>} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse
  end

  test "enquire_link timeout cancel by peer action", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      {:handle_pdu, _pdu}, st -> {:ok, st}
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 1050})
    Timer.sleep(50)

    action_pdu = %Pdu{SMPPEX.Pdu.Factory.enquire_link | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(ctx[:server], action_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 2100})
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_send_pdu_result, _, :ok}, # enquire_link sent
      {:handle_pdu, _},                  # pdu from server
      {:handle_send_pdu_result, _, :ok} # no timeout or stop, new enquire_link sent
    ] = ctx[:callbacks].()

  end


  test "stop by enquire_link timeout", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      {:terminate, _reason, _los_pdus}, _st -> nil
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 1050})
    Kernel.send(esme, {:check_timers, time + 2050})
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_send_pdu_result, _, :ok}, # enquire_link sent
      {:terminate, {:timers, :enquire_link_timer}, _los_pdus}
    ] = ctx[:callbacks].()
    refute Process.alive?(esme)
  end

  test "stop by inactivity timeout", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      {:terminate, _reason, _los_pdus}, _st -> nil
    end)

    ESME.send_pdu(esme, pdu)
    time = SMPPEX.Time.monotonic
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:check_timers, time + 10050})
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:terminate, {:timers, :inactivity_timer}, []}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "lost_pdus", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_send_pdu_result, _pdu, _result}, st -> st
      {:terminate, _reason, _los_pdus}, _st -> nil
    end)

    ESME.send_pdu(esme, pdu)
    Timer.sleep(50)
    Process.exit(esme, :oops)
    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_send_pdu_result, _, _},
      {:terminate, :oops, [%SMPPEX.Pdu{command_id: 2}]}
    ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "timeout event", ctx do
    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
    end)

    Kernel.send(esme, {:timeout, make_ref(), :emit_tick})
    Timer.sleep(50)

    assert [
      {:init, _, _}
    ] = ctx[:callbacks].()

    assert Process.alive?(esme)
  end

  test "code_change ok", ctx do
    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:code_change, _old_vsn, _extra}, st -> {:ok, st}
    end)

    Sys.suspend(esme)
    Sys.change_code(esme, SupportESME, '0.0.1', :some_extra)
    Sys.resume(esme)

    assert [
      {:init, _, _},
      {:code_change, '0.0.1', :some_extra}
    ] = ctx[:callbacks].()
  end

  test "code_change fail", ctx do
    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:code_change, _old_vsn, _extra}, _st -> {:error, :oops}
    end)

    Sys.suspend(esme)
    Sys.change_code(esme, SupportESME, '0.0.1', :some_extra)
    Sys.resume(esme)

    assert [
      {:init, _, _},
      {:code_change, '0.0.1', :some_extra}
    ] = ctx[:callbacks].()
  end

  test "socket closed", ctx do
    Process.flag(:trap_exit, true)

    _esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_socket_closed}, st -> {:ooops, st}
      {:terminate, _reason, _los_pdus}, _st -> nil
    end)

    Server.stop(ctx[:server])

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_socket_closed},
      {:terminate, :ooops, []}
    ] = ctx[:callbacks].()
  end

  test "socket error", ctx do
    Process.flag(:trap_exit, true)

    esme = ctx[:esme].(fn
      {:init, _socket, _transport}, st -> {:ok, st}
      {:handle_socket_error, :wow_such_socket_error}, st -> {:ooops, st}
      {:terminate, _reason, _los_pdus}, _st -> nil
    end)

    {_ok, _closed, error} = SupportESME.socket_messages
    Kernel.send(esme, {error, :socket, :wow_such_socket_error})

    Timer.sleep(50)

    assert [
      {:init, _, _},
      {:handle_socket_error, :wow_such_socket_error},
      {:terminate, :ooops, []}
    ] = ctx[:callbacks].()
  end

end
