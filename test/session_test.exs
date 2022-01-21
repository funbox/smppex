defmodule SMPPEX.SessionTest do
  use ExUnit.Case

  alias :timer, as: Timer
  alias :sys, as: Sys

  alias Support.TCP.Server
  alias SMPPEX.Session
  alias SMPPEX.Pdu

  setup do
    server = Server.start_link()

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
      session_init_limit: :infinity,
      enquire_link_resp_limit: 1000,
      inactivity_limit: 10_000,
      response_limit: 2000,
      timer_resolution: 100_000
    ]

    esme_with_opts = fn handler, opts ->
      case SMPPEX.ESME.start_link(
             {127, 0, 0, 1},
             Server.port(server),
             {Support.Session, {callback_agent, handler}},
             esme_opts: opts
           ) do
        {:ok, pid} -> pid
        other -> other
      end
    end

    esme = &esme_with_opts.(&1, esme_opts)

    {:ok, esme: esme, esme_with_opts: esme_with_opts, callbacks: callbacks, server: server}
  end

  test "send_pdu", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
      end)

    Session.send_pdu(esme, pdu)
    Timer.sleep(50)

    assert {:ok, {:pdu, pdu1}, _} = Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()
    assert Pdu.mandatory_field(pdu, :system_id) == Pdu.mandatory_field(pdu1, :system_id)
    assert Pdu.mandatory_field(pdu, :password) == Pdu.mandatory_field(pdu1, :password)
  end

  test "send_pdu sequence_numbers", ctx do
    pdu1 = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    pdu2 = SMPPEX.Pdu.Factory.bind_transceiver("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
      end)

    Session.send_pdu(esme, pdu1)
    Session.send_pdu(esme, pdu2)
    Timer.sleep(50)

    assert {:ok, {:pdu, pdu1r}, rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, pdu2r}, _} = rest_data |> SMPPEX.Protocol.parse()
    assert Pdu.sequence_number(pdu1r) == 1
    assert Pdu.sequence_number(pdu2r) == 2
  end

  test "reply, reply sequence_number", ctx do
    pdu = %Pdu{
      SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      | sequence_number: 123
    }

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_pdu, _pdu}, st -> {:ok, st}
      end)

    Server.send(ctx[:server], pdu_data)
    Timer.sleep(50)

    assert [{:init, _, _}, {:handle_pdu, received_pdu}] = ctx[:callbacks].()

    reply_pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0) |> Pdu.as_reply_to(received_pdu)
    Session.send_pdu(esme, reply_pdu)
    Timer.sleep(50)

    assert {:ok, {:pdu, reply_received}, _} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.sequence_number(reply_received) == 123
  end

  test "stop", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:terminate, _reason, _lost_pdus}, _st -> :stop
      end)

    Timer.sleep(50)

    assert :ok = Session.stop(esme, :oops)

    assert [
             {:init, _, _},
             {:terminate, :oops, _lost_pdus}
           ] = ctx[:callbacks].()

    receive do
      x -> assert {:EXIT, ^esme, :oops} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "cast", ctx do
    ref = make_ref()

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_cast, _}, st -> {:noreply, st}
      end)

    Session.cast(esme, ref)
    Timer.sleep(10)

    assert [{:init, _, _}, {:handle_cast, ^ref}] = ctx[:callbacks].()
  end

  test "cast through GenServer.cast", ctx do
    ref = make_ref()

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_cast, _}, st -> {:noreply, st}
      end)

    GenServer.cast(esme, ref)
    Timer.sleep(10)

    assert [{:init, _, _}, {:handle_cast, ^ref}] = ctx[:callbacks].()
  end

  test "cast with pdu", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_cast, :req}, st -> {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}
        {:handle_send_pdu_result, _, _}, st -> st
      end)

    assert :ok == Session.cast(esme, :req)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "terminate with sending pdus", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _, _}, st -> st
        {:terminate, _, _}, st -> {:stop, [SMPPEX.Pdu.Factory.enquire_link()], st}
      end)

    assert :ok == Session.stop(esme)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "terminate with invalid response", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:terminate, _reason, _lost_pdus}, _st -> :bad_resp
      end)

    assert :ok = Session.stop(esme, :oops)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_terminate_reply, :bad_resp}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "cast with stop", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_cast, :req}, st -> {:stop, :ooops, st}
        {:terminate, _, _}, _ -> :stop
      end)

    Session.cast(esme, :req)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_cast, :req},
             {:terminate, :ooops, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "cast with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_cast, _}, _st -> :foo
        {:terminate, _, _}, _ -> :stop
      end)

    Session.cast(esme, :bar)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_cast_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "call", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_call, :req, _from}, st -> {:reply, :got_it, st}
      end)

    assert :got_it == Session.call(esme, :req)

    assert [{:init, _, _}, {:handle_call, :req, _from}] = ctx[:callbacks].()
  end

  test "call through GenServer.call", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_call, :req, _from}, st -> {:reply, :got_it, st}
      end)

    assert :got_it == GenServer.call(esme, :req)

    assert [{:init, _, _}, {:handle_call, :req, _from}] = ctx[:callbacks].()
  end

  test "call with delayed reply", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st ->
          {:ok, st}

        {:handle_call, :req, from}, st ->
          spawn(fn -> Session.reply(from, :got_it) end)
          {:noreply, st}
      end)

    assert :got_it == Session.call(esme, :req)
  end

  test "call with delayed reply and pdu", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st ->
          {:ok, st}

        {:handle_call, :req, from}, st ->
          spawn(fn -> Session.reply(from, :got_it) end)
          {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}

        {:handle_send_pdu_result, _, _}, st ->
          st
      end)

    assert :got_it == Session.call(esme, :req)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "call with delayed reply and stop", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st ->
          {:ok, st}

        {:handle_call, :req, from}, st ->
          spawn(fn ->
            Timer.sleep(20)
            Session.reply(from, :got_it)
          end)

          {:stop, :ooops, st}

        {:terminate, _, _}, _ ->
          :stop
      end)

    spawn_link(fn -> Session.call(esme, :req) end)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_call, :req, _from},
             {:terminate, :ooops, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "call with invalid handle_call reply", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_call, _, _}, _st -> :foo
        {:terminate, _, _}, _ -> :stop
      end)

    spawn(fn -> Session.call(esme, :bar) end)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_call_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "info", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_info, :req}, st -> {:noreply, st}
      end)

    Kernel.send(esme, :req)
    Timer.sleep(50)

    assert [{:init, _, _}, {:handle_info, :req}] = ctx[:callbacks].()
  end

  test "info with pdu", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_info, :req}, st -> {:noreply, [SMPPEX.Pdu.Factory.enquire_link()], st}
        {:handle_send_pdu_result, _, _}, st -> st
      end)

    Kernel.send(esme, :req)

    Timer.sleep(50)

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "info with stop", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_info, :req}, st -> {:stop, :ooops, st}
        {:terminate, _, _}, _ -> :stop
      end)

    Kernel.send(esme, :req)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_info, :req},
             {:terminate, :ooops, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "info with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_info, _}, _st -> :foo
        {:terminate, _, _}, _ -> :stop
      end)

    send(esme, :bar)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_info_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "init", ctx do
    _esme = ctx[:esme].(fn {:init, _socket, _transport}, st -> {:ok, st} end)
    Timer.sleep(50)

    assert [{:init, _, _}] = ctx[:callbacks].()
  end

  test "init, stop from init", ctx do
    assert {:error, :oops} =
             ctx[:esme].(fn {:init, _socket, _transport}, _st -> {:stop, :oops} end)
  end

  test "handle_pdu with ok", ctx do
    pdu = %Pdu{
      SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      | sequence_number: 123
    }

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    _esme =
      ctx[:esme].(fn
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
    pdu = %Pdu{
      SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      | sequence_number: 123
    }

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    _esme =
      ctx[:esme].(fn
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

    assert {:ok, {:pdu, reply_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(reply_pdu) == :bind_transmitter_resp
  end

  test "handle_pdu with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = %Pdu{
      SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      | sequence_number: 123
    }

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_pdu, _pdu}, st -> {:stop, :nopenope, st}
        {:terminate, _, _}, _st -> :stop
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

  test "handle_pdu with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    pdu = %Pdu{
      SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      | sequence_number: 123
    }

    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_pdu, _pdu}, _st -> :foo
        {:terminate, _, _}, _st -> :stop
      end)

    Server.send(ctx[:server], pdu_data)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_pdu_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "handle_resp ok", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
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

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
      end)

    Session.send_pdu(esme, pdu)
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

    assert {:ok, {:pdu, _}, rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = SMPPEX.Protocol.parse(rest_data)

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "handle_resp ok with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:stop, :nopenope, st}
        {:terminate, _, _}, _ -> :stop
      end)

    Session.send_pdu(esme, pdu)
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

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    pdu = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    Session.send_pdu(esme, pdu)
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

    assert Pdu.command_name(bind_resp) == :bind_transmitter_resp
    assert Pdu.command_name(submit_sm_resp) == :submit_sm_resp
  end

  test "handle_resp with unknown resp", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
      end)

    Session.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 2}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, :ok}
           ] = ctx[:callbacks].()
  end

  test "handle_resp with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, _st -> :foo
        {:terminate, _, _}, _ -> :stop
      end)

    Session.send_pdu(esme, pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_resp_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "handle_resp_timeout with ok", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp_timeout, _pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})
    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, :ok},
             {:handle_resp_timeout, [timeout_pdu]}
           ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"
  end

  test "handle_resp_timeout with invalid pdu", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "too_long_password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp_timeout, _pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})
    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, invalid_pdu, {:error, _}}
           ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(invalid_pdu, :system_id) == "system_id1"
  end

  test "handle_resp_timeout with ok and pdus", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp_timeout, _pdu}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
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
             {:handle_send_pdu_result, _, :ok}
           ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"

    assert {:ok, {:pdu, _}, rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, enquire_link_pdu}, _rest_data} = SMPPEX.Protocol.parse(rest_data)

    assert Pdu.command_name(enquire_link_pdu) == :enquire_link
  end

  test "handle_resp_timeout with stop", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp_timeout, _pdu}, st -> {:stop, :nopenope, st}
        {:terminate, _, _}, _ -> :stop
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, :ok},
             {:handle_resp_timeout, [_timeout_pdu]},
             {:terminate, :nopenope, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "handle_resp_timeout with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp_timeout, _pdu}, _st -> :foo
        {:terminate, _, _}, _ -> :stop
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    Kernel.send(esme, {:check_expired_pdus, time + 2050})

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_resp_timeout_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "handle_send_pdu_result", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "too_long_password")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
      end)

    Session.send_pdu(esme, pdu)
    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, {:error, _}}
           ] = ctx[:callbacks].()
  end

  test "handle_unparsed_pdu with ok", ctx do
    Server.send(ctx[:server], <<
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

    _esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_unparsed_pdu, _pdu, _error}, st -> {:ok, st}
      end)

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_unparsed_pdu, _pdu, "Unknown command_id"}
           ] = ctx[:callbacks].()
  end

  test "handle_unparsed_pdu with ok and pdus", ctx do
    _esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_unparsed_pdu, _pdu, _error}, st -> {:ok, [SMPPEX.Pdu.Factory.enquire_link()], st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
      end)

    Server.send(ctx[:server], <<
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

    assert {:ok, {:pdu, reply_pdu}, _rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert Pdu.command_name(reply_pdu) == :enquire_link
  end

  test "handle_unparsed_pdu with ok and stop", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_unparsed_pdu, _pdu, _error}, st -> {:stop, :nopenope, st}
        {:terminate, _pdu, _los_pdus}, _st -> :stop
      end)

    Server.send(ctx[:server], <<
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
             {:init, _, _},
             {:handle_unparsed_pdu, _pdu, "Unknown command_id"},
             {:terminate, :nopenope, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "handle_unparsed_pdu with invalid reply", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_unparsed_pdu, _pdu, _error}, _st -> :foo
        {:terminate, _pdu, _los_pdus}, _st -> :stop
      end)

    Server.send(ctx[:server], <<
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

    receive do
      x -> assert {:EXIT, ^esme, {:bad_handle_unparsed_pdu_reply, :foo}} = x
    after
      50 ->
        assert false
    end

    refute Process.alive?(esme)
  end

  test "enquire_link by timeout", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    assert {:ok, {:pdu, _}, rest_data} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, enquire_link}, _} = rest_data |> SMPPEX.Protocol.parse()
    assert Pdu.command_name(enquire_link) == :enquire_link
  end

  test "enquire_link by timeout and consequent submit_sm", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    pdu = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    Session.send_pdu(esme, pdu)

    Timer.sleep(50)

    assert {:ok, {:pdu, _}, rest_data0} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, enquire_link}, rest_data1} = rest_data0 |> SMPPEX.Protocol.parse()
    assert {:ok, {:pdu, submit_sm}, _} = rest_data1 |> SMPPEX.Protocol.parse()

    assert Pdu.sequence_number(enquire_link) < Pdu.sequence_number(submit_sm)
  end

  test "enquire_link cancel by peer action", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 950})
    Timer.sleep(50)

    action_pdu = %Pdu{SMPPEX.Pdu.Factory.enquire_link() | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(ctx[:server], action_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    assert {:ok, {:pdu, _bind_pdu}, rest} =
             Server.received_data(ctx[:server]) |> SMPPEX.Protocol.parse()

    assert {:ok, {:pdu, _enquire_link_resp}, <<>>} = rest |> SMPPEX.Protocol.parse()
  end

  test "enquire_link timeout cancel by peer action", ctx do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    action_pdu = %Pdu{SMPPEX.Pdu.Factory.enquire_link() | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(ctx[:server], action_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 2100})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             # bind_transmitter sent
             {:handle_send_pdu_result, _, :ok},
             {:handle_resp, _, _}
           ] = ctx[:callbacks].()
  end

  test "stop by enquire_link timeout", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
        {:handle_timeout, reason}, _st -> reason
        {:terminate, _reason, _los_pdus}, _st -> :stop
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    Kernel.send(esme, {:tick, time + 2050})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             # bind_transmitter sent
             {:handle_send_pdu_result, _, :ok},
             {:handle_resp, _, _},
             {:handle_timeout, :enquire_link_timer},
             {:terminate, :enquire_link_timer, _los_pdus}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "stop by inactivity timeout", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
        {:handle_timeout, reason}, _st -> reason
        {:terminate, _reason, _los_pdus}, _st -> :stop
      end)

    Session.send_pdu(esme, pdu)
    time = SMPPEX.Compat.monotonic_time()
    Timer.sleep(50)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 10_050})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             # bind_transmitter sent
             {:handle_send_pdu_result, _, :ok},
             {:handle_resp, _, _},
             {:handle_timeout, :inactivity_timer},
             {:terminate, :inactivity_timer, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "stop by session_init_time", ctx do
    Process.flag(:trap_exit, true)

    esme =
      ctx[:esme_with_opts].(
        fn
          {:init, _socket, _transport}, st -> {:ok, st}
          {:handle_timeout, reason}, _st -> reason
          {:terminate, _reason, _los_pdus}, _st -> :stop
        end,
        session_init_limit: 1000
      )

    time = SMPPEX.Compat.monotonic_time()

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_timeout, :session_init_timer},
             {:terminate, :session_init_timer, []}
           ] = ctx[:callbacks].()

    refute Process.alive?(esme)
  end

  test "stop by session_init_time cancel: esme", ctx do
    esme =
      ctx[:esme_with_opts].(
        fn
          {:init, _socket, _transport}, st -> {:ok, st}
          {:handle_send_pdu_result, _pdu, _result}, st -> st
          {:handle_resp, _pdu, _original_pdu}, st -> {:ok, st}
          {:terminate, _reason, _los_pdus}, _st -> :stop
        end,
        session_init_limit: 1000
      )

    time = SMPPEX.Compat.monotonic_time()

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    Session.send_pdu(esme, pdu)

    reply_pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(ctx[:server], reply_pdu_data)
    Timer.sleep(50)

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, _},
             {:handle_resp, _, _}
           ] = ctx[:callbacks].()

    assert Process.alive?(esme)
  end

  test "stop by session_init_time cancel: mc", ctx do
    esme =
      ctx[:esme_with_opts].(
        fn
          {:init, _socket, _transport}, st -> {:ok, st}
          {:handle_send_pdu_result, _pdu, _result}, st -> st
        end,
        session_init_limit: 1000
      )

    time = SMPPEX.Compat.monotonic_time()

    pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid")
    Session.send_pdu(esme, pdu)

    Kernel.send(esme, {:tick, time + 1050})
    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_send_pdu_result, _, _}
           ] = ctx[:callbacks].()

    assert Process.alive?(esme)
  end

  test "lost_pdus", ctx do
    Process.flag(:trap_exit, true)

    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_send_pdu_result, _pdu, _result}, st -> st
        {:terminate, _reason, _los_pdus}, _st -> :stop
      end)

    Session.send_pdu(esme, pdu)
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
    esme = ctx[:esme].(fn {:init, _socket, _transport}, st -> {:ok, st} end)

    Kernel.send(esme, {:timeout, make_ref(), :emit_tick})
    Timer.sleep(50)

    assert [
             {:init, _, _}
           ] = ctx[:callbacks].()

    assert Process.alive?(esme)
  end

  test "code_change ok", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:code_change, _old_vsn, _extra}, st -> {:ok, st}
      end)

    Sys.suspend(esme)
    Sys.change_code(esme, Support.Session, '0.0.1', :some_extra)
    Sys.resume(esme)

    assert [
             {:init, _, _},
             {:code_change, '0.0.1', :some_extra}
           ] = ctx[:callbacks].()
  end

  test "code_change fail", ctx do
    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:code_change, _old_vsn, _extra}, _st -> {:error, :oops}
      end)

    Sys.suspend(esme)
    Sys.change_code(esme, Support.Session, '0.0.1', :some_extra)
    Sys.resume(esme)

    assert [
             {:init, _, _},
             {:code_change, '0.0.1', :some_extra}
           ] = ctx[:callbacks].()
  end

  test "socket closed", ctx do
    Process.flag(:trap_exit, true)

    _esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_socket_closed}, st -> {:ooops, st}
        {:terminate, _reason, _los_pdus}, _st -> :stop
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

    esme =
      ctx[:esme].(fn
        {:init, _socket, _transport}, st -> {:ok, st}
        {:handle_socket_error, :wow_such_socket_error}, st -> {:ooops, st}
        {:terminate, _reason, _los_pdus}, _st -> :stop
      end)

    {_ok, _closed, error, _passive} = Support.Session.socket_messages()
    Kernel.send(esme, {error, :socket, :wow_such_socket_error})

    Timer.sleep(50)

    assert [
             {:init, _, _},
             {:handle_socket_error, :wow_such_socket_error},
             {:terminate, :ooops, []}
           ] = ctx[:callbacks].()
  end
end
