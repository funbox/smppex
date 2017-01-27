defmodule SMPPEX.MCTest do
  use ExUnit.Case

  alias :timer, as: Timer
  alias :ranch, as: Ranch

  alias SMPPEX.Protocol.CommandNames
  alias Support.MC, as: SupportMC
  alias SMPPEX.MC
  alias SMPPEX.ESME.Sync, as: ESME
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory

  setup do

    {pid, mc_server} = SupportMC.start_link([
      mc_opts: [
        enquire_link_limit: 1000,
        enquire_link_resp_limit: 1000,
        inactivity_limit: 10000,
        response_limit: 2000,
        timer_resolution: 100000
      ]
    ])
    port = Ranch.get_port(mc_server)

    Timer.sleep(50)
    {:ok, esme} = SMPPEX.ESME.Sync.start_link("127.0.0.1", port)
    session_init_time = SMPPEX.Time.monotonic

    Timer.sleep(50)
    {:ok,
      port: port,
      esme: esme,
      st_backup: pid,
      callbacks: fn() -> SupportMC.callbacks_received(pid) end,
      mc: SupportMC.mc(pid),
      session_init_time: session_init_time,
    }
  end

  test "start_link" do
    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:ok, _} = MC.start({SupportESME, %{callbacks: [], callback_backup: pid}}, [transport_opts: [port: 0]])
  end

  test "stop" do
    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:ok, mc_server} = MC.start({SupportESME, %{callbacks: [], callback_backup: pid}}, [transport_opts: [port: 0]])
    assert :ok == MC.stop(mc_server)
  end

  test "send_pdu", ctx do
    out_pdu = Factory.bind_transmitter("system_id", "password")

    MC.send_pdu(ctx[:mc], out_pdu)

    Timer.sleep(50)
    assert [{:pdu, in_pdu}] = ESME.wait_for_pdus(ctx[:esme])
    assert Pdu.field(in_pdu, :system_id) == "system_id"
    assert Pdu.field(in_pdu, :password) == "password"
  end

  test "send_pdu, sequence_number", ctx do
    out_pdu = Factory.bind_transmitter("system_id", "password")

    MC.send_pdu(ctx[:mc], out_pdu)
    MC.send_pdu(ctx[:mc], out_pdu)

    Timer.sleep(50)
    assert [{:pdu, in_pdu1}, {:pdu, in_pdu2}] = ESME.wait_for_pdus(ctx[:esme])
    assert Pdu.sequence_number(in_pdu1) == 1
    assert Pdu.sequence_number(in_pdu2) == 2
  end

  test "stop_session", ctx do
    MC.stop_session(ctx[:mc])
    Timer.sleep(50)

    assert [{:init}, {:handle_stop}] = ctx[:callbacks].()
    refute Process.alive?(ctx[:mc])
  end

  test "cast", ctx do
    ref = make_ref()
    MC.cast(ctx[:mc], ref)
    Timer.sleep(10)

    assert [{:init}, {:handle_cast, ref}] == ctx[:callbacks].()
  end

  test "call", ctx do
    ref = make_ref()
    MC.call(ctx[:mc], ref)

    assert [{:init}, {:handle_call, _, ^ref}] = ctx[:callbacks].()
  end

  test "call with delayed reply", ctx do
    assert :delayed_reply == MC.call(ctx[:mc], :reply_delayed)
  end

  test "info", ctx do
    ref = make_ref()
    Kernel.send ctx[:mc], ref
    Timer.sleep(10)

    assert [{:init}, {:handle_info, ref}] == ctx[:callbacks].()
  end

  test "init", ctx do
    assert [{:init}] == ctx[:callbacks].()
  end

  test "init, stop from init" do

    Process.flag(:trap_exit, true)
    {:ok, ref} = MC.start({Support.StoppingMC, :ooops})

    port = Ranch.get_port(ref)

    Timer.sleep(50)
    {:ok, esme} = SMPPEX.ESME.Sync.start_link("127.0.0.1", port)

    pdu = Factory.bind_transmitter("system_id", "password")
    assert :stop = ESME.request(esme, pdu)
  end

  test "handle_pdu", ctx do
    pdu = Factory.bind_transmitter("system_id", "password")

    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    Timer.sleep(50)

    assert [{:init}, {:handle_pdu, received_pdu}] = ctx[:callbacks].()
    assert Pdu.mandatory_field(received_pdu, :system_id) == "system_id"
    assert Pdu.mandatory_field(received_pdu, :password) == "password"
  end

  test "handle_pdu (several)", ctx do
    pdu1 = Factory.bind_transmitter("system_id", "password")
    pdu2 = Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")

    SMPPEX.ESME.send_pdu(ctx[:esme], pdu1)
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu2)
    Timer.sleep(50)

    assert [{:init}, {:handle_pdu, received_pdu1}, {:handle_pdu, received_pdu2}] = ctx[:callbacks].()
    assert Pdu.mandatory_field(received_pdu1, :system_id) == "system_id"
    assert Pdu.mandatory_field(received_pdu1, :password) == "password"
    assert Pdu.mandatory_field(received_pdu2, :source_addr) == "from"
    assert Pdu.mandatory_field(received_pdu2, :destination_addr) == "to"
  end

  test "handle_resp", ctx do
    pdu = Factory.bind_transmitter("system_id", "password")
    MC.send_pdu(ctx[:mc], pdu)
    Timer.sleep(50)

    [{:pdu, bind}] = ESME.pdus(ctx[:esme])
    reply_pdu = Factory.bind_transmitter_resp(0, "sid")
    SMPPEX.ESME.reply(ctx[:esme], bind, reply_pdu)
    Timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, received_reply_pdu, _}
    ] = ctx[:callbacks].()
    assert Pdu.mandatory_field(received_reply_pdu, :system_id) == "sid"
  end

  test "handle_resp with unknown resp", ctx do
    pdu = Factory.bind_transmitter("system_id", "password")
    MC.send_pdu(ctx[:mc], pdu)
    Timer.sleep(50)

    reply_pdu = %Pdu{Factory.bind_transmitter_resp(0, "sid") | sequence_number: 2}
    SMPPEX.ESME.with_session(ctx[:esme], fn(s) -> SMPPEX.Session.send_pdu(s, reply_pdu) end)
    Timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
    ] = ctx[:callbacks].()
  end

  test "handle_resp_timeout", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    MC.send_pdu(ctx[:mc], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 2050})
    reply_pdu = %Pdu{Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    SMPPEX.ESME.with_session(ctx[:esme], fn(s) -> SMPPEX.Session.send_pdu(s, reply_pdu) end)

    Timer.sleep(50)
    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp_timeout, timeout_pdu},
    ] = ctx[:callbacks].()

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"
  end

  test "handle_send_pdu_result", ctx do
    pdu = Factory.bind_transmitter("system_id1", "too_long_password")
    MC.send_pdu(ctx[:mc], pdu)
    Timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, {:error, _}}
    ] = ctx[:callbacks].()
  end

  test "enquire_link by timeout", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 1050})
    Timer.sleep(50)

    assert [{:ok, _}, {:pdu, enquire_link}] = ESME.pdus(ctx[:esme])
    assert Pdu.command_id(enquire_link) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end

  test "stop by bind timeout" do
    {pid, mc_server} = SupportMC.start_link([
      mc_opts: [
        session_init_limit: 30,
        timer_resolution: 5
      ]
    ])
    port = Ranch.get_port(mc_server)

    Timer.sleep(50)
    {:ok, _} = ESME.start_link("127.0.0.1", port)

    Timer.sleep(50)
    mc = SupportMC.mc(pid)
    refute Process.alive?(mc)
  end

  test "enquire_link cancel by peer action", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 950})
    Timer.sleep(50)

    action_pdu = Factory.enquire_link
    SMPPEX.ESME.send_pdu(ctx[:esme], action_pdu)
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 1050})
    Timer.sleep(50)

    assert [{:ok, _}, {:ok, _}] = ESME.pdus(ctx[:esme])
  end

  test "enquire_link timeout cancel by peer action", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 1050})
    Timer.sleep(50)

    action_pdu = Factory.enquire_link
    SMPPEX.ESME.send_pdu(ctx[:esme], action_pdu)
    Timer.sleep(50)

    Kernel.send(ctx[:mc], {:tick, time + 2100})
    Timer.sleep(50)

    assert [{:ok, _}, {:pdu, _bind_pdu}, {:ok, _}, {:pdu, _enquire_link_pdu}] = ESME.pdus(ctx[:esme])
    assert Process.alive?(ctx[:mc])
  end

  test "stop by enquire_link timeout", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)
    Kernel.send(ctx[:mc], {:tick, time + 1050})
    Kernel.send(ctx[:mc], {:tick, time + 2050})
    Timer.sleep(50)

    assert [
      {:init},
      {:handle_pdu, _},
      {:handle_send_pdu_result, _, _}, # Enquire link
      {:handle_stop}
    ] = ctx[:callbacks].()
    refute Process.alive?(ctx[:mc])
  end

  test "stop by inactivity timeout", ctx do
    pdu = Factory.bind_transmitter("system_id1", "pass1")
    SMPPEX.ESME.send_pdu(ctx[:esme], pdu)
    time = ctx[:session_init_time]
    Timer.sleep(50)
    Kernel.send(ctx[:mc], {:tick, time + 10050})
    Timer.sleep(50)

    assert [
      {:init},
      {:handle_pdu, _}, # bind_transmitter sent
      {:handle_stop}
    ] = ctx[:callbacks].()
    refute Process.alive?(ctx[:mc])
  end

end
