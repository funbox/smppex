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
    {pid, mc} = SupportMC.start_link([
      enquire_link_limit: 1000,
      enquire_link_resp_limit: 1000,
      inactivity_limit: 10000,
      response_limit: 2000
    ])
    port = Ranch.get_port(mc)

    Timer.sleep(50)
    {:ok, esme} = SMPPEX.ESME.Sync.start_link("127.0.0.1", port)

    Timer.sleep(50)
    {:ok,
      port: port,
      esme: esme,
      st_backup: pid,
      callbacks: fn() -> SupportMC.callbacks_received(pid) end,
      mc: SupportMC.mc(pid)
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

  test "reply, reply sequence_number", ctx do
    in_pdu = Factory.bind_transmitter("system_id", "password")

    ESME.send_pdu(ctx[:esme], in_pdu)
    ESME.send_pdu(ctx[:esme], in_pdu)
    ESME.send_pdu(ctx[:esme], in_pdu)

    Timer.sleep(50)
    assert [{:init}, _, _, {:handle_pdu, received_in_pdu}] = ctx[:callbacks].()
    assert Pdu.sequence_number(received_in_pdu) == 3

    out_pdu = SMPPEX.Pdu.Factory.bind_transmitter_resp(0)
    MC.reply(ctx[:mc], received_in_pdu, out_pdu)

    Timer.sleep(50)
    assert [{:resp, reply, _}] = ESME.wait_for_pdus(ctx[:esme])

    assert Pdu.sequence_number(reply) == 3
  end

  test "stop_session", ctx do
    MC.stop_session(ctx[:mc])
    Timer.sleep(50)

    assert [{:init}, {:handle_stop}] = ctx[:callbacks].()
    refute Process.alive?(ctx[:mc])
  end

end

