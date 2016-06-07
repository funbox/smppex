defmodule SMPPEX.ESMETest do
  use ExUnit.Case

  alias SMPPEX.Protocol.CommandNames
  alias Support.TCP.Server
  alias Support.ESME, as: SupportESME
  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  setup do
    server = Server.start_link
    :timer.sleep(50)

    {callback_backup, esme} = SupportESME.start_link({127,0,0,1}, Server.port(server), [
      enquire_link_limit: 1000,
      enquire_link_resp_limit: 1000,
      inactivity_limit: 10000,
      response_limit: 2000
    ])

    {:ok, esme: esme, callback_backup: callback_backup, server: server}
  end

  test "start_link" do
    server = Server.start_link
    :timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:ok, _} = ESME.start_link({127,0,0,1}, Server.port(server), {SupportESME, %{callbacks: [], callback_backup: pid}})
  end

  test "start_link by hostname" do
    server = Server.start_link
    :timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:ok, _} = ESME.start_link('localhost', Server.port(server), {SupportESME, %{callbacks: [], callback_backup: pid}})
  end

  test "start_link by hostname as a string" do
    server = Server.start_link
    :timer.sleep(50)

    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:ok, _} = ESME.start_link("localhost", Server.port(server), {SupportESME, %{callbacks: [], callback_backup: pid}})
  end

  test "start_link when MC is down" do
    server = Server.start_link
    port = Server.port(server)
    {:ok, sock} = :gen_tcp.connect('localhost', port, [])
    :ok = :gen_tcp.close(sock)

    Process.flag(:trap_exit, true)
    {:ok, pid} = Agent.start_link(fn() -> [] end)
    assert {:error, :econnrefused} = ESME.start_link("localhost", port, {SupportESME, %{callbacks: [], callback_backup: pid}})
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

  test "call with delayed reply", context do
    assert :delayed_reply == ESME.call(context[:esme], :reply_delayed)
  end

  test "info", context do
    ref = make_ref
    Kernel.send context[:esme], ref
    :timer.sleep(10)

    assert [{:init}, {:handle_info, ^ref}] = SupportESME.callbacks_received(context[:esme])
  end

  test "init", context do
    assert [{:init}] == SupportESME.callbacks_received(context[:esme])
  end

  test "init, stop from init" do
    server = Server.start_link
    :timer.sleep(50)

    Process.flag(:trap_exit, true)
    assert {:error, :oops} == ESME.start_link({127,0,0,1}, Server.port(server), {Support.StoppingESME, :oops})
  end

  test "handle_pdu", context do
    pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password") | sequence_number: 123 }
    {:ok, pdu_data} = SMPPEX.Protocol.build(pdu)
    Server.send(context[:server], pdu_data)
    :timer.sleep(50)

    assert [{:init}, {:handle_pdu, received_pdu}] = SupportESME.callbacks_received(context[:esme])
    assert Pdu.mandatory_field(received_pdu, :system_id) == "system_id"
    assert Pdu.mandatory_field(received_pdu, :password) == "password"
    assert Pdu.sequence_number(received_pdu) == 123
  end

  test "handle_resp", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    ESME.send_pdu(context[:esme], pdu)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, received_reply_pdu, _}
    ] = SupportESME.callbacks_received(context[:esme])
    assert Pdu.mandatory_field(received_reply_pdu, :system_id) == "sid"
  end

  test "handle_resp (with additional submit_sm)", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    ESME.send_pdu(context[:esme], pdu)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    pdu = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 2}, {"to", 1, 2}, "message")
    ESME.send_pdu(context[:esme], pdu)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.submit_sm_resp(0) | sequence_number: 2}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, bind_resp, _},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp, submit_sm_resp, _}
    ] = SupportESME.callbacks_received(context[:esme])
    assert Pdu.command_id(bind_resp) |> CommandNames.name_by_id == {:ok, :bind_transmitter_resp}
    assert Pdu.command_id(submit_sm_resp) |> CommandNames.name_by_id == {:ok, :submit_sm_resp}
  end

  test "handle_resp with unknown resp", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    ESME.send_pdu(context[:esme], pdu)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 2}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
    ] = SupportESME.callbacks_received(context[:esme])
  end

  test "handle_resp_timeout", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 2050})
    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok},
      {:handle_resp_timeout, timeout_pdu},
    ] = SupportESME.callbacks_received(context[:esme])

    assert Pdu.mandatory_field(timeout_pdu, :system_id) == "system_id1"
  end

  test "handle_send_pdu_result", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "too_long_password")
    ESME.send_pdu(context[:esme], pdu)
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, {:error, _}}
    ] = SupportESME.callbacks_received(context[:esme])
  end

  test "enquire_link by timeout", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 1050})
    :timer.sleep(50)

    assert {:ok, {:pdu, _}, rest_data} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
    assert {:ok, {:pdu, enquire_link}, _} = rest_data |> SMPPEX.Protocol.parse
    assert Pdu.command_id(enquire_link) |> CommandNames.name_by_id == {:ok, :enquire_link}
  end


  test "enquire_link cancel by peer action", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 950})
    :timer.sleep(50)

    action_pdu = %Pdu{ SMPPEX.Pdu.Factory.enquire_link | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(context[:server], action_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 1050})
    :timer.sleep(50)

    assert {:ok, {:pdu, _bind_pdu}, <<>>} = Server.received_data(context[:server]) |> SMPPEX.Protocol.parse
  end

  test "enquire_link timeout cancel by peer action", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 1050})
    :timer.sleep(50)

    action_pdu = %Pdu{ SMPPEX.Pdu.Factory.enquire_link | sequence_number: 1}
    {:ok, action_pdu_data} = SMPPEX.Protocol.build(action_pdu)
    Server.send(context[:server], action_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 2100})
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_send_pdu_result, _, :ok}, # enquire_link sent
      {:handle_pdu, _},                  # pdu from server
      {:handle_send_pdu_result, _, :ok} # no timeout or stop, new enquire_link sent
    ] = SupportESME.callbacks_received(context[:esme])

  end


  test "stop by enquire_link timeout", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 1050})
    Kernel.send(context[:esme], {:tick, time + 2050})
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_send_pdu_result, _, :ok}, # enquire_link sent
      {:handle_stop}
    ] = SupportESME.callbacks_received_backuped(context[:callback_backup])
    refute Process.alive?(context[:esme])
  end

  test "stop by inactivity timeout", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    Kernel.send(context[:esme], {:tick, time + 10050})
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_stop}
    ] = SupportESME.callbacks_received_backuped(context[:callback_backup])
    refute Process.alive?(context[:esme])
  end

  test "bind fail", context do
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1")
    ESME.send_pdu(context[:esme], pdu)
    time = :erlang.system_time(:milli_seconds)
    :timer.sleep(50)

    reply_pdu = %Pdu{ SMPPEX.Pdu.Factory.bind_transmitter_resp(1) | sequence_number: 1}
    {:ok, reply_pdu_data} = SMPPEX.Protocol.build(reply_pdu)
    Server.send(context[:server], reply_pdu_data)
    :timer.sleep(50)

    assert [
      {:init},
      {:handle_send_pdu_result, _, :ok}, # bind_transmitter sent
      {:handle_resp, _, _},
      {:handle_stop}
    ] = SupportESME.callbacks_received_backuped(context[:callback_backup])
    refute Process.alive?(context[:esme])
  end

end

