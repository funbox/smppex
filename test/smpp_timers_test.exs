defmodule SMPPEX.SMPPTimersTest do
  use ExUnit.Case

  alias SMPPEX.SMPPTimers

  test "session_init_timer" do
    timers = SMPPTimers.new(0, 30, 30, 30, 600)

    assert {:ok, timers} == SMPPTimers.handle_tick(timers, 29)
    assert {:stop, :session_init_timer} == SMPPTimers.handle_tick(timers, 31)
  end

  test "enquire_link (stop)" do
    timers0 = SMPPTimers.new(0, 30, 60, 30, 600)

    timers = SMPPTimers.handle_bind(timers0, 0)

    assert {:ok, timers} == SMPPTimers.handle_tick(timers, 59)
    assert {:enquire_link, timers1} = SMPPTimers.handle_tick(timers, 61)
    assert {:ok, timers1} == SMPPTimers.handle_tick(timers1, 89)
    assert {:stop, :enquire_link_timer} == SMPPTimers.handle_tick(timers1, 91)
  end

  test "enquire_link (recover by peer action)" do
    timers0 = SMPPTimers.new(0, 30, 60, 30, 600)

    timers = SMPPTimers.handle_bind(timers0, 0)

    assert {:ok, timers} == SMPPTimers.handle_tick(timers, 59)
    assert {:enquire_link, timers1} = SMPPTimers.handle_tick(timers, 61)
    assert {:ok, timers1} == SMPPTimers.handle_tick(timers1, 89)
    timers2 = SMPPTimers.handle_peer_action(timers1, 89)
    assert {:ok, timers2} == SMPPTimers.handle_tick(timers2, 91)
  end

  test "enquire_link (recover by peer transaction)" do
    timers0 = SMPPTimers.new(0, 30, 60, 30, 600)

    timers = SMPPTimers.handle_bind(timers0, 0)

    assert {:ok, timers} == SMPPTimers.handle_tick(timers, 59)
    assert {:enquire_link, timers1} = SMPPTimers.handle_tick(timers, 61)
    assert {:ok, timers1} == SMPPTimers.handle_tick(timers1, 89)
    timers2 = SMPPTimers.handle_peer_transaction(timers1, 89)
    assert {:ok, timers2} == SMPPTimers.handle_tick(timers2, 91)
  end

  test "inactivity_timer" do
    timers0 = SMPPTimers.new(0, 30, 60, 30, 600)

    timers1 = SMPPTimers.handle_bind(timers0, 0)
    timers2 = SMPPTimers.handle_peer_action(timers1, 599)

    assert {:stop, :inactivity_timer} == SMPPTimers.handle_tick(timers2, 601)
  end

  test "inactivity_timer (recover by peer transaction)" do
    timers0 = SMPPTimers.new(0, 30, 60, 30, 600)

    timers1 = SMPPTimers.handle_bind(timers0, 0)
    timers2 = SMPPTimers.handle_peer_transaction(timers1, 599)

    assert {:ok, timers2} == SMPPTimers.handle_tick(timers2, 601)
  end
end
