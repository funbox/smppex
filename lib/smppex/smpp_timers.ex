defmodule SMPPEX.SMPPTimers do
  @moduledoc false

  alias SMPPEX.SMPPTimers

  # :established -> :bound
  # :active <-> :waiting_resp
  defstruct connection_time: 0,
            session_init_state: :established,
            session_init_limit: 0,
            last_peer_action_time: 0,
            enquire_link_state: :active,
            enquire_link_limit: 0,
            enquire_link_resp_limit: 0,
            last_transaction_time: 0,
            inactivity_limit: 0

  @type t :: %SMPPTimers{}

  @type stop_reason :: :session_init_timer | :inactivity_timer | :enquire_link_timer

  @spec new(non_neg_integer, timeout, timeout, timeout, timeout) :: t

  def new(time, session_init_limit, enquire_link_limit, enquire_link_resp_limit, inactivity_limit) do
    %SMPPTimers{
      connection_time: time,
      session_init_limit: session_init_limit,
      enquire_link_limit: enquire_link_limit,
      enquire_link_resp_limit: enquire_link_resp_limit,
      inactivity_limit: inactivity_limit
    }
  end

  @spec handle_bind(t, non_neg_integer) :: t

  def handle_bind(timers, time) do
    handle_peer_transaction(%SMPPTimers{timers | session_init_state: :bound}, time)
  end

  @spec handle_peer_transaction(t, non_neg_integer) :: t

  def handle_peer_transaction(timers, time) do
    handle_peer_action(%SMPPTimers{timers | last_transaction_time: time}, time)
  end

  @spec handle_peer_action(t, non_neg_integer) :: t

  def handle_peer_action(timers, time) do
    %SMPPTimers{timers | last_peer_action_time: time, enquire_link_state: :active}
  end

  @type tick_result :: {:ok, t} | {:stop, reason :: stop_reason} | {:enquire_link, t}

  @spec handle_tick(t, non_neg_integer) :: tick_result

  def handle_tick(timers, time) do
    case timers.session_init_state do
      :established -> handle_unbound_tick(timers, time)
      :bound -> handle_bound_tick(timers, time)
    end
  end

  defp handle_unbound_tick(timers, time) do
    case time - timers.connection_time > timers.session_init_limit do
      true -> {:stop, :session_init_timer}
      false -> {:ok, timers}
    end
  end

  defp handle_bound_tick(timers, time) do
    case time - timers.last_transaction_time > timers.inactivity_limit do
      true -> {:stop, :inactivity_timer}
      false -> check_enquire_link(timers, time)
    end
  end

  defp check_enquire_link(timers, time) do
    case timers.enquire_link_state do
      :active -> check_active_enquire_link(timers, time)
      :waiting_resp -> check_waiting_enquire_link(timers, time)
    end
  end

  defp check_active_enquire_link(timers, time) do
    case time - timers.last_peer_action_time > timers.enquire_link_limit do
      true -> {:enquire_link, %SMPPTimers{timers | enquire_link_state: :waiting_resp}}
      false -> {:ok, timers}
    end
  end

  defp check_waiting_enquire_link(timers, time) do
    case time - timers.last_peer_action_time >
           timers.enquire_link_limit + timers.enquire_link_resp_limit do
      true -> {:stop, :enquire_link_timer}
      false -> {:ok, timers}
    end
  end
end
