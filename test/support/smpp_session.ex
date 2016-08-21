defmodule Support.SMPPSession do
  @moduledoc false

  defstruct [
    :pid,
    :spied_data_pid
  ]

  alias Support.SMPPSession

  def create(pid) do
    handle_pdu = fn(_pdu_info) -> :ok end
    {:ok, spied_data_pid} = Agent.start_link(fn() -> %{callbacks_received: [], pdu_handler: handle_pdu} end)
    %SMPPSession{spied_data_pid: spied_data_pid, pid: pid}
  end

  def protocol(session) do
    session.pid
  end

  def save_callback(session, name, args) do
    Agent.update(session.spied_data_pid, fn(data) ->
      %{data |
        callbacks_received: [{name, args} | data.callbacks_received]
      }
    end)
  end

  def set_pdu_handler(session, handle_pdu) do
    Agent.update(session.spied_data_pid, fn(data) ->
      %{data | pdu_handler: handle_pdu}
    end)
  end

  def callbacks_received(session) do
    Agent.get(session.spied_data_pid, fn(data) -> Enum.reverse(data.callbacks_received) end)
  end

  def stop(session) do
    Agent.stop(session.spied_data_pid)
  end
end

defimpl SMPPEX.SMPPHandler, for: Support.SMPPSession do

  alias Support.SMPPSession

  def after_init(session) do
    SMPPSession.save_callback(session, :after_init, [])
  end

  def handle_parse_error(session, error) do
    SMPPSession.save_callback(session, :handle_parse_error, [error])
  end

  def handle_pdu(session, {:unparsed_pdu, raw_pdu, error} = pdu_info) do
    SMPPSession.save_callback(session, :handle_pdu, [{:unparsed_pdu, raw_pdu, error}])
    handle_pdu = Agent.get(session.spied_data_pid, fn(data) -> data.pdu_handler end)
    handle_pdu.(pdu_info)
  end

  def handle_pdu(session, {:pdu, pdu} = pdu_info) do
    SMPPSession.save_callback(session, :handle_pdu, [{:pdu, pdu}])
    handle_pdu = Agent.get(session.spied_data_pid, fn(data) -> data.pdu_handler end)
    handle_pdu.(pdu_info)
  end

  def handle_socket_closed(session) do
    SMPPSession.save_callback(session, :handle_socket_closed, [])
  end

  def handle_socket_error(session, reason) do
    SMPPSession.save_callback(session, :handle_socket_error, [reason])
  end

  def handle_stop(session) do
    SMPPSession.save_callback(session, :handle_stop, [])
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    SMPPSession.save_callback(session, :handle_send_pdu_result, [pdu, send_pdu_result])
    session
  end
end

