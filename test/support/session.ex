defmodule Support.Session do
  @moduledoc false

  use SMPPEX.Session

  @transport :ranch_tcp

  def socket_messages, do: @transport.messages

  def init(socket, transport, st) do
    Process.flag(:trap_exit, true)
    register(st, {:init, socket, transport})
  end

  def handle_pdu(pdu, st) do
    register(st, {:handle_pdu, pdu})
  end

  def handle_unparsed_pdu(pdu, error, st) do
    register(st, {:handle_unparsed_pdu, pdu, error})
  end

  def handle_resp(pdu, original_pdu, st) do
    register(st, {:handle_resp, pdu, original_pdu})
  end

  def handle_resp_timeout(pdus, st) do
    register(st, {:handle_resp_timeout, pdus})
  end

  def handle_send_pdu_result(pdu, result, st) do
    register(st, {:handle_send_pdu_result, pdu, result})
  end

  def handle_socket_error(error, st) do
    register(st, {:handle_socket_error, error})
  end

  def handle_socket_closed(st) do
    register(st, {:handle_socket_closed})
  end

  def handle_call(request, from, st) do
    register(st, {:handle_call, request, from})
  end

  def handle_cast(request, st) do
    register(st, {:handle_cast, request})
  end

  def handle_info(request, st) do
    register(st, {:handle_info, request})
  end

  def code_change(old_vsn, st, extra) do
    register(st, {:code_change, old_vsn, extra})
  end

  def terminate(reason, lost_pdus, st) do
    register(st, {:terminate, reason, lost_pdus})
  end

  defp register({pid, handler} = st, callback_info) do
    Agent.update(pid, fn(callbacks) ->
      [callback_info | callbacks]
    end)
    handler.(callback_info, st)
  end

end
