defmodule Support.SMPPSession do
  @moduledoc false

  defstruct [
    callbacks_received: nil,
    pdu_handler: nil,
  ]

  @transport :ranch_tcp
  @timeout 1000

  alias SMPPEX.TransportSession
  alias __MODULE__, as: SMPPSession

  @behaviour TransportSession

  def start_link(host, port, pid) do
    sock_opts = [:binary, {:packet, 0}, {:active, :once}]
    {:ok, socket} = @transport.connect(host, port, sock_opts, @timeout)
    {:ok, session} = TransportSession.start_link(socket, @transport, {__MODULE__, [pid]})
    session
  end

  defp save_callback(st, name, args) do
    Agent.update(
      st.callbacks_received,
      fn(callbacks) ->
        [{name, args} | callbacks]
      end
    )
    st
  end

  def set_pdu_handler(session, pdu_handler) do
    TransportSession.call(session, {:set_pdu_handler, pdu_handler})
  end

  def send_pdus(session, pdus) do
    TransportSession.call(session, {:send_pdus, pdus})
  end

  def test_reply(session) do
    TransportSession.call(session, :test_reply)
  end

  def stop(session, reason \\ :normal) do
    TransportSession.call(session, {:stop, reason})
  end

  def init(_socket, _transport, [pid]) do
    Process.flag(:trap_exit, true)
    {:ok, %SMPPSession{pdu_handler: fn(_pdu) -> {:ok, []} end, callbacks_received: pid}}
  end

  def handle_pdu(pdu_info, st) do
    new_st = save_callback(st, :handle_pdu, [pdu_info])
    :erlang.list_to_tuple(
      :erlang.tuple_to_list(st.pdu_handler.(pdu_info)) ++ [new_st]
    )
  end

  def handle_send_pdu_result(pdu, result, st) do
    save_callback(st, :handle_send_pdu_result, [pdu, result])
  end

  def handle_call({:set_pdu_handler, pdu_handler}, _from, st) do
    {:reply, :ok, [], %SMPPSession{st | pdu_handler: pdu_handler}}
  end

  def handle_call(:callbacks_received, _from, st) do
    {:reply, Enum.reverse(st.callbacks_received), [], st}
  end

  def handle_call(:test_reply, from, st) do
    spawn(fn -> TransportSession.reply(from, :test_reply) end)
    {:noreply, [], st}
  end

  def handle_call({:send_pdus, pdus}, _from, st) do
    {:reply, :ok, pdus, st}
  end

  def handle_call({:stop, reason}, _from, st) do
    {:stop, reason, :ok, [], st}
  end

  def handle_call(request, _from, st) do
    {:reply, :ok, [], save_callback(st, :handle_call, [request])}
  end

  def handle_cast(request, st) do
    {:noreply, [], save_callback(st, :handle_cast, [request])}
  end

  def handle_info(request, st) do
    {:noreply, [], save_callback(st, :handle_cast, [request])}
  end

  def handle_socket_closed(st) do
    {:socket_closed, save_callback(st, :handle_socket_closed, [])}
  end

  def handle_socket_error(error, st) do
    {error, save_callback(st, :handle_socket_error, [error])}
  end

  def terminate(reason, st) do
    save_callback(st, :terminate, [reason])
  end

  def code_change(old_vsn, st, extra) do
    {:ok, save_callback(st, :code_change, [old_vsn, extra])}
  end

end
