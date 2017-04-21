defmodule Support.ESME do
  @moduledoc false

  use SMPPEX.ESME

  # We need Agent, not simple state to retain callback history when ESME stops

  def start_link(host, port, esme_opts \\ []) do
    {:ok, callback_backup} = Agent.start_link(fn() -> nil end)
    {:ok, esme} = SMPPEX.ESME.start_link(host, port, {__MODULE__, %{callbacks: [], callback_backup: callback_backup}}, [{:esme_opts, esme_opts}])
    {callback_backup, esme}
  end

  def callbacks_received_backuped(pid) do
    Agent.get(pid, fn(st) ->
      Enum.reverse(st)
    end)
  end

  def callbacks_received(esme) do
    SMPPEX.ESME.call(esme, :callbacks_received)
  end

  def init(st) do
    new_st = register_callback(st, {:init})
    {:ok, new_st}
  end

  def handle_pdu(pdu, st) do
    register_callback(st, {:handle_pdu, pdu})
  end

  def handle_resp(pdu, original_pdu, st) do
    register_callback(st, {:handle_resp, pdu, original_pdu})
  end

  def handle_resp_timeout(pdu, st) do
    register_callback(st, {:handle_resp_timeout, pdu})
  end

  def handle_send_pdu_result(pdu, result, st) do
    register_callback(st, {:handle_send_pdu_result, pdu, result})
  end

  def handle_stop(reason, lost_pdus, st) do
    register_callback(st, {:handle_stop, reason, lost_pdus})
    {:normal, st}
  end

  def handle_call(:callbacks_received, _, st) do
    {:reply, Enum.reverse(st.callbacks), st}
  end

  def handle_call(:reply_delayed, from, st) do
    spawn(fn() -> GenServer.reply(from, :delayed_reply) end)
    {:noreply, st}
  end

  def handle_call(request, from, st) when is_function(request) do
    new_st = register_callback(st, {:handle_call, from, request})
    {:reply, request.(new_st), st}
  end

  def handle_call(request, from, st) do
    new_st = register_callback(st, {:handle_call, from, request})
    {:reply, request, new_st}
  end

  def handle_cast(request, st) do
    register_callback(st, {:handle_cast, request})
  end

  def handle_info(request, st) do
    register_callback(st, {:handle_info, request})
  end

  defp register_callback(st, callback_info) do
    new_st = %{st | callbacks: [callback_info | st.callbacks]}
    Agent.update(st.callback_backup, fn(_) ->
      new_st.callbacks
    end)
    new_st
  end

end
