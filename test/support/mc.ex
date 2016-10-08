defmodule Support.MC do
  @moduledoc false

  use SMPPEX.MC

  require Logger

  # We need Agent, not simple state to retain state when MC stops

  def start_link(mc_opts \\ []) do
    {:ok, st_backup} = Agent.start_link(fn() -> nil end)
    {:ok, mc_server} = SMPPEX.MC.start({__MODULE__, %{callbacks: [], st_backup: st_backup, mc: nil}}, [mc_opts: mc_opts, transport_opts: [port: 0]])
    {st_backup, mc_server}
  end

  def callbacks_received(pid) do
    Agent.get(pid, fn(st) ->
      Enum.reverse(st.callbacks)
    end)
  end

  def mc(pid) do
    Agent.get(pid, fn(st) -> st.mc end)
  end

  def init(_sock, _transport, st) do
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

  def handle_stop(st) do
    register_callback(st, {:handle_stop})
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
    new_st = %{st | callbacks: [callback_info | st.callbacks], mc: self}
    Agent.update(st.st_backup, fn(_) -> new_st end)
    new_st
  end

end
