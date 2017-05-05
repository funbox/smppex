defmodule Support.LegacyMC do
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
    new_st = register_callback(st, :init)
    {:ok, new_st}
  end

  def handle_stop(st) do
     register_callback(st, :handle_stop)
  end

  defp register_callback(st, callback_info) do
    new_st = %{st | callbacks: [callback_info | st.callbacks], mc: self()}
    Agent.update(st.st_backup, fn(_) -> new_st end)
    new_st
  end

end
