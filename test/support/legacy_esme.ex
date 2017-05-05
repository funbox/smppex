defmodule Support.LegacyESME do
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

  def handle_stop(st) do
    register_callback(st, :handle_stop)
  end

  defp register_callback(st, callback_info) do
    new_st = %{st | callbacks: [callback_info | st.callbacks]}
    Agent.update(st.callback_backup, fn(_) ->
      new_st.callbacks
    end)
    new_st
  end

end
