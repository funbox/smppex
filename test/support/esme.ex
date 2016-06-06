defmodule Support.ESME do

  use SMPPEX.ESME

  # We need Agent, not simple state to retain callback history when ESME stops

  def start_link(host, port, esme_opts \\ []) do
    {:ok, st_store} = Agent.start_link(fn() -> [] end)
    {:ok, esme} = SMPPEX.ESME.start_link(host, port, {__MODULE__, st_store}, [{:esme_opts, esme_opts}])
    {st_store, esme}
  end

  def callbacks_received(st_store) do
    Agent.get(st_store, fn(st) ->
      Enum.reverse(st)
    end)
  end

  def init(st_store) do
    register_callback(st_store, {:init})
    {:ok, st_store}
  end

  def handle_pdu(pdu, st_store) do
    register_callback(st_store, {:handle_pdu, pdu})
  end

  def handle_resp(pdu, original_pdu, st_store) do
    register_callback(st_store, {:handle_resp, pdu, original_pdu})
  end

  def handle_resp_timeout(pdu, st_store) do
    register_callback(st_store, {:handle_resp_timeout, pdu})
  end

  def handle_send_pdu_result(pdu, result, st_store) do
    register_callback(st_store, {:handle_send_pdu_result, pdu, result})
  end

  def handle_stop(st_store) do
    register_callback(st_store, {:handle_stop})
  end

  def handle_call(request, from, st_store) when is_function(request) do
    register_callback(st_store, {:handle_call, from, request})
    {:reply, request.(st_store), st_store}
  end

  def handle_call(request, from, st_store) do
    register_callback(st_store, {:handle_call, from, request})
    {:reply, request, st_store}
  end

  def handle_cast(request, st_store) do
    register_callback(st_store, {:handle_cast, request})
  end

  def handle_info(request, st_store) do
    register_callback(st_store, {:handle_info, request})
  end

  defp register_callback(st_store, callback_info) do
    Agent.update(st_store, fn(st) ->
      [callback_info | st]
    end)
    st_store
  end

end

defmodule Support.StoppingESME do

  use SMPPEX.ESME

  def init(stop_reason) do
    {:stop, stop_reason}
  end

end
