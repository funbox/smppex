defmodule SMPPEX.ESME.Sync do

  use SMPPEX.ESME

  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  require Logger

  @default_timeout 5000

  # Public interface

  @spec start_link(term, non_neg_integer, Keyword.t) :: GenServer.on_start

  def start_link(host, port, opts \\ []) do
    ESME.start_link(host, port, {__MODULE__, %{from: nil, pdu: nil, additional_pdus: [], state: :free}}, opts)
  end

  @spec request(esme :: pid, pdu :: Pdu.t, timeout :: non_neg_integer) :: {:ok, resp :: Pdu.t} | :timeout | :stop | {:error, reason :: term}

  def request(esme, pdu, timeout \\ @default_timeout) do
    try do
      ESME.call(esme, {:request, pdu}, timeout)
    catch
      :exit, {:timeout, _} -> :timeout
    end
  end

  @type awaited :: {:pdu, pdu :: Pdu.t} | {:resp, resp_pdu :: Pdu.t, original_pdu :: Pdu.t} | {:timeout, pdu :: Pdu.t} | {:error, pdu :: Pdu.t, reason :: any}

  @spec wait_for_pdus(esme :: pid, timeout :: non_neg_integer) :: [awaited] | :timeout | :stop

  def wait_for_pdus(esme, timeout \\ @default_timeout) do
    try do
      ESME.call(esme, :wait_for_pdus, timeout)
    catch
      :exit, {:timeout, _} -> :timeout
    end
  end

  @spec pdus(esme :: pid, timeout) :: [awaited]

  def pdus(esme, timeout \\ @default_timeout) do
    ESME.call(esme, :pdus, timeout)
  end

  @spec stop(esme :: pid) :: :ok

  def stop(esme) do
    ESME.stop(esme)
  end

  # ESME callbacks

  def handle_call({:request, pdu}, from, st) do
    ESME.send_pdu(self, pdu)
    new_st = %{ st | from: from, pdu: pdu, state: :wait_for_resp }
    {:noreply, new_st}
  end

  def handle_call(:pdus, _from, st) do
    do_get_pdus(st)
  end

  def handle_call(:wait_for_pdus, from, st) do
    case st.additional_pdus do
      [_ | _] -> do_get_pdus(st)
      [] ->
        new_st = %{ st | from: from, state: :wait_for_pdus }
        {:noreply, new_st}
    end
  end

  def handle_resp(pdu, original_pdu, st) do
    case st.pdu != nil and Pdu.same?(original_pdu, st.pdu) and st.state == :wait_for_resp do
      true ->
        GenServer.reply(st.from, {:ok, pdu})
        do_set_free(st)
      false ->
        do_push_to_waiting({:resp, pdu, original_pdu}, st)
    end
  end

  def handle_resp_timeout(pdu, st) do
    case Pdu.same?(pdu, st.pdu) and st.state == :wait_for_resp do
      true ->
        GenServer.reply(st.from, :timeout)
        do_set_free(st)
      false ->
        do_push_to_waiting({:timeout, pdu}, st)
    end
  end

  def handle_pdu(pdu, st) do
    do_push_to_waiting({:pdu, pdu}, st)
  end

  def handle_stop(st) do
    case st.from do
      nil -> :nop
      from -> GenServer.reply(from, :stop)
    end
  end

  def handle_send_pdu_result(pdu, result, st) do
    case result do
      :ok -> st
      {:error, error} ->
        case Pdu.same?(pdu, st.pdu) and st.state == :wait_for_resp do
          true ->
            GenServer.reply(st.from, {:error, error})
            do_set_free(st)
          false ->
            do_push_to_waiting({:error, pdu, error}, st)
        end
    end
  end

  defp do_push_to_waiting(pdu_info, st) do
    pdus = [pdu_info | st.additional_pdus]
    case st.state == :wait_for_pdus do
      true ->
        GenServer.reply(st.from, pdus)
        %{ do_set_free(st) | additional_pdus: [] }
      false ->
        %{ st | additional_pdus: pdus }
    end
  end

  defp do_set_free(st), do: %{ st | from: nil, pdu: nil, state: :free }

  defp do_get_pdus(st) do
    pdus = Enum.reverse(st.additional_pdus)
    new_st = %{ st | additional_pdus: [] }
    {:reply, pdus, new_st}
  end

end
