defmodule SMPPEX.ESME.Sync do

  use SMPPEX.ESME

  alias SMPPEX.ESME
  alias SMPPEX.Pdu

  require Logger

  # Public interface

  def start_link(host, port, opts \\ []) do
    ESME.start_link(host, port, {__MODULE__, %{request_from: nil, pdu: nil, additional_pdus: []}}, opts)
  end

  def request(esme, pdu) do
    ESME.call(esme, {:request, pdu})
  end

  def stop(esme) do
    ESME.stop(esme)
  end

  # ESME callbacks

  def handle_call({:request, pdu}, from, st) do
    ESME.send_pdu(self, pdu)
    new_st = %{ st | request_from: from, pdu: pdu }
    {:noreply, new_st}
  end

  def handle_resp(pdu, original_pdu, st) do
    case Pdu.same?(original_pdu, st.pdu) do
      true ->
        new_st = %{ st | request_from: nil, pdu: nil, additional_pdus: [] }
        GenServer.reply(st.request_from, {:ok, pdu, Enum.reverse(st.additional_pdus)})
        new_st
      false -> st
    end
  end

  def handle_resp_timeout(pdu, st) do
    case Pdu.same?(pdu, st.pdu) do
      true ->
        new_st = %{ st | request_from: nil, additional_pdus: [] }
        GenServer.reply(st.request_from, {:timeout, Enum.reverse(st.additional_pdus)})
        new_st
      false -> st
    end
  end

  def handle_pdu(pdu, st) do
    new_st = %{ st | additional_pdus: [pdu | st.additional_pdus] }
    new_st
  end

  def handle_stop(st) do
    case st.request_from do
      nil -> :nop
      from -> GenServer.reply(from, {:stop, Enum.reverse(st.additional_pdus)})
    end
  end

  def handle_send_pdu_result(pdu, result, st) do
    case result do
      :ok -> st
      {:error, error} ->
        case Pdu.same?(pdu, st.pdu) do
          true ->
            new_st = %{ st | request_from: nil, additional_pdus: [] }
            GenServer.reply(st.from, {:error, error, Enum.reverse(st.additional_pdus)})
            new_st
          false ->
            st
        end
    end
  end

end
