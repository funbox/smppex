defmodule SMPPEX.ESME.SMPPHandler do
  @moduledoc false

  alias SMPPEX.ESME.SMPPHandler

  defstruct [
    :esme
  ]

  @type t :: %SMPPHandler{}

  @spec new(pid) :: t

  def new(esme), do: %__MODULE__{esme: esme}

end

defimpl SMPPEX.SMPPHandler, for: SMPPEX.ESME.SMPPHandler do

  alias SMPPEX.ESME, as: ESME

  require Logger

  def handle_pdu(_session, {:unparsed_pdu, raw_pdu, error}) do
    {:stop, {:parse_error, {:unparsed_pdu, raw_pdu, error}}}
  end

  def handle_pdu(session, {:pdu, pdu}) do
    :ok = ESME.handle_pdu(session.esme, pdu)
  end

  def handle_stop(session, reason) do
    :ok = ESME.handle_stop(session.esme, reason)
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    :ok = ESME.handle_send_pdu_result(session.esme, pdu, send_pdu_result)
    session
  end

end
