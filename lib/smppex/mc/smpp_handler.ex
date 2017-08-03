defmodule SMPPEX.MC.SMPPHandler do
  @moduledoc false

  alias SMPPEX.MC.SMPPHandler

  defstruct [
    :mc_conn
  ]

  @type t :: %SMPPHandler{}

  @spec new(pid) :: t

  def new(mc_conn), do: %__MODULE__{mc_conn: mc_conn}

end

defimpl SMPPEX.SMPPHandler, for: SMPPEX.MC.SMPPHandler do

  alias SMPPEX.MC, as: MC

  require Logger

  def handle_pdu(session, {:unparsed_pdu, raw_pdu, error}) do
    :ok = MC.handle_parse_error(session.mc_conn, {:unparsed_pdu, raw_pdu, error})
  end

  def handle_pdu(session, {:pdu, pdu}) do
    :ok = MC.handle_pdu(session.mc_conn, pdu)
  end

  def handle_stop(session, reason) do
    :ok = MC.handle_stop(session.mc_conn, reason)
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    :ok = MC.handle_send_pdu_result(session.mc_conn, pdu, send_pdu_result)
    session
  end

end
