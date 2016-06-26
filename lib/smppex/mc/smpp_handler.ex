defmodule SMPPEX.MC.SMPPHandler do

  defstruct [
    :mc_conn
  ]

  def new(mc_conn), do: %__MODULE__{mc_conn: mc_conn}

end

defimpl SMPPEX.SMPPHandler, for: SMPPEX.MC.SMPPHandler do

  alias SMPPEX.MC, as: MC

  require Logger

  def after_init(_session) do
  end

  def handle_parse_error(session, error) do
    Logger.info("mc_conn #{inspect session.mc_conn}, parse error: #{inspect error}, stopping")
  end

  def handle_pdu(session, {:unparsed_pdu, raw_pdu, error}) do
    Logger.info("mc_conn #{inspect session.mc_conn}, unknown pdu: #{inspect raw_pdu}(#{inspect error}), stopping")
    :stop
  end

  def handle_pdu(session, {:pdu, pdu}) do
    :ok = MC.handle_pdu(session.mc_conn, pdu)
  end

  def handle_socket_closed(session) do
    Logger.info("mc_conn #{inspect session.mc_conn}, socket closed, stopping")
  end

  def handle_socket_error(session, reason) do
    Logger.info("mc_conn #{inspect session.mc_conn}, socket error #{inspect reason}, stopping")
  end

  def handle_stop(session) do
    :ok = MC.handle_stop(session.mc_conn)
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    :ok = MC.handle_send_pdu_result(session.mc_conn, pdu, send_pdu_result)
    session
  end

end

