defprotocol SMPPEX.SMPPHandler do
  @moduledoc false

  alias SMPPEX.Protocol, as: SMPP
  alias SMPPEX.Pdu, as: Pdu

  @type session :: term
  @type ref :: term
  @type socket :: port
  @type transport :: module
  @type protocol :: pid

  @type init_error :: term
  @type init_reult :: {:ok, session} | init_error
  @type stop_reason :: term

  @type handle_pdu_result :: :ok | {:ok, session} | {:ok, session, [Pdu.t]} | {:stop, session, [Pdu.t], :stop_reason} | {:stop, stop_reason}

  @spec handle_pdu(session, SMPP.pdu_parse_result) :: handle_pdu_result
  def handle_pdu(session, parse_result)

  @spec handle_stop(session, stop_reason) :: any
  def handle_stop(session, reason)

  @type send_pdu_result :: :ok | {:error, term}

  @spec handle_send_pdu_result(session, Pdu.t, send_pdu_result) :: session
  def handle_send_pdu_result(session, pdu, send_pdu_result)

end
