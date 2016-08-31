defprotocol SMPPEX.SMPPHandler do
  @moduledoc false

  alias SMPPEX.Protocol, as: SMPP
  alias SMPPEX.Pdu, as: Pdu

  @type session :: any
  @type ref :: any
  @type socket :: port
  @type transport :: module
  @type protocol :: pid

  @type init_error :: any
  @type init_reult :: {:ok, session} | init_error

  @spec after_init(session) :: any
  def after_init(session)

  @spec handle_parse_error(session, SMPP.error) :: any
  def handle_parse_error(session, error)

  @type handle_pdu_result :: :ok | {:ok, session} | {:ok, session, [Pdu.t]} | {:stop, session, [Pdu.t]} | :stop

  @spec handle_pdu(session, SMPP.pdu_parse_result) :: handle_pdu_result
  def handle_pdu(session, parse_result)

  @spec handle_socket_closed(session) :: any
  def handle_socket_closed(session)

  @spec handle_socket_error(session, any) :: any
  def handle_socket_error(session, reason)

  @spec handle_stop(session) :: any
  def handle_stop(session)

  @type send_pdu_result :: :ok | {:error, any}

  @spec handle_send_pdu_result(session, Pdu.t, send_pdu_result) :: session
  def handle_send_pdu_result(session, pdu, send_pdu_result)

end
