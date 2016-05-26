defmodule SMPPEX.SimpleClient do
  defstruct [
    :pid,
    :system_id,
    :password,
    :source_addr,
    :destination_addr,
    :message,
    :seq
  ]

  require Logger

  alias SMPPEX.SimpleClient
  alias SMPPEX.Pdu.Factory
  alias SMPPEX.ClientPool
  alias SMPPEX.Session
  alias SMPPEX.Pdu

  def create(host, port, system_id, password, source_addr, destination_addr, message) do
    {:ok, seq} = Agent.start_link(fn -> 1 end)
    handler = fn(_ref, _socket, _transport, protocol) ->
      {:ok, %SimpleClient{
        pid: protocol,
        system_id: system_id,
        password: password,
        source_addr: source_addr,
        destination_addr: destination_addr,
        message: message,
        seq: seq
      }}
    end

    client_pool = ClientPool.start(handler)
    {:ok, socket} = :gen_tcp.connect(host, port, [:binary, {:packet, 0}])
    ClientPool.start_session(client_pool, socket)
    client_pool
  end

  def send_pdu(session) do
    pdu = Factory.submit_sm(
      {session.source_addr, 5, 1},
      {session.destination_addr, 1, 1},
      session.message,
      1
    )
    Session.send_pdu(session.pid, set_sequence_number(session, pdu))
  end

  def handle_submit_sm_resp(session, _pdu) do
    Session.stop(session.pid)
  end

  def set_sequence_number(session, pdu) do
    sequence_number = Agent.get_and_update(session.seq, fn(seq_num) -> {seq_num, seq_num + 1} end)
    %Pdu{ pdu | sequence_number: sequence_number}
  end

end

defimpl SMPPEX.SMPPHandler, for: SMPPEX.SimpleClient do

  alias SMPPEX.SimpleClient
  alias SMPPEX.Session
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory
  alias SMPPEX.Protocol.CommandNames

  require Logger

  def after_init(session) do
    pdu = Factory.bind_transceiver(session.system_id, session.password)
    Session.send_pdu(session.pid, SimpleClient.set_sequence_number(session, pdu))
  end

  def handle_parse_error(_session, error) do
    Logger.info("parse error: #{inspect error}")
  end

  def handle_pdu(_session, {:unparsed_pdu, raw_pdu, error}) do
    Logger.info("unparsed pdu: #{inspect raw_pdu}, error: #{inspect error}")
  end

  def handle_pdu(session, {:pdu, pdu}) do
    Logger.info("in pdu: #{inspect pdu}")
    case pdu |> Pdu.command_id |> CommandNames.name_by_id do
      {:ok, :bind_transceiver_resp} ->
        SimpleClient.send_pdu(session)
      {:ok, :submit_sm_resp} ->
        SimpleClient.handle_submit_sm_resp(session, pdu)
    end
    :ok
  end

  def handle_socket_closed(_session) do
    Logger.info("socket closed")
  end

  def handle_socket_error(_session, reason) do
    Logger.info("socket error: #{inspect reason}")
  end

  def handle_stop(_session) do
    Logger.info("stop")
  end

  def handle_send_pdu_result(session, pdu, send_pdu_result) do
    Logger.info("out pdu: #{inspect pdu} (#{send_pdu_result})")
    session
  end

end
