defmodule SMPPEX.Session do

  alias SMPPEX.Session
  alias SMPPEX.TCP.Connection
  alias SMPPEX.Pdu
  alias SMPPEX.Protocol

  defstruct connection: nil, smpp_handler: nil, buffer: <<>>

  @spec new(port, any, list) :: pid

  def new(socket, smpp_handler, connection_opts \\ []) do
    session = %Session{ smpp_handler: smpp_handler }
    {:ok, pid} = Connection.start_link(socket, session, connection_opts)
    pid
  end

  @type send_result :: :ok | {:error, any}

  @spec send(pid, Pdu.t) :: send_result
  def send(pid, pdu) do
    case Protocol.build(pdu) do
      {:ok, bin_pdu} ->
        case Connection.send(pid, bin_pdu) do
          :ok -> :ok
          :error -> {:error, "Connection error"}
        end
      {:error, _} = err -> err
    end
  end

  @spec close(pid) :: :ok
  def close(pid), do: Connection.close(pid)

end


defimpl SMPPEX.TCP.ConnectionHandler, for: SMPPEX.Session do

  alias SMPPEX.Session
  alias SMPPEX.Protocol

  require Logger

  def handle_connected(handler, connection) do
    log(handler, :debug, "established")
    smpp_handler = SMPPHandler.handle_connected(connection)
    %Session{ handler | connection: connection, smpp_handler: smpp_handler }
  end

  def handle_data_received(handler, data) do
    log(handler, :debug, "data received: #{inspect data}")
    buffer = handler.buffer <> data
    case process_packets(handler, buffer) do
      {:ok, new_buffer} -> {:ok, %Session{ handler | buffer: new_buffer } }
      {:error, _} = err -> err
    end
  end

  def handle_data_sent(handler, data) do
    log(handler, :debug, "data sent: #{inspect data}")
  end

  def handle_peer_closed(handler) do
    log(handler, :info, "closed remotely")
    SMPPHandler.handle_peer_closed(handler.smpp_handler)
  end

  def handle_closed(handler) do
    log(handler, :info, "closed")
    SMPPHandler.handle_closed(handler.smpp_handler)
  end

  def handle_error_closed(handler, error) do
    log(handler, :info, "closed due to tcp error: #{inspect error}")
    SMPPHandler.handle_error_closed(handler.smpp_handler, error)
  end

  defp process_packets(handler, data) do
    case Protocol.parse(data) do
      {:ok, nil, rest} -> {:ok, rest}
      {:ok, parse_result, rest} ->
        handle_parse_result(handler, parse_result)
        process_packets(handler, rest)
      {:error, _} = err ->
        log(handler, :warn, "incoming SMPP packet parsing error: #{inspect err}, initiating connection close")
        err
    end
  end

  defp log(handler, level, message) do
    Logger.log(level, "Session #{inspect handler.connection} #{message}")
  end

  defp handle_parse_result(handler, {:pdu, pdu}) do
    log(handler, :debug, "packet received: #{inspect pdu}")
    SMPPHandler.handle_pdu(handler.smpp_handler, pdu)
  end

  defp handle_parse_result(handler, {:unparsed_pdu, pdu, error}) do
    log(handler, :debug, "malformed packet received: #{inspect pdu}")
    SMPPHandler.handle_unparsed_pdu(handler.smpp_handler, pdu, error)
  end

end

