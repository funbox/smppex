defmodule SMPPEX.Ranch.Protocol do

  @behaviour :ranch_protocol
  use GenServer

  require Logger
  alias SMPPEX.Protocol, as: SMPP
  alias SMPPEX.SMPPHandler

  # Application.start(:ranch, :permanent)
  # :ranch_server.set_new_listener_opts(:foo, 400, [:foo, :bar])
  # :ranch_conns_sup.start_link(:foo, :worker, :brutal_kill, :ranch_tcp, 5000, SMPPEX.Ranch.Protocol)
  # {:ok, sock} = :gen_tcp.connect('rubybox.ru', 80, [])
  # :ranch_tcp.controlling_process(sock, s)
  # :ranch_conns_sup.start_protocol(s, sock)

  def start_link(ref, socket, transport, opts) do
	:proc_lib.start_link(__MODULE__, :init, [ref, socket, transport, opts])
  end

  def init(ref, socket, transport, opts) do
    handler = :proplists.get_value(:handler, opts)
    case SMPPHandler.init(handler, ref, socket, transport, self) do
      {:ok, handler} ->
        :ok = :proc_lib.init_ack({:ok, self})
        :ok = :ranch.accept_ack(ref)
        :ok = transport.setopts(socket, [{:active, :once}])
        SMPPHandler.setup_socket(handler, socket, transport)
        state = %{
          ref: ref,
          socket: socket,
          transport: transport,
          handler: handler,
          buffer: <<>>
        }
        :gen_server.enter_loop(__MODULE__, [], state)
      other ->
        :ok = :proc_lib.init_ack({:error, other})
    end
  end

  def handle_info(message, state) do
    {ok, closed, error} = state.transport.messages
    socket = state.socket
    case message do
      {^ok, ^socket, data} ->
        handle_data(state, data)
      {^closed, ^socket} ->
        handle_socket_closed(state)
      {^error, ^socket, reason} ->
        handle_socket_error(state, reason)
      other ->
        Logger.info("Unrecognized message: #{inspect other}")
    end
  end

  defp handle_data(state, data) do
    full_data = state.buffer <> data
    parse_pdus(state, full_data)
  end

  defp parse_pdus(state, data) do
    case SMPP.parse(data) do
      {:ok, nil, data} ->
        new_state = %{state | buffer: data}
        {:noreply, new_state}
      {:ok, parse_result, rest_data} ->
        handle_parse_result(state, parse_result, rest_data)
      {:error, error} ->
        handle_parse_error(state, error)
    end
  end

  defp handle_parse_error(state, error) do
    SMPPHandler.handle_parse_error(state.handler, error)
    {:stop, :normal, state}
  end

  defp handle_parse_result(state, parse_result, rest_data) do
    case SMPPHandler.handle_pdu(state.handler, parse_result) do
      {:ok, handler} ->
        parse_pdus(%{state | handler: handler}, rest_data)
      :stop ->
        {:stop, :normal, state}
    end
  end

  defp handle_socket_closed(state) do
    SMPPHandler.handle_socket_closed(state.handler)
    {:stop, :normal, state}
  end

  defp handle_socket_error(state, reason) do
    SMPPHandler.handle_socket_error(state.handler, reason)
    {:stop, :normal, state}
  end

end


