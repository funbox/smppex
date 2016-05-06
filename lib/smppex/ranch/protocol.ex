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

  def send_pdu(pid, pdu, timeout) do
    GenServer.call(pid, {:send_pdu, pdu}, timeout)
  end

  def send_pdu(pid, pdu) do
    GenServer.call(pid, {:send_pdu, pdu})
  end

  def stop(pid, timeout) do
    GenServer.call(pid, :stop, timeout)
  end

  def stop(pid) do
    GenServer.call(pid, :stop)
  end

  def send_pdus_async(pid, pdus) do
    GenServer.cast(pid, {:send_pdus, pdus})
  end

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

  def handle_call({:send_pdu, pdu}, _from, state) do
    {:reply, do_send_pdu(state, pdu), state}
  end

  def handle_call(:stop, _from, state) do
    do_stop(state)
    {:stop, :normal, :ok, state}
  end

  def handle_cast({:send_pdus, pdus}, state) do
    {:noreply, send_pdus(state, pdus)}
  end

  defp do_send_pdu(state, pdu) do
    case SMPP.build(pdu) do
      {:ok, binary} -> state.transport.send(state.socket, binary)
      error -> error
    end
  end

  defp send_pdus(state, []), do: state
  defp send_pdus(state, [pdu | pdus]) do
    new_handler = SMPPHandler.handle_send_pdu_result(state.handler, send_pdu(state, pdu))
    send_pdus(%{state | handler: new_handler}, pdus)
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
    do_stop(state)
  end

  #   @type handle_pdu_result :: {:ok, handler} | {:ok, handler, [Pdu.t]} | {:stop, handler, [Pdu.t]} | :stop

  defp handle_parse_result(state, parse_result, rest_data) do
    case SMPPHandler.handle_pdu(state.handler, parse_result) do
      {:ok, handler} ->
        parse_pdus(%{state | handler: handler}, rest_data)
      {:ok, handler, pdus} ->
        new_state = send_pdus(%{ state | handler: handler }, pdus)
        parse_pdus(new_state, rest_data)
      {:stop, handler, pdus} ->
        new_state = send_pdus(%{ state | handler: handler }, pdus)
        do_stop(new_state)
      :stop ->
        do_stop(state)
    end
  end

  defp handle_socket_closed(state) do
    SMPPHandler.handle_socket_closed(state.handler)
    SMPPHandler.handle_stop(state.handler)
    {:stop, :normal, state}
  end

  defp handle_socket_error(state, reason) do
    SMPPHandler.handle_socket_error(state.handler, reason)
    do_stop(state)
  end

  defp do_stop(state) do
    SMPPHandler.handle_stop(state.handler)
    :ok = state.transport.close(state.socket)
    {:stop, :normal, state}
  end

end


