defmodule SMPPEX.Session do
  @moduledoc false

  @behaviour :ranch_protocol

  use GenServer
  require Logger

  alias :gen_server, as: ErlangGenServer
  alias :proc_lib, as: ProcLib
  alias :proplists, as: Proplists
  alias :ranch, as: Ranch

  alias SMPPEX.Protocol, as: SMPP
  alias SMPPEX.SMPPHandler
  alias SMPPEX.Pdu

  @spec send_pdu(pid, Pdu.t) :: :ok

  def send_pdu(pid, pdu) do
    send_pdus(pid, [pdu])
  end

  @spec send_pdus(pid, [Pdu.t]) :: :ok

  def send_pdus(pid, pdus) do
    GenServer.cast(pid, {:send_pdus, pdus})
  end

  @spec send_binary(pid, data :: binary) :: :ok

  def send_binary(pid, data) do
    GenServer.cast(pid, {:send_binary, data})
  end

  @spec stop(pid, term) :: :ok

  def stop(pid, reason) do
    GenServer.cast(pid, {:stop, reason})
  end

  # @spec start_link(Ranch.ref, term, module, Keyword.t) :: {:ok, pid} | {:error, term}
  # Ranch handles this return type, but Dialyzer is not happy with it

  def start_link(ref, socket, transport, opts) do
	   ProcLib.start_link(__MODULE__, :init, [ref, socket, transport, opts])
  end

  def init(ref, socket, transport, opts) do
    session_factory = Proplists.get_value(:handler, opts)
    case session_factory.(ref, socket, transport, self()) do
      {:ok, session} ->
        :ok = ProcLib.init_ack({:ok, self()})
        :ok = Ranch.accept_ack(ref)
        state = %{
          ref: ref,
          socket: socket,
          transport: transport,
          session: session,
          buffer: <<>>
        }
        wait_for_data(state)
        ErlangGenServer.enter_loop(__MODULE__, [], state)
      {:error, _} = error ->
        :ok = ProcLib.init_ack(error)
    end
  end

  defp wait_for_data(state) do
    :ok = state.transport.setopts(state.socket, [{:active, :once}])
  end

  def handle_info(message, state) do
    {ok, closed, error} = state.transport.messages
    case message do
      {^ok, _socket, data} ->
        handle_data(state, data)
      {^closed, _socket} ->
        handle_socket_closed(state)
      {^error, _socket, reason} ->
        handle_socket_error(state, reason)
      other ->
        Logger.info("Unrecognized message: #{inspect other}")
        do_stop(state, :unrecognized_message)
    end
  end

  def handle_cast({:send_binary, data}, state) do
    do_send_binary(state, data)
    {:noreply, state}
  end

  def handle_cast({:send_pdus, pdus}, state) do
    {:noreply, do_send_pdus(state, pdus)}
  end

  def handle_cast({:stop, reason}, state) do
    do_stop(state, reason)
  end

  defp do_send_binary(state, bin) do
    state.transport.send(state.socket, bin)
  end

  defp do_send_pdu(state, pdu) do
    case SMPP.build(pdu) do
      {:ok, bin} ->
        do_send_binary(state, bin)
      error ->
        Logger.info("Error #{inspect error}")
        error
    end
  end

  defp do_send_pdus(state, []), do: state
  defp do_send_pdus(state, [pdu | pdus]) do
    new_session = SMPPHandler.handle_send_pdu_result(state.session, pdu, do_send_pdu(state, pdu))
    do_send_pdus(%{state | session: new_session}, pdus)
  end

  defp handle_data(state, data) do
    full_data = state.buffer <> data
    parse_pdus(state, full_data)
  end

  defp parse_pdus(state, data) do
    case SMPP.parse(data) do
      {:ok, nil, data} ->
        new_state = %{state | buffer: data}
        wait_for_data(state)
        {:noreply, new_state}
      {:ok, parse_result, rest_data} ->
        handle_parse_result(state, parse_result, rest_data)
      {:error, error} ->
        handle_parse_error(state, error)
    end
  end

  defp handle_parse_error(state, error) do
    do_stop(state, {:parse_error, error})
  end

  defp handle_parse_result(state, parse_result, rest_data) do
    case SMPPHandler.handle_pdu(state.session, parse_result) do
      :ok ->
        parse_pdus(state, rest_data)
      {:ok, session} ->
        parse_pdus(%{state | session: session}, rest_data)
      {:ok, session, pdus} ->
        new_state = do_send_pdus(%{state | session: session}, pdus)
        parse_pdus(new_state, rest_data)
      {:stop, session, pdus, reason} ->
        new_state = do_send_pdus(%{state | session: session}, pdus)
        do_stop(new_state, reason)
      {:stop, reason} ->
        do_stop(state, reason)
    end
  end

  defp handle_socket_closed(state) do
    do_stop(state, :socket_closed)
  end

  defp handle_socket_error(state, reason) do
    do_stop(state, {:socket_error, reason})
  end

  defp do_stop(state, reason) do
    _ = state.transport.close(state.socket)
    SMPPHandler.handle_stop(state.session, reason)
    {:stop, :normal, state}
  end

end
