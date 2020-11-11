defmodule SMPPEX.TransportSession do
  @moduledoc false

  @behaviour :ranch_protocol

  use GenServer
  require Logger

  alias :proc_lib, as: ProcLib
  alias :ranch, as: Ranch
  alias :gen_server, as: GenServerErl

  alias SMPPEX.Protocol, as: SMPP
  alias SMPPEX.Pdu
  alias __MODULE__, as: TransportSession

  @timeout 5000

  defstruct [
    :ref,
    :socket,
    :transport,
    :mode,
    :module,
    :module_opts,
    :module_state,
    :buffer
  ]

  @type socket :: term
  @type transport :: module
  @type reason :: term
  @type state :: term
  @type reply :: term
  @type send_pdu_result :: :ok | {:error, term}
  @type opts :: term
  @type from :: GenServer.from()
  @type request :: term

  @callback init(socket, transport, opts) ::
              {:ok, state}
              | {:eror, reason}

  @callback handle_pdu(SMPP.pdu_parse_result(), state) ::
              {:ok, [Pdu.t()], state}
              | {:stop, reason, [Pdu.t()], state}

  @callback handle_send_pdu_result(Pdu.t(), send_pdu_result, state) :: state

  @callback handle_call(request, from, state) ::
              {:reply, reply, [Pdu.t()], state}
              | {:noreply, [Pdu.t()], state}
              | {:stop, reason, reply, [Pdu.t()], state}
              | {:stop, reason, [Pdu.t()], state}

  @callback handle_cast(request, state) ::
              {:noreply, [Pdu.t()], state}
              | {:stop, reason, [Pdu.t()], state}

  @callback handle_info(request, state) ::
              {:noreply, [Pdu.t()], state}
              | {:stop, reason, [Pdu.t()], state}

  @callback handle_socket_closed(state) :: {reason, state}
  @callback handle_socket_error(error :: term, state) :: {reason, state}

  @callback terminate(reason, state) :: {[Pdu.t()], state}

  @callback code_change(old_vsn :: term | {:down, term}, state, extra :: term) ::
              {:ok, state}
              | {:error, reason}

  # Ranch protocol behaviour

  def start_link(ref, transport, opts) do
    start_link(:mc, {ref, transport, opts})
  end

  def start_link(mode, args) do
    ProcLib.start_link(__MODULE__, :init, [{mode, args}])
  end

  # Manual start, without Ranch

  def start_esme(socket, transport, opts) do
    ref = make_ref()

    case start_link(:esme, {socket, ref, transport, opts}) do
      {:ok, pid} -> grant_socket(pid, ref, transport, socket)
      {:error, _err} = err -> err
    end
  end

  def cast(server, request) do
    GenServer.cast(server, {:cast, request})
  end

  def call(server, request, timeout \\ @timeout) do
    GenServer.call(server, {:call, request}, timeout)
  end

  def reply(from, rep) do
    GenServer.reply(from, rep)
  end

  defp grant_socket(pid, ref, transport, socket) do
    transport.controlling_process(socket, pid)
    Kernel.send(pid, {:socket_granted, ref})
    {:ok, pid}
  end

  def init({:mc, {ref, transport, opts}}) do
    :ok = ProcLib.init_ack({:ok, self()})
    {:ok, socket} = Ranch.handshake(ref)
    {module, module_opts} = opts

    case module.init(socket, transport, module_opts) do
      {:ok, module_state} ->
        state = %TransportSession{
          ref: ref,
          socket: socket,
          transport: transport,
          mode: :mc,
          module: module,
          module_opts: module_opts,
          module_state: module_state,
          buffer: <<>>
        }

        enter_loop(state)

      {:stop, reason} ->
        _ = transport.close(socket)
        Process.exit(self(), reason)
    end
  end

  def init({:esme, {socket, ref, transport, opts}}) do
    {module, module_opts} = opts

    case module.init(socket, transport, module_opts) do
      {:ok, module_state} ->
        :ok = ProcLib.init_ack({:ok, self()})
        :ok = accept_grant(ref)

        state = %TransportSession{
          ref: ref,
          socket: socket,
          transport: transport,
          mode: :esme,
          module: module,
          module_opts: module_opts,
          module_state: module_state,
          buffer: <<>>
        }

        enter_loop(state)

      {:stop, reason} ->
        ProcLib.init_ack({:error, reason})
    end
  end

  defp accept_grant(ref) do
    receive do
      {:socket_granted, ^ref} -> :ok
    end
  end

  defp enter_loop(state) do
    case wait_for_data(state) do
      {:noreply, new_state} ->
        GenServerErl.enter_loop(__MODULE__, [], new_state)

      {:stop, reason, _state} ->
        Process.exit(self(), reason)
    end
  end

  defp wait_for_data(state) do
    {_ok, closed, _error, _passive} = state.transport.messages

    case state.transport.setopts(state.socket, [{:active, :once}]) do
      :ok -> {:noreply, state}
      {:error, ^closed} -> handle_socket_closed(state)
      {:error, reason} -> handle_socket_error(state, reason)
    end
  end

  def handle_info(message, state) do
    {ok, closed, error, _passive} = state.transport.messages

    case message do
      {^ok, _socket, data} ->
        handle_data(state, data)

      {^closed, _socket} ->
        handle_socket_closed(state)

      {^error, _socket, reason} ->
        handle_socket_error(state, reason)

      _ ->
        do_handle_info(message, state)
    end
  end

  defp handle_socket_closed(state) do
    {reason, new_module_state} = state.module.handle_socket_closed(state.module_state)
    stop(%TransportSession{state | module_state: new_module_state}, reason)
  end

  defp handle_socket_error(state, error) do
    {reason, new_module_state} = state.module.handle_socket_error(error, state.module_state)
    stop(%TransportSession{state | module_state: new_module_state}, reason)
  end

  defp do_handle_info(message, state) do
    case state.module.handle_info(message, state.module_state) do
      {:noreply, pdus, module_state} ->
        {:noreply, send_pdus(module_state, state, pdus)}

      {:stop, reason, pdus, module_state} ->
        {:stop, reason, send_pdus(module_state, state, pdus)}
    end
  end

  def handle_call({:call, request}, from, state) do
    case state.module.handle_call(request, from, state.module_state) do
      {:reply, reply, pdus, module_state} ->
        {:reply, reply, send_pdus(module_state, state, pdus)}

      {:noreply, pdus, module_state} ->
        {:noreply, send_pdus(module_state, state, pdus)}

      {:stop, reason, reply, pdus, module_state} ->
        {:stop, reason, reply, send_pdus(module_state, state, pdus)}

      {:stop, reason, pdus, module_state} ->
        {:stop, reason, send_pdus(module_state, state, pdus)}
    end
  end

  def handle_call(request, from, state) do
    handle_call({:call, request}, from, state)
  end

  def handle_cast({:cast, request}, state) do
    case state.module.handle_cast(request, state.module_state) do
      {:noreply, pdus, module_state} ->
        {:noreply, send_pdus(module_state, state, pdus)}

      {:stop, reason, pdus, module_state} ->
        {:stop, reason, send_pdus(module_state, state, pdus)}
    end
  end

  def handle_cast(request, state) do
    handle_cast({:cast, request}, state)
  end

  defp send_binary(state, bin) do
    state.transport.send(state.socket, bin)
  end

  defp send_pdu(state, pdu) do
    case SMPP.build(pdu) do
      {:ok, bin} ->
        send_binary(state, bin)

      error ->
        Logger.info("Error #{inspect(error)}")
        error
    end
  end

  defp send_pdus(module_state, state, []) do
    %TransportSession{state | module_state: module_state}
  end

  defp send_pdus(module_state, state, [pdu | pdus]) do
    new_module_state =
      state.module.handle_send_pdu_result(pdu, send_pdu(state, pdu), module_state)

    send_pdus(new_module_state, state, pdus)
  end

  defp handle_data(state, data) do
    full_data = state.buffer <> data
    parse_pdus(state, full_data)
  end

  defp parse_pdus(state, data) do
    case SMPP.parse(data) do
      {:ok, nil, data} ->
        new_state = %{state | buffer: data}
        wait_for_data(new_state)

      {:ok, parse_result, rest_data} ->
        handle_parse_result(state, parse_result, rest_data)

      {:error, error} ->
        handle_parse_error(state, error)
    end
  end

  defp handle_parse_error(state, error) do
    stop(state, {:parse_error, error})
  end

  defp handle_parse_result(state, parse_result, rest_data) do
    case state.module.handle_pdu(parse_result, state.module_state) do
      {:ok, pdus, module_state} ->
        parse_pdus(send_pdus(module_state, state, pdus), rest_data)

      {:stop, reason, pdus, module_state} ->
        stop(send_pdus(module_state, state, pdus), reason)
    end
  end

  defp stop(state, reason) do
    _ = state.transport.close(state.socket)
    {:stop, reason, state}
  end

  def terminate(reason, state) do
    {pdus, new_module_state} = state.module.terminate(reason, state.module_state)
    send_pdus(new_module_state, state, pdus)
  end

  def code_change(old_vsn, state, extra) do
    case state.module.code_change(old_vsn, state.module_state, extra) do
      {:ok, new_module_state} ->
        {:ok, %TransportSession{state | module_state: new_module_state}}

      {:error, _} = err ->
        err
    end
  end
end
