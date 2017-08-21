defmodule SMPPEX.ESME do
  @moduledoc """
  Module for implementing custom SMPP ESME entities.

  `SMPPEX.ESME` represents a `GenServer` process which spawns and interacts with `SMPPEX.Session`
  `ranch_protocol`. The session is spawned under control of `ranch` supervision tree.
  The session makes all requests to the ESME process *syncronously* (via `GenServer.call`),
  while the ESME process makes only *asyncronous*(via `GenServer.cast`) requests to the session.

  This is made intentionally since this allows:
  * to avoid any kind of deadlocks while the session and the ESME process interact actively;
  * to control incoming SMPP message rate to avoid overflooding;
  * not to lose any control over connection because of the asyncronous nature of TCP implementation in OTP.

  To implement an ESME entitiy, one should implement several callbacks (`SMPPEX.ESME` behaviour).
  The most proper way to do it is to `use` `SMPPEX.ESME`:

  ```
  defmodule MyESME do
    use SMPPEX.ESME

    # ...Callback implementation

  end
  ```

  In this case all callbacks have reasonable defaults.
  """

  alias :erlang, as: Erlang

  alias SMPPEX.ESME
  alias SMPPEX.Pdu
  alias SMPPEX.PduStorage
  alias SMPPEX.Session
  alias SMPPEX.SMPPTimers

  require Logger

  @behaviour Session

  defstruct [
    :module,
    :module_state,
    :timers,
    :pdus,
    :response_limit,
    :sequence_number,
    :time,
    :timer_resolution,
    :tick_timer_ref
  ]

  @default_timeout 5000
  @default_enquire_link_limit 30000
  @default_enquire_link_resp_limit 30000
  @default_inactivity_limit :infinity
  @default_response_limit 60000

  @default_timer_resolution 100

  @default_call_timeout 5000

  @default_transport :ranch_tcp

  @type state :: term
  @type request :: term
  @type reason :: term
  @type reply :: term
  @type send_pdu_result :: SMPPEX.SMPPHandler.send_pdu_result
  @type session :: pid
  @type from :: Session.from

  @callback init(Session.socket, Session.transport, args :: term) ::
    {:ok, state} |
    {:stop, reason}

  @callback handle_pdu(pdu :: Pdu.t, state) ::
    {:ok, state} |
    {:ok, [Pdu.t], state} |
    {:stop, reason, state}

  @callback handle_unparsed_pdu(pdu :: RawPdu.t, error :: term, state) ::
    {:ok, state} |
    {:ok, [Pdu.t], state} |
    {:stop, reason, state}

  @callback handle_resp(pdu :: Pdu.t, original_pdu :: Pdu.t, state) ::
    {:ok, state} |
    {:ok, [Pdu.t], state} |
    {:stop, reason, state}

  @callback handle_resp_timeout(pdus :: [Pdu.t], state) ::
    {:ok, state} |
    {:ok, [Pdu.t], state} |
    {:stop, reason, state}

  @callback handle_send_pdu_result(pdu :: Pdu.t, send_pdu_result, state) :: state

  @callback handle_socket_error(error :: term, state) :: {exit_reason :: term, state}

  @callback handle_socket_closed(state) :: {exit_reason :: term, state}

  @callback handle_call(request, from, state) ::
    {:reply, reply, state} |
    {:reply, reply, [Pdu.t], state} |
    {:noreply, [Pdu.t], state} |
    {:noreply, state} |
    {:stop, reason, reply, state} |
    {:stop, reason, state}

  @callback handle_cast(request, from, state) ::
    {:noreply, state} |
    {:noreply, [Pdu.t], state} |
    {:stop, reason, state}

  @callback handle_info(request, state) ::
    {:noreply, state} |
    {:noreply, [Pdu.t], state} |
    {:stop, reason, state}

  @callback terminate(reason, lost_pdus :: [Pdu.t], state) :: any

  @callback code_change(old_vsn :: term | {:down, term}, state, extra :: term) ::
    {:ok, state} |
    {:error, reason}

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour SMPPEX.ESME

      require Logger

      @doc false
      def init(_socket, _transport, args) do
        {:ok, args}
      end

      @doc false
      def handle_pdu(_pdu, state), do: {:ok, state}

      @doc false
      def handle_unparsed_pdu(_pdu, _error, state), do: {:ok, state}

      @doc false
      def handle_resp(_pdu, _original_pdu, state), do: {:ok, state}

      @doc false
      def handle_resp_timeout(_pdus, state), do: {:ok, state}

      @doc false
      def handle_send_pdu_result(_pdu, _result, state), do: state

      @doc false
      def handle_socket_error(error, state), do: {{:socket_error, error}, state}

      @doc false
      def handle_socket_closed(state), do: {:socket_closed, state}

      @doc false
      def handle_call(_request, _from, state), do: {:reply, :ok, state}

      @doc false
      def handle_cast(_request, state), do: {:noreply, state}

      @doc false
      def handle_info(_request, state), do: {:noreply, state}

      @doc false
      def terminate(reason, lost_pdus, _state) do
        Logger.info("ESME #{self()} stopped with reason: #{inspect reason}, lost_pdus: #{inspect lost_pdus}")
      end

      @doc false
      def code_change(_vsn, state, _extra), do: {:ok, state}

      defoverridable [
        init: 3,
        handle_pdu: 2,
        handle_unparsed_pdu: 3,
        handle_resp: 3,
        handle_resp_timeout: 2,
        handle_send_pdu_result: 3,
        handle_socket_error: 2,
        handle_socket_closed: 1,
        handle_call: 3,
        handle_cast: 2,
        handle_info: 2,
        terminate: 3,
        code_change: 3
      ]
    end
  end

  # Public interface

  @spec start_link(host :: term, port :: non_neg_integer, {module, args :: term}, opts :: Keyword.t) :: GenServer.on_start

  @doc """
  Starts ESME entitiy.

  The function does not return until ESME successfully connects to the specified
  `host` and `port` and initializes or fails.

  `module` is the callback module which should implement `SMPPEX.ESME` behaviour.
  `args` is the argument passed to the `init` callback.
  `opts` is a keyword list of different options:
  * `:transport` is Ranch transport used for TCP connection: either `ranch_tcp` (the default) or
  `ranch_ssl`;
  * `:gen_server_opts` is a list of options passed directly to the underlying `GenServer.start_link` call,
  the default is `[]`;
  * `:timeout` is timeout for the whole connect and initialization process. The default is #{@default_timeout} ms;
  * `:esme_opts` is a keyword list of ESME options:
      - `:timer_resolution` is interval of internal `ticks` on which time related events happen, like checking timeouts
      for pdus, checking SMPP timers, etc. The default is #{@default_timer_resolution} ms;
      - `:enquire_link_limit` is value for enquire_link SMPP timer, i.e. the interval of SMPP session inactivity after which
      enquire_link PDU is send to "ping" the connetion. The default value is #{@default_enquire_link_limit} ms;
      - `:enquire_link_resp_limit` is the maximum time for which ESME waits for enquire_link PDU response. If the
      response is not received within this interval of time and no activity from the peer occurs, the session is then considered
      dead and the ESME stops. The default value is #{@default_enquire_link_resp_limit} ms;
      - `:inactivity_limit` is the maximum time for which the peer is allowed not to send PDUs (which are not response PDUs).
      If no such PDUs are received within this interval of time, ESME stops. The default is #{@default_inactivity_limit} ms;
      - `:response_limit` is the maximum time to wait for a response for a previously sent PDU. If the response is
      not received within this interval, `handle_resp_timeout` callback is triggered for the original pdu. If the response
      is received later, it is discarded. The default value is #{@default_response_limit} ms.
  If `:esme_opts` list of options is ommited, all options take their default values.

  The whole `opts` argument may also be ommited in order to start ESME with the defaults.

  The returned value is either `{:ok, pid}` or `{:error, reason}`.
  """
  def start_link(host, port, {_module, _args} = mod_with_args, opts \\ []) do
    transport = Keyword.get(opts, :transport, @default_transport)
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    sock_opts = [:binary, {:packet, 0}, {:active, :once}]
    esme_opts = Keyword.get(opts, :esme_opts, [])

    case transport.connect(convert_host(host), port, sock_opts, timeout) do
      {:ok, socket} ->
        session_opts = {__MODULE__, [mod_with_args, esme_opts]}
        case Session.start_link(socket, transport, session_opts) do
          {:ok, pid} ->
            {:ok, pid}
          {:error, _} = error ->
            transport.close(socket)
            error
        end
      {:error, _} = error -> error
    end
  end

  @spec send_pdu(session, Pdu.t) :: :ok

  @doc """
  Sends outcoming PDU from the ESME.

  The whole command is sent to the ESME asyncronously. The further lifecycle of the PDU
  can be traced through callbacks.
  """
  def send_pdu(pid, pdu) do
    Session.call(pid, {:send_pdu, pdu})
  end

  @spec stop(session) :: :ok

  @doc """
  Stops ESME asyncronously.

  The very moment of the SMPP session termination can be traced via `handle_stop` callback.
  """
  def stop(pid, reason \\ :normal) do
    Session.call(pid, {:stop, reason})
  end

  @spec call(session, request :: term, timeout) :: term

  @doc """
  Makes a syncronous call to ESME.

  The call is handled by `handle_call/3` ESME callback.
  """
  def call(pid, request, timeout \\ @default_call_timeout) do
    Session.call(pid, {:call, request}, timeout)
  end

  @spec cast(session, request :: term) :: :ok

  @doc """
  Makes an asyncronous call to ESME.

  The call is handled by `handle_cast/2` ESME callback.
  """
  def cast(pid, request) do
    Session.cast(pid, {:cast, request})
  end

  @spec reply(from, response :: term) :: :ok

  def reply(from, response) do
    Session.reply(from, response)
  end

  # SMPP.Session callbacks

  def init(socket, transport, [{module, args}, esme_opts]) do
    case module.init(socket, transport, args) do
      {:ok, state} ->
        timer_resolution = Keyword.get(esme_opts, :timer_resolution, @default_timer_resolution)
        timer_ref = Erlang.start_timer(timer_resolution, self(), :emit_tick)

        enquire_link_limit = Keyword.get(esme_opts, :enquire_link_limit,  @default_enquire_link_limit)
        enquire_link_resp_limit = Keyword.get(esme_opts, :enquire_link_resp_limit,  @default_enquire_link_resp_limit)
        inactivity_limit = Keyword.get(esme_opts, :inactivity_limit, @default_inactivity_limit)

        time = SMPPEX.Time.monotonic

        timers = SMPPTimers.new(
          time,
          :infinity,
          enquire_link_limit,
          enquire_link_resp_limit,
          inactivity_limit
        )

        {:ok, pdu_storage} = PduStorage.start_link()
        response_limit = Keyword.get(esme_opts, :response_limit, @default_response_limit)

        {:ok, %ESME{
          module: module,
          module_state: state,
          timers: timers,
          pdus: pdu_storage,
          response_limit: response_limit,
          sequence_number: 0,
          time: time,
          timer_resolution: timer_resolution,
          tick_timer_ref: timer_ref
        }}
      {:stop, _} = stop ->
        stop
    end
  end

  def handle_pdu({:unparsed_pdu, raw_pdu, error}, st) do
    {st.module.handle_unparsed_pdu(raw_pdu, error, st.module_state), st}
    |> process_handle_unparsed_pdu_reply()
  end

  def handle_pdu({:pdu, pdu}, st) do
    if Pdu.resp?(pdu) do
      pdu
      |> handle_resp_pdu(st)
      |> process_handle_resp_reply()
    else
      pdu
      |> handle_non_resp_pdu(st)
      |> process_handle_pdu_reply()
    end
  end

  def handle_send_pdu_result(pdu, send_pdu_result, st) do
    new_module_state = st.module.handle_send_pdu_result(pdu, send_pdu_result, st.module_state)
    %ESME{st | module_state: new_module_state}
  end

  def handle_call({:send_pdu, pdu}, _from, st) do
    {{:reply, :ok, [pdu], st.module_state}, st}
    |> process_handle_call_reply()
  end

  def handle_call({:stop, reason}, _from, st) do
    {{:stop, reason, :ok, st.module_state}, st}
    |> process_handle_call_reply()
  end

  def handle_call({:call, request}, from, st) do
    {st.module.handle_call(request, from, st.module_state), st}
    |> process_handle_call_reply()
  end

  def handle_cast({:cast, request}, st) do
    {st.module.handle_cast(request, st.module_state), st}
    |> process_handle_cast_reply()
  end

  @doc false
  def handle_info({:timeout, _timer_ref, :emit_tick}, st) do
    new_tick_timer_ref = Erlang.start_timer(st.timer_resolution, self(), :emit_tick)
    Erlang.cancel_timer(st.tick_timer_ref)
    Kernel.send self(), {:check_timers, SMPPEX.Time.monotonic}
    Kernel.send self(), {:check_expired_pdus, SMPPEX.Time.monotonic}
    {:noreply, [], %ESME{st | tick_timer_ref: new_tick_timer_ref}}
  end

  def handle_info({:check_timers, time}, st) do
    check_timers(time, st)
  end

  def handle_info({:check_expired_pdus, time}, st) do
    check_expired_pdus(time, st)
  end

  def handle_info(request, st) do
    {st.module.handle_info(request, st.module_state), st}
    |> process_handle_info_reply()
  end

  def handle_socket_closed(st) do
    {reason, new_module_state} = st.module.handle_socket_closed(st.module_state)
    {reason, %ESME{st | module_state: new_module_state}}
  end

  def handle_socket_error(error, st) do
    {reason, new_module_state} = st.module.handle_socket_error(error, st.module_state)
    {reason, %ESME{st | module_state: new_module_state}}
  end

  def terminate(reason, st) do
    lost_pdus = PduStorage.fetch_all(st.pdus)
    st.module.terminate(reason, lost_pdus, st.module_state)
  end

  def code_change(old_vsn, st, extra) do
    case st.module.code_change(old_vsn, st.module_state, extra) do
      {:ok, new_module_state} ->
        {:ok, %ESME{st | module_state: new_module_state}}
      {:error, _} = err ->
        err
    end
  end

  # Private

  defp handle_non_resp_pdu(pdu, st) do
    new_timers = SMPPTimers.handle_peer_transaction(st.timers, st.time)
    {
      st.module.handle_pdu(pdu, st.module_state),
      %ESME{st | timers: new_timers}
    }
  end

  defp handle_resp_pdu(pdu, st) do
    sequence_number = Pdu.sequence_number(pdu)
    new_timers = SMPPTimers.handle_peer_action(st.timers, st.time)
    new_st = %ESME{st | timers: new_timers}
    case PduStorage.fetch(st.pdus, sequence_number) do
      [] ->
        Logger.info("esme #{inspect self()}, resp for unknown pdu(sequence_number: #{sequence_number}), dropping")
        {{:ok, new_st.module_state}, new_st}
      [original_pdu] ->
        handle_resp_for_known_pdu(pdu, original_pdu, new_st)
    end
  end

  defp handle_resp_for_known_pdu(pdu, original_pdu, st) do
    new_st = update_timer_bind_status(pdu, st)
    {
      new_st.module.handle_resp(pdu, original_pdu, new_st.module_state),
      new_st
    }
  end

  defp update_timer_bind_status(pdu, st) do
    if Pdu.bind_resp?(pdu) && Pdu.success_resp?(pdu) do
      new_timers = SMPPTimers.handle_bind(st.timers, st.time)
      %ESME{st | timers: new_timers}
    else
      st
    end
  end

  defp check_expired_pdus(time, st) do
    case PduStorage.fetch_expired(st.pdus, time) do
      [] ->
        {:noreply, [], st}
      pdus ->
        module_reply = st.module.handle_resp_timeout(pdus, st.module_state)
        process_handle_resp_timeout_reply({module_reply, st})
    end
  end

  defp check_timers(time, st) do
    case SMPPTimers.handle_tick(st.timers, time) do
      {:ok, new_timers} ->
        new_st = %ESME{st | timers: new_timers, time: time}
        {:noreply, [], new_st}
      {:stop, reason} ->
        Logger.info("esme #{inspect self()}, being stopped by timers(#{reason})")
        {:stop, {:timers, reason}, [], st}
      {:enquire_link, new_timers} ->
        enquire_link = SMPPEX.Pdu.Factory.enquire_link
        {new_st, pdus} = save_sent_pdus(
          [enquire_link],
          %ESME{st | timers: new_timers, time: time}
        )
        {:noreply, pdus, new_st}
    end
  end

  defp save_sent_pdus(pdus, st, pdus_to_send \\ [])
  defp save_sent_pdus([], st, pdus_to_send), do: {st, Enum.reverse(pdus_to_send)}
  defp save_sent_pdus([pdu | pdus], st, pdus_to_send) do
    if Pdu.resp?(pdu) do
      save_sent_pdus(pdus, st, [pdu | pdus_to_send])
    else
      sequence_number = st.sequence_number + 1
      new_pdu = %Pdu{pdu | sequence_number: sequence_number}
      true = PduStorage.store(st.pdus, new_pdu, st.time + st.response_limit)
      new_st = %ESME{st | sequence_number: sequence_number}
      save_sent_pdus(pdus, new_st, [new_pdu | pdus_to_send])
    end
  end

  defp process_handle_pdu_reply({{:ok, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_pdu_reply({{:ok, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_pdu_reply({{:stop, _reason, _mst}, _st} = arg), do: process_reply(arg)

  defp process_handle_unparsed_pdu_reply({{:ok, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_unparsed_pdu_reply({{:ok, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_unparsed_pdu_reply({{:stop, _reason, _mst}, _st} = arg), do: process_reply(arg)

  defp process_handle_resp_reply({{:ok, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_resp_reply({{:ok, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_resp_reply({{:stop, _reason, _mst}, _st} = arg), do: process_reply(arg)

  defp process_handle_resp_timeout_reply({{:ok, mst}, st}), do: process_reply({{:noreply, mst}, st})
  defp process_handle_resp_timeout_reply({{:ok, pdus, mst}, st}), do: process_reply({{:noreply, pdus, mst}, st})
  defp process_handle_resp_timeout_reply({{:stop, reason, mst}, st} = arg), do: process_reply(arg)

  defp process_handle_call_reply({{:reply, _reply, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_call_reply({{:reply, _reply, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_call_reply({{:noreply, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_call_reply({{:noreply, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_call_reply({{:stop, _rsn, _reply, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_call_reply({{:stop, _rsn, _mst}, _st} = arg), do: process_reply(arg)

  defp process_handle_cast_reply({{:noreply, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_cast_reply({{:noreply, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_cast_reply({{:stop, _rsn, _mst}, _st} = arg), do: process_reply(arg)

  defp process_handle_info_reply({{:noreply, _pdus, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_info_reply({{:noreply, _mst}, _st} = arg), do: process_reply(arg)
  defp process_handle_info_reply({{:stop, _rsn, _mst}, _st} = arg), do: process_reply(arg)

  defp process_reply({{:ok, module_state}, st}) do
    {:ok, [], %ESME{st | module_state: module_state}}
  end
  defp process_reply({{:ok, pdus, module_state}, st}) do
    {new_st, pdus_to_send} = save_sent_pdus(pdus, st)
    {:ok, pdus_to_send, %ESME{new_st | module_state: module_state}}
  end
  defp process_reply({{:reply, reply, module_state}, st}) do
    {:reply, reply, [], %ESME{st | module_state: module_state}}
  end
  defp process_reply({{:reply, reply, pdus, module_state}, st}) do
    {new_st, pdus_to_send} = save_sent_pdus(pdus, st)
    {:reply, reply, pdus_to_send, %ESME{new_st | module_state: module_state}}
  end
  defp process_reply({{:noreply, module_state}, st}) do
    {:noreply, [], %ESME{st | module_state: module_state}}
  end
  defp process_reply({{:noreply, pdus, module_state}, st}) do
    {new_st, pdus_to_send} = save_sent_pdus(pdus, st)
    {:noreply, pdus_to_send, %ESME{new_st | module_state: module_state}}
  end
  defp process_reply({{:stop, reason, reply, module_state}, st}) do
    {:stop, reason, reply, [], %ESME{st | module_state: module_state}}
  end
  defp process_reply({{:stop, reason, module_state}, st}) do
    {:stop, reason, [], %ESME{st | module_state: module_state}}
  end

  defp convert_host(host) when is_binary(host), do: to_char_list(host)
  defp convert_host(host), do: host

end
