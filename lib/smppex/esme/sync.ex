defmodule SMPPEX.ESME.Sync do
  @moduledoc """
  `SMPPEX.ESME.Sync` is an ESME implementation of `SMPPEX.Session`. It allows to send PDUs
  to SMSCs in a syncronous way, i.e. blocking till the response PDU comes.

  One can use with `SMPPEX.ESME.Sync` all
  methods provided by `SMPPEX.Session` like `SMPPEX.Session.send_pdu/2`, etc.
  """

  use SMPPEX.Session

  alias SMPPEX.ESME
  alias SMPPEX.Pdu
  alias SMPPEX.Session

  require Logger

  @default_timeout 5000

  defmodule St do
    @moduledoc false

    defstruct resp_waiters: %{},
              pdu_waiter: nil,
              additional_pdus: []
  end

  # Public interface

  @spec start_link(host :: term, port :: non_neg_integer, opts :: Keyword.t()) ::
          GenServer.on_start()

  @doc """
  Starts `SMPPEX.ESME.Sync`.

  `opts` is a keyword list of `SMPPEX.ESME` options which is directly passed to
  the underlying `SMPPEX.ESME.start_link/4` call.
  """
  def start_link(host, port, opts \\ []) do
    ESME.start_link(
      host,
      port,
      {__MODULE__, %St{}},
      opts
    )
  end

  @spec request(esme :: pid, pdu :: Pdu.t(), timeout :: non_neg_integer) ::
          {:ok, resp :: Pdu.t()} | :timeout | :stop | {:error, reason :: term}

  @doc """
  Syncronously sends a PDU and wait for at most `timeout` ms for a response PDU.

  The default timeout is #{@default_timeout} ms.

  The result value is one of:
  * `{:ok, resp}`, where `resp` is a successfully received response PDU;
  * `:timeout` if the response PDU was not received within `timeout`;
  * `:stop` if the ESME stopped while a response was awaited;
  * `{:error, reason}` if the request failed.

  """
  def request(esme, pdu, timeout \\ @default_timeout) do
    call(esme, {:request, pdu}, timeout)
  end

  @type awaited ::
          {:pdu, pdu :: Pdu.t()}
          | {:resp, resp_pdu :: Pdu.t(), original_pdu :: Pdu.t()}
          | {:timeout, pdu :: Pdu.t()}
          | {:error, pdu :: Pdu.t(), reason :: any}

  @spec wait_for_pdus(esme :: pid, timeout :: non_neg_integer) :: [awaited] | :timeout | :stop

  @doc """
  Syncronously wait for incoming PDUs. If the ESME already have some received PDUs,
  they are returned immediately.

  The default timeout is #{@default_timeout} ms.

  The result value is `:timeout`, `:stop` or a list of the following items:
  * `{:pdu, pdu}`, where `pdu` is an incoming PDU;
  * `{:resp, resp_pdu, original_pdu}` where `resp_pdu` is an incoming reply for a
  previously sent `original_pdu`;
  * `{:timeout, pdu}` for `pdu`'s which have not received a response within ESME timeout;
  * `{:error, pdu, reason}` for outcoming PDUs which were not successfully sent due to `reason`;
  * `{:ok, pdu}` for outcoming PDUs which were successfully sent.

  `:timeout` returned value indicates that the ESME didn't receive any PDUs within `timeout`.
  `:stop` value indicates that the ESME stopped while waiting for incoming PDUs.

  """
  def wait_for_pdus(esme, timeout \\ @default_timeout) do
    call(esme, :wait_for_pdus, timeout)
  end

  @spec pdus(esme :: pid, timeout) :: [awaited]

  @doc """
  A nonblocking version of `wait_for_pdus/2`.

  The difference is that it always immediately returns a list of items(maybe empty)
  and never returns `:timeout` or `:stop`.
  """
  def pdus(esme, timeout \\ @default_timeout) do
    Session.call(esme, :pdus, timeout)
  end

  @spec stop(esme :: pid) :: :ok

  @doc """
  Stops ESME.
  """
  def stop(esme) do
    Session.stop(esme)
  end

  # Session callbacks

  @impl SMPPEX.Session
  def init(_socket, _transport, st) do
    Process.flag(:trap_exit, true)
    {:ok, st}
  end

  @impl SMPPEX.Session
  def handle_call({:call, {:request, pdu}, from}, _from, st) do
    {:reply, :ok, [pdu], add_resp_waiter(st, pdu, from)}
  end

  @impl SMPPEX.Session
  def handle_call(:pdus, _from, st) do
    pdus = Enum.reverse(st.additional_pdus)
    new_st = %St{st | additional_pdus: []}
    {:reply, pdus, new_st}
  end

  @impl SMPPEX.Session
  def handle_call({:call, :wait_for_pdus, from}, _from, st) do
    new_st =
      case st.additional_pdus do
        [_ | _] ->
          pdus = Enum.reverse(st.additional_pdus)
          reply(from, pdus)
          %St{st | additional_pdus: []}

        [] ->
          if st.pdu_waiter do
            reply(st.pdu_waiter, [])
          end

          %St{st | pdu_waiter: from}
      end

    {:reply, :ok, new_st}
  end

  @impl SMPPEX.Session
  def handle_resp(pdu, original_pdu, st) do
    if has_resp_waiter?(st, original_pdu) do
      resp_waiter = get_resp_waiter(st, original_pdu)
      reply(resp_waiter, {:ok, pdu})
      {:ok, delete_resp_waiter(st, original_pdu)}
    else
      {:ok, push_to_waiting({:resp, pdu, original_pdu}, st)}
    end
  end

  @impl SMPPEX.Session
  def handle_resp_timeout(pdus, st) do
    {:ok, process_timeouts(pdus, st)}
  end

  @impl SMPPEX.Session
  def handle_pdu(pdu, st) do
    {:ok, push_to_waiting({:pdu, pdu}, st)}
  end

  @impl SMPPEX.Session
  def handle_socket_closed(st) do
    {:normal, st}
  end

  @impl SMPPEX.Session
  def handle_socket_error(error, st) do
    {{:socket_error, error}, st}
  end

  @impl SMPPEX.Session
  def terminate(_reason, _los_pdus, st) do
    case st.pdu_waiter do
      nil -> :nop
      from -> reply(from, :stop)
    end

    for from <- Map.values(st.resp_waiters) do
      reply(from, :stop)
    end

    :stop
  end

  @impl SMPPEX.Session
  def handle_send_pdu_result(pdu, result, st) do
    case result do
      :ok ->
        if has_resp_waiter?(st, pdu) do
          st
        else
          push_to_waiting({:ok, pdu}, st)
        end

      {:error, error} = err ->
        if has_resp_waiter?(st, pdu) do
          resp_waiter = get_resp_waiter(st, pdu)
          reply(resp_waiter, err)
          delete_resp_waiter(st, pdu)
        else
          push_to_waiting({:error, pdu, error}, st)
        end
    end
  end

  defp process_timeouts([], st), do: st

  defp process_timeouts([pdu | pdus], st) do
    if has_resp_waiter?(st, pdu) do
      resp_waiter = get_resp_waiter(st, pdu)
      reply(resp_waiter, :timeout)
      process_timeouts(pdus, delete_resp_waiter(st, pdu))
    else
      process_timeouts(pdus, push_to_waiting({:timeout, pdu}, st))
    end
  end

  defp has_resp_waiter?(st, pdu), do: Map.has_key?(st.resp_waiters, pdu.ref)

  defp add_resp_waiter(st, pdu, from) do
    %St{st | resp_waiters: Map.put(st.resp_waiters, Pdu.ref(pdu), from)}
  end

  defp get_resp_waiter(st, pdu) do
    Map.get(st.resp_waiters, Pdu.ref(pdu))
  end

  defp delete_resp_waiter(st, pdu) do
    %St{st | resp_waiters: Map.delete(st.resp_waiters, Pdu.ref(pdu))}
  end

  defp push_to_waiting(pdu_info, st) do
    pdus = [pdu_info | st.additional_pdus]

    if st.pdu_waiter do
      reply(st.pdu_waiter, Enum.reverse(pdus))
      %St{st | pdu_waiter: nil, additional_pdus: []}
    else
      %St{st | additional_pdus: pdus}
    end
  end

  defp call(pid, request, timeout) do
    ref = make_ref()
    from = {ref, self()}
    :ok = Session.call(pid, {:call, request, from})

    receive do
      {^ref, response} -> response
    after
      timeout -> :timeout
    end
  end

  defp reply({ref, pid} = _from, response) do
    send(pid, {ref, response})
  end
end
