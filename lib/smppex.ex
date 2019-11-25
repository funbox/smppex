defmodule SMPPEX do
  @moduledoc ~S"""
  SMPPEX is a framework for building SMPP servers and clients (which are often
  referred to as MC and ESME entities respectevely).

  The major features exposed by the library are:

    * `SMPPEX.ESME` module and behaviour for implementing ESME entities;
    * `SMPPEX.MC` module and behaviour for implementing MC entities;
    * `SMPPEX.ESME.Sync` module representing simple ready to use SMPP client.

  Also one of the core features of the library is simplicity: both code simplicity and simplicity of use.

    * The library does not have much TCP handling or session management functionality,
    it is based on great [`ranch`](https://github.com/ninenines/ranch) library.
    * SMPP session is symmetric(used both in ESME and MC) and is implemented as
    `ranch_protocol` behaviour.
    * The library includes an easy and ready to use SMPP client (`SMPP.ESME.Sync`) which
    has capabilities of synchronous SMS sending and do not require implementing ESME behavior.
    There is also an SMPP testing tool [`smppsend`](https://github.com/funbox/smppsend)
    based on this client.

  ## SMPPEX.ESME.Sync

  `SMPPEX.ESME.Sync` is the most straightforward way to interact with an SMSC. Example:

      {:ok, esme} = SMPPEX.ESME.Sync.start_link(host, port)

      bind = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
      {:ok, _bind_resp} = SMPPEX.ESME.Sync.request(esme, bind)

      # We are bound, let's send a message

      submit_sm = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 1}, {"to", 1, 1}, "hello!")
      {:ok, submit_sm_resp} = SMPPEX.ESME.Sync.request(esme, submit_sm)

      # Message is sent, let's get the obtained id:

      message_id = SMPPEX.Pdu.field(submit_sm_resp, :message_id)

      # Now let's wait for a delivery report:

      delivery_report? = fn(pdu) ->
        SMPPEX.Pdu.command_name(pdu) == :deliver_sm and
          SMPPEX.Pdu.field(pdu, :receipted_message_id) == message_id
      end

      delivery_reports = case SMPPEX.ESME.Sync.wait_for_pdus(esme, 60000) do
        :stop ->
          Logger.info("Ooops, ESME stopped")
          []
        :timeout ->
          Logger.info("No DLR in 60 seconds")
          []
        received_items ->
          # Let's filter out DLRs for the previously submitted message
          for {:pdu, pdu}  <- received_items, delivery_report?.(pdu), do: pdu
      end

  ## SMPPEX.ESME

  `SMPPEX.ESME` can be used when more complicated client logic is needed, for example
  custom immediate reactions to all incoming PDUs, rps/window control, etc.

  `SMPPEX.Session` provides "empty" defaults for all required callbacks, so minimal ESME
  could be very simple:

      defmodule DummyESME do
        use SMPPEX.Session

        def start_link(host, port) do
          SMPPEX.ESME.start_link(host, port, {__MODULE__, []})
        end
      end

  It is still completely functional:

      {:ok, esme} = DummyESME.start_link(host, port)
      SMPPEX.Session.send_pdu(esme, SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))

  Here's a more complicated example of ESME, which does the following:

    * Receives port number and three arguments:

        - `waiting_pid` -- a pid of the process which will be informed when ESME stops;
        - `count` -- count of PDUs to send;
        - `window` -- window size, the maximum number of sent PDU's without resps.

    * Connects to the specified port on localhost and issues a bind command.
    * Starts to send predefined PDUs after bind at maximum possible rate but regarding window size.
    * Stops after all PDUs are sent and notifies the waiting process.

  ```elixir
  defmodule SMPPBenchmarks.ESME do

    use SMPPEX.Session
    require Logger

    @from {"from", 1, 1}
    @to {"to", 1, 1}
    @message "hello"

    @system_id "system_id"
    @password "password"

    def start_link(port, waiting_pid, count, window) do
      SMPPEX.ESME.start_link("127.0.0.1", port, {__MODULE__, [waiting_pid, count, window]})
    end

    def init(_, _, [waiting_pid, count, window]) do
      Kernel.send(self(), :bind)
      {:ok, %{waiting_pid: waiting_pid, count_to_send: count, count_waiting_resp: 0, window: window}}
    end

    def handle_resp(pdu, _original_pdu, st) do
      case SMPPEX.Pdu.command_name(pdu) do
        :submit_sm_resp ->
          new_st = %{ st | count_waiting_resp: st.count_waiting_resp - 1 }
          send_pdus(new_st)
        :bind_transmitter_resp ->
          send_pdus(st)
        _ ->
          {:ok, st}
      end
    end

    def handle_resp_timeout(pdu, st) do
      Logger.error("PDU timeout: #{inspect pdu}, terminating")
      {:stop, :resp_timeout, st}

    end

    def terminate(reason, _, st) do
      Logger.info("ESME stopped with reason #{inspect reason}")
      Kernel.send(st.waiting_pid, {self(), :done})
      :stop
    end

    def handle_info(:bind, st) do
      {:noreply, [SMPPEX.Pdu.Factory.bind_transmitter(@system_id, @password)], st}
    end

    defp send_pdus(st) do
      cond do
        st.count_to_send > 0 ->
          count_to_send = min(st.window - st.count_waiting_resp, st.count_to_send)
          new_st = %{ st | count_waiting_resp: st.window, count_to_send: st.count_to_send - count_to_send }
          {:ok, make_pdus(count_to_send), new_st}
        st.count_waiting_resp > 0 ->
          {:ok, st}
        true ->
          Logger.info("All PDUs sent, all resps received, terminating")
          {:stop, :normal, st}
      end
    end

    defp make_pdus(0), do: []
    defp make_pdus(n) do
      for _ <- 1..n, do: SMPPEX.Pdu.Factory.submit_sm(@from, @to, @message)
    end

  end
  ```

  Not all callbacks are used yet in this example, for the full list see `SMPPEX.Session` documentation.

  ## SMPPEX.MC

  `SMPPEX.MC` is used for _receiving_ and handling SMPP connections.

  Here is an example of a very simple MC, which does the following:

  * Starts and listens to connections on the specified port.
  * Responds with OK status to all incoming binds.
  * Responds with incremental message ids to all incoming `submit_sm` packets (regardless of the bind state).

  ```elixir

  defmodule MC do

    use SMPPEX.Session

    alias SMPPEX.Pdu
    alias SMPPEX.Pdu.Factory, as: PduFactory

    def start(port) do
      SMPPEX.MC.start({__MODULE__, []}, [transport_opts: [port: port]])
    end

    def init(_socket, _transport, []) do
      {:ok, 0}
    end

    def handle_pdu(pdu, last_id) do
      case Pdu.command_name(pdu) do
        :submit_sm ->
          {:ok, [PduFactory.submit_sm_resp(0, to_string(last_id)) |> Pdu.as_reply_to(pdu)], last_id + 1}
        :bind_transmitter ->
          {:ok, [PduFactory.bind_transmitter_resp(0) |> Pdu.as_reply_to(pdu)], last_id}
        _ ->
          {:ok, last_id}
      end
    end

  end
  ```

  """
end
