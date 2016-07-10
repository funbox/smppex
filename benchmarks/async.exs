defmodule Benchmarks.Async do

  require Logger

  alias :timer, as: Timer

  defmodule MC do

    use SMPPEX.MC

    def start(port) do
      SMPPEX.MC.start({__MODULE__, []}, [transport_opts: [port: port]])
    end

    def init(_socket, _transport, []) do
      {:ok, 0}
    end

    def handle_pdu(pdu, last_id) do
      case pdu |> SMPPEX.Pdu.command_id |> SMPPEX.Protocol.CommandNames.name_by_id do
        {:ok, :submit_sm} ->
          SMPPEX.MC.reply(self, pdu, SMPPEX.Pdu.Factory.submit_sm_resp(0, to_string(last_id)))
          last_id + 1
        {:ok, :bind_transmitter} ->
          SMPPEX.MC.reply(self, pdu, SMPPEX.Pdu.Factory.bind_transmitter_resp(0))
          last_id
        {:ok, :enquire_link} ->
          SMPPEX.MC.reply(self, pdu, SMPPEX.Pdu.Factory.enquire_link_resp)
          last_id
        _ -> last_id
      end
    end
  end

  defmodule ESME do

    use SMPPEX.ESME

    @from {"from", 1, 1}
    @to {"to", 1, 1}
    @message "hello"

    def start_link(port, waiting_pid, count, window) do
      SMPPEX.ESME.start_link("127.0.0.1", port, {__MODULE__, [waiting_pid, count, window]})
    end

    def init([waiting_pid, count, window]) do
      {:ok, %{waiting_pid: waiting_pid, count_to_send: count, count_waiting_resp: 0, window: window}}
    end

    def handle_resp(pdu, _original_pdu, st) do
      case pdu |> SMPPEX.Pdu.command_id |> SMPPEX.Protocol.CommandNames.name_by_id do
        {:ok, :submit_sm_resp} ->
          new_st = %{ st | count_waiting_resp: st.count_waiting_resp - 1 }
          send_pdus(new_st)
        {:ok, :bind_transmitter_resp} ->
          send_pdus(st)
        _ ->
          st
      end
    end

    def handle_resp_timeout(pdu, st) do
      Logger.error("PDU timeout: #{inspect pdu}, terminating")
      SMPPEX.ESME.stop(self)
      st
    end

    def handle_stop(st) do
      Logger.info("ESME stopped")
      Kernel.send(st.waiting_pid, {self, :done})
      st
    end

    defp send_pdus(st) do
      cond do
        st.count_to_send > 0 ->
          count_to_send = min(st.window - st.count_waiting_resp, st.count_to_send)
          :ok = do_send(self, count_to_send)
          %{ st | count_waiting_resp: st.window, count_to_send: st.count_to_send - count_to_send }
        st.count_waiting_resp > 0 ->
          st
        true ->
          Logger.info("All PDUs sent, all resps received, terminating")
          SMPPEX.ESME.stop(self)
          st
      end
    end

    defp do_send(_esme, n) when n <= 0, do: :ok
    defp do_send(esme, n) do
      submit_sm = SMPPEX.Pdu.Factory.submit_sm(@from, @to, @message)
      :ok = SMPPEX.ESME.send_pdu(esme, submit_sm)
      do_send(esme, n - 1)
    end

  end

  @default_port 33333
  @default_pdu_count 100000
  @default_window 5000

  def run([]), do: run([@default_port, @default_pdu_count, @default_window])
  def run([port, pdu_count, window]) do

    Logger.info("Starting MC on port #{port}")
    {:ok, _} = MC.start(port)
    Timer.sleep(50)

    Logger.info("Starting ESME with window #{window}")
    {:ok, esme} = ESME.start_link(port, self, pdu_count, window)
    SMPPEX.ESME.send_pdu(esme, SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password"))

    Logger.info("Sending #{pdu_count} PDUs...")
    {time, _} = Timer.tc(fn() ->
      receive do
        {^esme, :done} -> :ok
        msg -> Logger.error("Unexpected message received: #{inspect msg}")
      end
    end)

    time_ms = div(time, 1000)
    pdu_rate = if time_ms > 0 do
      pdu_count * 1000 / time_ms
    else
      "undifined"
    end

    Logger.info("Completed in #{time_ms}ms with avg rate #{pdu_rate} pdu/s")
  end

end

System.argv |> Enum.map(&String.to_integer/1) |> Benchmarks.Async.run

