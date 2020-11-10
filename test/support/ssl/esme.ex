defmodule Support.SSL.ESME do
  @moduledoc false

  alias :timer, as: Timer

  use SMPPEX.Session

  @host "localhost"

  @system_id "system_id"
  @password "password"

  def start_link(port, delay \\ nil) do
    SMPPEX.ESME.start_link(
      @host,
      port,
      {__MODULE__, %{pid: self(), delay: delay}},
      transport: :ranch_ssl,
      socket_opts: [
        cacertfile: 'test/support/ssl/ca.crt',
        verify: :verify_peer
      ]
    )
  end

  @impl true
  def init(_socket, _transport, st) do
    if st.delay do
      Timer.sleep(st.delay)
    end
    send(self(), :bind)
    {:ok, st}
  end

  @impl true
  def handle_info(:bind, st) do
    pdu = SMPPEX.Pdu.Factory.bind_transceiver(@system_id, @password)
    {:noreply, [pdu], st}
  end

  @impl true
  def handle_resp(resp, original_pdu, st) do
    send(st.pid, {resp, original_pdu})
    {:stop, :normal, st}
  end
end
