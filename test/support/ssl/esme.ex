defmodule Support.SSL.ESME do
  @moduledoc false

  use SMPPEX.Session

  @host "localhost"

  @system_id "system_id"
  @password "password"

  def start_link(port) do
    SMPPEX.ESME.start_link(
      @host,
      port,
      {__MODULE__, %{pid: self()}},
      transport: :ranch_ssl,
      transport_opts: [
        port:  port,
        certfile: 'test/support/ssl/host.crt',
        keyfile: 'test/support/ssl/host.key'
      ]
    )
  end

  @impl true
  def init(_socket, _transport, st) do
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
