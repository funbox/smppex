defmodule Support.SSL.MC do
  @moduledoc false

  use SMPPEX.Session

  alias SMPPEX.MC
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory, as: PduFactory

  def start(port) do
    MC.start(
      {__MODULE__, []},
      transport: :ranch_ssl,
      transport_opts: [
        port: port,
        certfile: 'test/support/ssl/host.crt',
        keyfile: 'test/support/ssl/host.key'
      ]
    )
  end

  def stop(ref), do: MC.stop(ref)

  @impl true
  def init(_socket, _transport, []) do
    {:ok, 0}
  end

  @impl true
  def handle_pdu(pdu, last_id) do
    case Pdu.command_name(pdu) do
      :bind_transceiver ->
        {:ok, [PduFactory.bind_transceiver_resp(0) |> Pdu.as_reply_to(pdu)], last_id}

      _ ->
        {:ok, last_id}
    end
  end
end
