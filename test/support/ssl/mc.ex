defmodule Support.SSL.MC do
  @moduledoc false

  alias :timer, as: Timer

  use SMPPEX.Session

  alias SMPPEX.MC
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Factory, as: PduFactory

  def start(port, certname, accept \\ true) do
    MC.start(
      {__MODULE__, [accept]},
      transport: :ranch_ssl,
      transport_opts: %{
        socket_opts: [
          port: port,
          certfile: 'test/support/ssl/#{certname}',
          keyfile: 'test/support/ssl/cert.key'
        ]
      }
    )
  end

  def stop(ref), do: MC.stop(ref)

  @impl true
  def init(_socket, _transport, [accept]) do
    if accept do
      {:ok, 0}
    else
      Timer.sleep(100)
      {:stop, :ooops}
    end
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
