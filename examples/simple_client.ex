defmodule SMPPSimpleClient do

  alias SMPPEX.ESME.Sync, as: ESME
  alias SMPPEX.Pdu.Factory

  require Logger

  def run(host, port, system_id, password, source_addr, destination_addr, message) do

    {:ok, esme} = ESME.start_link(host, port)

    bind = Factory.bind_transceiver(system_id, password)
    Logger.info("bind: #{inspect bind}")

    resp = ESME.request(esme, bind)
    Logger.info("bind resp: #{inspect resp}")

    submit_sm = Factory.submit_sm(
      {source_addr, 5, 1},
      {destination_addr, 1, 1},
      message,
      1
    )
    Logger.info("submit_sm: #{inspect submit_sm}")

    resp = ESME.request(esme, submit_sm)
    Logger.info("submit_sm resp: #{inspect resp}")

    ESME.stop(esme)

  end

end
