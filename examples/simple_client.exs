defmodule SMPPSimpleClient do

  alias SMPPEX.ESME.Sync, as: ESME
  alias SMPPEX.Pdu.Factory
  alias SMPPEX.Pdu.PP

  require Logger

  @args [
    host: :string,
    port: :integer,
    system_id: :string,
    password: :string,
    source_addr: :string,
    destination_addr: :string,
    message: :string
  ]

  def main(args) do
    args |> parse_args |> check_args |> process
  end

  defp parse_args(args) do
    {options, _, _} = OptionParser.parse(args,
      strict: @args
    )
    options
  end

  defp check_args(options) do
    missing = for option <- Keyword.keys(@args), not Keyword.has_key?(options, option), do: option
    case missing do
      [] -> options
      _ ->
        IO.puts "Missing options: #{inspect missing}"
        exit(:shutdown)
    end
  end

  defp process(options) do

    {:ok, esme} = ESME.start_link(options[:host], options[:port])

    bind = Factory.bind_transceiver(options[:system_id], options[:password])
    bind |> PP.fopmat |> Logger.info

    resp = ESME.request(esme, bind)
    pp_resp(resp)

    submit_sm = Factory.submit_sm(
      {options[:source_addr], 5, 1},
      {options[:destination_addr], 1, 1},
      options[:message],
      1
    )
    submit_sm |> PP.fopmat |> Logger.info

    resp = ESME.request(esme, submit_sm)
    pp_resp(resp)

    # Wait for deliver report
    pdus = ESME.wait_for_pdus(esme, 15000)
    pp_pdus(pdus)

    ESME.stop(esme)

  end

  defp pp_resp(resp) do
    case resp do
      {:ok, pdu} ->
        pdu |> PP.fopmat |> Logger.info
      other ->
        other |> Logger.error
    end
  end

  defp pp_pdus([{:pdu, pdu} | pdus]) do
    pdu |> PP.fopmat |> Logger.info
    pp_pdus(pdus)
  end

  defp pp_pdus([]) do
  end

  defp pp_pdus(other) do
    other |> Logger.error
  end
end

SMPPSimpleClient.main(System.argv)
