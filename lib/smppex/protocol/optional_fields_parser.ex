defmodule SMPPEX.Protocol.OptionalFieldsParser do
  import SMPPEX.Protocol.ParseResult
  import SMPPEX.Protocol.Unpack

  def parse(bin), do: parse(bin, Map.new)

  def parse(<<>>, parsed_fields) do
    {:ok, parsed_fields}
  end

  def parse(bin, parsed_fields) do
    case tlv(bin) do
      {:ok, {tag, value}, rest} ->
        parse(rest, Map.put(parsed_fields, tag, value))
      {:error, error} -> error(error)
    end
  end

end
