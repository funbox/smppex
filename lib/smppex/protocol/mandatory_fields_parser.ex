defmodule SMPPEX.Protocol.MandatoryFieldsParser do
  import SMPPEX.ParseResult
  import SMPPEX.Protocol.Unpack

  def parse(bin, map), do: parse(bin, map, Map.new)

  defp parse(bin, [], parsed_fields) do
    ok(parsed_fields, bin)
  end

  defp parse(bin, [field_spec | rest_field_specs], parsed_fields) do
    case parse_field(bin, field_spec, parsed_fields) do
      {:ok, new_parsed_fields, rest} ->
        parse(rest, rest_field_specs, new_parsed_fields)
      {:error, error} ->
        {field_name, _} = field_spec
        error("Error parsing field #{inspect field_name}", error)
    end
  end

  defp parse_field(bin, {field_name, spec}, parsed_fields) do
    case read_value(bin, spec, parsed_fields) do
      {:ok, value, rest} ->
        ok(Map.put(parsed_fields, field_name, value), rest)
      {:error, parse_error} -> parse_error
    end
  end

  defp read_value(bin, {:c_octet_string, {:max, n}}, parsed_fields) do
    c_octet_string(bin, {:max, value(n, parsed_fields)})
  end

  defp read_value(bin, {:integer, n}, _parsed_fields) do
    integer(bin, n)
  end

  defp value(n, parsed_fields) when is_integer(n), do: n
  defp value(n, parsed_fields) when is_atom(n), do: parsed_fields[n]

end

