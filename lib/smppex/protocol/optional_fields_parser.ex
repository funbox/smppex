defmodule SMPPEX.Protocol.OptionalFieldsParser do
  import SMPPEX.Protocol.Unpack
  import SMPPEX.Protocol.TlvFormat

  def parse(bin), do: parse(bin, Map.new)

  def parse(<<>>, parsed_fields) do
    {:ok, parsed_fields}
  end

  def parse(bin, parsed_fields) do
    case tlv(bin) do
      {:ok, {tag, value}, rest} ->
        case parse_format(tag, value) do
          {:ok, parsed} ->
            parse(rest, Map.put(parsed_fields, tag, parsed))
          {:error, error} -> {:error, {"Invalid format for tlv #{inspect tag}", error}}
        end
      {:error, _} = err -> err
    end
  end

  defp parse_format(tag, value) do
    case format_by_id(tag) do
      {:ok, format} -> parse_known_tlv(value, format)
      :unknown -> {:ok, value} # unknown tlvs are always valid
    end
  end

  defp parse_known_tlv(value, {:integer, size}) do
    bit_size = size * 8
    case value do
      <<int :: big-unsigned-integer-size(bit_size)>> -> {:ok, int}
      _ -> {:error, "Invalid integer"}
    end
  end

  defp parse_known_tlv(value, {:c_octet_string, {:max, size}}) do
    case c_octet_string(value, {:max, size}) do
      {:ok, str, ""} -> {:ok, str}
      _ -> {:error, "Invalid c_octet_string"}
    end
  end

  defp parse_known_tlv(value, {:octet_string, size}) when is_integer(size) do
    case value do
      << _ :: binary-size(size) >> -> {:ok, value}
      _ -> {:error, "Invalid octet_string"}
    end
  end

  defp parse_known_tlv(value, {:octet_string, {from, to}}) do
    if byte_size(value) >= from and byte_size(value) <= to do
      {:ok, value}
    else
      {:error, "Invalid octet_string"}
    end
  end

end
