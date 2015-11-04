defmodule SMPPEX.Protocol.OptionalFieldsBuilder do
  import SMPPEX.Protocol.Pack
  import SMPPEX.Protocol.PackResult
  import SMPPEX.Protocol.TlvFormat

  use Bitwise

  def build(fields) do
    fields |> Dict.to_list |> build([])
  end

  defp build([], built), do: {:ok, built}

  defp build([{id, value} | rest ], built) do
    case build_tlv(id, value) do
      {:ok, bin} -> build(rest, [bin | built])
      {:error, error} -> error("Error building optional fields", error)
    end
  end

  defp build_tlv(id, value) do
    case build_value(id, value) do
      {:ok, bin} -> tlv(id, bin)
      {:error, error} -> error("Invalid value for Tag #{id}", error)
    end
  end

  defp build_value(id, value) do
    format = format_by_id(id)
    case format do
      {:ok, format} -> build_value_with_format(value, format)
      :unknown -> build_value_without_format(value)
    end
  end

  @invalid_octet_string "TLV Octet String: invalid value"
  @invalid_unknown_tlv_value "TLV: value for unknown Tag is not binary"

  defp build_value_with_format(value, {:integer, size}), do: integer(value, size)
  defp build_value_with_format(value, {:c_octet_string, {:max, size}}), do: c_octet_string(value, {:max, size})
  defp build_value_with_format(value, {:octet_string, size}) when is_integer(size), do: octet_string(value, size)
  defp build_value_with_format(value, {:octet_string, {_from, _to}})
    when not is_binary(value) do
      error(@invalid_octet_string)
  end

  defp build_value_with_format(value, {:octet_string, {from, to}})
    when byte_size(value) > to or byte_size(value) < from do
      error(@invalid_octet_string)
  end

  defp build_value_with_format(value, {:octet_string, {_from, _to}}), do: {:ok, value}

  defp build_value_without_format(value) when not is_binary(value), do: error(@invalid_unknown_tlv_value)

  defp build_value_without_format(value), do: {:ok, value}

end
