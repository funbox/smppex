defmodule SMPPEX.Protocol.OptionalFieldsBuilder do
  @moduledoc false

  alias SMPPEX.Protocol.Pack
  alias SMPPEX.Protocol.TlvFormat

  use Bitwise

  @spec build(map) :: {:ok, iodata} | {:error, any}

  def build(fields) when is_map(fields) do
    fields |> Map.to_list |> build([])
  end

  defp build([], built), do: {:ok, built}

  defp build([{name, value} | rest], built) when is_atom(name) do
    case TlvFormat.id_by_name(name) do
      {:ok, id} -> build([{id, value} | rest], built)
      :unknown -> {:error, "Can't find id for name #{inspect name}"}
    end
  end

  defp build([{id, value} | rest], built) do
    case build_tlv(id, value) do
      {:ok, bin} -> build(rest, [bin | built])
      {:error, error} -> {:error, {"Error building optional fields", error}}
    end
  end

  defp build_tlv(id, value) do
    case build_value(id, value) do
      {:ok, bin} -> Pack.tlv(id, bin)
      {:error, error} -> {:error, {"Invalid value for Tag #{inspect id}", error}}
    end
  end

  defp build_value(id, value) do
    format = TlvFormat.format_by_id(id)
    case format do
      {:ok, format} -> build_value_with_format(value, format)
      :unknown -> build_value_without_format(value)
    end
  end

  @invalid_octet_string "TLV Octet String: invalid value"
  @invalid_unknown_tlv_value "TLV: value for unknown Tag is not binary"

  defp build_value_with_format(value, {:integer, size}), do: Pack.integer(value, size)
  defp build_value_with_format(value, {:c_octet_string, {:max, size}}), do: Pack.c_octet_string(value, {:max, size})
  defp build_value_with_format(value, {:octet_string, size}) when is_integer(size), do: Pack.octet_string(value, size)
  defp build_value_with_format(value, {:octet_string, {_from, _to}})
    when not is_binary(value) do
      {:error, @invalid_octet_string}
  end

  defp build_value_with_format(value, {:octet_string, {from, to}})
    when byte_size(value) > to or byte_size(value) < from do
      {:error, @invalid_octet_string}
  end

  defp build_value_with_format(value, {:octet_string, {_from, _to}}), do: {:ok, value}

  defp build_value_without_format(value) when not is_binary(value), do: {:error, @invalid_unknown_tlv_value}

  defp build_value_without_format(value), do: {:ok, value}

end
