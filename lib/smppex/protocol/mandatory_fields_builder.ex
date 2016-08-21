defmodule SMPPEX.Protocol.MandatoryFieldsBuilder do
  @moduledoc false

  import SMPPEX.Protocol.Pack
  alias SMPPEX.Protocol.MandatoryFieldsSpecs

  @spec build(map, MandatoryFieldsSpecs.fields_spec) :: {:ok, iodata} | {:error, any}

  def build(fields, spec) when is_map(fields) do
    build(fields, Enum.reverse(spec), [])
  end

  defp build(_fields, [], built), do: {:ok, built}

  defp build(fields, [field_spec | specs], built) do
    case build_field(fields, field_spec) do
      {:ok, new_fields, bin} -> build(new_fields, specs, [bin | built])
      {:error, error} -> {:error, error}
    end
  end

  defp build_field(fields, {name, {:octet_string, len}}) when is_atom(len) do
    case fields[name] do
      bin when is_binary(bin) -> {:ok, Map.put(fields, len, byte_size(bin)), bin}
      _ -> {:error, "Field #{name} is not an octet_string"}
    end
  end

  defp build_field(fields, {name, {:times, times, subspecs}}) when is_atom(times) do
    case fields[name] do
      values when is_list(values) ->
        case build_subfields(values, subspecs, []) do
          {:ok, bins} -> {:ok, Map.put(fields, times, length(values)), bins}
          {:error, error} -> {:error, error}
        end
      _ -> {:error, "Field #{name} is not a list"}
    end
  end

  defp build_field(fields, {:case, cases}) when is_list(cases) do
    build_cases(fields, cases)
  end

  defp build_field(fields, {name, simple_spec}) when is_tuple(simple_spec) do
    case build_simple_value(fields[name], simple_spec) do
      {:ok, bin} -> {:ok, fields, bin}
      {:error, error} -> {:error, {"Error building simple field #{name}", error}}
    end
  end

  defp build_subfields([], _specs, built), do: {:ok, Enum.reverse(built)}
  defp build_subfields([value | values], specs, built) do
    case build(value, specs) do
      {:ok, bin} -> build_subfields(values, specs, [bin | built])
      {:error, error} -> {:error, error}
    end
  end

  defp build_cases(_fields, []), do: {:error, "No case matched given fields"}
  defp build_cases(fields, [{cond_name, cond_value, specs} | other_cases]) do
    if fields[cond_name] == cond_value do
      case build(fields, specs) do
        {:ok, bin} -> {:ok, fields, bin}
        {:error, error} -> {:error, error}
      end
    else
      build_cases(fields, other_cases)
    end
  end

  defp build_simple_value(value, {:c_octet_string, {:max, n}}), do: c_octet_string(value, {:max, n})

  defp build_simple_value(value, {:c_octet_string, {:fixed, n}}), do: c_octet_string(value, {:fixed, n})

  defp build_simple_value(value, {:octet_string, n}), do: octet_string(value, n)

  defp build_simple_value(value, {:integer, n}), do: integer(value, n)

end

