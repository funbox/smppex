defmodule SMPPEX.Protocol.MandatoryFieldsParser do
  @moduledoc false

  import SMPPEX.Protocol.Unpack

  alias SMPPEX.Protocol.MandatoryFieldsSpecs

  @spec parse(binary, MandatoryFieldsSpecs.fields_spec) :: {:ok, map, binary} | {:error, any}

  def parse(bin, spec), do: parse(bin, spec, Map.new)

  @spec parse(binary, MandatoryFieldsSpecs.fields_spec, map) :: {:ok, map, binary} | {:error, any}

  def parse(bin, [], parsed_fields) do
    {:ok, parsed_fields, bin}
  end

  def parse(bin, [field_spec | rest_field_specs], parsed_fields) do
    case parse_field(bin, field_spec, parsed_fields) do
      {:ok, new_parsed_fields, rest} ->
        parse(rest, rest_field_specs, new_parsed_fields)
      {:error, error} ->
        {:error, {"Error parsing field(s) #{inspect field_spec}", error}}
    end
  end

  defp parse_field(bin, {field_name, spec}, parsed_fields) when is_tuple(spec) do
    case read_value(bin, spec, parsed_fields) do
      {:ok, value, rest} ->
        {:ok, Map.put(parsed_fields, field_name, value), rest}
      {:error, _} = err -> err
    end
  end

  defp parse_field(bin, {:case, cases}, parsed_fields) when is_list(cases) do
    read_cases(bin, cases, parsed_fields)
  end

  defp read_value(bin, {:c_octet_string, {:max, n}}, parsed_fields) do
    c_octet_string(bin, {:max, expand(n, parsed_fields)})
  end

  defp read_value(bin, {:integer, n}, parsed_fields) do
    integer(bin, expand(n, parsed_fields))
  end

  defp read_value(bin, {:c_octet_string, {:fixed, n}}, parsed_fields) do
    c_octet_string(bin, {:fixed, expand(n, parsed_fields)})
  end

  defp read_value(bin, {:octet_string, n}, parsed_fields) do
    octet_string(bin, expand(n, parsed_fields))
  end

  defp read_value(bin, {:times, n, specs}, parsed_fields) do
    case read_values(bin, {expand(n, parsed_fields), []}, specs, Map.new) do
      {:ok, values, rest} -> {:ok, Enum.reverse(values), rest}
      {:error, _} = err -> err
    end
  end

  defp read_values(bin, {0, values}, _specs, _parsed_fields), do: {:ok, values, bin}

  defp read_values(bin, {n, values}, specs, parsed_fields) do
    case parse(bin, specs, parsed_fields) do
      {:ok, parsed_fields_inner, rest} ->
        read_values(rest, {n-1, [parsed_fields_inner | values]}, specs, parsed_fields)
      {:error, _} = err -> err
    end
  end

  defp read_cases(_bin, [], _parsed_fields), do: {:error, "No cases left"}
  defp read_cases(bin, [current_case | rest_cases], parsed_fields) do
    {field, value, specs} = current_case
    if expand(field, parsed_fields) == value do
      parse(bin, specs, parsed_fields)
    else
      read_cases(bin, rest_cases, parsed_fields)
    end
  end

  defp expand(n, _parsed_fields) when is_integer(n), do: n
  defp expand(n, parsed_fields) when is_atom(n), do: parsed_fields[n]

end
