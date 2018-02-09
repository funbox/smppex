defmodule SMPPEX.Pdu.Oserl do
  @moduledoc """
  Module for converting SMPPEX Pdu structs to format used by [Oserl](https://github.com/iamaleksey/oserl) library.

  """

  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.Oserl, as: OserlPdu
  alias SMPPEX.Protocol.TlvFormat

  @type t :: {
          command_id :: non_neg_integer,
          command_status :: non_neg_integer,
          sequence_number :: non_neg_integer,
          [{field_name :: atom, field_value :: term}]
        }

  @spec to(pdu :: Pdu.t()) :: OserlPdu.t()

  @doc """
  Converts SMPPEX Pdu to Oserl format.

  Unknown optional values are ignored, since Oserl stores them by symbolic names.
  """
  def to(pdu) do
    {
      Pdu.command_id(pdu),
      Pdu.command_status(pdu),
      Pdu.sequence_number(pdu),
      fields_to_list(pdu)
    }
  end

  @spec from(oserl_pdu :: OserlPdu.t()) :: Pdu.t()

  @doc """
  Converts PDU from Oserl format to SMPPEX Pdu.
  """
  def from({command_id, command_status, sequence_number, field_list} = _oserl_pdu) do
    converted_field_list = Enum.map(field_list, &list_to_string(&1))
    {mandatory, optional} = list_to_fields(converted_field_list, %{}, %{})

    Pdu.new(
      {command_id, command_status, sequence_number},
      mandatory,
      optional
    )
  end

  defp fields_to_list(pdu) do
    (pdu |> Pdu.mandatory_fields() |> Map.to_list() |> Enum.map(&string_to_list(&1))) ++
      (pdu |> Pdu.optional_fields() |> Map.to_list() |> ids_to_names)
  end

  defp list_to_fields([], mandatory, optional), do: {mandatory, optional}

  defp list_to_fields([{name, value} | list], mandatory, optional) do
    case kind(name) do
      {:optional, id} -> list_to_fields(list, mandatory, Map.put(optional, id, value))
      :mandatory -> list_to_fields(list, Map.put(mandatory, name, value), optional)
    end
  end

  defp kind(id) when is_integer(id), do: {:optional, id}

  defp kind(name) when is_atom(name) do
    case TlvFormat.id_by_name(name) do
      {:ok, id} -> {:optional, id}
      :unknown -> :mandatory
    end
  end

  defp ids_to_names(by_ids, by_names \\ [])

  defp ids_to_names([], by_names), do: by_names

  defp ids_to_names([{name, value} | by_ids], by_names) when is_atom(name),
    do: ids_to_names(by_ids, [{name, value} | by_names])

  defp ids_to_names([{id, value} | by_ids], by_names) when is_integer(id) do
    case TlvFormat.name_by_id(id) do
      {:ok, name} -> ids_to_names(by_ids, [string_to_list({name, value}) | by_names])
      :unknown -> ids_to_names(by_ids, [{id, value} | by_names])
    end
  end

  defp list_to_string({key, value}) when is_list(value), do: {key, List.to_string(value)}
  defp list_to_string({key, value}), do: {key, value}

  defp string_to_list({key, value}) when is_binary(value),
    do: {key, :erlang.binary_to_list(value)}

  defp string_to_list({key, value}), do: {key, value}
end
