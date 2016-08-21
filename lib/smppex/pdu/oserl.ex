defmodule SMPPEX.Pdu.Oserl do

  alias SMPPEX.Pdu
  alias SMPPEX.Protocol.TlvFormat

  def to(pdu) do
    {
      Pdu.command_id(pdu),
      Pdu.command_status(pdu),
      Pdu.sequence_number(pdu),
      fields_to_list(pdu)
    }
  end

  def from({command_id, command_status, sequence_number, field_list}) do
    {mandatory, optional} = list_to_fields(field_list, %{}, %{})
    Pdu.new(
      {command_id, command_status, sequence_number},
      mandatory,
      optional
    )
  end

  defp fields_to_list(pdu) do
    (pdu |> Pdu.mandatory_fields |> Map.to_list) ++
      (pdu |> Pdu.optional_fields |> Map.to_list |> ids_to_names)
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
  defp ids_to_names([{name, value} | by_ids], by_names) when is_atom(name), do: ids_to_names(by_ids, [{name, value} | by_names])
  defp ids_to_names([{id, value} | by_ids], by_names) when is_integer(id) do
    case TlvFormat.name_by_id(id) do
      {:ok, name} -> ids_to_names(by_ids, [{name, value} | by_names])
      :unknown -> ids_to_names(by_ids, by_names)
    end
  end

end
