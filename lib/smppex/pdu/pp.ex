defmodule SMPPEX.Pdu.PP do
  @moduledoc """
  Module for colored pretty printing Pdu structs.
  """

  alias IO.ANSI, as: C
  alias SMPPEX.Pdu
  alias SMPPEX.Protocol.TlvFormat

  @pad ""
  @indent "  "
  @field_inspect_limit 999999

  @spec format(pdu :: Pdu.t, indent :: String.t, pad :: String.t) :: iolist

  @doc """
  Forms an iolist containing colored Pdu dump.

  `indent` is the string prepended to each line of the dump ("#{@indent}" by default).
  `pad` is the string prepended to nested lines of the dump together with `indent`.
  The default is "#{@pad}".

  ## Example

      iex> pdu = SMPPEX.Pdu.Factory.submit_sm({"from", 1, 1}, {"to", 1, 1}, "hello")

  Then `pdu |> SMPPEX.Pdu.PP.format |> IO.puts` will print:

  ```
  pdu: submit_sm
    command_id: 4
    command_status: 0 (ok)
    sequence_number: 0
  mandatory fields:
    dest_addr_npi: 1
    dest_addr_ton: 1
    destination_addr: "to"
    registered_delivery: 0
    short_message: "hello"
    source_addr: "from"
    source_addr_npi: 1
    source_addr_ton: 1
  optional fields: []
  ```

  """

  def format(pdu, indent \\ @indent, pad \\ @pad) do
    [ "\n", pdu |> pdu_lines |> Enum.map(fn([section_head | section_lines]) ->
      [ pad, section_head, "\n", section_lines |> Enum.map(fn(line) ->
        [ pad, indent, line, "\n"]
      end) ]
    end) ]
  end

  defp pdu_lines(pdu) do
    [
      header(pdu),
      mandatory_fields(pdu),
      optional_fields(pdu)
    ]
  end

  defp header(pdu) do
    [
      name(pdu),
      [ pp_field_name("command_id"), ": ", pdu |> Pdu.command_id |> inspect |> pp_val ],
      [ pp_field_name("command_status"), ": ", pdu |> Pdu.command_status |> pp_command_status ],
      [ pp_field_name("sequence_number"), ": ", pdu |> Pdu.sequence_number |> inspect |> pp_val ]
    ]
  end

  defp name(pdu) do
    ["pdu: ", pp_command_name(pdu)]
  end

  defp mandatory_fields(pdu) do
    [ [ "mandatory fields:", pdu |> Pdu.mandatory_fields |> pp_empty_list ] ] ++
      (pdu |> Pdu.mandatory_fields |> Map.to_list |> pp_fields)
  end

  defp optional_fields(pdu) do
    [ [ "optional fields:", pdu |> Pdu.optional_fields |> pp_empty_list ] ] ++
      (pdu |> Pdu.optional_fields |> Map.to_list |> name_known_tlvs |> pp_fields)
  end

  defp name_known_tlvs(_, res \\ [])
  defp name_known_tlvs([], res), do: Enum.reverse(res)
  defp name_known_tlvs([{k, v} | left], res) do
    case TlvFormat.name_by_id(k) do
      {:ok, name} -> name_known_tlvs(left, [{name, v} | res])
      :unknown -> name_known_tlvs(left, [{k, v} | res])
    end
  end

  defp pp_empty_list(map) when map == %{}, do: " []"
  defp pp_empty_list(_), do: ""

  defp pp_command_status(status) do
    case status do
      0 -> [C.green, C.bright, "0 (ok)", C.reset]
      _ -> [C.red, C.bright, "#{status} (error)", C.reset]
    end
  end

  defp pp_field_name(field_name) do
    [C.green, field_name, C.reset]
  end

  defp pp_val(str) do
    [C.yellow, str, C.reset]
  end

  defp pp_fields(fields) do
    fields |> Enum.sort |> Enum.map(fn({key, val}) ->
      [key |> to_string |> pp_field_name , ": ", val |> inspect(limit: @field_inspect_limit) |> pp_val]
    end)
  end

  defp pp_command_name(pdu) do
    name = pdu |> Pdu.command_name |> to_string
    [C.cyan, C.bright, name, C.reset]
  end

end
