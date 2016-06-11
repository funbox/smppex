defmodule SMPPEX.Pdu.PP do

  alias SMPPEX.Pdu
  alias SMPPEX.Protocol.TlvFormat

  use Dye

  @pad "  "

  @spec fopmat(Pdu.t) :: iolist

  def fopmat(pdu) do
    [
      name(pdu),
      header(pdu),
      mandatory_fields(pdu),
      optional_fields(pdu)
    ]
  end

  defp name(pdu) do
    ["\n", "pdu: ", pp_command_name(pdu), "\n"]
  end

  defp pp_command_name(pdu) do
    name = pdu |> Pdu.command_name |> to_string
    ~s/#{name}/Cd
  end

  defp header(pdu) do
    [
      @pad, pp_field_name("command_id"), ": ", pdu |> Pdu.command_id |> inspect |> pp_val, "\n",
      @pad, pp_field_name("command_id"), ": ", pdu |> Pdu.command_status |> pp_command_status, "\n",
      @pad, pp_field_name("sequence_number"), ": ", pdu |> Pdu.sequence_number |> inspect |> pp_val, "\n"
    ]
  end

  defp pp_command_status(status) do
    case status do
      0 -> ~s/0 (ok)/Gd
      _ -> ~s/#{status} (error)/Rd
    end
  end

  defp pp_field_name(field_name) do
    ~s/#{field_name}/gd
  end

  defp mandatory_fields(pdu) do
    [
      "mandatory fields:",
      Pdu.mandatory_fields(pdu) |> Map.to_list |> pp_fields
    ]
  end

  defp optional_fields(pdu) do
    [
      "optional fields:",
      Pdu.optional_fields(pdu) |> Map.to_list |> name_known_tlvs |> pp_fields
    ]
  end

  defp name_known_tlvs(_, res \\ [])
  defp name_known_tlvs([], res), do: Enum.reverse(res)
  defp name_known_tlvs([{k, v} | left], res) do
    case TlvFormat.name_by_id(k) do
      {:ok, name} -> name_known_tlvs(left, [{name, v} | res])
      :unknown -> name_known_tlvs(left, [{k, v} | res])
    end
  end

  defp pp_val(str) when is_binary(str) do
    ~s/#{str}/yd
  end

  defp pp_val(int) when is_integer(int) do
    ~s/#{int}/yd
  end

  defp pp_val(val) do
    val
  end

  defp pp_fields(fields) do
    case fields do
      [] -> " []\n"
      _ -> ["\n", fields |> Enum.sort |> Enum.map(fn({key, val}) ->
          [@pad, key |> to_string |> pp_field_name , ": ", val |> inspect |> pp_val, "\n"]
        end) ]
    end
  end
end

