defmodule SMPPEX.Protocol do

  def parse(bin) when byte_size(bin) < 4 do
    {[], bin}
  end

  def parse(bin) do
    <<command_length :: big-unsigned-integer-size(32), rest :: binary >> = bin
    cond do
      command_length < 16 ->
        {:fatal_error, "Invalid PDU command_length #{inspect command_length}"}
      command_length <= byte_size(bin) ->
        body_length = command_length - 16
        << header :: binary-size(12), body :: binary-size(body_length), next_pdus :: binary >> = rest
        {parse_pdu(header, body), next_pdus}
      true ->
        {[], bin}
    end
  end

  defp parse_pdu(header, body) do
    pdu = parse_header(header)
    parse_body(pdu.command_id, pdu, body)
  end

  defp parse_header(<<command_id :: big-unsigned-integer-size(32), command_status :: big-unsigned-integer-size(32), sequence_number :: big-unsigned-integer-size(32)>>) do
   %SMPPEX.Pdu{
     command_id: command_id,
     command_status: command_status,
     sequence_number: sequence_number
   }
  end

  defp parse_body(command_id, pdu, body) do
    map = mandatory_field_map(command_id)
    case parse_mandatory_fields(map, body) do
      {:ok, fields, rest_tlvs} ->
        case parse_optional_fields(rest_tlvs) do
          {:ok, tlvs} ->
            [ %SMPPEX.Pdu{ pdu | mandatory: fields, optional: tlvs } ]
          error -> {:error, {"TLV parse error", error}}
        end
      error -> {:error, {"Fields parse error", error}}
    end
  end

  defp mandatory_field_map(_command_id), do: nil

  defp parse_mandatory_fields(_map, body), do: {:ok, %{}, body}
  defp parse_optional_fields(_body), do: {:ok, %{}}

end
