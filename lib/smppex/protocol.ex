defmodule SMPPEX.Protocol do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Protocol.MandatoryFieldsSpecs
  alias SMPPEX.Protocol.MandatoryFieldsParser
  import SMPPEX.ParseResult

  def parse(bin) when byte_size(bin) < 4 do
    ok(nil, bin)
  end

  def parse(bin) do
    <<command_length :: big-unsigned-integer-size(32), rest :: binary >> = bin
    cond do
      command_length < 16 ->
        error("Invalid PDU command_length #{inspect command_length}")
      command_length <= byte_size(bin) ->
        body_length = command_length - 16
        << header :: binary-size(12), body :: binary-size(body_length), next_pdus :: binary >> = rest
        ok(parse_pdu(header, body), next_pdus)
      true ->
        ok(nil, bin)
    end
  end

  defp parse_pdu(header, body) do
    pdu_with_header = case parse_header(header) do
      {:ok, pdu} ->
        parse_body(pdu.command_name, pdu, body)
      {:unknown, pdu} ->
        pdu
    end
    %SMPPEX.Pdu{ pdu_with_header | body: body }
  end

  defp parse_header(<<command_id :: big-unsigned-integer-size(32), command_status :: big-unsigned-integer-size(32), sequence_number :: big-unsigned-integer-size(32)>>) do
    case CommandNames.name_by_id(command_id) do
      {:ok, name} ->
        {:ok, %SMPPEX.Pdu{
          command_id: command_id,
          command_name: name,
          command_status: command_status,
          sequence_number: sequence_number
        }}
      :unknown ->
        {:unknown, %SMPPEX.Pdu{
          command_id: command_id,
          command_status: command_status,
          sequence_number: sequence_number,
          valid: false
        }}
    end
  end

  defp parse_body(command_name, pdu, body) do
    spec = MandatoryFieldsSpecs.spec_for(command_name)
    case MandatoryFieldsParser.parse(body, spec) do
      {:ok, fields, rest} ->
        case parse_optional_fields(rest) do
          {:ok, tlvs} ->
            %SMPPEX.Pdu{ pdu | mandatory: fields, optional: tlvs }
          error -> error("TLV parse error", error)
        end
      error -> error("Mandatory fields parse error", error)
    end
  end

  defp parse_optional_fields(_body), do: {:ok, Map.new}

end
