defmodule SMPPEX.Protocol do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Protocol.MandatoryFieldsSpecs
  alias SMPPEX.Protocol.MandatoryFieldsParser
  alias SMPPEX.Protocol.OptionalFieldsParser
  alias SMPPEX.Pdu

  import SMPPEX.Protocol.ParseResult

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
    header = parse_header(header)
    {command_id, _, _} = header
    case CommandNames.name_by_id(command_id) do
      {:ok, name} -> parse_body(name, header, body)
      :unknown -> {:unknown_pdu, {header, body}}
    end
  end

  defp parse_header(<<command_id :: big-unsigned-integer-size(32), command_status :: big-unsigned-integer-size(32), sequence_number :: big-unsigned-integer-size(32)>>) do
    {command_id, command_status, sequence_number}
  end

  defp parse_body(command_name, header, body) do
    spec = MandatoryFieldsSpecs.spec_for(command_name)
    case MandatoryFieldsParser.parse(body, spec) do
      {:ok, fields, rest} ->
        case OptionalFieldsParser.parse(rest) do
          {:ok, tlvs} ->
            {:pdu, Pdu.new(header, fields, tlvs)}
          {:error, error} -> {:unparsed_pdu, {header, body}, error}
        end
      {:error, error} -> {:unparsed_pdu, {header, body}, error}
    end
  end

end
