defmodule SMPPEX.Protocol do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Protocol.MandatoryFieldsSpecs
  alias SMPPEX.Protocol.MandatoryFieldsParser
  alias SMPPEX.Protocol.OptionalFieldsParser
  alias SMPPEX.RawPdu
  alias SMPPEX.Pdu

  import SMPPEX.Protocol.ParseResult

  def parse(bin) when byte_size(bin) < 4 do
    ok(nil, bin)
  end

  @type error :: list
  @type pdu_parse_result :: {:pdu, Pdu.t} | {:unparsed_pdu, RawPdu.t, error}
  @type parse_result :: {:ok, nil, binary} | {:ok, pdu_parse_result, binary} | {:error, error}

  @spec parse(binary) :: parse_result

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

  @spec parse_pdu(binary, binary) :: pdu_parse_result

  defp parse_pdu(header, body) do
    header = parse_header(header)
    raw_pdu = RawPdu.new(header, body)
    case CommandNames.name_by_id(RawPdu.command_id(raw_pdu)) do
      {:ok, name} -> parse_body(name, raw_pdu)
      :unknown -> {:unparsed_pdu, raw_pdu, "Unknown command_id"}
    end
  end

  defp parse_header(<<command_id :: big-unsigned-integer-size(32), command_status :: big-unsigned-integer-size(32), sequence_number :: big-unsigned-integer-size(32)>>) do
    {command_id, command_status, sequence_number}
  end

  defp parse_body(command_name, raw_pdu) do
    spec = MandatoryFieldsSpecs.spec_for(command_name)
    case MandatoryFieldsParser.parse(RawPdu.body(raw_pdu), spec) do
      {:ok, fields, rest} ->
        case OptionalFieldsParser.parse(rest) do
          {:ok, tlvs} ->
            {:pdu, Pdu.new(RawPdu.header(raw_pdu), fields, tlvs)}
          {:error, error} -> {:unparsed_pdu, raw_pdu, error}
        end
      {:error, error} -> {:unparsed_pdu, raw_pdu, error}
    end
  end

end
