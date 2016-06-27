defmodule SMPPEX.Pdu.Multipart do

  alias :proplists, as: Proplists

  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.UDH

  @concateneated_8bit_ref_ie_id 0x00
  @concateneated_16bit_ref_ie_id 0x08

  @error_invalid_8bit_ie "Invalid 8bit refrence number concatenated short messages info"
  @error_invalid_16bit_ie "Invalid 16bit refrence number concatenated short messages info"
  @error_invalid_pdu "Pdu has no message field"
  @error_not_a_multipart_message "Message is not multipart"

  @error_invalid_ref_num "Invalid refrence number in multipart info"
  @error_invalid_count "Invalid count in multipart info"
  @error_invalid_seq_num "Invalid sequence number in multipart info"

  @error_invalid_max "Invalid limits for splitting message"
  @error_invalid_message "Invalid message"

  @type actual_part_info :: {non_neg_integer, non_neg_integer, non_neg_integer}
  @type part_info :: :single | actual_part_info
  @type extract_result :: {:ok, part_info, binary} | {:error, any}
  @type extract_source :: Pdu.t | binary

  @spec extract(extract_source) :: extract_result

  def extract(message) when is_binary(message) do
    case UDH.extract(message) do
      {:ok, ies, message} ->
        case extract_from_ies(ies) do
          {:ok, part_info} -> {:ok, part_info, message}
          {:error, _} = err -> err
        end
      {:error, _} = err -> err
    end
  end

  def extract(pdu) do
    message = Pdu.field(pdu, :message_payload) || Pdu.field(pdu, :short_message)
    if message do
      if UDH.has_udh?(pdu) do
        extract(message)
      else
        {:error, @error_not_a_multipart_message}
      end
    else
      {:error, @error_invalid_pdu}
    end
  end

  @spec extract_from_ies(list(UDH.ie)) :: {:ok, part_info} | {:error, any}

  def extract_from_ies(ies) do
    cond do
      Proplists.is_defined(@concateneated_8bit_ref_ie_id, ies) ->
        @concateneated_8bit_ref_ie_id |> Proplists.get_value(ies) |> parse_8bit
      Proplists.is_defined(@concateneated_16bit_ref_ie_id, ies) ->
        @concateneated_16bit_ref_ie_id |> Proplists.get_value(ies) |> parse_16bit
      true -> {:ok, :single}
    end
  end

  defp parse_8bit(<< ref_num :: integer-unsigned-size(8), count :: integer-unsigned-size(8), seq_num :: integer-unsigned-size(8) >>) do
    {:ok, {ref_num, count, seq_num}}
  end
  defp parse_8bit(_), do: {:error, @error_invalid_8bit_ie}

  defp parse_16bit(<< ref_num :: integer-big-unsigned-size(16), count :: integer-unsigned-size(8), seq_num :: integer-unsigned-size(8) >>) do
    {:ok, {ref_num, count, seq_num}}
  end
  defp parse_16bit(_), do: {:error, @error_invalid_16bit_ie}

  @spec multipart_ie(actual_part_info) :: {:error, any} | {:ok, UDH.ie}

  def multipart_ie({ref_num, _count, _seq_num}) when ref_num < 0 or ref_num > 65535, do: {:error, @error_invalid_ref_num}
  def multipart_ie({_ref_num, count, _seq_num}) when count < 1 or count > 255, do: {:error, @error_invalid_count}
  def multipart_ie({_ref_num, _count, seq_num}) when seq_num < 1 or seq_num > 255, do: {:error, @error_invalid_seq_num}

  def multipart_ie({ref_num, count, seq_num}) do
    {:ok, if ref_num > 255 do
      {@concateneated_16bit_ref_ie_id,
        <<ref_num :: integer-big-unsigned-size(16), count :: integer-unsigned-size(8), seq_num :: integer-unsigned-size(8) >>}
    else
      {@concateneated_8bit_ref_ie_id,
        <<ref_num :: integer-unsigned-size(8), count :: integer-unsigned-size(8), seq_num :: integer-unsigned-size(8) >>}
    end}
  end

  @spec prepend_message_with_part_info(actual_part_info, binary) :: {:error, any} | {:ok, binary}

  def prepend_message_with_part_info(part_info, message) do
    case multipart_ie(part_info) do
      {:ok, ie} -> UDH.add([ie], message)
      {:error, _} = err -> err
    end
  end

  @type split_result :: {:ok, :unsplit} | {:ok, :split, [binary]} | {:error, any}

  @spec split_message(integer, binary, integer) :: split_result

  def split_message(_ref_num, message, _max_len) when not is_binary(message), do: {:error, @error_invalid_message}
  def split_message(ref_num, _message, _max_len) when ref_num < 0, do: {:error, @error_invalid_ref_num}
  def split_message(ref_num, _message, _max_len) when ref_num > 65535, do: {:error, @error_invalid_ref_num}
  def split_message(_ref_num, _message, max_len) when max_len < 0, do: {:error, @error_invalid_max}

  def split_message(ref_num, message, max_len) do
    case prepend_message_with_part_info({ref_num, 1, 1}, <<>>) do
      {:ok, bin} ->
        max_split = if max_len >= byte_size(bin), do: max_len - byte_size(bin), else: 0
        split_message(ref_num, message, max_len, max_split)
      {:error, _} = err -> err
    end
  end

  @spec split_message(integer, binary, integer, integer) :: split_result
  def split_message(_ref_num, message, _max_unsplit, _max_split) when not is_binary(message), do: {:error, @error_invalid_message}
  def split_message(ref_num, _message, _max_unsplit, _max_split) when ref_num < 0, do: {:error, @error_invalid_ref_num}
  def split_message(ref_num, _message, _max_unsplit, _max_split) when ref_num > 65535, do: {:error, @error_invalid_ref_num}
  def split_message(_ref_num, _message, max_unsplit, _max_split) when max_unsplit < 0, do: {:error, @error_invalid_max}
  def split_message(_ref_num, _message, _max_unsplit, max_split) when max_split < 0, do: {:error, @error_invalid_max}

  def split_message(ref_num, message, max_unsplit, max_split) do
    message_size = byte_size(message)
    if message_size <= max_unsplit do
      {:ok, :unsplit}
    else
      if max_split > 0 do
        part_count = message_part_count(message_size, max_split)
        split_message_into_parts(ref_num, part_count, message, max_split, [], 1)
      else
        {:error, @error_invalid_max}
      end
    end
  end

  defp message_part_count(message_size, max_size) do
    if rem(message_size, max_size) == 0 do
      div(message_size, max_size)
    else
      1 + div(message_size, max_size)
    end
  end

  defp split_message_into_parts(_ref_num, count, <<>>, _max_len, parts, n) when n > count, do: {:ok, :split, Enum.reverse(parts)}
  defp split_message_into_parts(ref_num, count, message, max_len, parts, n) do
    {part, rest} = case message do
      << part :: binary-size(max_len), rest :: binary >> -> {part, rest}
      last_part -> {last_part, <<>>}
    end

    case prepend_message_with_part_info({ref_num, count, n}, part) do
      {:ok, part_with_info} ->
        split_message_into_parts(ref_num, count, rest, max_len, [part_with_info | parts], n + 1)
      {:error, _} = err -> err
    end
  end

end
