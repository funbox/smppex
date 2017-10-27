defmodule SMPPEX.Pdu.Multipart do
  @moduledoc """
  Module for operating with multipart information packed as UDH in message body.
  """

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

  @type actual_part_info :: {ref_num :: non_neg_integer, count :: non_neg_integer, seq_num :: non_neg_integer}
  @type part_info :: :single | actual_part_info
  @type extract_result :: {:ok, part_info, binary} | {:error, term}
  @type extract_source :: Pdu.t | binary

  @spec extract(extract_source) :: extract_result

  @doc """
  Extracts multipart information from PDU or directly from binary message.

  Return one of the following:
  * `{:ok, :single, message}` if the `message` does not contain any multipart information and represents a
  single message;
  * `{:ok, {ref_num, count, seq_num}, message}` if the original message contains multipart information in
  UDH fields. The outcoming `message` is cleared from UDH bytes.
  * `{:error, reason}`

  ## Example

      iex> data = <<0x05, 0x00, 0x03, 0x03, 0x02, 0x01, "message">>
      iex> SMPPEX.Pdu.Multipart.extract(data)
      {:ok, {3,2,1}, "message"}

      iex> data = <<0x06, 0x08, 0x04, 0x00, 0x03, 0x02, 0x01, "message">>
      iex> SMPPEX.Pdu.Multipart.extract(data)
      {:ok, {3,2,1}, "message"}

      iex> pdu = Pdu.new({1,0,1}, %{esm_class: 0b01000000, short_message: <<0x05, 0x00, 0x03, 0x03, 0x02, 0x01, "message">>})
      iex> SMPPEX.Pdu.Multipart.extract(pdu)
      {:ok, {3,2,1}, "message"}

      iex> pdu = Pdu.new({1,0,1}, %{short_message: <<0x05, 0x00, 0x03, 0x03, 0x02, 0x01, "message">>})
      iex> SMPPEX.Pdu.Multipart.extract(pdu)
      {:error, "#{@error_not_a_multipart_message}"}

  """
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

  @doc """
  Extracts multipart information from already parsed list of UDH IEs.

  Return one of the following:
  * `{:ok, :single}` if IEs do not contain any multipart message related ones;
  * `{:ok, {ref_num, count, seq_num}}` if there are multipart message related IEs (the first is taken);
  * `{:error, reason}` in case of errors.

  ## Example

      iex> ies = [{0, <<0x03, 0x02, 0x01>>}]
      iex> SMPPEX.Pdu.Multipart.extract_from_ies(ies)
      {:ok, {3, 2, 1}}

      iex> ies = [{0, <<0x03, 0x02, 0x01>>}, {8, <<0x00, 0x04, 0x02, 0x01>>}]
      iex> SMPPEX.Pdu.Multipart.extract_from_ies(ies)
      {:ok, {3, 2, 1}}

      iex> ies = [{8, <<0x00, 0x03, 0x02, 0x01>>}]
      iex> SMPPEX.Pdu.Multipart.extract_from_ies(ies)
      {:ok, {3, 2, 1}}

      iex> ies = [{8, <<0x00, 0x03, 0x02>>}]
      iex> SMPPEX.Pdu.Multipart.extract_from_ies(ies)
      {:error, "#{@error_invalid_16bit_ie}"}

      iex> ies = []
      iex> SMPPEX.Pdu.Multipart.extract_from_ies(ies)
      {:ok, :single}

  """
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

  @spec multipart_ie(actual_part_info) :: {:error, term} | {:ok, UDH.ie}

  @doc """
  Generates IE encoding multipart information.

  ## Example

      iex> SMPPEX.Pdu.Multipart.multipart_ie({3,2,1})
      {:ok, {0, <<0x03, 0x02, 0x01>>}}

      iex> SMPPEX.Pdu.Multipart.multipart_ie({256,2,1})
      {:ok, {8, <<0x01, 0x00, 0x02, 0x01>>}}

      iex> SMPPEX.Pdu.Multipart.multipart_ie({1, 1, 256})
      {:error, "#{@error_invalid_seq_num}"}

  """
  def multipart_ie({ref_num, _count, _seq_num}) when ref_num < 0 or ref_num > 65_535, do: {:error, @error_invalid_ref_num}
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

  @spec prepend_message_with_part_info(actual_part_info, binary) :: {:error, term} | {:ok, binary}

  @doc """
  Prepends message with multipart info encoded as UDH.

  ## Example

      iex> SMPPEX.Pdu.Multipart.prepend_message_with_part_info({3,2,1}, "message")
      {:ok, <<0x05, 0x00, 0x03, 0x03, 0x02, 0x01, "message">>}

      iex> SMPPEX.Pdu.Multipart.prepend_message_with_part_info({256,2,1}, "message")
      {:ok, <<0x06, 0x08, 0x04, 0x01, 0x00, 0x02, 0x01, "message">>}

  """
  def prepend_message_with_part_info(part_info, message) do
    case multipart_ie(part_info) do
      {:ok, ie} -> UDH.add([ie], message)
      {:error, _} = err -> err
    end
  end

  @type split_result :: {:ok, :unsplit} | {:ok, :split, [binary]} | {:error, term}

  @spec split_message(ref_num :: integer, message :: binary, max_len :: integer) :: split_result

  @doc """
  Splits message into parts prepending each part with multipart information UDH
  so that the resulting size of each part does not exceed `max_len` bytes.

  The result is one of the following:
  * `{:ok, :unsplit}` if the message already fits into `max_len` bytes;
  * `{:ok, :split, parts}` if the message was succesfully split into `parts`;
  * `{:error, reason}` in case of errors.

  ## Example

      iex> SMPPEX.Pdu.Multipart.split_message(123, "abc", 3)
      {:ok, :unsplit}

      iex> SMPPEX.Pdu.Multipart.split_message(123, "abcdefg", 6)
      {:error, "#{@error_invalid_max}"}

      iex> SMPPEX.Pdu.Multipart.split_message(123, "abcdefghi", 8)
      {:ok, :split, [
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x01, "ab">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x02, "cd">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x03, "ef">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x04, "gh">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x05, "i">>
      ]}

  """

  def split_message(_ref_num, message, _max_len) when not is_binary(message), do: {:error, @error_invalid_message}
  def split_message(ref_num, _message, _max_len) when ref_num < 0, do: {:error, @error_invalid_ref_num}
  def split_message(ref_num, _message, _max_len) when ref_num > 65_535, do: {:error, @error_invalid_ref_num}
  def split_message(_ref_num, _message, max_len) when max_len < 0, do: {:error, @error_invalid_max}

  def split_message(ref_num, message, max_len) do
    case prepend_message_with_part_info({ref_num, 1, 1}, <<>>) do
      {:ok, bin} ->
        max_split = if max_len >= byte_size(bin), do: max_len - byte_size(bin), else: 0
        split_message(ref_num, message, max_len, max_split)
      {:error, _} = err -> err
    end
  end

  @spec split_message(ref_num :: integer, message :: binary, max_len :: integer, max_split :: integer) :: split_result

  @doc """
  Splits message into parts not exceeding `max_split` bytes and prepending each part with multipart information UDH.
  The message is not split if its size does not exceed `max_len` bytes.

  The results format is the same as in `split_message/3`.

  ## Example

      iex> SMPPEX.Pdu.Multipart.split_message(123, "abcdefghi", 0, 2)
      {:ok, :split, [
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x01, "ab">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x02, "cd">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x03, "ef">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x04, "gh">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x05, "i">>
      ]}

  """

  def split_message(_ref_num, message, _max_len, _max_split) when not is_binary(message), do: {:error, @error_invalid_message}
  def split_message(ref_num, _message, _max_len, _max_split) when ref_num < 0, do: {:error, @error_invalid_ref_num}
  def split_message(ref_num, _message, _max_len, _max_split) when ref_num > 65_535, do: {:error, @error_invalid_ref_num}
  def split_message(_ref_num, _message, max_len, _max_split) when max_len < 0, do: {:error, @error_invalid_max}
  def split_message(_ref_num, _message, _max_len, max_split) when max_split < 0, do: {:error, @error_invalid_max}

  def split_message(ref_num, message, max_len, max_split) do
    message_size = byte_size(message)
    if message_size <= max_len do
      {:ok, :unsplit}
    else
      if max_split > 0 do
        part_count = message_part_count(message_size, max_split)
        split_message_into_parts({ref_num, part_count, 1}, message, max_split, [])
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

  defp split_message_into_parts({_ref_num, count, n}, <<>>, _max_len, parts) when n > count, do: {:ok, :split, Enum.reverse(parts)}
  defp split_message_into_parts({ref_num, count, n} = part_info, message, max_len, parts) do
    {part, rest} = case message do
      << part :: binary-size(max_len), rest :: binary >> -> {part, rest}
      last_part -> {last_part, <<>>}
    end

    case prepend_message_with_part_info(part_info, part) do
      {:ok, part_with_info} ->
        split_message_into_parts({ref_num, count, n + 1}, rest, max_len, [part_with_info | parts])
      {:error, _} = err -> err
    end
  end

end
