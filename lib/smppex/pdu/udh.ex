defmodule SMPPEX.Pdu.UDH do

  alias SMPPEX.Pdu
  use Bitwise

  @esm_class_gsm_udhi 0b01000000

  @error_invalid_udh_data "Invalid UDH data"
  @error_invalid_udh_length "Invalid UDH lengh"
  @error_invalid_udh_ie_length "Invalid UDH IE lengh"
  @error_invalid_udh_ie_id "Invalid UDH IE id"
  @error_invalid_udh_ie_data "Invalid UDH IE data"
  @error_invalid_message_data "Invalid Message data"
  @error_udh_data_too_long "UDH is too long"

  @spec has_udh?(Pdu.t) :: boolean

  def has_udh?(pdu) do
    case Pdu.field(pdu, :esm_class) do
      nil -> false
      esm_class -> has_udh_flag?(esm_class)
    end
  end

  defp has_udh_flag?(esm_class) do
    esm_class && (esm_class &&& @esm_class_gsm_udhi) == @esm_class_gsm_udhi
  end

  @type ie :: {byte, binary}
  @spec extract(binary) :: {:error, any} | {:ok, list(ie), binary}

  def extract(data) do
    case data do
      << udh_length :: integer-unsigned-size(8), rest :: binary >> ->
        case rest do
          << ies_data :: binary-size(udh_length), message :: binary >> ->
            parse_ies(ies_data, message)
          _ -> {:error, @error_invalid_udh_length}
        end
      _ -> {:error, @error_invalid_udh_data}
    end
  end

  defp parse_ies(ies_data, message) do
    case parse_ies_data(ies_data, []) do
      {:ok, ies} -> {:ok, ies, message}
      {:error, _} = err -> err
    end
  end

  defp parse_ies_data(<<>>, parsed), do: {:ok, Enum.reverse(parsed)}
  defp parse_ies_data(<< ie_id :: integer-unsigned-size(8), ie_len :: integer-unsigned-size(8), ie_data_and_rest :: binary >>, parsed) do
    case ie_data_and_rest do
      << ie_data :: binary-size(ie_len), rest :: binary >> ->
        parse_ies_data(rest, [{ie_id, ie_data}| parsed])
      _ -> {:error, @error_invalid_udh_ie_length}
    end
  end
  defp parse_ies_data(_, _), do: {:error, @error_invalid_udh_data}

  @spec add(list(ie), binary) :: {:ok, binary} | {:error, any}

  def add(ies, message) do
    case pack_ies(ies, []) do
      {:ok, data} -> concat_ie_data_and_message(data, message)
      {:error, _} = err -> err
    end
  end

  defp pack_ies([], packed), do: {:ok, packed |> Enum.reverse |> Enum.join}
  defp pack_ies([ie | ies], packed) do
    case pack_ie(ie) do
      {:ok, packed_ie} -> pack_ies(ies, [packed_ie | packed])
      {:error, _} = err -> err
    end
  end

  defp pack_ie({id, _}) when not is_integer(id), do: {:error, @error_invalid_udh_ie_id}
  defp pack_ie({id, _}) when id < 0x00 or id > 0xFF, do: {:error, @error_invalid_udh_ie_id}
  defp pack_ie({_, value}) when not is_binary(value), do: {:error, @error_invalid_udh_ie_data}
  defp pack_ie({_, value}) when byte_size(value) > 0xFF, do: {:error, @error_invalid_udh_ie_data}
  defp pack_ie({id, value}) do
    len = byte_size(value)
    packed_ie = << id :: integer-unsigned-size(8), len :: integer-unsigned-size(8), value :: binary >>
    {:ok, packed_ie}
  end

  defp concat_ie_data_and_message(_data, message) when not is_binary(message), do: {:error, @error_invalid_message_data}
  defp concat_ie_data_and_message(data, _message) when byte_size(data)  > 0xFF, do: {:error, @error_udh_data_too_long}
  defp concat_ie_data_and_message(data, message) do
    data_len = byte_size(data)
    {:ok, << data_len :: integer-unsigned-size(8), data :: binary, message :: binary >>}
  end

end


