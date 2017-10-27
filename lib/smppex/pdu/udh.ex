defmodule SMPPEX.Pdu.UDH do
  @moduledoc """
  Module for parsing encoded IEs from UDHs.
  """

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

  @spec has_udh?(pdu :: Pdu.t()) :: boolean

  @doc """
  Checks if message in PDU has UDH (by inspecting `esm_class` field).

  ## Example

      iex> pdu = SMPPEX.Pdu.new({1,0,1}, %{esm_class: 0}, %{})
      iex> SMPPEX.Pdu.UDH.has_udh?(pdu)
      false

      iex> pdu = SMPPEX.Pdu.new({1,0,1}, %{esm_class: 0b01000000}, %{})
      iex> SMPPEX.Pdu.UDH.has_udh?(pdu)
      true

  """
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
  @spec extract(message :: binary) :: {:error, term} | {:ok, list(ie), binary}

  @doc """
  Extracts list of IEs from UDH.

  Returns one of the following
  * `{:ok, ies, message}` where `ies` is a list of IE tuples `{id, value}` and `message`
  is the original message without UDH;
  * `{:error, reason}` in case of errors.

  ## Example

      iex> data = <<5, 0, 3, 197, 3, 3, "message">>
      iex> SMPPEX.Pdu.UDH.extract(data)
      {:ok, [{0, <<197, 3, 3>>}], "message"}

      iex> data = <<0x0B, 0x05, 0x04, 0x06, 0x2d, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>
      iex> SMPPEX.Pdu.UDH.extract(data)
      {:ok, [{0x05, <<0x06, 0x2d, 0x00, 0x00>>}, {0x00, <<0x01, 0x02, 0x01>>}], "message"}

      iex> data = <<0x10, "short">>
      iex> SMPPEX.Pdu.UDH.extract(data)
      {:error, "#{@error_invalid_udh_length}"}

      iex> data = <<0x06, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>
      iex> SMPPEX.Pdu.UDH.extract(data)
      {:error, "#{@error_invalid_udh_data}"}

      iex> data = <<5, 0, 4, 197, 3, 3, "message">>
      iex> SMPPEX.Pdu.UDH.extract(data)
      {:error, "#{@error_invalid_udh_ie_length}"}

  """
  def extract(data) do
    case data do
      <<udh_length::integer-unsigned-size(8), rest::binary>> ->
        case rest do
          <<ies_data::binary-size(udh_length), message::binary>> ->
            parse_ies(ies_data, message)

          _ ->
            {:error, @error_invalid_udh_length}
        end

      _ ->
        {:error, @error_invalid_udh_data}
    end
  end

  defp parse_ies(ies_data, message) do
    case parse_ies_data(ies_data, []) do
      {:ok, ies} -> {:ok, ies, message}
      {:error, _} = err -> err
    end
  end

  defp parse_ies_data(<<>>, parsed), do: {:ok, Enum.reverse(parsed)}

  defp parse_ies_data(
         <<
           ie_id::integer-unsigned-size(8),
           ie_len::integer-unsigned-size(8),
           ie_data_and_rest::binary
         >>,
         parsed
       ) do
    case ie_data_and_rest do
      <<ie_data::binary-size(ie_len), rest::binary>> ->
        parse_ies_data(rest, [{ie_id, ie_data} | parsed])

      _ ->
        {:error, @error_invalid_udh_ie_length}
    end
  end

  defp parse_ies_data(_, _), do: {:error, @error_invalid_udh_data}

  @spec add(list(ie), binary) :: {:ok, binary} | {:error, any}

  @doc """
  Encodes IEs and prepends message with the encoded value.

  The result is one of the following:
  * `{:ok, message}` where message is the original message prefixed with UDH;
  * `{:error, reason}` in case of errors.

  ## Example

      iex> ies = [{0x05, <<0x06, 0x2d, 0x00, 0x00>>}, {0x00, <<0x01, 0x02, 0x01>>}]
      iex> SMPPEX.Pdu.UDH.add(ies, "message")
      {:ok, <<0x0B, 0x05, 0x04, 0x06, 0x2d, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x01, "message">>}

      iex> ies = [{0, 123}]
      iex> SMPPEX.Pdu.UDH.add(ies, "message")
      {:error, "#{@error_invalid_udh_ie_data}"}

      iex> ies = [{345, "ie"}]
      iex> SMPPEX.Pdu.UDH.add(ies, "message")
      {:error, "#{@error_invalid_udh_ie_id}"}

      iex> ies = [{-1, "ie"}]
      iex> SMPPEX.Pdu.UDH.add(ies, "message")
      {:error, "#{@error_invalid_udh_ie_id}"}

      iex> ies = [{0, <<1 :: integer-size(2040)>>}]
      iex> SMPPEX.Pdu.UDH.add(ies, "message")
      {:error, "#{@error_udh_data_too_long}"}
  """
  def add(ies, message) do
    case pack_ies(ies, []) do
      {:ok, data} -> concat_ie_data_and_message(data, message)
      {:error, _} = err -> err
    end
  end

  defp pack_ies([], packed), do: {:ok, packed |> Enum.reverse() |> Enum.join()}

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
    packed_ie = <<id::integer-unsigned-size(8), len::integer-unsigned-size(8), value::binary>>
    {:ok, packed_ie}
  end

  defp concat_ie_data_and_message(_data, message) when not is_binary(message),
    do: {:error, @error_invalid_message_data}

  defp concat_ie_data_and_message(data, _message) when byte_size(data) > 0xFF,
    do: {:error, @error_udh_data_too_long}

  defp concat_ie_data_and_message(data, message) do
    data_len = byte_size(data)
    {:ok, <<data_len::integer-unsigned-size(8), data::binary, message::binary>>}
  end
end
