defmodule SMPPEX.Protocol.Unpack do
  @moduledoc false

  alias SMPPEX.Protocol.Unpack.Helpers

  @null 0

  @unexpected_data_end "Unexpected end of data"
  @invalid_c_octet_string_format "C-Octet String: wrong format"
  @invalid_fixed_c_octet_string "C-Octet String(fixed): invalid"
  @invalid_c_octet_string_no_terminator "C-Octet String(var): null terminator not found"


  @type unpack_error_result :: {:error, any}

  @type integer_size :: 1 | 2 | 4

  @spec integer(binary, integer_size) :: unpack_error_result | {:ok, non_neg_integer, binary}

  def integer(bin, size) when (size == 1 or size == 2 or size == 4) and is_binary(bin) do
    integer_bit_size = size * 8
    case bin do
      <<int :: big-unsigned-integer-size(integer_bit_size), rest :: binary>> -> {:ok, int, rest}
      _ -> {:error, @unexpected_data_end}
    end
  end

  @type length_spec :: {:fixed, pos_integer} | {:max, pos_integer}
  @type kind :: :ascii | :hex | :dec
  @type c_octet_string_result :: unpack_error_result | {:ok, binary, binary}

  @spec c_octet_string(binary, length_spec) :: c_octet_string_result

  def c_octet_string(bin, length_spec) when is_binary(bin) do
    c_octet_string(bin, length_spec, :ascii)
  end

  @spec c_octet_string(binary, length_spec, kind) :: c_octet_string_result

  def c_octet_string(bin, {:fixed, len}, kind) when len >= 1 and is_binary(bin) do
    str_length = len - 1
    case bin do
      << @null :: size(8), rest :: binary >> -> {:ok, "", rest}
      << str :: binary-size(str_length), @null :: size(8), rest :: binary >> ->
        case valid_kind?(str, kind) do
          true -> {:ok, str, rest}
          false -> {:error, @invalid_c_octet_string_format}
        end
      << _ :: binary-size(len), _ :: binary >> -> {:error, @invalid_fixed_c_octet_string}
      _ -> {:error, @unexpected_data_end}
    end
  end

  def c_octet_string(bin, {:max, len}, kind) when len >= 1 and is_binary(bin) do
    case Helpers.take_until(bin, @null, len) do
      {str, rest} -> case valid_kind?(str, kind) do
        true -> {:ok, str, rest}
        false -> {:error, @invalid_c_octet_string_format}
      end
      :not_found -> {:error, @invalid_c_octet_string_no_terminator}
    end
  end

  @spec valid_kind?(binary, kind) :: boolean

  defp valid_kind?(_str, :ascii), do: true
  defp valid_kind?(str, :dec), do: Helpers.dec?(str)
  defp valid_kind?(str, :hex), do: Helpers.hex?(str)

  @spec octet_string(binary, non_neg_integer) :: unpack_error_result | {:ok, binary, binary}

  def octet_string(bin, len) when len >= 0 and is_binary(bin) do
    case bin do
      << str :: binary-size(len), rest :: binary >> -> {:ok, str, rest}
      _ -> {:error, @unexpected_data_end}
    end
  end

  @type parsed_tlv :: {non_neg_integer, binary}

  @spec tlv(binary) :: unpack_error_result | {:ok, parsed_tlv, binary}

  def tlv(bin) when byte_size(bin) < 4 do
    {:error, @unexpected_data_end}
  end

  def tlv(<<tag :: big-unsigned-integer-size(16), len :: big-unsigned-integer-size(16), value_and_rest :: binary>>) do
    case value_and_rest do
      << value :: binary-size(len), rest :: binary >> -> {:ok, {tag, value}, rest}
      _ -> {:error, @unexpected_data_end}
    end
  end

end
