defmodule SMPPEX.Protocol.Unpack do

  alias SMPPEX.Protocol.Unpack.Helpers
  import SMPPEX.Protocol.ParseResult

  @null 0

  @unexpected_data_end "Unexpected end of data"

  @invalid_c_octet_string_format "C-Octet String: wrong format"
  @invalid_fixed_c_octet_string "C-Octet String(fixed): invalid"
  @invalid_c_octet_string_no_terminator "C-Octet String(var): null terminator not found"

  def integer(bin, size) when (size == 1 or size == 2 or size == 4) and is_binary(bin) do
    integer_bit_size = size * 8
    case bin do
      <<int :: big-unsigned-integer-size(integer_bit_size), rest :: binary>> -> {:ok, int, rest}
      _ -> error(@unexpected_data_end)
    end
  end

  def c_octet_string(bin, length_spec) when is_binary(bin) do
    c_octet_string(bin, length_spec, :ascii)
  end

  def c_octet_string(bin, {:fixed, length}, kind) when length >= 1 and is_binary(bin) do
    str_length = length - 1
    case bin do
      << @null :: size(8), rest :: binary >> -> {:ok, "", rest}
      << str :: binary-size(str_length), @null :: size(8), rest :: binary >> ->
        case valid_kind?(str, kind) do
          true -> {:ok, str, rest}
          false -> error(@invalid_c_octet_string_format)
        end
      << _ :: binary-size(length), _ :: binary >> -> error(@invalid_fixed_c_octet_string)
      _ -> error(@unexpected_data_end)
    end
  end

  def c_octet_string(bin, {:max, max}, kind) when max >= 1 and is_binary(bin) do
    case Helpers.take_until(bin, @null, max) do
      {str, rest} -> case valid_kind?(str, kind) do
        true -> {:ok, str, rest}
        false -> error(@invalid_c_octet_string_format)
      end
      :not_found -> error(@invalid_c_octet_string_no_terminator)
    end
  end

  defp valid_kind?(_str, :ascii), do: true
  defp valid_kind?(str, :dec), do: Helpers.dec?(str)
  defp valid_kind?(str, :hex), do: Helpers.hex?(str)

  def octet_string(bin, length) when length >= 0 and is_binary(bin) do
    case bin do
      << str :: binary-size(length), rest :: binary >> -> {:ok, str, rest}
      _ -> error(@unexpected_data_end)
    end
  end

  def tlv(bin) when byte_size(bin) < 4 do
    error(@unexpected_data_end)
  end

  def tlv(<<tag :: big-unsigned-integer-size(16), length :: big-unsigned-integer-size(16), value_and_rest :: binary>>) do
    case value_and_rest do
      << value :: binary-size(length), rest :: binary >> -> {:ok, {tag, value}, rest}
      _ -> error(@unexpected_data_end)
    end
  end

end
