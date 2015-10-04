defmodule SMPPEX.Protocol.Unpack do

  alias SMPPEX.Protocol.Unpack.Helpers
  import SMPPEX.Protocol.ParseResult

  @null 0

  @invalid_integer_size "Invalid integer size"
  @unexpected_data_end "Unexpected end of data"
  @invalid_c_octet_string_length "Invalid length for C-Octet String"
  @invalid_c_octet_string_format "C-Octet String does not match format"
  @invalid_fixed_c_octet_string "Malformed fixed C-Octet String"
  @invalid_c_octet_string_no_terminator "Invalid C-Octet String: null terminator not found"
  @invalid_c_octet_string_max "Invalid max for C-Octet String"
  @invalid_octet_string_length "Invalid length for Octet String"

  def integer(bin, size) when size == 1 or size == 2 or size == 4 do
    integer_bit_size = size * 8
    case bin do
      <<int :: big-unsigned-integer-size(integer_bit_size), rest :: binary>> -> ok(int, rest)
      _ -> error(@unexpected_data_end)
    end
  end

  def integer(_bin, _size) do
    error(@invalid_integer_size)
  end

  def c_octet_string(bin, length_spec) do
    c_octet_string(bin, length_spec, :ascii)
  end

  def c_octet_string(_bin, {:fixed, length}, _kind) when length < 1 do
    error(@invalid_c_octet_string_length)
  end

  def c_octet_string(bin, {:fixed, length}, kind) do
    str_length = length - 1
    case bin do
      << @null :: size(8), rest :: binary >> -> ok("", rest)
      << str :: binary-size(str_length), @null :: size(8), rest :: binary >> ->
        case valid_kind?(str, kind) do
          true -> ok(str, rest)
          false -> error(@invalid_c_octet_string_format)
        end
      << _ :: binary-size(length), _ :: binary >> -> error(@invalid_fixed_c_octet_string)
      _ -> error(@unexpected_data_end)
    end
  end

  def c_octet_string(_bin, {:max, max}, _kind) when max < 1 do
    error(@invalid_c_octet_string_max)
  end

  def c_octet_string(bin, {:max, max}, kind) do
    case Helpers.take_until(bin, @null, max) do
      {str, rest} -> case valid_kind?(str, kind) do
        true -> ok(str, rest)
        false -> error(@invalid_c_octet_string_format)
      end
      :not_found -> error(@invalid_c_octet_string_no_terminator)
    end
  end

  defp valid_kind?(_str, :ascii) do
    true
  end

  defp valid_kind?(str, :dec) do
    Helpers.dec? str
  end

  defp valid_kind?(str, :hex) do
    Helpers.hex? str
  end

  def octet_string(_bin, length) when length < 0, do: error(@invalid_octet_string_length)

  def octet_string(bin, length) do
    case bin do
      << str :: binary-size(length), rest :: binary >> -> ok(str, rest)
      _ -> error(@unexpected_data_end)
    end
  end

  def tlv(bin) when byte_size(bin) < 4 do
    error(@unexpected_data_end)
  end

  def tlv(<<tag :: big-unsigned-integer-size(16), length :: big-unsigned-integer-size(16), value_and_rest :: binary>>) do
    case value_and_rest do
      << value :: binary-size(length), rest :: binary >> -> ok({tag, value}, rest)
      _ -> error(@unexpected_data_end)
    end
  end

end

