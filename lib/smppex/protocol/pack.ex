defmodule SMPPEX.Protocol.Pack do
  @moduledoc false

  use Bitwise
  alias SMPPEX.Protocol.Unpack.Helpers

  @null 0
  @null_str << @null >>

  @invalid_integer "Integer: invalid value"
  @invalid_c_octet_string_value "C-Octet String: invalid value"
  @invalid_c_octet_string_format "C-Octet String: invalid format"
  @invalid_octet_string "Octet String: invalid value"
  @invalid_tlv_tag "TLV: invalid tag"
  @invalid_tlv_value "TLV: invalid value"

  @type pack_ok_result :: {:ok, binary}
  @type pack_error_result :: {:error, any}
  @type pack_result :: pack_ok_result | pack_error_result

  @type integer_size :: 1 | 2 | 4

  @spec integer(any, integer_size) :: pack_result

  def integer(nil, size), do: integer(0, size)
  def integer(int, _size) when not is_integer(int), do: {:error, @invalid_integer}

  def integer(int, size) when (size == 1 or size == 2 or size == 4) do
    integer_bit_size = size * 8
    if int >= 0 and (int < (1 <<< integer_bit_size)) do
      {:ok, <<int :: big-unsigned-integer-size(integer_bit_size)>>}
    else
      {:error, @invalid_integer}
    end
  end

  @type kind :: :ascii | :hex | :dec
  @type length_spec :: {:fixed, pos_integer} | {:max, pos_integer}

  @spec c_octet_string(any, length_spec) :: pack_result

  def c_octet_string(nil, length_spec), do: c_octet_string("", length_spec, :ascii)
  def c_octet_string(str, length_spec), do: c_octet_string(str, length_spec, :ascii)

  @spec c_octet_string(any, length_spec, kind) :: pack_result

  def c_octet_string(str, _spec, _kind) when not is_binary(str), do: {:error, @invalid_c_octet_string_value}
  def c_octet_string(nil, length_spec, kind), do: c_octet_string("", length_spec, kind)

  def c_octet_string(str, {:fixed, len}, kind) when is_integer(len) and len >= 1 do
    c_octet_string_with_general_check(str, &(&1 == len), kind)
  end

  def c_octet_string(str, {:max, len}, kind) when is_integer(len) and len >= 1 do
    c_octet_string_with_general_check(str, &(&1 <= len), kind)
  end

  @spec c_octet_string_with_general_check(binary, (integer -> boolean), kind) :: pack_result

  defp c_octet_string_with_general_check(str, check, kind) when is_function(check) do
    str_length = byte_size(str)
    cond do
      str == "" -> {:ok, @null_str}
      check.(str_length + 1) ->
        if valid_kind?(str, kind) do
          {:ok,  str <> @null_str}
        else
          {:error, @invalid_c_octet_string_format}
        end
      true -> {:error, @invalid_c_octet_string_value}
    end
  end

  @spec valid_kind?(binary, kind) :: boolean

  defp valid_kind?(_str, :ascii), do: true
  defp valid_kind?(str, :dec), do: Helpers.dec?(str)
  defp valid_kind?(str, :hex), do: Helpers.hex?(str)

  @spec octet_string(binary, non_neg_integer) :: pack_result

  def octet_string(str, _len) when not is_binary(str), do: {:error, @invalid_octet_string}
  def octet_string(str, len) when is_integer(len) and len >= 0 do
    if byte_size(str) == len do
      {:ok, str}
    else
      {:error, @invalid_octet_string}
    end
  end

  @spec tlv(integer, binary) :: pack_result

  def tlv(_tag, str) when not is_binary(str), do: {:error, @invalid_tlv_value}
  def tlv(tag, _str) when not is_integer(tag), do: {:error, @invalid_tlv_tag}
  def tlv(tag, _str) when tag < 0, do: {:error, @invalid_tlv_tag}
  def tlv(tag, _str) when tag > 65535, do: {:error, @invalid_tlv_tag}
  def tlv(_tag, str) when byte_size(str) >= 65536, do: {:error, @invalid_tlv_value}

  def tlv(tag, str) do
    len = byte_size(str)
    {:ok, <<tag :: big-unsigned-integer-size(16), len :: big-unsigned-integer-size(16), str :: binary >>}
  end

end
