defmodule SMPPEX.Protocol.Unpack do

  alias SMPPEX.Protocol.Unpack.Helpers

  @null 0

  @type parsed_value :: integer | binary
  @type parse_error :: {:error, binary}
  @type parse_ok :: {:ok, parsed_value, binary}
  @type parse_result :: parse_ok | parse_error

  @type integer_size :: 1 | 2 | 4
  @spec integer(binary, integer_size) :: parse_result
  def integer(bin, size) when size == 1 or size == 2 or size == 4 do
    integer_bit_size = size * 8
    case bin do
      <<int :: big-unsigned-integer-size(integer_bit_size), rest :: binary>> -> {:ok, int, rest}
      _ -> unexpected_data_end
    end
  end

  def integer(bin, size) do
    {:error, "Invalid integer size #{inspect size}"}
  end

  @type c_octet_kind :: :ascii | :hex | :dec
  @type c_octet_lengh_spec :: {:fixed, integer} | {:var, integer}

  @spec c_octet_string(binary, c_octet_lengh_spec) :: parse_result
  def c_octet_string(bin, length_spec) do
    c_octet_string(bin, length_spec, :ascii)
  end

  @spec c_octet_string(binary, c_octet_lengh_spec, c_octet_kind) :: parse_result

  def c_octet_string(_bin, {:fixed, length}, _kind) when length < 1 do
    {:error, "Invalid length #{inspect length} for C-Octet String"}
  end

  def c_octet_string(bin, {:fixed, length}, kind) do
    bit_length = length * 8
    case bin do
      << @null :: size(8), rest :: binary >> -> {:ok, nil, rest}
      << str :: binary-size(bit_length), @null :: size(8), rest :: binary >> ->
        case valid_kind?(str, kind) do
          true -> {:ok, str, rest}
          false -> {:error, "C-Octet String does not match format #{inspect kind}"}
        end
      _ -> unexpected_data_end
    end
  end

  def c_octet_string(bin, {:var, max}, _kind) when max < 1 do
    {:error, "Invalid max #{inspect max} for variable length C-Octet String"}
  end

  def c_octet_string(bin, {:var, max}, kind) do
    case Helpers.take_until(bin, @null, max) do
      {str, rest} -> case valid_kind?(str, kind) do
        true -> {:ok, str, rest}
        false -> {:error, "C-Octet String does not match format #{inspect kind}"}
      end
      not_found -> {:error, "Invalid C-Octet String"}
    end
  end

  @spec octet_string(binary, integer) :: parse_result
  def octet_string(_bin, length) when length < 0, do: {:error, "Invalid length #{inspect length} for Octet String"}

  def octet_string(bin, length) do
    case bin do
      << str :: binary-size(length), rest :: binary >> -> {:ok, str, rest}
      _ -> unexpected_data_end
    end
  end

  @spec valid_kind?(binary, c_octet_kind) :: boolean
  def valid_kind?(str, :ascii) do
    true
  end

  def valid_kind?(str, :dec) do
    Helpers.dec? str
  end

  def valid_kind?(str, :hex) do
    Helpers.hex? str
  end

  @spec unexpected_data_end() :: parse_error
  defp unexpected_data_end do
    {:error, "Unexpected end of data"}
  end

end

