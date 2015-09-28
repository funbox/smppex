defmodule SMPPEX.Protocol.Unpack do

  alias SMPPEX.Protocol.Unpack.Helpers

  @null 0

  def integer(bin, size) when size == 1 or size == 2 or size == 4 do
    integer_bit_size = size * 8
    case bin do
      <<int :: big-unsigned-integer-size(integer_bit_size), rest :: binary>> -> {:ok, int, rest}
      _ -> unexpected_data_end
    end
  end

  def integer(_bin, size) do
    {:error, "Invalid integer size #{inspect size}"}
  end

  def c_octet_string(bin, length_spec) do
    c_octet_string(bin, length_spec, :ascii)
  end

  def c_octet_string(_bin, {:fixed, length}, _kind) when length < 1 do
    {:error, "Invalid length #{inspect length} for C-Octet String"}
  end

  def c_octet_string(bin, {:fixed, length}, kind) do
    str_length = length - 1
    case bin do
      << @null :: size(8), rest :: binary >> -> {:ok, nil, rest}
      << str :: binary-size(str_length), @null :: size(8), rest :: binary >> ->
        case valid_kind?(str, kind) do
          true -> {:ok, str, rest}
          false -> {:error, "C-Octet String does not match format #{inspect kind}"}
        end
      << _ :: binary-size(length), _ :: binary >> -> {:error, "Malformed fixed C-Octet String"}
      _ -> unexpected_data_end
    end
  end

  def c_octet_string(_bin, {:max, max}, _kind) when max < 1 do
    {:error, "Invalid max #{inspect max} for variable length C-Octet String"}
  end

  def c_octet_string(bin, {:max, max}, kind) do
    case Helpers.take_until(bin, @null, max) do
      {str, rest} -> case valid_kind?(str, kind) do
        true -> {:ok, str, rest}
        false -> {:error, "C-Octet String does not match format #{inspect kind}"}
      end
      :not_found -> {:error, "Invalid C-Octet String"}
    end
  end

  def octet_string(_bin, length) when length < 0, do: {:error, "Invalid length #{inspect length} for Octet String"}

  def octet_string(bin, length) do
    case bin do
      << str :: binary-size(length), rest :: binary >> -> {:ok, str, rest}
      _ -> unexpected_data_end
    end
  end

  defp valid_kind?(str, :ascii) do
    true
  end

  defp valid_kind?(str, :dec) do
    Helpers.dec? str
  end

  defp valid_kind?(str, :hex) do
    Helpers.hex? str
  end

  defp unexpected_data_end do
    {:error, "Unexpected end of data"}
  end

end

