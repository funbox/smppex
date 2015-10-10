defmodule SMPPEX.Protocol.PackTest do
  use ExUnit.Case

  import SMPPEX.Protocol.Pack

  test "integer" do
    assert_raise FunctionClauseError, fn() -> integer(123, 3) end

    assert {:error, _} = integer(:bad_int, 1)

    assert {:error, _} = integer(256, 1)
    assert {:error, _} = integer(65536, 1)
    assert {:error, _} = integer(4294967296, 1)

    assert {:error, _} = integer(-1, 1)
    assert {:error, _} = integer(-1, 1)
    assert {:error, _} = integer(-1, 1)

    assert {:ok, <<0xFF>>} = integer(255, 1)
    assert {:ok, <<0xFF, 0xFF>>} = integer(65535, 2)
    assert {:ok, <<0xFF, 0xFF, 0xFF, 0xFF>>} = integer(4294967295, 4)

    assert {:ok, <<0x00>>} = integer(0, 1)
    assert {:ok, <<0x00, 0x00>>} = integer(0, 2)
    assert {:ok, <<0x00, 0x00, 0x00, 0x00>>} = integer(0, 4)
  end

  test "c_octet_string" do
    assert_raise FunctionClauseError, fn() -> c_octet_string("foo", {:bad, :spec}) end
    assert_raise FunctionClauseError, fn() -> c_octet_string("foo", {:max, 123}, :bad_format) end
    assert {:error, _} = c_octet_string(:not_a_string, {:max, 123})
  end

  test "c_octet_string, fixed" do
    assert {:error, _} = c_octet_string("abc", {:fixed, 3})
    assert {:error, _} = c_octet_string("a", {:fixed, 3})

    assert {:error, _} = c_octet_string("aX", {:fixed, 3}, :hex)
    assert {:error, _} = c_octet_string("0X", {:fixed, 3}, :dec)

    assert {:ok, << 0 >>} = c_octet_string("", {:fixed, 3})
    assert {:ok, << 0 >>} = c_octet_string(nil, {:fixed, 3})
    assert {:ok, << ?a, ?b, 0 >>} = c_octet_string("ab", {:fixed, 3})

    assert {:ok, <<?a, ?b, 0>>} = c_octet_string("ab", {:fixed, 3}, :hex)
    assert {:ok, <<?0, ?1, 0>>} = c_octet_string("01", {:fixed, 3}, :dec)
  end

  test "c_octet_string, var" do
    assert {:error, _} = c_octet_string("abc", {:max, 3})

    assert {:error, _} = c_octet_string("aX", {:max, 3}, :hex)
    assert {:error, _} = c_octet_string("0X", {:max, 3}, :dec)

    assert {:ok, << 0 >>} = c_octet_string("", {:max, 3})
    assert {:ok, << 0 >>} = c_octet_string(nil, {:max, 3})

    assert {:ok, << ?a, 0 >>} = c_octet_string("a", {:max, 3})
    assert {:ok, << ?a, ?b, 0 >>} = c_octet_string("ab", {:max, 3})

    assert {:ok, <<?a, ?b, 0>>} = c_octet_string("ab", {:max, 3}, :hex)
    assert {:ok, <<?0, ?1, 0>>} = c_octet_string("01", {:max, 3}, :dec)
  end

  test "octet_string" do
    assert_raise FunctionClauseError, fn() -> octet_string("foo", :bad_length) end
    assert_raise FunctionClauseError, fn() -> octet_string("foo", -1) end

    assert {:error, _} = octet_string(:not_a_string, 12)
    assert {:error, _} = octet_string("abc", 2)
    assert {:error, _} = octet_string("abc", 4)

    assert {:ok, "abc"} == octet_string("abc", 3)
  end
end

