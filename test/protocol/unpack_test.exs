defmodule SMPPEX.Protocol.UnpackTest do
  use ExUnit.Case

  alias SMPPEX.Protocol.Unpack

  test "integer" do
    assert {:error, _} = Unpack.integer(<<>>, 1)
    assert {:error, _} = Unpack.integer(<<1>>, 2)
    assert {:error, _} = Unpack.integer(<<1, 2, 3>>, 4)

    assert {:ok, 1, <<2, 3>>} == Unpack.integer(<<1, 2, 3>>, 1)
    assert {:ok, 1, <<3>>} == Unpack.integer(<<0, 1, 3>>, 2)
    assert {:ok, 1, <<5>>} == Unpack.integer(<<0, 0, 0, 1, 5>>, 4)
  end

  test "c_octet_string: fixed, ascii" do
    assert {:ok, "", "rest"} == Unpack.c_octet_string(<<0, "rest">>, {:fixed, 1}, :ascii)
    assert {:error, _} = Unpack.c_octet_string(<<"ab", 0>>, {:fixed, 4})

    assert {:ok, "ab", "c"} == Unpack.c_octet_string(<<"ab", 0, "c">>, {:fixed, 3}, :ascii)
  end

  test "c_octet_string: fixed, hex" do
    assert {:ok, "0123456789abcdefABCDEF", "c"} ==
             Unpack.c_octet_string(<<"0123456789abcdefABCDEF", 0, "c">>, {:fixed, 23}, :hex)

    assert {:error, _} =
             Unpack.c_octet_string(<<"0123456789abXdefABCDEF", 0, "c">>, {:fixed, 23}, :hex)
  end

  test "c_octet_string: fixed, dec" do
    assert {:ok, "0123456789", "c"} ==
             Unpack.c_octet_string(<<"0123456789", 0, "c">>, {:fixed, 11}, :dec)

    assert {:error, _} = Unpack.c_octet_string(<<"01234X6789", 0, "c">>, {:fixed, 11}, :dec)
  end

  test "c_octet_string: var max, ascii" do
    assert {:error, _} = Unpack.c_octet_string(<<"hi">>, {:max, 1}, :ascii)

    assert {:ok, "", "rest"} == Unpack.c_octet_string(<<0, "rest">>, {:max, 3}, :ascii)

    assert {:ok, "ab", "c"} == Unpack.c_octet_string(<<"ab", 0, "c">>, {:max, 3}, :ascii)
    assert {:ok, "ab", "c"} == Unpack.c_octet_string(<<"ab", 0, "c">>, {:max, 4}, :ascii)
  end

  test "c_octet_string: var max, hex" do
    assert {:ok, "0123456789abcdefABCDEF", "c"} ==
             Unpack.c_octet_string(<<"0123456789abcdefABCDEF", 0, "c">>, {:max, 23}, :hex)

    assert {:error, _} =
             Unpack.c_octet_string(<<"0123456789abXdefABCDEF", 0, "c">>, {:max, 23}, :hex)
  end

  test "c_octet_string: var max, dec" do
    assert {:ok, "0123456789", "c"} ==
             Unpack.c_octet_string(<<"0123456789", 0, "c">>, {:max, 11}, :dec)

    assert {:error, _} = Unpack.c_octet_string(<<"01234F6789", 0, "c">>, {:max, 11}, :dec)
  end

  test "octet_string" do
    assert {:ok, "", "123"} == Unpack.octet_string("123", 0)
    assert {:ok, "12", "3"} == Unpack.octet_string("123", 2)
    assert {:error, _} = Unpack.octet_string("123", 4)
  end

  test "tlv" do
    assert {:error, _} = Unpack.tlv(<<0, 1, 0, 2, 0>>)
    assert {:error, _} = Unpack.tlv(<<0, 1, 0, -1, 0>>)

    assert {:ok, {1, <<3, 4>>}, <<5>>} == Unpack.tlv(<<0, 1, 0, 2, 3, 4, 5>>)
  end
end
