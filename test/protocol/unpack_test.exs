defmodule SMPPEX.Protocol.UnpackTest do
  use AssertHelpers

  import SMPPEX.Protocol.Unpack

  test "integer" do
    assert_error integer(<<>>, 1)
    assert_error integer(<<1>>, 2)
    assert_error integer(<<1,2,3>>, 4)
    assert_error integer(<<1,2,3>>, 3)

    assert {:ok, 1, <<2,3>>} == integer(<<1,2,3>>, 1)
    assert {:ok, 1, <<3>>} == integer(<<0,1,3>>, 2)
    assert {:ok, 1, <<5>>} == integer(<<0,0,0,1,5>>, 4)
  end

  test "c_octet_string: fixed, ascii" do
    assert_error c_octet_string(<<"hi">>, {:fixed, 0})
    assert_error c_octet_string(<<"hi">>, {:fixed, 0}, :ascii)

    assert {:ok, "", "rest"} == c_octet_string(<<0, "rest">>, {:fixed, 1}, :ascii)
    assert_error c_octet_string(<<"ab", 0>>, {:fixed, 4})

    assert {:ok, "ab", "c"} == c_octet_string(<<"ab", 0, "c">>, {:fixed, 3}, :ascii)
  end

  test "c_octet_string: fixed, hex" do
    assert {:ok, "0123456789abcdefABCDEF", "c"} == c_octet_string(<<"0123456789abcdefABCDEF", 0, "c">>, {:fixed, 23}, :hex)
    assert_error c_octet_string(<<"0123456789abXdefABCDEF", 0, "c">>, {:fixed, 23}, :hex)
  end

  test "c_octet_string: fixed, dec" do
    assert {:ok, "0123456789", "c"} == c_octet_string(<<"0123456789", 0, "c">>, {:fixed, 11}, :dec)
    assert_error c_octet_string(<<"01234X6789", 0, "c">>, {:fixed, 11}, :dec)
  end

  test "c_octet_string: var max, ascii" do
    assert_error c_octet_string(<<"hi">>, {:max, 0}, :ascii)
    assert_error c_octet_string(<<"hi">>, {:max, 1}, :ascii)

    assert {:ok, "", "rest"} == c_octet_string(<<0, "rest">>, {:max, 3}, :ascii)

    assert {:ok, "ab", "c"} == c_octet_string(<<"ab", 0, "c">>, {:max, 3}, :ascii)
    assert {:ok, "ab", "c"} == c_octet_string(<<"ab", 0, "c">>, {:max, 4}, :ascii)
  end

  test "c_octet_string: var max, hex" do
    assert {:ok, "0123456789abcdefABCDEF", "c"} == c_octet_string(<<"0123456789abcdefABCDEF", 0, "c">>, {:max, 23}, :hex)
    assert_error c_octet_string(<<"0123456789abXdefABCDEF", 0, "c">>, {:max, 23}, :hex)
  end

  test "c_octet_string: var max, dec" do
    assert {:ok, "0123456789", "c"} == c_octet_string(<<"0123456789", 0, "c">>, {:max, 11}, :dec)
    assert_error c_octet_string(<<"01234F6789", 0, "c">>, {:max, 11}, :dec)
  end

  test "octet_string" do
    assert_error octet_string("123", -1)
    assert {:ok, "", "123"} == octet_string("123", 0)
    assert {:ok, "12", "3"} == octet_string("123", 2)
    assert_error octet_string("123", 4)
  end

  test "tlv" do
    assert_error tlv(<<0,1,0,2,0>>)
    assert_error tlv(<<0,1,0,-1,0>>)

    assert {:ok, {1, <<3,4>>}, <<5>>} == tlv(<<0,1,0,2,3,4,5>>)
  end
end

