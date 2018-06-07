defmodule SMPPEX.Pdu.NetworkErrorCodeTest do
  alias SMPPEX.Pdu.NetworkErrorCode

  use ExUnit.Case

  doctest NetworkErrorCode

  test "decode" do
    assert_raise FunctionClauseError, fn ->
      NetworkErrorCode.decode(<<1,2,3,4>>)
    end
  end

  test "encode" do
    assert_raise FunctionClauseError, fn ->
      NetworkErrorCode.encode(256, 1)
    end
    assert_raise FunctionClauseError, fn ->
      NetworkErrorCode.encode(1, 65_536)
    end
  end
end
