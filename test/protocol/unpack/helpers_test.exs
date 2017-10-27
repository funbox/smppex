defmodule SMPPEX.Protocol.Unpack.HelpersTest do
  use ExUnit.Case

  alias SMPPEX.Protocol.Unpack.Helpers

  test "hex?" do
    assert Helpers.hex?("hello") == false
    assert Helpers.hex?("012345678abcdefABCDEF") == true
    assert Helpers.hex?({}) == false
    assert Helpers.hex?([]) == false
    assert Helpers.hex?(:abc) == false
  end

  test "dec?" do
    assert Helpers.dec?("hello") == false
    assert Helpers.dec?("0123456789") == true
    assert Helpers.dec?({}) == false
    assert Helpers.dec?([]) == false
    assert Helpers.dec?(:abc) == false
  end

  test "take_until" do
    assert Helpers.take_until(<<>>, 0, 0) == :not_found
    assert Helpers.take_until(<<1, 2, 3>>, 0, 10) == :not_found
    assert Helpers.take_until(<<1, 2, 0>>, 0, 2) == :not_found
    assert Helpers.take_until(<<1, 2, 0>>, 0, 3) == {<<1, 2>>, <<>>}
    assert Helpers.take_until(<<1, 2, 0, 3, 4>>, 0, 100) == {<<1, 2>>, <<3, 4>>}
    assert Helpers.take_until(<<1, 2, 3, 4>>, 0, 100) == :not_found
  end
end
