defmodule SMPPEX.Protocol.Unpack.HelpersTest do
  use ExUnit.Case

  import SMPPEX.Protocol.Unpack.Helpers

  test "hex?" do
    assert hex?("hello") == false
    assert hex?("012345678abcdefABCDEF") == true
    assert hex?({}) == false
    assert hex?([]) == false
    assert hex?(:abc) == false
  end

  test "dec?" do
    assert dec?("hello") == false
    assert dec?("0123456789") == true
    assert dec?({}) == false
    assert dec?([]) == false
    assert dec?(:abc) == false
  end

  test "take_until" do
    assert take_until(<<>>, 0, 0) == :not_found
    assert take_until(<<1,2,3>>, 0, 10) == :not_found
    assert take_until(<<1,2,0>>, 0, 2) == :not_found
    assert take_until(<<1,2,0>>, 0, 3) == {<<1,2>>, <<>>}
    assert take_until(<<1,2,0,3,4>>, 0, 100) == {<<1,2>>, <<3,4>>}
    assert take_until(<<1,2,3,4>>, 0, 100) == :not_found
  end

end


