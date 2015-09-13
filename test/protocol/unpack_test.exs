defmodule SMPPEX.Protocol.UnpackTest do
  use ExUnit.Case

  import SMPPEX.Protocol.Unpack

  test "integer" do
    assert match? {:error, _}, integer(<<>>, 1)
    assert match? {:error, _}, integer(<<1>>, 2)
    assert match? {:error, _}, integer(<<1,2,3>>, 4)
    assert match? {:error, _}, integer(<<1,2,3>>, 3)

    assert {:ok, 1, <<2,3>>} == integer(<<1,2,3>>, 1)
    assert {:ok, 1, <<3>>} == integer(<<0,1,3>>, 2)
    assert {:ok, 1, <<5>>} == integer(<<0,0,0,1,5>>, 4)
  end
end

