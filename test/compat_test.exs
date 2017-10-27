defmodule SMPPEX.CompatTest do
  use ExUnit.Case

  alias SMPPEX.Compat
  alias :timer, as: Timer

  test "monotonic_time" do
    t1 = SMPPEX.Compat.monotonic_time()
    Timer.sleep(1)
    t2 = SMPPEX.Compat.monotonic_time()

    assert t1 < t2
  end

  test "to_charlist" do
    assert 'abc' = Compat.to_charlist("abc")
  end
end
