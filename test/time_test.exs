defmodule SMPPEX.TimeTest do
  use ExUnit.Case

  test "monotonic" do
    t1 = SMPPEX.Time.monotonic
    :timer.sleep(2)
    t2 = SMPPEX.Time.monotonic

    assert t1 < t2
  end

end
