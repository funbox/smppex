defmodule SMPPEX.TimeTest do
  use ExUnit.Case

  test "monotonic" do
    t1 = SMPPEX.Compat.monotonic_time
    :timer.sleep(2)
    t2 = SMPPEX.Compat.monotonic_time

    assert t1 < t2
  end

end
