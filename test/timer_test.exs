defmodule SMPPEX.TimerTest do
  use ExUnit.Case

  test "timer" do
    start_time = :erlang.system_time(:milli_seconds)

    SMPPEX.Timer.start_link(self, 50)

    :timer.sleep(125)

    end_time = :erlang.system_time(:milli_seconds)

    first_tick = receive do
      {:tick, time} -> time
      after 1 -> :error
    end

    second_tick = receive do
      {:tick, time} -> time
      after 1 -> :error
    end

    assert first_tick > start_time
    assert second_tick > start_time

    assert first_tick < end_time
    assert second_tick < end_time
  end

end

