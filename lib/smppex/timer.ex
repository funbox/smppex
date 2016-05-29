defmodule SMPPEX.Timer do

  @default_interval 1000

  def start_link, do: start_link(self)

  def start_link(pid, interval \\ @default_interval) do
    spawn_link(fn() ->
      loop(pid, interval)
    end)
  end

  defp loop(pid, interval) do
    :timer.sleep(interval)
    time = :erlang.system_time(:milli_seconds)
    send(pid, {:tick, time})
    loop(pid, interval)
  end

end
