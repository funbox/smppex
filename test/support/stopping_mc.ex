defmodule Support.StoppingMC do

  alias :timer, as: Timer

  use SMPPEX.MC

  def init(_sock, _transport, stop_reason) do
    Timer.sleep(50)
    {:stop, stop_reason}
  end

end


