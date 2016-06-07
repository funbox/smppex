defmodule Support.StoppingESME do

  use SMPPEX.ESME

  def init(stop_reason) do
    {:stop, stop_reason}
  end

end

