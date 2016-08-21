defmodule Support.StoppingESME do
  @moduledoc false

  use SMPPEX.ESME

  def init(stop_reason) do
    {:stop, stop_reason}
  end

end

