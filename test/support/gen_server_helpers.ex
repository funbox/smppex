defmodule Support.GenServerHelpers do

  alias :gen_server, as: GenTCP

  def stop_gen_server(pid) do
    try do
      GenTCP.stop(pid, :kill, 0)
    catch
      :exit, _ -> :ok
    end
  end

end

