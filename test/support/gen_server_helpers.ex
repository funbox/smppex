defmodule Support.GenServerHelpers do

  def stop_gen_server(pid) do
    try do
      :gen_server.stop(pid, :kill, 0)
    catch
      :exit, _ -> :ok
    end
  end

end

