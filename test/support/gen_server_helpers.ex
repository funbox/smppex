defmodule Support.GenServerHelpers do
  @moduledoc false

  alias :gen_server, as: ErlangGenServer

  def stop_gen_server(pid) do
    try do
      ErlangGenServer.stop(pid, :kill, 0)
    catch
      :exit, _ -> :ok
    end
  end

end

