defmodule SMPPEX.TCP.Listener do

  defstruct socket: nil, port: 0, tcp_opts: [], handler: nil

  alias SMPPEX.TCP.Listener

  @reuquired_tcp_opts [:binary, {:packet, :raw}, {:active, false}, {:reuseaddr, true}]

  def new({port, opts}, handler) when is_integer(port) and is_list(opts) and is_function(handler) do
    tcp_opts = add_required_tcp_options(opts)
    %Listener{port: port, tcp_opts: tcp_opts, handler: handler}
  end

  defp add_required_tcp_options(opts) do
    @reuquired_tcp_opts ++ opts
  end

end

defimpl SMPPEX.TCP.ClientHandler, for: SMPPEX.TCP.Listener do

  alias SMPPEX.TCP.Listener

  def init(listener) do
    {:ok, socket} = :gen_tcp.listen(listener.port, listener.tcp_opts)
    %Listener{ listener | socket: socket }
  end

  def accept(listener) do
    {:ok, client} = :gen_tcp.accept(listener.socket)
    :ok = :inet.setopts(client, [{:mode, :binary}])
    handler = listener.handler
    _ = case handler.(client) do
      {:ok, pid} when is_pid(pid) ->
        :gen_tcp.controlling_process(client, pid)
      {:error, _} -> :nop
    end
    listener
  end

end


