defmodule SMPPEX.TCP.Listener do

  defstruct socket: nil, port: 0, tcp_opts: 0, handler: nil

  alias SMPPEX.TCP.Listener

  @reuquired_tcp_opts [binary: true, packet: :raw, active: false, reuseaddr: true]

  def new({port, opts}, handler) when is_integer(port) and is_list(opts) and is_function(handler) do
    tcp_opts = add_required_tcp_options(opts)
    %Listener{port: port, tcp_opts: opts, handler: handler}
  end

  defp add_required_tcp_options(opts) do
    Keyword.merge(opts, @reuquired_tcp_opts, fn(_, v) -> v end)
  end

end

defimpl SMPPEX.TCP.ClientHandler, for: SMPPEX.TCP.Listener do

  alias SMPPEX.TCP.Listener

  def init(listener) do
    {:ok, socket} = :gen_tcp.listen(listener.port, listener.tcp_opts)
    {:ok, port} = :inet.port(socket)
    %Listener{ listener | socket: socket, port: port }
  end

  def accept(listener) do
    {:ok, client} = :gen_tcp.accept(listener.socket)
    case listener.handler.(client) do
      {:ok, pid} when is_pid(pid) ->
        :gen_tcp.controlling_process(client, pid)
      {:error, _} -> :nop
    end
    listener
  end

end


