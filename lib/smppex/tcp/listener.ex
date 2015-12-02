defmodule SMPPEX.TCP.Listener do

  alias SMPPEX.TCP.Listener

  defstruct socket: nil, port: 0, tcp_opts: [], handler: nil

  @type handler_result :: {:ok, pid} | {:error, any}
  @type handler :: (port -> handler_result)

  @type t :: %Listener{
    socket: port,
    port: integer,
    tcp_opts: list,
    handler: handler
  }

  @reuquired_tcp_opts [:binary, {:packet, :raw}, {:active, false}, {:reuseaddr, true}]

  @type listen_opts :: {integer, list}

  @spec new(listen_opts, handler) :: t

  def new({port, opts}, handler) when is_integer(port) and is_list(opts) and is_function(handler) do
    tcp_opts = add_required_tcp_options(opts)
    %Listener{port: port, tcp_opts: tcp_opts, handler: handler}
  end

  defp add_required_tcp_options(opts) do
    @reuquired_tcp_opts ++ opts
  end

  @spec init(t) :: t

  def init(listener) do
    {:ok, socket} = :gen_tcp.listen(listener.port, listener.tcp_opts)
    %Listener{ listener | socket: socket }
  end

  @spec accept(t) :: t

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


