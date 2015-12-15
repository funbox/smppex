defprotocol SMPPEX.TCP.ConnectionHandler do
  @spec handle_connected(h, pid) :: h when h: var
  def handle_connected(handler, connection)

  @spec handle_data_received(h, iodata) :: h when h: var
  def handle_data_received(handler, data)

  @spec handle_data_sent(h, iodata) :: h when h: var
  def handle_data_sent(handler, data)

  @spec handle_peer_closed(any) :: any
  def handle_peer_closed(handler)

  @spec handle_closed(any) :: any
  def handle_closed(handler)

  @spec handle_error_closed(any, any) :: any
  def handle_error_closed(handler, error)
end

defmodule SMPPEX.TCP.Connection do
  defstruct [:socket, :handler]

  alias SMPPEX.TCP.Connection
  alias SMPPEX.TCP.ConnectionHandler

  use GenServer

  @spec start_link(port, any, list) :: GenServer.on_start

  def start_link(socket, handler, opts \\ []) do
    GenServer.start_link(__MODULE__, [socket, handler], opts)
  end

  @spec send(pid, iodata) :: any

  def send(connection, data), do: GenServer.call(connection, {:send, data})

  @spec close(pid) :: any

  def close(connection), do: GenServer.call(connection, :close)

  # GenServer

  def init([socket, handler]) do
    prepare_for_recv(socket)
    case ConnectionHandler.handle_connected(handler, self) do
      {:ok, new_handler} -> {:ok, %Connection{socket: socket, handler: new_handler}}
      {:error, _} = err -> err
    end
  end

  def handle_call({:send, data}, _f, st) do
    case send_data(data, st) do
      {:ok, new_st} -> {:reply, :ok, new_st}
      {:error, error} ->
        ConnectionHandler.handle_error_closed(st.handler, error)
        {:stop, :normal, :error, st}
    end
  end

  def handle_call(:close, _f, st) do
    ConnectionHandler.handle_closed(st.handler)
    {:stop, :normal, :ok, st}
  end

  def handle_info({:tcp, _s, data}, st) do
    case handle_data(data, st) do
      {:ok, new_st} ->
        prepare_for_recv(st.socket)
        {:noreply, new_st}
      {:error, error} ->
        ConnectionHandler.handle_error_closed(st.handler, error)
        {:stop, :normal, st}
    end
  end

  def handle_info({:tcp_closed, _s}, st) do
    ConnectionHandler.handle_peer_closed(st.handler)
    {:stop, :normal, st}
  end

  def handle_info({:tcp_error, _s, error}, st) do
    ConnectionHandler.handle_error_closed(st.handler, error)
    {:stop, :normal, st}
  end

  # Private

  defp prepare_for_recv(socket) do
    :ok = :inet.setopts(socket, [{:active, :once}])
  end

  defp send_data(data, st) do
    case :gen_tcp.send(st.socket, data) do
      {:error, _} = err -> err
      :ok -> case ConnectionHandler.handle_data_sent(st.handler, data) do
        {:ok, handler} -> {:ok, %Connection{st | handler: handler}}
        {:error, _} = err -> err
      end
    end
  end

  defp handle_data(data, st) do
    case ConnectionHandler.handle_data_received(st.handler, data) do
      {:ok, handler} -> {:ok, %Connection{st | handler: handler}}
      {:error, _} = err -> err
    end
  end


end
