defprotocol SMPPEX.TCP.ConnectionHandler do
  def handle_connected(handler, socket)
  def handle_data_received(handler, data)
  def handle_data_sent(handler, data)
  def handle_peer_closed(handler)
  def handle_closed(handler)
  def handle_error_closed(handler, error)
end

defmodule SMPPEX.TCP.Connection do
  defstruct [:socket, :handler]

  alias SMPPEX.TCP.Connection

  use GenServer

  def start_link(socket, handler, opts \\ []) do
    GenServer.start_link(__MODULE__, [socket, handler], opts)
  end

  def send(connection, data), do: GenServer.call(connection, {:send, data})
  def close(connection), do: GenServer.call(connection, :close)

  # GenServer

  def init([socket, handler]) do
    prepare_for_recv(socket)
    case ConnectionHandler.handle_connected(handler, socket) do
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
