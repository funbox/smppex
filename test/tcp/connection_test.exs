defmodule SMPPEX.TCP.ConnectionTest do

  use ExUnit.Case

  alias SMPPEX.TCP.Server
  alias SMPPEX.TCP.Listener
  alias SMPPEX.TCP.Connection

  alias Support.TCP.Helpers, as: TCPHelpers
  alias Support.GenServerHelpers
  alias Support.TCP.ConnectionHandlerSpy

  def start_server(port, handler) do
    client_handler = fn(client) ->
      Connection.start_link(client, handler)
    end
    listener = Listener.new({port, []}, client_handler)
    {:ok, server} = Server.start_link(listener)
    server
  end

  def connect_server(port, fun) do
    {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, :raw}, {:active, false}])
    :timer.sleep(10)
    fun.(socket)
    :timer.sleep(10)
    :ok = :gen_tcp.close(socket)
    :timer.sleep(10)
  end

  def with_server_and_connected_client(fun) do
    handler = ConnectionHandlerSpy.start_link
    port = TCPHelpers.find_free_port
    server = start_server(port, handler)

    connect_server(port, fn(socket) ->
      data = ConnectionHandlerSpy.get(handler)
      fun.({data.connection, socket})
    end)

    GenServerHelpers.stop_gen_server(server)
    spied_data = ConnectionHandlerSpy.get(handler)
    ConnectionHandlerSpy.stop(handler)
    spied_data
  end

  test "start_link && handle_connected callback" do
    spied_data = with_server_and_connected_client(fn _ -> :ok end)
    assert spied_data.connection != nil
    assert is_pid(spied_data.connection)
  end

  test "handle_data_received callback" do
    spied_data = with_server_and_connected_client fn {_server_connection, client_socket} ->
      :gen_tcp.send(client_socket, "foo")
      :gen_tcp.send(client_socket, "bar")
    end
    received_data = spied_data.received_data |> List.flatten |> Enum.join
    assert received_data == "foobar"
  end

  test "handle_peer_closed callback" do
    spied_data = with_server_and_connected_client fn _ -> :ok end
    assert spied_data.peer_closed
  end

  test "send && handle_data_sent callback" do
    spied_data = with_server_and_connected_client fn {server_connection, client_socket} ->
      spawn fn ->
        Connection.send(server_connection, "foo")
        Connection.send(server_connection, "bar")
      end
      {:ok, "foobar"} = :gen_tcp.recv(client_socket, 6)
    end

    sent_data = spied_data.sent_data |> List.flatten |> Enum.join
    assert sent_data == "foobar"
  end

  test "close && handle_closed callback" do
    spied_data = with_server_and_connected_client fn {server_connection, _client_socket} ->
      spawn fn ->
        Connection.close(server_connection)
      end
    end

    assert spied_data.closed
  end
end

