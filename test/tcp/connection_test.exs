defmodule SMPPEX.TCP.ConnectionTest do

  use ExUnit.Case

  alias SMPPEX.TCP.Server
  alias SMPPEX.TCP.Listener
  alias SMPPEX.TCP.Connection

  defmodule ConnectionHandlerSpy do
    defstruct pid: nil

    def start_link do
      {:ok, pid} = Agent.start_link(fn ->
        %{socket: nil, sent_data: [], received_data: [], closed: false, peer_closed: false, error_closed: nil}
      end)

      %ConnectionHandlerSpy{ pid: pid }
    end

    def get_data(spy) do
      Agent.get(spy.pid, fn data -> data end)
    end

    def stop(spy) do
      Agent.stop(spy.pid)
    end
  end

  defimpl SMPPEX.TCP.ConnectionHandler, for: ConnectionHandlerSpy do
    def handle_connected(handler, socket) do
      Agent.update(handler.pid, fn data -> %{data | socket: socket} end)
      {:ok, handler}
    end

    def handle_data_received(handler, received_data) do
      Agent.update(handler.pid, fn data -> %{data | received_data: data.received_data ++ [received_data]} end)
      {:ok, handler}
    end

    def handle_data_sent(handler, sent_data) do
      Agent.update(handler.pid, fn data -> %{data | received_data: data.sent_data ++ [sent_data]} end)
      {:ok, handler}
    end

    def handle_peer_closed(handler) do
      Agent.update(handler.pid, fn data -> %{data | peer_closed: true} end)
    end

    def handle_closed(handler) do
      Agent.update(handler.pid, fn data -> %{data | closed: true} end)
    end

    def handle_error_closed(handler, error) do
      Agent.update(handler.pid, fn data -> %{data | error_closed: error} end)
    end
  end

  def start_server(port, handler) do
    client_handler = fn(client) ->
      Connection.start_link(client, handler)
    end
    listener = Listener.new({port, []}, client_handler)
    {:ok, server} = Server.start_link(listener)
    server
  end


  test "handle_connected" do

  end

end

