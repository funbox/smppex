defmodule Support.TCP.ConnectionHandlerSpy do
  defstruct pid: nil

  alias Support.TCP.ConnectionHandlerSpy

  def start_link do
    {:ok, pid} = Agent.start_link(fn ->
      %{connection: nil, sent_data: [], received_data: [], closed: false, peer_closed: false, error_closed: nil}
    end)

    %ConnectionHandlerSpy{ pid: pid }
  end

  def get(spy) do
    Agent.get(spy.pid, fn data -> data end)
  end

  def stop(spy) do
    Agent.stop(spy.pid)
  end
end

defimpl SMPPEX.TCP.ConnectionHandler, for: Support.TCP.ConnectionHandlerSpy do
  def handle_connected(handler, connection) do
    Agent.update(handler.pid, fn data -> %{data | connection: connection} end)
    {:ok, handler}
  end

  def handle_data_received(handler, received_data) do
    Agent.update(handler.pid, fn data -> %{data | received_data: data.received_data ++ [received_data]} end)
    {:ok, handler}
  end

  def handle_data_sent(handler, sent_data) do
    Agent.update(handler.pid, fn data -> %{data | sent_data: data.sent_data ++ [sent_data]} end)
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


