defmodule SMPPEX.TCP.Server.SupervisorTest do

  use ExUnit.Case

  alias SMPPEX.TCP.Connection
  alias Support.TCP.Helpers, as: TCPHelpers
  alias Support.TCP.ConnectionHandlerSpy

  def connect_server(port, fun) do
    :timer.sleep(10)
    {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, :raw}, {:active, false}])
    :timer.sleep(10)
    fun.(socket)
    :timer.sleep(10)
    :ok = :gen_tcp.close(socket)
    :timer.sleep(10)
  end

  def trap_exit(fun) do
    Process.flag(:trap_exit, true)
    fun.()
    :timer.sleep(10)
    Process.flag(:trap_exit, false)
  end

  def with_supervised_server(handler, fun) do
    port = TCPHelpers.find_free_port
    SMPPEX.TCP.Server.Supervisor.start_link(:test_srv, {port, []}, handler)

    fun.(port)

    SMPPEX.TCP.Server.Supervisor.stop(:test_srv)
  end

  test "send && handle_data_sent callback" do
    trap_exit fn ->

      handler = ConnectionHandlerSpy.start_link

      with_supervised_server handler, fn(port) ->
        connect_server(port, fn(socket) ->
          data = ConnectionHandlerSpy.get(handler)
          connection = data.connection

          spawn fn ->
            Connection.send(connection, "foo")
            Connection.send(connection, "bar")
          end
          {:ok, "foobar"} = :gen_tcp.recv(socket, 6)

        end)
      end

      spied_data = ConnectionHandlerSpy.get(handler)
      ConnectionHandlerSpy.stop(handler)

      sent_data = spied_data.sent_data |> List.flatten |> Enum.join
      assert sent_data == "foobar"

    end

  end

  test "server restart" do
    trap_exit fn ->

      handler = ConnectionHandlerSpy.start_link
      with_supervised_server handler, fn(port) ->

        server_name = SMPPEX.TCP.Server.Supervisor.server_name(:test_srv)
        pid = Process.whereis(server_name)
        Process.exit(pid, :some_reason)

        :timer.sleep(100)

        {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, :raw}, {:active, false}])
        :ok = :gen_tcp.close(socket)

      end
      ConnectionHandlerSpy.stop(handler)

    end
  end

  test "server shutdown" do
    trap_exit fn ->

      handler = ConnectionHandlerSpy.start_link
      with_supervised_server handler, fn(port) ->

        SMPPEX.TCP.Server.Supervisor.stop(:test_srv)

        :timer.sleep(10)

        {:error, :econnrefused} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, {:packet, :raw}, {:active, false}])
      end
      ConnectionHandlerSpy.stop(handler)

    end
  end

end
