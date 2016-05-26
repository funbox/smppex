defmodule SMPPEX.SessionTest do
  use ExUnit.Case

  defmodule TestPool do
    def create do
      {:ok, pid} = Agent.start_link(fn() -> %{sessions: []} end)

      handler = fn(_ref, _socket, _transport, _protocol) ->
        session = TestSession.create
        Agent.update(pid, fn(pool_data) -> %{pool_data | sessions: [session | pool_data.sessions]} end)
        {:ok, session}
      end

      client_pool = ClientPool.start(handler)
      Agent.update(pid, fn(pool_data) -> %{pool_data | client_pool: client_pool} end)

      pid
    end

    def stop(pid) do
      {:ok, client_pool} = Agent.get(pid, fn(data) -> data.client_pool end)
      for session <- sessions(pid), do: TestSession.stop(session)
      Agent.stop(pid)
      ClientPool.stop(client_pool)
    end

    def connect(pid, host, port) do
      {:ok, client_pool} = Agent.get(pid, fn(data) -> data.client_pool end)
      {:ok, socket} = :gen_tcp.connect(host, port, [:binary, {:packet, 0}])
      ClientPool.start_session(client_pool, socket)
    end

    def sessions(pid) do
      {:ok, sessions} = Agent.get(pid, fn(data) -> data.sessions end)
      sessions
    end
  end

  defmodule TestSession do
    defstruct [
      :spied_data_pid
    ]

    def create do
      {:ok, spied_data_pid} = Agent.start_link(fn() -> %{calls: []} end)
      %TestSession{spied_data_pid: spied_data_pid}
    end

    def save_callback(session, name, args) do
      Agent.update(session.spied_data_pid, fn(data) ->
        %{data |
          callbacks_received: [{name, args} | data.calls]
        }
      end)
    end

    def stop(session) do
      Agent.stop(session.spied_data_pid)
    end
  end

  defimpl SMPPEX.SMPPHandler, for: TestSession do

    def after_init(session) do
      TestSession.save_callback(session, :after_init, [])
    end

    def handle_parse_error(session, error) do
      TestSession.save_callback(session, :after_init, [])
    end

    def handle_pdu(session, {:unparsed_pdu, raw_pdu, error}) do
      TestSession.save_callback(session, :handle_pdu, [{:unparsed_pdu, raw_pdu, error}])
    end

    def handle_pdu(session, {:pdu, pdu}) do
      TestSession.save_callback(session, :handle_pdu, [{:pdu, pdu}])
      :ok
    end

    def handle_socket_closed(session) do
      TestSession.save_callback(session, :handle_socket_closed, [])
    end

    def handle_socket_error(session, reason) do
      TestSession.save_callback(session, :handle_socket_error, [])
    end

    def handle_stop(session) do
      TestSession.save_callback(session, :handle_stop, [])
    end

    def handle_send_pdu_result(session, pdu, send_pdu_result) do
      TestSession.save_callback(session, :handle_send_pdu_result, [pdu, send_pdu_result])
      session
    end
  end

end
