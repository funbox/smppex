defmodule Support.ClientPool do

  alias :gen_tcp, as: GenTCP

  alias Support.SMPPSession
  alias SMPPEX.ClientPool

  def create do
    {:ok, pid} = Agent.start_link(fn() -> %{sessions: [], client_pool: nil} end)

    handler = fn(_ref, _socket, _transport, protocol) ->
      session = SMPPSession.create(protocol)
      Agent.update(pid, fn(pool_data) -> %{pool_data | sessions: [session | pool_data.sessions]} end)
      {:ok, session}
    end

    client_pool = SMPPEX.ClientPool.start(handler)
    Agent.update(pid, fn(pool_data) -> %{pool_data | client_pool: client_pool} end)

    pid
  end

  def stop(pid) do
    client_pool = Agent.get(pid, fn(data) -> data.client_pool end)
    for session <- sessions(pid), do: SMPPSession.stop(session)
    Agent.stop(pid)
    ClientPool.stop(client_pool)
  end

  def connect(pid, host, port) do
    client_pool = Agent.get(pid, fn(data) -> data.client_pool end)
    {:ok, socket} = GenTCP.connect(host, port, [:binary, {:packet, 0}])
    ClientPool.start_session(client_pool, socket)
  end

  def sessions(pid) do
    Agent.get(pid, fn(data) -> data.sessions end)
  end
end

