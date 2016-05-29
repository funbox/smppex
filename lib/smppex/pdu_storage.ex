defmodule SMPPEX.PduStorage do
  use GenServer

  defstruct [
    :by_sequence_number
  ]

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  def store(storage, %Pdu{} = pdu, expire_time) do
    GenServer.call(storage, {:store, pdu, expire_time})
  end

  def fetch(storage, sequence_number) do
    GenServer.call(storage, {:fetch, sequence_number})
  end

  def fetch_expired(storage, expire_time) do
    GenServer.call(storage, {:fetch_expired, expire_time})
  end

  def init([]) do
    {:ok, %PduStorage{
      by_sequence_number: :ets.new(:pdu_storage_by_sequence_number, [:set])
    }}
  end

  def handle_call({:store, pdu, expire_time}, _from, st) do
    sequence_number = Pdu.sequence_number(pdu)
    result = :ets.insert_new(st.by_sequence_number, {sequence_number, {expire_time, pdu}})
    {:reply, result, st}
  end

  def handle_call({:fetch, sequence_number}, _from, st) do
    case :ets.lookup(st.by_sequence_number, sequence_number) do
      [{^sequence_number, {_expire_time, pdu}}] ->
        true = :ets.delete(st.by_sequence_number, sequence_number)
        {:reply, [pdu], st}
      [] ->
        {:reply, [], st}
    end
  end

  def handle_call({:fetch_expired, expire_time}, _from, st) do
    expired = :ets.select(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [:'$2'] }])
    expired_count = length(expired)
    ^expired_count = :ets.select_delete(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [true] }])
    {:reply, expired, st}
  end

end

