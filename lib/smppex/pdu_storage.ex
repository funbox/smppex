defmodule SMPPEX.PduStorage do
  @moduledoc false

  use GenServer

  alias :ets, as: ETS

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  defstruct [
    :by_sequence_number
  ]

  @type t :: %PduStorage{}

  @spec start_link :: GenServer.on_start

  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  @spec store(pid, Pdu.t, non_neg_integer) :: boolean

  def store(storage, %Pdu{} = pdu, expire_time) do
    GenServer.call(storage, {:store, pdu, expire_time})
  end

  @spec fetch(pid, non_neg_integer) :: [Pdu.t]

  def fetch(storage, sequence_number) do
    GenServer.call(storage, {:fetch, sequence_number})
  end

  @spec fetch_all(pid) :: [Pdu.t]

  def fetch_all(storage) do
    GenServer.call(storage, :fetch_all)
  end

  @spec fetch_expired(pid, non_neg_integer) :: [Pdu.t]

  def fetch_expired(storage, expire_time) do
    GenServer.call(storage, {:fetch_expired, expire_time})
  end

  @spec stop(pid) :: :ok

  def stop(storage) do
    GenServer.call(storage, :stop)
  end

  def init([]) do
    {:ok, %PduStorage{
      by_sequence_number: ETS.new(:pdu_storage_by_sequence_number, [:set])
    }}
  end

  def handle_call({:store, pdu, expire_time}, _from, st) do
    sequence_number = Pdu.sequence_number(pdu)
    result = ETS.insert_new(st.by_sequence_number, {sequence_number, {expire_time, pdu}})
    {:reply, result, st}
  end

  def handle_call({:fetch, sequence_number}, _from, st) do
    case ETS.take(st.by_sequence_number, sequence_number) do
      [{^sequence_number, {_expire_time, pdu}}] ->
        {:reply, [pdu], st}
      [] ->
        {:reply, [], st}
    end
  end

  def handle_call(:fetch_all, _from, st) do
    pdus = for {_sn, {_ex, pdu}} <- ETS.tab2list(st.by_sequence_number), do: pdu
    {:reply, pdus, st}
  end

  def handle_call({:fetch_expired, expire_time}, _from, st) do
    expired = ETS.select(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [:'$2']}])
    expired_count = length(expired)
    ^expired_count = ETS.select_delete(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [true]}])
    {:reply, expired, st}
  end

  def handle_call(:stop, _from, st) do
    {:stop, :normal, :ok, st}
  end
end
