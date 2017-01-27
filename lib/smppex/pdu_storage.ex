defmodule SMPPEX.PduStorage do
  @moduledoc false

  use GenServer

  alias :ets, as: ETS

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  @default_next_sequence_number 1

  defstruct [
    :by_sequence_number,
    :next_sequence_number
  ]

  @type t :: %PduStorage{}

  @spec start_link :: GenServer.on_start

  def start_link(next_sequence_number \\ @default_next_sequence_number) do
    GenServer.start_link(__MODULE__, [next_sequence_number])
  end

  @spec store(pid, Pdu.t, non_neg_integer) :: boolean

  def store(pid, %Pdu{} = pdu, expire_time) do
    GenServer.call(pid, {:store, pdu, expire_time})
  end

  @spec fetch(pid, non_neg_integer) :: [Pdu.t]

  def fetch(pid, sequence_number) do
    GenServer.call(pid, {:fetch, sequence_number})
  end

  @spec fetch_expired(pid, non_neg_integer) :: [Pdu.t]

  def fetch_expired(pid, expire_time) do
    GenServer.call(pid, {:fetch_expired, expire_time})
  end

  @spec reserve_sequence_number(pid) :: :pos_integer

  @doc """
  Reserve a sequence number by getting current next sequence number and then incrementing.
  Useful if you need to track sequence numbers externally.
  """

  def reserve_sequence_number(pid) do
    GenServer.call(pid, :reserve_sequence_number)
  end

  def init([next_sequence_number]) do
    {:ok, %PduStorage{
      by_sequence_number: ETS.new(:pdu_storage_by_sequence_number, [:set]),
      next_sequence_number: next_sequence_number
    }}
  end

  def handle_cast(:reset_sequence_number, st) do
    st = %PduStorage{st | next_sequence_number: @default_next_sequence_number}
    {:noreply, st}
  end

  def handle_call({:store, pdu, expire_time}, _from, st) do
    sequence_number = Pdu.sequence_number(pdu)
    result = ETS.insert_new(st.by_sequence_number, {sequence_number, {expire_time, pdu}})
    {:reply, result, st}
  end

  def handle_call({:fetch, sequence_number}, _from, st) do
    case ETS.lookup(st.by_sequence_number, sequence_number) do
      [{^sequence_number, {_expire_time, pdu}}] ->
        true = ETS.delete(st.by_sequence_number, sequence_number)
        {:reply, [pdu], st}
      [] ->
        {:reply, [], st}
    end
  end

  def handle_call({:fetch_expired, expire_time}, _from, st) do
    expired = ETS.select(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [:'$2']}])
    expired_count = length(expired)
    ^expired_count = ETS.select_delete(st.by_sequence_number, [{ {:'_', {:'$1', :'$2'}}, [{:'<', :'$1', expire_time}], [true]}])
    {:reply, expired, st}
  end

  def handle_call(:reserve_sequence_number, _from, st) do
    {sequence_number, new_st} = increment_sequence_number(st)
    {:reply, sequence_number, new_st}
  end

  defp increment_sequence_number(st) do
    {st.next_sequence_number, %PduStorage{st | next_sequence_number: st.next_sequence_number + 1}}
  end

end
