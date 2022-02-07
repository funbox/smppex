defmodule SMPPEX.PduStorage do
  @moduledoc false

  alias :ets, as: ETS

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  defstruct [
    :by_sequence_number
  ]

  @type t :: %PduStorage{}

  @spec new :: %PduStorage{}

  def new do
    %PduStorage{
      by_sequence_number: ETS.new(:pdu_storage_by_sequence_number, [:set])
    }
  end

  @spec store(t, Pdu.t(), non_neg_integer) :: boolean

  def store(storage, %Pdu{} = pdu, expire_time) do
    sequence_number = Pdu.sequence_number(pdu)
    ETS.insert_new(storage.by_sequence_number, {sequence_number, {expire_time, pdu}})
  end

  @spec fetch(t, non_neg_integer) :: [Pdu.t()]

  def fetch(storage, sequence_number) do
    case ETS.take(storage.by_sequence_number, sequence_number) do
      [{^sequence_number, {_expire_time, pdu}}] ->
        [pdu]

      [] ->
        []
    end
  end

  @spec fetch_expired(t, non_neg_integer) :: [Pdu.t()]

  def fetch_expired(storage, expire_time) do
    expired =
      ETS.select(storage.by_sequence_number, [
        {{:_, {:"$1", :"$2"}}, [{:<, :"$1", expire_time}], [:"$2"]}
      ])

    expired_count = length(expired)

    ^expired_count =
      ETS.select_delete(storage.by_sequence_number, [
        {{:_, {:"$1", :"$2"}}, [{:<, :"$1", expire_time}], [true]}
      ])

    expired
  end

  @spec fetch_all(t) :: [Pdu.t()]

  def fetch_all(storage) do
    pdus = for {_sn, {_ex, pdu}} <- ETS.tab2list(storage.by_sequence_number), do: pdu
    ETS.delete_all_objects(storage.by_sequence_number)
    pdus
  end
end
