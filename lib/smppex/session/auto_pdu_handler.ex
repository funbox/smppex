defmodule SMPPEX.Session.AutoPduHandler do
  @moduledoc false

  defstruct [
    :by_sequence_number,
    :by_ref
  ]

  alias __MODULE__, as: AutoPduHandler
  alias :ets, as: ETS

  alias SMPPEX.Pdu.Factory, as: PduFactory
  alias SMPPEX.Pdu
  alias SMPPEX.Compat

  @type t :: %AutoPduHandler{}

  def new do
    %AutoPduHandler{
      by_sequence_number: ETS.new(:by_sequence_number, [:set]),
      by_ref: ETS.new(:by_ref, [:set])
    }
  end

  @spec enquire_link(t, non_neg_integer, non_neg_integer) :: {Pdu.t(), non_neg_integer}

  def enquire_link(handler, expire_time, sequence_number) do
    # increment before use
    sequence_number = sequence_number + 1
    pdu = %Pdu{PduFactory.enquire_link() | sequence_number: sequence_number}
    ETS.insert_new(handler.by_ref, {Pdu.ref(pdu), true})
    ETS.insert_new(handler.by_sequence_number, {sequence_number, {expire_time, pdu}})
    {pdu, sequence_number}
  end

  @spec handle_send_pdu_result(t, Pdu.t()) :: :proceed | :skip

  def handle_send_pdu_result(handler, pdu) do
    case Compat.ets_take(handler.by_ref, Pdu.ref(pdu)) do
      [_] -> :skip
      [] -> :proceed
    end
  end

  @spec handle_pdu(t, Pdu.t(), non_neg_integer) :: :proceed | {:skip, [Pdu.t()], non_neg_integer}

  def handle_pdu(handler, pdu, sequence_number) do
    cond do
      Pdu.resp?(pdu) ->
        handle_resp(handler, pdu, sequence_number)

      Pdu.command_name(pdu) == :enquire_link ->
        handle_enquire_link(handler, pdu, sequence_number)

      true ->
        :proceed
    end
  end

  @spec drop_expired(t, non_neg_integer) :: non_neg_integer

  def drop_expired(handler, now_time) do
    ETS.select_delete(handler.by_sequence_number, [
      {{:_, {:"$1", :"$2"}}, [{:<, :"$1", now_time}], [true]}
    ])
  end

  defp handle_resp(handler, pdu, sequence_number) do
    case Compat.ets_take(handler.by_sequence_number, Pdu.sequence_number(pdu)) do
      [_] -> {:skip, [], sequence_number}
      [] -> :proceed
    end
  end

  defp handle_enquire_link(handler, pdu, sequence_number) do
    resp = PduFactory.enquire_link_resp() |> Pdu.as_reply_to(pdu)

    ETS.insert_new(handler.by_ref, {Pdu.ref(resp), true})

    {:skip, [resp], sequence_number}
  end
end
