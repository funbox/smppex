defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.TlvFormat
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  @type t :: %Pdu{}

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    ref: nil,
    mandatory: %{},
    optional: %{},
  ]

  @type header :: {integer, integer, integer} | integer

  @spec new(header, map, map) :: t

  def new(header, m_fields \\ %{}, opt_fields \\ %{}) do
    case header do
      c_id when is_integer(c_id) ->
        %Pdu{
          command_id: c_id,
          ref: make_ref,
          mandatory: m_fields,
          optional: opt_fields
        }
      {c_id, c_status, s_number} ->
        %Pdu{
          command_id: c_id,
          command_status: c_status,
          sequence_number: s_number,
          ref: make_ref,
          mandatory: m_fields,
          optional: opt_fields
        }
    end
  end

  @spec command_id(t) :: integer

  def command_id(pdu) do
    pdu.command_id
  end

  @spec command_status(t) :: integer

  def command_status(pdu) do
    pdu.command_status
  end

  @spec sequence_number(t) :: integer

  def sequence_number(pdu) do
    pdu.sequence_number
  end

  @spec mandatory_field(t, integer | atom) :: any

  def mandatory_field(pdu, name) when is_atom(name) do
    pdu.mandatory[name]
  end

  def mandatory_field(pdu, id) when is_integer(id) do
    case CommandNames.name_by_id(id) do
      {:ok, name} -> pdu.mandatory[name]
      :unknown -> nil
    end
  end

  @spec optional_field(t, integer | atom) :: any

  def optional_field(pdu, id) when is_integer(id) do
    pdu.optional[id]
  end

  def optional_field(pdu, name) when is_atom(name) do
    case TlvFormat.id_by_name(name) do
      {:ok, id} -> optional_field(pdu, id)
      :unknown -> nil
    end
  end

  @spec field(t, integer | atom) :: any

  def field(pdu, id_or_name) do
    mandatory_field(pdu, id_or_name) || optional_field(pdu, id_or_name)
  end

  @spec optional_fields(t) :: map

  def optional_fields(pdu), do: pdu.optional

  @spec mandatory_fields(t) :: map

  def mandatory_fields(pdu), do: pdu.mandatory

  @spec same?(t, t) :: boolean

  def same?(pdu1, pdu2), do: pdu1.ref != nil and pdu2.ref != nil and pdu1.ref == pdu2.ref

end
