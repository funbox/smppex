defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.TlvFormat
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  @type t :: %Pdu{
    command_id: integer,
    command_status: integer,
    sequence_number: integer,
    mandatory: map,
    optional: map
  }

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    mandatory: %{},
    optional: %{},
  ]

  @type header :: {integer, integer, integer}

  @spec new(header, map, map) :: t

  def new({command_id, command_status, sequence_number}, mandatory_fields, optional_fields) do
    %Pdu{
      command_id: command_id,
      command_status: command_status,
      sequence_number: sequence_number,
      mandatory: mandatory_fields,
      optional: optional_fields
    }
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

end
