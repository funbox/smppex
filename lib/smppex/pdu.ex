defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.TlvFormat
  alias SMPPEX.Pdu

  @type t :: %Pdu{
    command_id: integer,
    command_status: integer,
    sequence_number: integer,
    mandatory: map(),
    optional: map()
  }

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    mandatory: %{},
    optional: %{},
  ]

  def new({command_id, command_status, sequence_number}, mandatory_fields, optional_fields) do
    %Pdu{
      command_id: command_id,
      command_status: command_status,
      sequence_number: sequence_number,
      mandatory: mandatory_fields,
      optional: optional_fields
    }
  end

  def command_id(pdu) do
    pdu.command_id
  end

  def command_status(pdu) do
    pdu.command_status
  end

  def sequence_number(pdu) do
    pdu.sequence_number
  end

  def mandatory_field(pdu, name) when is_atom(name) do
    pdu.mandatory[name]
  end

  def optional_field(pdu, id) when is_integer(id) do
    pdu.optional[id]
  end

  def optional_field(pdu, name) when is_atom(name) do
    case TlvFormat.id_by_name(name) do
      {:ok, id} -> optional_field(pdu, id)
      :unknown -> nil
    end
  end

  def field(pdu, id_or_name) do
    mandatory_field(pdu, id_or_name) || optional_field(pdu, id_or_name)
  end

  def optional_fields(pdu), do: pdu.optional

  def mandatory_fields(pdu), do: pdu.mandatory

end
