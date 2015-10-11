defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.TlvFormat
  alias SMPPEX.Pdu

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

  def get_mandatory_field(pdu, name) when is_atom(name) do
    pdu.mandatory[name]
  end

  def get_optional_field(pdu, id) when is_integer(id) do
    pdu.optional[id]
  end

  def get_optional_field(pdu, name) when is_atom(name) do
    case TlvFormat.id_by_name(name) do
      {:ok, id} -> get_optional_field(pdu, id)
      :unknown -> nil
    end
  end

  def get_field(pdu, id_or_name) do
    get_mandatory_field(pdu, id_or_name) || get_optional_field(pdu, id_or_name)
  end
end
