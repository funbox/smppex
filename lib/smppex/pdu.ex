defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  defstruct [
    command_id: 0,
    command_name: nil,
    command_status: 0,
    sequence_number: 0,
    valid: false,
    mandatory: %{},
    optional: %{},
    body: <<>>,
    parse_error: nil
  ]

  def new(command_id, command_status, sequence_number) do
    case CommandNames.name_by_id(command_id) do
      {:ok, name} ->
        {:ok, %Pdu{
          command_id: command_id,
          command_name: name,
          command_status: command_status,
          sequence_number: sequence_number,
          valid: true
        }}
      :unknown ->
        {:unknown, %Pdu{
          command_id: command_id,
          command_status: command_status,
          sequence_number: sequence_number,
          valid: false
        }}
    end
  end

  def valid?(pdu) do
    pdu.valid
  end

  def command_id(pdu) do
    pdu.command_id
  end

  def command_name(pdu) do
    pdu.command_name
  end

  def command_status(pdu) do
    pdu.command_status
  end

  def sequence_number(pdu) do
    pdu.sequence_number
  end

  def body(pdu) do
    pdu.body
  end

  def error(pdu) do
    pdu.parse_error
  end

  def set_invalid(pdu, error) do
    %Pdu{ pdu | parse_error: error, valid: false }
  end

  def get_mandatory_field(pdu, name) when is_atom(name) do
    pdu.mandatory[name]
  end

  def get_optional_field(pdu, id) when is_integer(id) do
    pdu.optional[id]
  end

  def get_optional_field(pdu, name) when is_atom(name) do
    pdu.optional[name]
  end

  def get_field(pdu, id_or_name) do
    get_mandatory_field(pdu, id_or_name) || get_optional_field(pdu, id_or_name)
  end

  def set_body(pdu, body) do
    %Pdu{ pdu | body: body }
  end

  def set_mandatory_fields(pdu, fields) do
    %Pdu{ pdu | mandatory: fields }
  end

  def set_optional_fields(pdu, tlvs) do
    %Pdu{ pdu | optional: tlvs }
  end
end
