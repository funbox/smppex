defmodule SMPPEX.Pdu do
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  defstruct [
    command_id: 0,
    command_name: nil,
    command_status: 0,
    sequence_number: 0,
    valid: true,
    mandatory: %{},
    optional: %{},
    body: <<>>
  ]

  def new(command_id, command_status, sequence_number) do
    case CommandNames.name_by_id(command_id) do
      {:ok, name} ->
        {:ok, %Pdu{
          command_id: command_id,
          command_name: name,
          command_status: command_status,
          sequence_number: sequence_number
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

  def get_field(pdu, name) do
    pdu.mandatory[name]
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
