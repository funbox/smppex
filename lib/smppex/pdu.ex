defmodule SMPPEX.Pdu do
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
end
