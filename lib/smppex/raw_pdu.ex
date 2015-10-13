defmodule SMPPEX.RawPdu do

  alias SMPPEX.RawPdu

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    body: ""
  ]

  def new({command_id, command_status, sequence_number}, body) do
    %RawPdu{
      command_id: command_id,
      command_status: command_status,
      sequence_number: sequence_number,
      body: body
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

  def body(pdu) do
    pdu.body
  end

end
