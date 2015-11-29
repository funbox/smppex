defmodule SMPPEX.RawPdu do

  alias SMPPEX.RawPdu

  @type t :: %RawPdu{
    command_id: integer,
    command_status: integer,
    sequence_number: integer,
    body: binary
  }

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    body: ""
  ]

  @type header :: {integer, integer, integer}

  @spec new(header, binary) :: t

  def new({command_id, command_status, sequence_number}, body) do
    %RawPdu{
      command_id: command_id,
      command_status: command_status,
      sequence_number: sequence_number,
      body: body
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

  @spec header(t) :: header

  def header(pdu) do
    {pdu.command_id, pdu.command_status, pdu.sequence_number}
  end

  @spec body(t) :: binary

  def body(pdu) do
    pdu.body
  end

end
