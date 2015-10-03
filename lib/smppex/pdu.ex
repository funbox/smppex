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
end
