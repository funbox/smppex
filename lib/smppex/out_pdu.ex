defmodule SMPPEX.OutPdu do
  alias SMPPEX.OutPdu

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    mandatory: %{},
    optional: %{}
  ]
end
