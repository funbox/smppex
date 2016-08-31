defmodule SMPPEX.Pdu.MessageState do
  @moduledoc """
  Module for operating with `deliver_sm` message states.
  """

  statuses = [
    {:ENROUTE, 1},
    {:DELIVERED, 2},
    {:EXPIRED, 3},
    {:DELETED, 4},
    {:UNDELIVERABLE, 5},
    {:ACCEPTED, 6},
    {:UNKNOWN, 7},
    {:REJECTED, 8}
  ]

  @type state :: integer
  @type state_name :: atom

  @spec code_by_name(state_name) :: state

  @doc """
  Converts atom representing message state to integer value.

  ## Example

      iex(1)> SMPPEX.Pdu.MessageState.code_by_name(:DELIVERED)
      2

  """
  def code_by_name(state_name)

  @spec format(state) :: String.t

  @doc """
  Converts integer message state value to string representation.

  ## Example

      iex(1)> SMPPEX.Pdu.MessageState.format(2)
      "DELIVERED"
      iex(2)> SMPPEX.Pdu.MessageState.format(12345)
      "12345"

  """
  def format(state)

  Enum.each statuses, fn({name, code}) ->
    def code_by_name(unquote(name)), do: unquote(code)

    def format(unquote(code)), do: unquote(to_string(name))
  end

  def format(code) when is_integer(code), do: to_string(code)

end
