defmodule SMPPEX.Pdu.MessageState do

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
  @spec format(state) :: String.t

  Enum.each statuses, fn({name, code}) ->
    def code_by_name(unquote(name)), do: unquote(code)

    def format(unquote(code)), do: unquote(to_string(name))
  end

  def format(code) when is_integer(code), do: to_string(code)

end
