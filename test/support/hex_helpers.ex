defmodule HexHelpers do
  def from_hex(hex_string) do
    hex = String.replace(hex_string, ~r/\s+/, "")
    Hexate.decode(hex)
  end

  defmacro __using__(_opts) do
    quote do
      import HexHelpers
    end
  end
end

