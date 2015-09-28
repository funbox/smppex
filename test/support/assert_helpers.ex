defmodule AssertHelpers do
  defmacro assert_error(expr) do
    quote bind_quoted: [expr: expr] do
      assert match? {:error, _}, expr
    end
  end

  defmacro __using__(_opts) do
    quote do
      use ExUnit.Case
      import AssertHelpers
    end
  end
end
