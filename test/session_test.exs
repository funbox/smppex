defmodule SMPPEX.SessionTest do
  use ExUnit.Case

  defmodule TestSession do
  end

  defimpl SMPPEX.SMPPHandler, for: TestSession do
  end
end
