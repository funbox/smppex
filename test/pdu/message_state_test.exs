defmodule SMPPEX.Pdu.MessageStateTest do
  alias SMPPEX.Pdu.MessageState

  use ExUnit.Case

  doctest MessageState

  test "code_by_name" do
    assert_raise FunctionClauseError, fn() ->
      MessageState.code_by_name(:NON_EXISTING_MESSAGE_STATE)
    end
  end

  test "format" do
    assert_raise FunctionClauseError, fn() ->
      MessageState.format(:not_an_integer)
    end
  end
end

