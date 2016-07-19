defmodule SMPPEX.Pdu.MessageStateTest do
  alias SMPPEX.Pdu.MessageState

  use ExUnit.Case

  test "code_by_name" do
    assert MessageState.code_by_name(:DELIVERED) == 2

    assert_raise FunctionClauseError, fn() ->
      MessageState.code_by_name(:NON_EXISTING_MESSAGE_STATE)
    end
  end

  test "format" do
    assert MessageState.format(2) == "DELIVERED"

    assert MessageState.format(123) == "123"

    assert_raise FunctionClauseError, fn() ->
      MessageState.format(:not_an_integer)
    end
  end
end

