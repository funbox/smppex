defmodule SMPPEX.Pdu.ErrorsTest do
  alias SMPPEX.Pdu.Errors

  use ExUnit.Case

  doctest SMPPEX.Pdu.Errors

  test "code_by_name" do
    assert_raise FunctionClauseError, fn() ->
      Errors.code_by_name(:NON_EXISTING_ERROR_CODE)
    end
  end

  test "format" do
    assert_raise FunctionClauseError, fn() ->
      Errors.format(:not_an_integer)
    end
  end

  test "description" do
    assert_raise FunctionClauseError, fn() ->
      Errors.description(:not_an_integer)
    end
  end
end


