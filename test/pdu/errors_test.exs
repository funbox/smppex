defmodule SMPPEX.Pdu.ErrorsTest do
  alias SMPPEX.Pdu.Errors

  use ExUnit.Case

  test "code_by_name" do
    assert Errors.code_by_name(:ROK) == 0

    assert_raise FunctionClauseError, fn() ->
      Errors.code_by_name(:NON_EXISTING_ERROR_CODE)
    end
  end

  test "format" do
    assert Errors.format(0) == "ROK"

    assert Errors.format(12345) == "12345"

    assert_raise FunctionClauseError, fn() ->
      Errors.format(:not_an_integer)
    end
  end

  test "description" do
    assert Errors.description(0) == "No Error"

    assert Errors.description(12345) == "12345"

    assert_raise FunctionClauseError, fn() ->
      Errors.description(:not_an_integer)
    end
  end
end


