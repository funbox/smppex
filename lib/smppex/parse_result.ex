defmodule SMPPEX.ParseResult do

  def ok(result, rest) do
    {:ok, result, rest}
  end

  def error(desc) do
    {:error, desc}
  end

  def error(desc, previous_error) do
    {:error, {desc, previous_error}}
  end

end
