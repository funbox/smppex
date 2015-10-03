defmodule SMPPEX.Protocol.ParseResult do

  def ok(result, rest) when is_binary(rest) do
    {:ok, result, rest}
  end

  def error(desc) do
    {:error, desc}
  end

  def error(desc, previous_error) do
    {:error, {desc, previous_error}}
  end

end
