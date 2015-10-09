defmodule SMPPEX.Protocol.PackResult do

  def ok(result) when is_binary(result) or is_list(result) do
    {:ok, result}
  end

  def error(desc) do
    {:error, desc}
  end

  def error(desc, previous_error) do
    {:error, {desc, previous_error}}
  end

end

