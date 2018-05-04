defmodule SMPPEX.Pdu.TONNPIDefaults do
  @moduledoc """
  Module for automatic TON/NPI detection
  """

  alias SMPPEX.Compat

  @digits ?0..?9

  @short_code 3..8
  @long_code 10..15

  def ton_npi(address) when is_binary(address) do
    address
    |> address_type()
    |> ton_npi_by_type()
  end

  defp address_type(address) do
    if has_only_digits?(address) do
      length = String.length(address)

      cond do
        length in @short_code -> :short_code
        length in @long_code -> :long_code
        true -> :unknown
      end
    else
      :alphanumeric
    end
  end

  defp has_only_digits?(address) do
    address
    |> Compat.to_charlist()
    |> Enum.all?(&(&1 in @digits))
  end

  defp ton_npi_by_type(:short_code), do: {3, 0}
  defp ton_npi_by_type(:long_code), do: {1, 1}
  defp ton_npi_by_type(:alphanumeric), do: {5, 0}
  defp ton_npi_by_type(:unknown), do: {0, 0}
end
