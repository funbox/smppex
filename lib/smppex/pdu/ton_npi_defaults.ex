defmodule SMPPEX.Pdu.TONNPIDefaults do
  @moduledoc """
  Module for automatic TON/NPI detection
  """

  def ton_npi(address) when is_binary(address) do
    address
    |> address_type()
    |> ton_npi_by_type()
  end

  @num ~r/\A\+?[[:digit:]]+\z/
  @alnum ~r/\A[[:alnum:]]{1,30}\z/

  defp address_type(address) do
    if Regex.match?(@num, address) do
      case String.length(address) do
        x when x in 3..8 -> :shortcode
        x when x in 9..16 -> :longcode
        _ -> :unknown
      end
    else
      if Regex.match?(@alnum, address) do
        :alphanumeric
      else
        :unknown
      end
    end
  end

  defp ton_npi_by_type(:shortcode), do: {3, 0}
  defp ton_npi_by_type(:longcode), do: {1, 1}
  defp ton_npi_by_type(:alphanumeric), do: {5, 0}
  defp ton_npi_by_type(:unknown), do: {0, 0}
end
