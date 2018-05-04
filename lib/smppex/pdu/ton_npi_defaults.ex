defmodule SMPPEX.Pdu.TONNPIDefaults do
  @moduledoc """
  Module for automatic TON/NPI detection
  """

  @short_code_re ~r/\A\d{3,8}\z/
  @long_code_lengths 10..15
  @numeric_re ~r/\A\+?(?<digits>\d+)\z/

  def ton_npi(address) when is_binary(address) do
    address
    |> address_type()
    |> ton_npi_by_type()
  end

  defp address_type(address) do
    if address =~ @short_code_re do
      :short_code
    else
      case Regex.named_captures(@numeric_re, address) do
        nil ->
          :alphanumeric

        %{"digits" => digits} ->
          if String.length(digits) in @long_code_lengths do
            :long_code
          else
            :unknown
          end
      end
    end
  end

  defp ton_npi_by_type(:short_code), do: {3, 0}
  defp ton_npi_by_type(:long_code), do: {1, 1}
  defp ton_npi_by_type(:alphanumeric), do: {5, 0}
  defp ton_npi_by_type(:unknown), do: {0, 0}
end
