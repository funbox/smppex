defmodule SMPPEX.Compat do
  @moduledoc false

  @spec monotonic_time() :: integer

  otp_release = :erlang.system_info(:otp_release)
  case :string.to_integer(otp_release) do
    {17, ''} ->
      def monotonic_time do
        {mega, sec, micro} = :erlang.now
        (mega * 1_000_000 + sec) * 1000 + div(micro, 1000)
      end
    {n, ''} when n >= 18 ->
      def monotonic_time do
        :erlang.monotonic_time(:milli_seconds)
      end
    _ ->
      raise "OTP version #{otp_release} is not supported"
  end

  @spec to_charlist(term) :: list

  if System.version |> Version.match?(">= 1.3.0") do
    def to_charlist(s), do: Kernel.to_charlist(s)
  else
    def to_charlist(s), do: Kernel.to_char_list(s)
  end

end
