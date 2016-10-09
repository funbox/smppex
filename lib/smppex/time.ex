defmodule SMPPEX.Time do

  @spec monotonic() :: integer

  otp_release = :erlang.system_info(:otp_release)
  case :string.to_integer(otp_release) do
    {17, ''} ->
      def monotonic do
        {mega, sec, micro} = :erlang.now
        (mega * 1_000_000 + sec) * 1000 + div(micro, 1000)
      end
    {n, ''} when n >= 18 ->
      def monotonic do
        :erlang.monotonic_time(:milli_seconds)
      end
    _ ->
      raise "OTP version #{otp_release} is not supported"
  end
end
