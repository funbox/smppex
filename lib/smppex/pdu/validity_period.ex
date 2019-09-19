defmodule SMPPEX.Pdu.ValidityPeriod do
  @moduledoc ~S"""
  Module for converting validity period to Unix timestamp.

  Module works both with the absolute format of validity period and with the relative one.
  In case of relative validity period, this module implements a naive representation of the month and year date shifting
  for the sake of simplicity.
  """

  @type validity_period :: String.t()
  @type timestamp :: non_neg_integer
  @type timestamp_origin :: non_neg_integer

  @spec to_timestamp(validity_period, timestamp_origin) ::
          {:ok, timestamp} | {:error, :invalid_validity_period}

  @doc ~S"""
  Converts `t:validity_period/0` to Unix timestamp according to the SMPP specification.

  In case of the relative format, this function uses a naive implementation of the month and date shifting.
  To be clear, it takes one month as 30 days and one year as 12 months.

  One who uses the function should implement the way how this time shift might be limited
  according to the SMPP specification:

  * A MC operator may choose to impose a limit on
  relative time offsets, thus either rejecting a message that exceeds such a limit or reducing the
  offset to the maximum relative time allowed.

  Returns `{:ok, timestamp}` if conversion was successful.

  Returns `{:error, :invalid_validity_period}` if `validity_period` is not consistent with the SMPP specification.

  In case of internal errors, however, this function raises an exception.

  ## Example (relative format)

      iex> timestamp_origin = ~N[2017-01-01 00:00:00] |>
      ...> DateTime.from_naive!("Etc/UTC") |>
      ...> DateTime.to_unix()
      iex> timestamp = SMPPEX.Pdu.ValidityPeriod.to_timestamp!("000000000005000R", timestamp_origin)
      iex> DateTime.from_unix!(timestamp) |> to_string()
      "2017-01-01 00:00:05Z"

  ## Example (absolute format)

      iex> {:ok, timestamp} = SMPPEX.Pdu.ValidityPeriod.to_timestamp("170610233429004+")
      iex> DateTime.from_unix!(timestamp) |> to_string()
      "2017-06-10 22:34:29Z"

  """

  def to_timestamp(validity_period, timestamp_origin \\ System.system_time(:second))

  def to_timestamp(
        <<y::binary-size(2), m::binary-size(2), d::binary-size(2), h::binary-size(2),
          mn::binary-size(2), s::binary-size(2), _t::binary-size(1), _nn::binary-size(2), "R">>,
        timestamp_origin
      ) do
    timestamp =
      timestamp_origin +
        String.to_integer(s) +
        String.to_integer(mn) * 60 +
        String.to_integer(h) * 3600 +
        String.to_integer(d) * 24 * 3600 +
        String.to_integer(m) * 30 * 24 * 3600 +
        String.to_integer(y) * 12 * 30 * 24 * 3600

    {:ok, timestamp}
  end

  def to_timestamp(
        <<y::binary-size(2), m::binary-size(2), d::binary-size(2), h::binary-size(2),
          mn::binary-size(2), s::binary-size(2), _t::binary-size(1), nn::binary-size(2),
          p::binary-size(1)>>,
        _
      ) do
    {:ok, datetime} =
      NaiveDateTime.new(
        2000 + String.to_integer(y),
        String.to_integer(m),
        String.to_integer(d),
        String.to_integer(h),
        String.to_integer(mn),
        String.to_integer(s)
      )

    seconds =
      datetime
      |> DateTime.from_naive!("Etc/UTC")
      |> DateTime.to_unix()

    hour_diff = div(String.to_integer(nn), 4)
    seconds_diff = hour_diff * 3600

    timestamp =
      case p do
        "+" ->
          seconds - seconds_diff

        "-" ->
          seconds + seconds_diff
      end

    {:ok, timestamp}
  end

  def to_timestamp(_, _) do
    {:error, :invalid_validity_period}
  end

  @spec to_timestamp!(validity_period, timestamp_origin) :: timestamp

  @doc """
  Converts `t:validity_period/0` to Unix timestamp according to the SMPP specification.
  The same as `to_timestamp/2` but raises an exception in case of error.
  """

  def to_timestamp!(validity_period, timestamp_origin \\ System.system_time(:second)) do
    case to_timestamp(validity_period, timestamp_origin) do
      {:ok, value} ->
        value

      {:error, :invalid_validity_period} ->
        raise ArgumentError, message: "Invalid validity period"
    end
  end
end
