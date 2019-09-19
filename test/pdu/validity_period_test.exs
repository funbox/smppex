defmodule SMPPEX.Pdu.ValidityPeriodTest do
  alias SMPPEX.Pdu.ValidityPeriod

  use ExUnit.Case

  doctest ValidityPeriod

  @time_now "2017-01-01 00:00:00"

  def to_unix(time) do
    time
    |> NaiveDateTime.from_iso8601!()
    |> DateTime.from_naive!("Etc/UTC")
    |> DateTime.to_unix()
  end

  describe "to_timestamp" do
    test "relative time format" do
      time_now = to_unix(@time_now)

      assert to_unix("2017-01-02 00:00:00") ==
               ValidityPeriod.to_timestamp!("000001000000000R", time_now)

      assert to_unix("2017-01-01 23:34:29") ==
               ValidityPeriod.to_timestamp!("000000233429000R", time_now)

      assert {:ok, to_unix("2017-01-01 23:34:29")} ==
               ValidityPeriod.to_timestamp("000000233429000R", time_now)
    end

    test "absolute time format" do
      assert to_unix("2002-06-10 23:34:29") == ValidityPeriod.to_timestamp!("020610233429000+")

      assert to_unix("2017-06-10 22:34:29") == ValidityPeriod.to_timestamp!("170610233429004+")

      assert to_unix("2002-06-10 23:34:29") == ValidityPeriod.to_timestamp!("020610233429000-")

      assert to_unix("2002-06-11 01:34:29") == ValidityPeriod.to_timestamp!("020610233429008-")
    end

    test "negative cases" do
      time_now = to_unix(@time_now)

      assert {:error, :invalid_validity_period} == ValidityPeriod.to_timestamp("", time_now)

      assert_raise ArgumentError, fn -> ValidityPeriod.to_timestamp!("", time_now) end
    end
  end
end
