defmodule SMPPEX.Protocol.OptionalFieldsBuilderTest do
  use ExUnit.Case

  import SMPPEX.Protocol.OptionalFieldsBuilder

  defp flat_bin({:ok, io_list}) do
    io_list |> List.flatten |> Enum.join
  end

  test "build unknown valid" do
    res = build(%{0x01 => "val"})
    assert {:ok, _} = res
    assert <<0, 0x01, 0, 0x03, ?v, ?a, ?l>> == flat_bin(res)
  end

  test "build unknown invalid" do
    big_bin = [0] |> Stream.cycle |> Enum.take(65536) |> to_string
    res = build(%{0x01 => big_bin})
    assert {:error, _} = res

    res = build(Map.put(%{}, -0x01, "test"))
    assert {:error, _} = res
  end

  test "build known valid: integer" do
    res = build(%{0x05 => 123})

    assert {:ok, _} = res
    assert <<0, 0x05, 0, 0x01, 123>> == flat_bin(res)
  end

  test "build known valid: c_octet_string" do
    res = build(%{0x1D => "test"})
    assert {:ok, _} = res
    assert <<0, 0x1D, 0, 0x05, "test", 0>> == flat_bin(res)
  end

  test "build known valid: octet_string" do
    res = build(%{0x0381 => "test"})
    assert {:ok, _} = res
    assert <<0x03, 0x81, 0, 0x04, "test">> == flat_bin(res)
  end

  test "build known invalid: integer" do
    res = build(%{0x05 => "sdf"})
    assert {:error, _} = res

    res = build(%{0x05 => -1})
    assert {:error, _} = res

    res = build(%{0x05 => 256})
    assert {:error, _} = res
  end

  test "build known invalid: c_octet_string" do
    res = build(%{0x1D => 123})
    assert {:error, _} = res

    s = <<1>> |> List.duplicate(65) |> Enum.join
    res = build(%{0x1E => << s :: binary, 0 >>})
    assert {:error, _} = res
  end

  test "build known invalid: octet_string" do
    res = build(%{0x0423 => "test"})
    assert {:error, _} = res
  end


end
