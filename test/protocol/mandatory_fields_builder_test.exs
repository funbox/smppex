defmodule SMPPEX.Protocol.MandatoryFieldsBuilderTest do
  use ExUnit.Case

  alias SMPPEX.Protocol.MandatoryFieldsBuilder

  defp flatten(io_list) do
    [io_list] |> List.flatten() |> Enum.join()
  end

  defp build_(fields, spec) do
    case MandatoryFieldsBuilder.build(fields, spec) do
      {:ok, io_list} -> {:ok, flatten(io_list)}
      err -> err
    end
  end

  test "build: integer" do
    spec = [
      {:a, {:integer, 1}}
    ]

    assert {:ok, <<01>>} == build_(%{a: 1}, spec)
  end

  test "build: c_octet_string fixed" do
    spec = [
      {:a, {:c_octet_string, {:fixed, 3}}}
    ]

    assert {:ok, <<01, 02, 00>>} == build_(%{a: <<01, 02>>}, spec)

    assert {:ok, <<00>>} == build_(%{a: ""}, spec)
  end

  test "build: c_octet_string var" do
    spec = [
      {:a, {:c_octet_string, {:max, 3}}}
    ]

    assert {:ok, <<01, 02, 00>>} == build_(%{a: <<01, 02>>}, spec)

    assert {:ok, <<01, 00>>} == build_(%{a: <<01>>}, spec)

    assert {:ok, <<00>>} == build_(%{a: ""}, spec)
  end

  test "build: octet_string" do
    spec = [
      {:a, {:octet_string, 3}}
    ]

    assert {:ok, <<01, 02, 03>>} == build_(%{a: <<01, 02, 03>>}, spec)
  end

  test "build: octet_string expanded" do
    spec = [
      {:sm_length, {:integer, 1}},
      {:short_message, {:octet_string, :sm_length}}
    ]

    assert {:ok, <<03, 01, 02, 03>>} == build_(%{short_message: <<01, 02, 03>>}, spec)
  end

  test "n-times build" do
    spec = [
      {:cnt, {:integer, 1}},
      {
        :array,
        {:times, :cnt, [
          {:a, {:c_octet_string, {:max, 3}}},
          {:b, {:integer, 1}}
        ]}
      }
    ]

    data = <<03, ?a, 00, 01, ?b, 00, 02, ?c, 00, 03>>

    fields = %{
      array: [
        %{a: "a", b: 1},
        %{a: "b", b: 2},
        %{a: "c", b: 3}
      ]
    }

    assert {:ok, data} == build_(fields, spec)
  end

  test "case parse" do
    spec = [
      {:a, {:integer, 1}},
      {:b, {:integer, 1}},
      {:case, [
        {:a, 1, [
          {:x, {:octet_string, 1}}
        ]},
        {:b, 2, [
          {:y, {:c_octet_string, {:max, 2}}}
        ]}
      ]}
    ]

    data = <<01, 02, ?z>>
    assert {:ok, data} == build_(%{a: 1, b: 2, x: "z"}, spec)

    data = <<02, 02, ?z, 00>>
    assert {:ok, data} == build_(%{a: 2, b: 2, y: "z"}, spec)

    assert {:error, _} = build_(%{a: 2, b: 3, y: "z"}, spec)
  end
end
