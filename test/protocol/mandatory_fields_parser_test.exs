defmodule SMPPEX.Protocol.MandatoryFieldsParserTest do
  use ExUnit.Case

  import SMPPEX.Protocol.MandatoryFieldsParser

  test "parse: integer" do
    spec = [
      {:a, {:integer, 1}}
    ]

    data = <<01, 02>>
    assert {:ok, %{a: 1}, <<02>>} == parse(data, spec)
  end

  test "parse: c_octet_string fixed" do
    spec = [
      {:a, {:c_octet_string, {:fixed, 3}}}
    ]

    data = <<01, 02, 00, 03>>
    assert {:ok, %{a: <<01, 02>>}, <<03>>} == parse(data, spec)

    data = <<00, 01, 02, 03>>
    assert {:ok, %{a: ""}, <<01, 02, 03>>} == parse(data, spec)
  end

  test "parse: c_octet_string var" do
    spec = [
      {:a, {:c_octet_string, {:max, 3}}}
    ]

    data = <<01, 02, 00, 03>>
    assert {:ok, %{a: <<01, 02>>}, <<03>>} == parse(data, spec)

    data = <<01, 00, 02, 03>>
    assert {:ok, %{a: <<01>>}, <<02, 03>>} == parse(data, spec)
  end

  test "parse: octet_string" do
    spec = [
      {:a, {:octet_string, 3}}
    ]

    data = <<01, 02, 03, 04>>
    assert {:ok, %{a: <<01, 02, 03>>}, <<04>>} == parse(data, spec)
  end

  test "parse: integer expanded" do
    spec = [
      {:a, {:integer, :b}}
    ]

    data = <<00, 01, 02>>
    assert {:ok, %{a: 1}, <<02>>} = parse(data, spec, %{b: 2})
  end

  test "parse: c_octet_string fixed expanded" do
    spec = [
      {:a, {:c_octet_string, {:fixed, :b}}}
    ]

    data = <<01, 02, 00, 03>>
    assert {:ok, %{a: <<01, 02>>}, <<03>>} = parse(data, spec, %{b: 3})

    data = <<00, 01, 02, 03>>
    assert {:ok, %{a: ""}, <<01, 02, 03>>} = parse(data, spec, %{b: 3})
  end

  test "parse: c_octet_string var expanded" do
    spec = [
      {:a, {:c_octet_string, {:max, :b}}}
    ]

    data = <<01, 02, 00, 03>>
    assert {:ok, %{a: <<01, 02>>}, <<03>>} = parse(data, spec, %{b: 3})

    data = <<01, 00, 02, 03>>
    assert {:ok, %{a: <<01>>}, <<02, 03>>} = parse(data, spec, %{b: 3})
  end

  test "parse: octet_string expanded" do
    spec = [
      {:a, {:octet_string, :b}}
    ]

    data = <<01, 02, 03, 04>>
    assert {:ok, %{a: <<01, 02, 03>>, b: 3}, <<04>>} == parse(data, spec, %{b: 3})
  end

  test "n-times parse" do
    spec = [
      {:array, {:times, 3, [
        {:a, {:c_octet_string, {:max, 3}}},
        {:b, {:integer, 1}}
      ]}}
    ]

    data = <<?a, 00, 01, ?b, 00, 02, ?c, 00, 03, 01, 02, 03>>
    array = [
      %{a: "a", b: 1},
      %{a: "b", b: 2},
      %{a: "c", b: 3}
    ]

    parse_result = parse(data, spec)

    assert {:ok, %{array: array}, <<01, 02, 03>>} == parse_result

  end

  test "n-times parse expanded" do
    spec = [
      {:array, {:times, :times, [
        {:a, {:c_octet_string, {:max, 3}}},
        {:b, {:integer, 1}}
      ]}}
    ]

    data = <<?a, 00, 01, ?b, 00, 02, ?c, 00, 03, 01, 02, 03>>

    array = [
      %{a: "a", b: 1},
      %{a: "b", b: 2},
      %{a: "c", b: 3}
    ]

    parse_result = parse(data, spec, %{times: 3})

    assert {:ok, %{array: array, times: 3}, <<01, 02, 03>>} == parse_result
  end

  test "case parse" do
    spec = [
      {:a, {:integer, 1}},
      {:b, {:integer, 1}},
      {:case,
        [{:a, 1, [
          {:x, {:c_octet_string, {:max, 2}}}
        ]},
        {:b, 2, [
          {:y, {:c_octet_string, {:max, 2}}}
        ]}]
      }
    ]

    data = <<01, 02, ?z, 00>>
    assert {:ok, %{a: 1, b: 2, x: "z"}, ""} == parse(data, spec)

    data = <<02, 02, ?z, 00>>
    assert {:ok, %{a: 2, b: 2, y: "z"}, ""} == parse(data, spec)

    data = <<02, 03, ?z, 00>>
    assert {:error, _} = parse(data, spec)

  end

end
