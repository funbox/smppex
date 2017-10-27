defmodule SMPPEX.Protocol.TlvFormatTest do
  use ExUnit.Case
  alias SMPPEX.Protocol.TlvFormat

  test "name_by_id" do
    assert TlvFormat.name_by_id(0x0005) == {:ok, :dest_addr_subunit}
    assert TlvFormat.name_by_id(0x5555) == :unknown
  end

  test "id_by_name" do
    assert TlvFormat.id_by_name(:dest_addr_subunit) == {:ok, 0x0005}
    assert TlvFormat.id_by_name(:dest_addr_subunit____foo) == :unknown
  end

  test "format_by_id" do
    assert TlvFormat.format_by_id(0x0005) == {:ok, {:integer, 1}}
    assert TlvFormat.format_by_id(0x5555) == :unknown
  end
end
