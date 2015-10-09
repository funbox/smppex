defmodule SMPPEX.Protocol.TlvFormatTest do

  use ExUnit.Case
  import SMPPEX.Protocol.TlvFormat

  test "name_by_id" do
    assert name_by_id(0x0005) == {:ok, :dest_addr_subunit}
    assert name_by_id(0x5555) == :unknown
  end

  test "id_by_name" do
    assert id_by_name(:dest_addr_subunit) == {:ok, 0x0005}
    assert id_by_name(:dest_addr_subunit____foo) == :unknown
  end

  test "format_by_id" do
    assert format_by_id(0x0005) == {:ok, {:integer, 1}}
    assert format_by_id(0x5555) == :unknown
  end
end
