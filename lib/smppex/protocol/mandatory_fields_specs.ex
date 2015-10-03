defmodule SMPPEX.Protocol.MandatoryFieldsSpecs do
  def spec_for(:bind_transmitter) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}},
      {:password,  {:c_octet_string, {:max, 9}}},
      {:system_type,  {:c_octet_string, {:max, 13}}},
      {:interface_version, {:integer, 1}},
      {:addr_ton, {:integer, 1}},
      {:addr_npi, {:integer, 1}},
      {:address_range, {:c_octet_string, {:max, 41}}}
    ]
  end

  def spec_for(_) do
    []
  end
end
