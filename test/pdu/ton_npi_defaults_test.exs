defmodule SMPPEX.Pdu.TONNPIDefaultsTest do
  alias SMPPEX.Pdu.TONNPIDefaults

  use ExUnit.Case

  test "ton_npi" do
    assert {5, 0} == TONNPIDefaults.ton_npi("alphanumeric0")
    assert {5, 0} == TONNPIDefaults.ton_npi("00+0")
    assert {1, 1} == TONNPIDefaults.ton_npi("71234567890")
    assert {1, 1} == TONNPIDefaults.ton_npi("+012345678901234")
    assert {3, 0} == TONNPIDefaults.ton_npi("12345")

    assert {0, 0} == TONNPIDefaults.ton_npi("+0123456789012345")
    assert {0, 0} == TONNPIDefaults.ton_npi("0")
    assert {0, 0} == TONNPIDefaults.ton_npi("7123456789071234567890")
  end
end
