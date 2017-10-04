defmodule SMPPEX.PduStorageTest do
  use ExUnit.Case

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  test "store" do
    storage = PduStorage.new
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass2") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu1, 321)
    assert false == PduStorage.store(storage, pdu2, 321)

    pdus = PduStorage.fetch(storage, 123)

    assert pdus == [pdu1]
  end

  test "fetch" do
    storage = PduStorage.new
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "pass") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu, 321)

    assert [pdu] == PduStorage.fetch(storage, 123)
    assert [] == PduStorage.fetch(storage, 124)
  end

  test "expire" do
    storage = PduStorage.new
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    assert true == PduStorage.store(storage, pdu1, 1000)
    assert true == PduStorage.store(storage, pdu2, 2000)

    assert [pdu1] == PduStorage.fetch_expired(storage, 1500)
    assert [] == PduStorage.fetch(storage, 123)
    assert [pdu2] == PduStorage.fetch(storage, 124)
  end

  test "stop && lost_pdus" do
    storage = PduStorage.new
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    assert true == PduStorage.store(storage, pdu1, 1000)
    assert true == PduStorage.store(storage, pdu2, 2000)

    assert Enum.sort([pdu1, pdu2]) == Enum.sort(PduStorage.fetch_all(storage))
  end

end
