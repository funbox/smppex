defmodule SMPPEX.PduStorageTest do
  use ExUnit.Case

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  test "store" do
    {:ok, storage} = PduStorage.start_link
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass2") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu1, 321)
    assert false == PduStorage.store(storage, pdu2, 321)

    pdus = PduStorage.fetch(storage, 123)

    assert pdus == [pdu1]
  end

  test "fetch" do
    {:ok, storage} = PduStorage.start_link
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "pass") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu, 321)

    assert [pdu] == PduStorage.fetch(storage, 123)
    assert [] == PduStorage.fetch(storage, 124)
  end

  test "expire" do
    {:ok, storage} = PduStorage.start_link
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    assert true == PduStorage.store(storage, pdu1, 1000)
    assert true == PduStorage.store(storage, pdu2, 2000)

    assert [pdu1] == PduStorage.fetch_expired(storage, 1500)
    assert [] == PduStorage.fetch(storage, 123)
    assert [pdu2] == PduStorage.fetch(storage, 124)
  end

  test "stop" do
    {:ok, storage} = PduStorage.start_link
    assert :ok == PduStorage.stop(storage)
    refute Process.alive?(storage)
  end
end
