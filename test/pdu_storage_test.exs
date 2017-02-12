defmodule SMPPEX.PduStorageTest do
  use ExUnit.Case

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  test "store" do
    {:ok, pid} = PduStorage.start_link

    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass2") | sequence_number: 123}

    assert true == PduStorage.store(pid, pdu1, 321)
    assert false == PduStorage.store(pid, pdu2, 321)

    pdus = PduStorage.fetch(pid, 123)

    assert pdus == [pdu1]
  end

  test "fetch" do
    {:ok, pid} = PduStorage.start_link

    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "pass") | sequence_number: 123}

    assert true == PduStorage.store(pid, pdu, 321)

    assert [pdu] == PduStorage.fetch(pid, 123)
    assert [] == PduStorage.fetch(pid, 124)
  end

  test "expire" do
    {:ok, pid} = PduStorage.start_link

    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    assert true == PduStorage.store(pid, pdu1, 1000)
    assert true == PduStorage.store(pid, pdu2, 2000)

    assert [pdu1] == PduStorage.fetch_expired(pid, 1500)
    assert [] == PduStorage.fetch(pid, 123)
    assert [pdu2] == PduStorage.fetch(pid, 124)
  end

  test "reserve sequence number" do
    {:ok, pid} = PduStorage.start_link 100

    SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    reserve_sequence_number  = PduStorage.reserve_sequence_number(pid)
    assert 100 == reserve_sequence_number
    assert 101 == PduStorage.reserve_sequence_number(pid)
    assert 102 == PduStorage.reserve_sequence_number(pid)
  end

end
