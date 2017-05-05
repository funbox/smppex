defmodule SMPPEX.PduStorageTest do
  use ExUnit.Case

  alias SMPPEX.PduStorage
  alias SMPPEX.Pdu

  test "store" do
    {:ok, storage} = PduStorage.start
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass1") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass2") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu1, 321)
    assert false == PduStorage.store(storage, pdu2, 321)

    pdus = PduStorage.fetch(storage, 123)

    assert pdus == [pdu1]
  end

  test "fetch" do
    {:ok, storage} = PduStorage.start
    pdu = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id", "pass") | sequence_number: 123}

    assert true == PduStorage.store(storage, pdu, 321)

    assert [pdu] == PduStorage.fetch(storage, 123)
    assert [] == PduStorage.fetch(storage, 124)
  end

  test "expire" do
    {:ok, storage} = PduStorage.start
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    assert true == PduStorage.store(storage, pdu1, 1000)
    assert true == PduStorage.store(storage, pdu2, 2000)

    assert [pdu1] == PduStorage.fetch_expired(storage, 1500)
    assert [] == PduStorage.fetch(storage, 123)
    assert [pdu2] == PduStorage.fetch(storage, 124)
  end

  test "stop && lost_pdus" do
    {:ok, a} = Agent.start_link(fn() -> [] end)
    pdu1 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id1", "pass") | sequence_number: 123}
    pdu2 = %Pdu{SMPPEX.Pdu.Factory.bind_transmitter("system_id2", "pass") | sequence_number: 124}

    spawn_link(fn() ->
      {:ok, storage} = PduStorage.start(fn(_pid, _reason, pdus) ->
        Agent.update(a, fn(_) -> pdus end)
      end)
      PduStorage.store(storage, pdu1, 1000)
      PduStorage.store(storage, pdu2, 2000)
    end)

    :timer.sleep(50)

    assert Agent.get(a, & &1) == [pdu1, pdu2]
  end

  test "stop && lost_pdus when no lost pdus present" do
    {:ok, a} = Agent.start_link(fn() -> nil end)

    spawn_link(fn() ->
      {:ok, _storage} = PduStorage.start(fn(_pid, _reason, pdus) ->
        Agent.update(a, fn(_) -> pdus end)
      end)
    end)

    :timer.sleep(50)

    assert Agent.get(a, & &1) == nil
  end

end
