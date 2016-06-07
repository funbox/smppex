defmodule SMPPEX.Pdu.Factory do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  def bind_transmitter(system_id, password, conn_type \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transmitter)
    bind(command_id, system_id, password, conn_type)
  end

  def bind_receiver(system_id, password, conn_type \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_receiver)
    bind(command_id, system_id, password, conn_type)
  end

  def bind_transceiver(system_id, password, conn_type \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transceiver)
    bind(command_id, system_id, password, conn_type)
  end

  def bind(command_id, system_id, password, conn_type \\ "") do
    Pdu.new(
      command_id,
      %{
        system_id: system_id,
        password: password,
        conn_type: conn_type
      }
    )
  end

  def bind_transmitter_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transmitter_resp)
    bind_resp(command_id, command_status, system_id)
  end

  def bind_receiver_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_receiver_resp)
    bind_resp(command_id, command_status, system_id)
  end

  def bind_transceiver_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transceiver_resp)
    bind_resp(command_id, command_status, system_id)
  end

  def bind_resp(command_id, command_status, system_id) do
    Pdu.new(
      {command_id, command_status, 0},
      %{
        system_id: system_id
      }
    )
  end

  def enquire_link do
    {:ok, command_id} = CommandNames.id_by_name(:enquire_link)
    Pdu.new(command_id)
  end

  def submit_sm({source_addr, source_addr_ton, source_addr_npi}, {dest_addr, dest_addr_ton, dest_addr_npi}, message, registered_delivery \\ 0) do
    {:ok, command_id} = CommandNames.id_by_name(:submit_sm)
    Pdu.new(
      command_id,
      %{
        source_addr: source_addr,
        source_addr_ton: source_addr_ton,
        source_addr_npi: source_addr_npi,
        destination_addr: dest_addr,
        dest_addr_ton: dest_addr_ton,
        dest_addr_npi: dest_addr_npi,
        short_message: message,
        registered_delivery: registered_delivery
      }
    )
  end

  def submit_sm_resp(command_status, message_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:submit_sm_resp)
    Pdu.new(
      {command_id, command_status, 0},
      %{
        message_id: message_id
      }
    )
  end

end
