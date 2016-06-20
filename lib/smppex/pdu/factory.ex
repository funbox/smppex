defmodule SMPPEX.Pdu.Factory do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  @message_state_delivered 2

  def bind_transmitter(system_id, password, opts \\ %{}) do
    bind(:bind_transmitter, system_id, password, opts)
  end

  def bind_receiver(system_id, password, opts \\ %{}) do
    bind(:bind_receiver, system_id, password, opts)
  end

  def bind_transceiver(system_id, password, opts \\ %{}) do
    bind(:bind_transceiver, system_id, password, opts)
  end


  def bind(command_name, system_id, password, opts \\ %{}) when command_name == :bind_transceiver or command_name == :bind_transmitter or command_name == :bind_receiver do
    mandatory = opts
      |> Map.put(:system_id, system_id)
      |> Map.put(:password, password)
    bind(command_name, mandatory)
  end

  def bind(command_name, opts) when command_name == :bind_transceiver or command_name == :bind_transmitter or command_name == :bind_receiver do
    {:ok, command_id} = CommandNames.id_by_name(command_name)
    Pdu.new(
      command_id,
      opts
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

  def enquire_link_resp(command_status \\ 0) do
    {:ok, command_id} = CommandNames.id_by_name(:enquire_link_resp)
    Pdu.new({command_id, command_status, 0})
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

  def deliver_sm({source_addr, source_addr_ton, source_addr_npi}, {dest_addr, dest_addr_ton, dest_addr_npi}, message) do
    {:ok, command_id} = CommandNames.id_by_name(:deliver_sm)
    Pdu.new(
      command_id,
      %{
        source_addr: source_addr,
        source_addr_ton: source_addr_ton,
        source_addr_npi: source_addr_npi,
        destination_addr: dest_addr,
        dest_addr_ton: dest_addr_ton,
        dest_addr_npi: dest_addr_npi,
        short_message: message
      }
    )
  end

  def delivery_report(message_id, {source_addr, source_addr_ton, source_addr_npi}, {dest_addr, dest_addr_ton, dest_addr_npi}, message \\ "", message_state \\ @message_state_delivered) do
    {:ok, command_id} = CommandNames.id_by_name(:deliver_sm)
    Pdu.new(
      command_id,
      %{
        source_addr: source_addr,
        source_addr_ton: source_addr_ton,
        source_addr_npi: source_addr_npi,
        destination_addr: dest_addr,
        dest_addr_ton: dest_addr_ton,
        dest_addr_npi: dest_addr_npi,
        short_message: message
      },
      %{
        message_state: message_state,
        receipted_message_id: message_id
      }
    )
  end

  def deliver_sm_resp(command_status \\ 0) do
    {:ok, command_id} = CommandNames.id_by_name(:deliver_sm_resp)
    Pdu.new({command_id, command_status, 0})
  end

end
