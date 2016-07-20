defmodule SMPPEX.Pdu.Factory do

  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu
  alias SMPPEX.Pdu.MessageState

  @spec bind_transmitter(String.t, String.t, map) :: Pdu.t

  def bind_transmitter(system_id, password, opts \\ %{}) do
    bind(:bind_transmitter, system_id, password, opts)
  end

  @spec bind_receiver(String.t, String.t, map) :: Pdu.t

  def bind_receiver(system_id, password, opts \\ %{}) do
    bind(:bind_receiver, system_id, password, opts)
  end

  @spec bind_transceiver(String.t, String.t, map) :: Pdu.t

  def bind_transceiver(system_id, password, opts \\ %{}) do
    bind(:bind_transceiver, system_id, password, opts)
  end

  @spec bind(atom, String.t, String.t, map) :: Pdu.t

  def bind(command_name, system_id, password, opts \\ %{}) when command_name == :bind_transceiver or command_name == :bind_transmitter or command_name == :bind_receiver do
    mandatory = opts
      |> Map.put(:system_id, system_id)
      |> Map.put(:password, password)
    bind(command_name, mandatory)
  end

  @spec bind(atom, map) :: Pdu.t

  def bind(command_name, opts) when command_name == :bind_transceiver or command_name == :bind_transmitter or command_name == :bind_receiver do
    {:ok, command_id} = CommandNames.id_by_name(command_name)
    Pdu.new(
      command_id,
      opts
    )
  end

  @spec bind_transmitter_resp(non_neg_integer, String.t) :: Pdu.t

  def bind_transmitter_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transmitter_resp)
    bind_resp(command_id, command_status, system_id)
  end

  @spec bind_receiver_resp(non_neg_integer, String.t) :: Pdu.t

  def bind_receiver_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_receiver_resp)
    bind_resp(command_id, command_status, system_id)
  end

  @spec bind_transceiver_resp(non_neg_integer, String.t) :: Pdu.t

  def bind_transceiver_resp(command_status, system_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:bind_transceiver_resp)
    bind_resp(command_id, command_status, system_id)
  end

  @spec bind_resp(non_neg_integer, non_neg_integer, String.t) :: Pdu.t

  def bind_resp(command_id, command_status, system_id) do
    Pdu.new(
      {command_id, command_status, 0},
      %{
        system_id: system_id
      }
    )
  end

  @spec enquire_link :: Pdu.t

  def enquire_link do
    {:ok, command_id} = CommandNames.id_by_name(:enquire_link)
    Pdu.new(command_id)
  end

  @spec enquire_link_resp(non_neg_integer) :: Pdu.t

  def enquire_link_resp(command_status \\ 0) do
    {:ok, command_id} = CommandNames.id_by_name(:enquire_link_resp)
    Pdu.new({command_id, command_status, 0})
  end

  @type addr :: {String.t, non_neg_integer, non_neg_integer}

  @spec submit_sm(addr, addr, String.t, non_neg_integer) :: Pdu.t

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

  @spec submit_sm_resp(non_neg_integer, String.t) :: Pdu.t

  def submit_sm_resp(command_status, message_id \\ "") do
    {:ok, command_id} = CommandNames.id_by_name(:submit_sm_resp)
    Pdu.new(
      {command_id, command_status, 0},
      %{
        message_id: message_id
      }
    )
  end

  @spec deliver_sm(addr, addr, String.t) :: Pdu.t

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

  @type message_state :: non_neg_integer | atom

  @spec delivery_report(String.t, addr, addr, String.t, message_state) :: Pdu.t

  def delivery_report(
    message_id,
    source,
    dest,
    message \\ "",
    message_state \\ :DELIVERED
  )

  def delivery_report(
    message_id,
    {_source_addr, _source_addr_ton, _source_addr_npi} = source,
    {_dest_addr, _dest_addr_ton, _dest_addr_npi} = dest,
    message,
    message_state
  ) when is_atom(message_state) do
    message_state_code = MessageState.code_by_name(message_state)
    delivery_report(message_id, source, dest, message, message_state_code)
  end

  def delivery_report(
    message_id,
    {source_addr, source_addr_ton, source_addr_npi},
    {dest_addr, dest_addr_ton, dest_addr_npi},
    message,
    message_state
  ) when is_integer(message_state) do
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

  @spec delivery_report_for_submit_sm(String.t, Pdu.t, String.t, message_state) :: Pdu.t

  def delivery_report_for_submit_sm(message_id, submit_sm, message \\ "", message_state \\ :DELIVERED) do
    source = Pdu.source(submit_sm)
    dest = Pdu.dest(submit_sm)
    delivery_report(message_id, dest, source, message, message_state)
  end

  @spec deliver_sm_resp(non_neg_integer) :: Pdu.t

  def deliver_sm_resp(command_status \\ 0) do
    {:ok, command_id} = CommandNames.id_by_name(:deliver_sm_resp)
    Pdu.new({command_id, command_status, 0})
  end

end
