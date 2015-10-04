defmodule SMPPEX.Protocol.TlvNames do
  tlvs = [
    {:dest_addr_subunit, 0x0005, {:integer, 1}},
    {:dest_network_type, 0x0006, quote do: {}},
    {:dest_bearer_type, 0x0007, quote do: {}},
    {:dest_telematics_id, 0x0008, quote do: {}},
    {:source_addr_subunit, 0x000D, quote do: {}},
    {:source_network_type, 0x000E, quote do: {}},
    {:source_bearer_type, 0x000F, quote do: {}},
    {:source_telematics_id, 0x0010, quote do: {}},
    {:qos_time_to_live, 0x0017, quote do: {}},
    {:payload_type, 0x0019, quote do: {}},
    {:additional_status_info_text, 0x001D, quote do: {}},
    {:receipted_message_id, 0x001E, quote do: {}},
    {:ms_msg_wait_facilities, 0x0030, quote do: {}},
    {:privacy_indicator, 0x0201, quote do: {}},
    {:source_subaddress, 0x0202, quote do: {}},
    {:dest_subaddress, 0x0203, quote do: {}},
    {:user_message_reference, 0x0204, quote do: {}},
    {:user_response_code, 0x0205, quote do: {}},
    {:source_port, 0x020A, quote do: {}},
    {:destination_port, 0x020B, quote do: {}},
    {:sar_msg_ref_num, 0x020C, quote do: {}},
    {:language_indicator, 0x020D, quote do: {}},
    {:sar_total_segments, 0x020E, quote do: {}},
    {:sar_segment_seqnum, 0x020F, quote do: {}},
    {:sc_interface_version, 0x0210, quote do: {}},
    {:callback_num_pres_ind, 0x0302, quote do: {}},
    {:callback_num_atag, 0x0303, quote do: {}},
    {:number_of_messages, 0x0304, quote do: {}},
    {:callback_num, 0x0381, quote do: {}},
    {:dpf_result, 0x0420, quote do: {}},
    {:set_dpf, 0x0421, quote do: {}},
    {:ms_availability_status, 0x0422, quote do: {}},
    {:network_error_code, 0x0423, quote do: {}},
    {:message_payload, 0x0424, quote do: {}},
    {:delivery_failure_reason, 0x0425, quote do: {}},
    {:more_messages_to_send, 0x0426, quote do: {}},
    {:message_state, 0x0427, quote do: {}},
    {:ussd_service_op, 0x0501, quote do: {}},
    {:display_time, 0x1201, quote do: {}},
    {:sms_signal, 0x1203, quote do: {}},
    {:ms_validity, 0x1204, quote do: {}},
    {:alert_on_message_delivery, 0x130C, quote do: {}},
    {:its_reply_type, 0x1380, quote do: {}},
    {:its_session_info, 0x1383, quote do: {}},
  ]

  Enum.each tlvs, fn({name, id, format}) ->
    def name_by_id(unquote(id)) do
      {:ok, unquote(name)}
    end

    def id_by_name(unquote(name)) do
      {:ok, unquote(id)}
    end

    def format_by_id(unquote(id)) do
      {:ok, unquote(format)}
    end
  end

  def name_by_id(_) do
    :unknown
  end

  def id_by_name(_) do
    :unknown
  end

  def format_by_id(_) do
    :unknown
  end
end
