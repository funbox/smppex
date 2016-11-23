defmodule SMPPEX.Protocol.TlvFormat do
  @moduledoc false

  @type integer_size :: 1 | 2 | 4
  @type tlv_value_spec :: {:integer, integer_size} | {:c_octet_string, {:max, pos_integer}} | {:octet_string, non_neg_integer} | {:octet_string, {non_neg_integer, non_neg_integer}}

  tlvs = [
    {:dest_addr_subunit, 0x0005, quote do: {:integer, 1}},
    {:dest_network_type, 0x0006, quote do: {:integer, 1}},
    {:dest_bearer_type, 0x0007, quote do: {:integer, 1}},
    {:dest_telematics_id, 0x0008, quote do: {:integer, 2}},
    {:source_addr_subunit, 0x000D, quote do: {:integer, 1}},
    {:source_network_type, 0x000E, quote do: {:integer, 1}},
    {:source_bearer_type, 0x000F, quote do: {:integer, 1}},
    {:source_telematics_id, 0x0010, quote do: {:integer, 1}},
    {:qos_time_to_live, 0x0017, quote do: {:integer, 4}},
    {:payload_type, 0x0019, quote do: {:integer, 1}},
    {:additional_status_info_text, 0x001D, quote do: {:c_octet_string, {:max, 256}}},
    {:receipted_message_id, 0x001E, quote do: {:c_octet_string, {:max, 65}}},
    {:ms_msg_wait_facilities, 0x0030, quote do: {:integer, 1}},
    {:privacy_indicator, 0x0201, quote do: {:integer, 1}},
    {:source_subaddress, 0x0202, quote do: {:octet_string, {2, 23}}},
    {:dest_subaddress, 0x0203, quote do: {:octet_string, {2, 23}}},
    {:user_message_reference, 0x0204, quote do: {:integer, 2}},
    {:user_response_code, 0x0205, quote do: {:integer, 1}},
    {:source_port, 0x020A, quote do: {:integer, 2}},
    {:destination_port, 0x020B, quote do: {:integer, 2}},
    {:sar_msg_ref_num, 0x020C, quote do: {:integer, 2}},
    {:language_indicator, 0x020D, quote do: {:integer, 1}},
    {:sar_total_segments, 0x020E, quote do: {:integer, 1}},
    {:sar_segment_seqnum, 0x020F, quote do: {:integer, 1}},
    {:sc_interface_version, 0x0210, quote do: {:integer, 1}},
    {:callback_num_pres_ind, 0x0302, quote do: {:integer, 1}},
    {:callback_num_atag, 0x0303, quote do: {:c_octet_string, {:max, 65}}},
    {:number_of_messages, 0x0304, quote do: {:integer, 1}},
    {:callback_num, 0x0381, quote do: {:octet_string, {4, 19}}},
    {:dpf_result, 0x0420, quote do: {:integer, 1}},
    {:set_dpf, 0x0421, quote do: {:integer, 1}},
    {:ms_availability_status, 0x0422, quote do: {:integer, 1}},
    {:network_error_code, 0x0423, quote do: {:octet_string, 3}},
    {:message_payload, 0x0424, quote do: {:octet_string, {0, 0xFFFF}}},
    {:delivery_failure_reason, 0x0425, quote do: {:integer, 1}},
    {:more_messages_to_send, 0x0426, quote do: {:integer, 1}},
    {:message_state, 0x0427, quote do: {:integer, 1}},
    {:ussd_service_op, 0x0501, quote do: {:integer, 1}},
    {:display_time, 0x1201, quote do: {:integer, 1}},
    {:sms_signal, 0x1203, quote do: {:integer, 2}},
    {:ms_validity, 0x1204, quote do: {:integer, 1}},
    {:alert_on_message_delivery, 0x130C, quote do: {:octet_string, 0}},
    {:its_reply_type, 0x1380, quote do: {:integer, 1}},
    {:its_session_info, 0x1383, quote do: {:octet_string, 2}},
  ]

  @spec name_by_id(integer) :: :unknown | {:ok, atom}
  @spec id_by_name(atom) :: :unknown | {:ok, non_neg_integer}
  @spec format_by_id(integer) :: :unknown | {:ok, tlv_value_spec}

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
