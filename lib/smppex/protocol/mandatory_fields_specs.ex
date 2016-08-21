defmodule SMPPEX.Protocol.MandatoryFieldsSpecs do
  @moduledoc false

  @type integer_size :: 1 | 2 | 4
  @type field_value_spec :: {:c_octet_string, {:max, pos_integer}} | {:c_octet_string, {:fixed, pos_integer}} | {:integer, integer_size} | {:octet_string, non_neg_integer} | {:octet_string, atom} | {:times, atom, fields_spec}
  @type case_spec :: {atom, any, fields_spec}
  @type cases_spec :: {:case, list(case_spec)}
  @type field_spec :: {atom, field_value_spec}
  @type fields_spec :: list(field_spec | cases_spec)

  @spec spec_for(atom) :: fields_spec

  def spec_for(:bind_transmitter) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}},
      {:password,  {:c_octet_string, {:max, 9}}},
      {:system_type,  {:c_octet_string, {:max, 13}}},
      {:interface_version, {:integer, 1}},
      {:addr_ton, {:integer, 1}},
      {:addr_npi, {:integer, 1}},
      {:address_range, {:c_octet_string, {:max, 41}}}
    ]
  end

  def spec_for(:bind_transmitter_resp) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}}
    ]
  end

  def spec_for(:bind_receiver) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}},
      {:password,  {:c_octet_string, {:max, 9}}},
      {:system_type,  {:c_octet_string, {:max, 13}}},
      {:interface_version, {:integer, 1}},
      {:addr_ton, {:integer, 1}},
      {:addr_npi, {:integer, 1}},
      {:address_range, {:c_octet_string, {:max, 41}}}
    ]
  end

  def spec_for(:bind_receiver_resp) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}}
    ]
  end

  def spec_for(:bind_transceiver) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}},
      {:password,  {:c_octet_string, {:max, 9}}},
      {:system_type,  {:c_octet_string, {:max, 13}}},
      {:interface_version, {:integer, 1}},
      {:addr_ton, {:integer, 1}},
      {:addr_npi, {:integer, 1}},
      {:address_range, {:c_octet_string, {:max, 41}}}
    ]
  end

  def spec_for(:bind_transceiver_resp) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}}
    ]
  end

  def spec_for(:outbind) do
    [
      {:system_id, {:c_octet_string, {:max, 16}}},
      {:password,  {:c_octet_string, {:max, 9}}}
    ]
  end

  def spec_for(:unbind) do
    []
  end

  def spec_for(:unbind_resp) do
    []
  end

  def spec_for(:generic_nack) do
    []
  end

  def spec_for(:submit_sm) do
    [
      {:service_type, {:c_octet_string, {:max, 6}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}},
      {:dest_addr_ton, {:integer, 1}},
      {:dest_addr_npi, {:integer, 1}},
      {:destination_addr, {:c_octet_string, {:max, 21}}},
      {:esm_class, {:integer, 1}},
      {:protocol_id, {:integer, 1}},
      {:priority_flag, {:integer, 1}},
      {:schedule_delivery_time, {:c_octet_string, {:fixed, 17}}},
      {:validity_period, {:c_octet_string, {:fixed, 17}}},
      {:registered_delivery, {:integer, 1}},
      {:replace_if_present_flag, {:integer, 1}},
      {:data_coding, {:integer, 1}},
      {:sm_default_msg_id, {:integer, 1}},
      {:sm_length, {:integer, 1}},
      {:short_message, {:octet_string, :sm_length}}
    ]
  end

  def spec_for(:submit_sm_resp) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}}
    ]
  end

  def spec_for(:submit_multi) do
    [
      {:service_type, {:c_octet_string, {:max, 6}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}},
      {:number_of_dests, {:integer, 1}},
      {:dest_addresses, {:times, :number_of_dests, [
        {:dest_flag, {:integer, 1}},
        {:case,
          [{:dest_flag, 1, [
            {:dest_addr_ton, {:integer, 1}},
            {:dest_addr_npi, {:integer, 1}},
            {:destination_addr, {:c_octet_string, {:max, 21}}}
          ]},
          {:dest_flag, 2, [
            {:dl_name, {:c_octet_string, {:max, 21}}}
          ]}]
        }
      ]}},
      {:esm_class, {:integer, 1}},
      {:protocol_id, {:integer, 1}},
      {:priority_flag, {:integer, 1}},
      {:schedule_delivery_time, {:c_octet_string, {:fixed, 17}}},
      {:validity_period, {:c_octet_string, {:fixed, 17}}},
      {:registered_delivery, {:integer, 1}},
      {:replace_if_present_flag, {:integer, 1}},
      {:data_coding, {:integer, 1}},
      {:sm_default_msg_id, {:integer, 1}},
      {:sm_length, {:integer, 1}},
      {:short_message, {:octet_string, :sm_length}}
    ]
  end

  def spec_for(:submit_multi_resp) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}},
      {:no_unsuccess, {:integer, 1}},
      {:unsuccess_smes, {:times, :no_unsuccess}, [
        {:dest_addr_ton, {:integer, 1}},
        {:dest_addr_npi, {:integer, 1}},
        {:destination_addr, {:c_octet_string, {:max, 21}}},
        {:error_status_code, {:integer, 4}}
      ]}
    ]
  end

  def spec_for(:deliver_sm) do
    [
      {:service_type, {:c_octet_string, {:max, 6}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}},
      {:dest_addr_ton, {:integer, 1}},
      {:dest_addr_npi, {:integer, 1}},
      {:destination_addr, {:c_octet_string, {:max, 21}}},
      {:esm_class, {:integer, 1}},
      {:protocol_id, {:integer, 1}},
      {:priority_flag, {:integer, 1}},
      {:schedule_delivery_time, {:c_octet_string, {:fixed, 17}}},
      {:validity_period, {:c_octet_string, {:fixed, 17}}},
      {:registered_delivery, {:integer, 1}},
      {:replace_if_present_flag, {:integer, 1}},
      {:data_coding, {:integer, 1}},
      {:sm_default_msg_id, {:integer, 1}},
      {:sm_length, {:integer, 1}},
      {:short_message, {:octet_string, :sm_length}}
    ]
  end

  def spec_for(:deliver_sm_resp) do
    [
      {:message_id, {:c_octet_string, {:max, 1}}}
    ]
  end

  def spec_for(:data_sm) do
    [
      {:service_type, {:c_octet_string, {:max, 6}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 65}}},
      {:dest_addr_ton, {:integer, 1}},
      {:dest_addr_npi, {:integer, 1}},
      {:destination_addr, {:c_octet_string, {:max, 65}}},
      {:esm_class, {:integer, 1}},
      {:data_coding, {:integer, 1}}
    ]
  end

  def spec_for(:data_sm_resp) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}}
    ]
  end

  def spec_for(:query_sm) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}}
    ]
  end

  def spec_for(:query_sm_resp) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}},
      {:final_date, {:c_octet_string, {:max, 17}}},
      {:message_state, {:integer, 1}},
      {:error_code, {:integer, 1}}
    ]
  end

  def spec_for(:cancel_sm) do
    [
      {:service_type, {:c_octet_string, {:max, 6}}},
      {:message_id, {:c_octet_string, {:max, 65}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}},
      {:dest_addr_ton, {:integer, 1}},
      {:dest_addr_npi, {:integer, 1}},
      {:destination_addr, {:c_octet_string, {:max, 21}}}
    ]
  end

  def spec_for(:cancel_sm_resp) do
    []
  end

  def spec_for(:replace_sm) do
    [
      {:message_id, {:c_octet_string, {:max, 65}}},
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 21}}},
      {:schedule_delivery_time, {:c_octet_string, {:fixed, 17}}},
      {:validity_period, {:c_octet_string, {:fixed, 17}}},
      {:registered_delivery, {:integer, 1}},
      {:sm_default_msg_id, {:integer, 1}},
      {:sm_length, {:integer, 1}},
      {:short_message, {:octet_string, :sm_length}}
    ]
  end

  def spec_for(:replace_sm_resp) do
    []
  end

  def spec_for(:enquire_link) do
    []
  end

  def spec_for(:enquire_link_resp) do
    []
  end

  def spec_for(:alert_notification) do
    [
      {:source_addr_ton, {:integer, 1}},
      {:source_addr_npi, {:integer, 1}},
      {:source_addr, {:c_octet_string, {:max, 61}}},
      {:esme_addr_ton, {:integer, 1}},
      {:esme_addr_npi, {:integer, 1}},
      {:esme_addr, {:c_octet_string, {:max, 61}}}
    ]
  end

  def spec_for(_) do
    []
  end
end
