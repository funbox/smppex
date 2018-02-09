defmodule SMPPEX.Protocol.MandatoryFieldsSpecs do
  @moduledoc false

  @type integer_size :: 1 | 2 | 4
  @type field_value_spec ::
          {:c_octet_string, {:max, pos_integer}}
          | {:c_octet_string, {:fixed, pos_integer}}
          | {:integer, integer_size}
          | {:octet_string, non_neg_integer}
          | {:octet_string, atom}
          | {:times, atom, fields_spec}
  @type case_spec :: {atom, any, fields_spec}
  @type cases_spec :: {:case, list(case_spec)}
  @type field_spec :: {atom, field_value_spec}
  @type fields_spec :: list(field_spec | cases_spec)

  @spec spec_for(atom) :: fields_spec

  def spec_for(name) do
    specs() |> Map.get(name, [])
  end

  defp specs do
    %{
      bind_transmitter: [
        {:system_id, {:c_octet_string, {:max, 16}}},
        {:password, {:c_octet_string, {:max, 9}}},
        {:system_type, {:c_octet_string, {:max, 13}}},
        {:interface_version, {:integer, 1}},
        {:addr_ton, {:integer, 1}},
        {:addr_npi, {:integer, 1}},
        {:address_range, {:c_octet_string, {:max, 41}}}
      ],
      bind_transmitter_resp: [
        {:system_id, {:c_octet_string, {:max, 16}}}
      ],
      bind_receiver: [
        {:system_id, {:c_octet_string, {:max, 16}}},
        {:password, {:c_octet_string, {:max, 9}}},
        {:system_type, {:c_octet_string, {:max, 13}}},
        {:interface_version, {:integer, 1}},
        {:addr_ton, {:integer, 1}},
        {:addr_npi, {:integer, 1}},
        {:address_range, {:c_octet_string, {:max, 41}}}
      ],
      bind_receiver_resp: [
        {:system_id, {:c_octet_string, {:max, 16}}}
      ],
      bind_transceiver: [
        {:system_id, {:c_octet_string, {:max, 16}}},
        {:password, {:c_octet_string, {:max, 9}}},
        {:system_type, {:c_octet_string, {:max, 13}}},
        {:interface_version, {:integer, 1}},
        {:addr_ton, {:integer, 1}},
        {:addr_npi, {:integer, 1}},
        {:address_range, {:c_octet_string, {:max, 41}}}
      ],
      bind_transceiver_resp: [
        {:system_id, {:c_octet_string, {:max, 16}}}
      ],
      outbind: [
        {:system_id, {:c_octet_string, {:max, 16}}},
        {:password, {:c_octet_string, {:max, 9}}}
      ],
      unbind: [],
      unbind_resp: [],
      generic_nack: [],
      submit_sm: [
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
      ],
      submit_sm_resp: [
        {:message_id, {:c_octet_string, {:max, 65}}}
      ],
      submit_multi: [
        {:service_type, {:c_octet_string, {:max, 6}}},
        {:source_addr_ton, {:integer, 1}},
        {:source_addr_npi, {:integer, 1}},
        {:source_addr, {:c_octet_string, {:max, 21}}},
        {:number_of_dests, {:integer, 1}},
        {
          :dest_addresses,
          {:times, :number_of_dests,
           [
             {:dest_flag, {:integer, 1}},
             {:case,
              [
                {:dest_flag, 1,
                 [
                   {:dest_addr_ton, {:integer, 1}},
                   {:dest_addr_npi, {:integer, 1}},
                   {:destination_addr, {:c_octet_string, {:max, 21}}}
                 ]},
                {:dest_flag, 2,
                 [
                   {:dl_name, {:c_octet_string, {:max, 21}}}
                 ]}
              ]}
           ]}
        },
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
      ],
      submit_multi_resp: [
        {:message_id, {:c_octet_string, {:max, 65}}},
        {:no_unsuccess, {:integer, 1}},
        {:unsuccess_smes, {:times, :no_unsuccess},
         [
           {:dest_addr_ton, {:integer, 1}},
           {:dest_addr_npi, {:integer, 1}},
           {:destination_addr, {:c_octet_string, {:max, 21}}},
           {:error_status_code, {:integer, 4}}
         ]}
      ],
      deliver_sm: [
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
      ],
      deliver_sm_resp: [
        {:message_id, {:c_octet_string, {:max, 65}}}
      ],
      data_sm: [
        {:service_type, {:c_octet_string, {:max, 6}}},
        {:source_addr_ton, {:integer, 1}},
        {:source_addr_npi, {:integer, 1}},
        {:source_addr, {:c_octet_string, {:max, 65}}},
        {:dest_addr_ton, {:integer, 1}},
        {:dest_addr_npi, {:integer, 1}},
        {:destination_addr, {:c_octet_string, {:max, 65}}},
        {:esm_class, {:integer, 1}},
        {:data_coding, {:integer, 1}}
      ],
      data_sm_resp: [
        {:message_id, {:c_octet_string, {:max, 65}}}
      ],
      query_sm: [
        {:message_id, {:c_octet_string, {:max, 65}}},
        {:source_addr_ton, {:integer, 1}},
        {:source_addr_npi, {:integer, 1}},
        {:source_addr, {:c_octet_string, {:max, 21}}}
      ],
      query_sm_resp: [
        {:message_id, {:c_octet_string, {:max, 65}}},
        {:final_date, {:c_octet_string, {:max, 17}}},
        {:message_state, {:integer, 1}},
        {:error_code, {:integer, 1}}
      ],
      cancel_sm: [
        {:service_type, {:c_octet_string, {:max, 6}}},
        {:message_id, {:c_octet_string, {:max, 65}}},
        {:source_addr_ton, {:integer, 1}},
        {:source_addr_npi, {:integer, 1}},
        {:source_addr, {:c_octet_string, {:max, 21}}},
        {:dest_addr_ton, {:integer, 1}},
        {:dest_addr_npi, {:integer, 1}},
        {:destination_addr, {:c_octet_string, {:max, 21}}}
      ],
      cancel_sm_resp: [],
      replace_sm: [
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
      ],
      replace_sm_resp: [],
      enquire_link: [],
      enquire_link_resp: [],
      alert_notification: [
        {:source_addr_ton, {:integer, 1}},
        {:source_addr_npi, {:integer, 1}},
        {:source_addr, {:c_octet_string, {:max, 61}}},
        {:esme_addr_ton, {:integer, 1}},
        {:esme_addr_npi, {:integer, 1}},
        {:esme_addr, {:c_octet_string, {:max, 61}}}
      ]
    }
  end
end
