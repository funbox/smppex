defmodule SMPPEX.Pdu.Errors do

  errors = [
    {:ROK,                 0x0000, "No Error"},
    {:RINVMSGLEN,          0x0001, "Message Length is invalid"},
    {:RINVCMDLEN,          0x0002, "Command Length is invalid"},
    {:RINVCMDID,           0x0003, "Invalid Command ID"},
    {:RINVBNDSTS,          0x0004, "Incorrect BIND Status for given command"},
    {:RALYBND,             0x0005, "ESME Already in Bound State"},
    {:RINVPRTFLG,          0x0006, "Invalid Priority Flag"},
    {:RINVREGDLVFLG,       0x0007, "Invalid Registered Delivery Flag"},
    {:RSYSERR,             0x0008, "System Error"},
    {:RINVSRCADR,          0x000A, "Invalid Source Address"},
    {:RINVDSTADR,          0x000B, "Invalid Dest Addr"},
    {:RINVMSGID,           0x000C, "Message ID is invalid"},
    {:RBINDFAIL,           0x000D, "Bind Failed"},
    {:RINVPASWD,           0x000E, "Invalid Password"},
    {:RINVSYSID,           0x000F, "Invalid System ID"},
    {:RCANCELFAIL,         0x0011, "Cancel SM Failed"},
    {:RREPLACEFAIL,        0x0013, "Replace SM Failed"},
    {:RMSGQFUL,            0x0014, "Message Queue Full"},
    {:RINVSERTYP,          0x0015, "Invalid Service Type"},
    {:RINVNUMDESTS,        0x0033, "Invalid destinations number"},
    {:RINVDLNAME,          0x0034, "Invalid Distribution List name"},
    {:RINVDESTFLAG,        0x0040, "Invalid Destination flag (submit_multi)"},
    {:RINVSUBREP,          0x0042, "Invalid submit with replace"},
    {:RINVESMCLASS,        0x0043, "Invalid esm_class field data"},
    {:RCNTSUBDL,           0x0044, "Cannot Submit to Distribution List"},
    {:RSUBMITFAIL,         0x0045, "submit_sm or submit_multi failed"},
    {:RINVSRCTON,          0x0048, "Invalid Source address TON"},
    {:RINVSRCNPI,          0x0049, "Invalid Source address NPI"},
    {:RINVDSTTON,          0x0050, "Invalid Destination addr TON"},
    {:RINVDSTNPI,          0x0051, "Invalid Destination addr NPI"},
    {:RINVSYSTYP,          0x0053, "Invalid system_type field"},
    {:RINVREPFLAG,         0x0054, "Invalid replace_if_present Flag"},
    {:RINVNUMMSGS,         0x0055, "Invalid number of messages"},
    {:RTHROTTLED,          0x0058, "Throttling error (ESME has exceeded allowed msg limits)"},
    {:RINVSCHED,           0x0061, "Invalid Scheduled Delivery Time"},
    {:RINVEXPIRY,          0x0062, "Invalid message validity period (Expiry time)"},
    {:RINVDFTMSGID,        0x0063, "Predefined Message Invalid or Not Found"},
    {:RX_T_APPN,           0x0064, "ESME Receiver Temporary App Err"},
    {:RX_P_APPN,           0x0065, "ESME Receiver Permanent App Err"},
    {:RX_R_APPN,           0x0066, "ESME Receiver Reject Message"},
    {:RQUERYFAIL,          0x0067, "query_sm request failed"},
    {:RINVTLVSTREAM,       0x00C0, "Error in the optional part of the PDU Body"},
    {:RTLVNOTALLWD,        0x00C1, "TLV not allowed"},
    {:RINVTLVLEN,          0x00C2, "Invalid Parameter Length"},
    {:RMISSINGTLV,         0x00C3, "Expected TLV missing"},
    {:RINVTLVVAL,          0x00C4, "Invalid TLV Value"},
    {:RDELIVERYFAILURE,    0x00FE, "Transaction Delivery Failure"},
    {:RUNKNOWNERR,         0x00FF, "Unknown Error"},
    {:RSERTYPUNAUTH,       0x0100, "ESME Not authorised to use specified service_type"},
    {:RPROHIBITED,         0x0101, "ESME Prohibited from using specified operation"},
    {:RSERTYPUNAVAIL,      0x0102, "Specified service_type is unavailable"},
    {:RSERTYPDENIED,       0x0103, "Specified service_type denied"},
    {:RINVDCS,             0x0104, "Invalid Data Coding Scheme"},
    {:RINVSRCADDRSUBUNIT,  0x0105, "Source Address Sub unit is invalid"},
    {:RINVDSTADDRSUBUNIT,  0x0106, "Destination Address Sub unit is invalid"},
    {:RINVBCASTFREQINT,    0x0107, "Broadcast Frequency Interval is invalid"},
    {:RINVBCASTALIAS_NAME, 0x0108, "Invalid Broadcast Alias Name"},
    {:RINVBCASTAREAFMT,    0x0109, "Invalid Broadcast Area Format"},
    {:RINVNUMBCAST_AREAS,  0x010A, "Number of Broadcast Areas is invalid"},
    {:RINVBCASTCNTTYPE,    0x010B, "Invalid Broadcast Content Type"},
    {:RINVBCASTMSGCLASS,   0x010C, "Broadcast Message Class is invalid"},
    {:RBCASTFAIL,          0x010D, "broadcast_sm operation failed"},
    {:RBCASTQUERYFAIL,     0x010E, "query_broadcast_sm failed"},
    {:RBCASTCANCELFAIL,    0x010F, "cancel_broadcast_sm failed"},
    {:RINVBCAST_REP,       0x0110, "Number of Repeated Broadcasts is invalid"},
    {:RINVBCASTSRVGRP,     0x0111, "Broadcast Service Group is invalid"},
    {:RINVBCASTCHANIND,    0x0112, "Broadcast Channel Indicator is invalid"},
    {:RDELIVERSMFAIL,      0x0601, "deliver_sm failed"},
  ]

  @type error_code :: integer
  @type error_name :: atom

  @spec code_by_name(error_name) :: error_code

  @doc """
  Converts atom representing SMPP error to its integer value.

  ## Example:

      iex(1)> SMPPEX.Pdu.Errors.code_by_name(:ROK)
      0

  """
  def code_by_name(name)

  @spec format(error_code) :: String.t

  @doc """
  Converts integer SMPP error code to string error identifier.

  ## Example

      iex(1)> SMPPEX.Pdu.Errors.format(0)
      "ROK"
      iex(2)> SMPPEX.Pdu.Errors.format(12345)
      "12345"

  """
  def format(code)

  @spec description(error_code) :: String.t

  @doc """
  Converts integer SMPP error code to text error description.

  ## Example

      iex(1)> SMPPEX.Pdu.Errors.description(0)
      "No Error"
      iex(2)> SMPPEX.Pdu.Errors.description(12345)
      "12345"

  """
  def description(error_code)

  Enum.each errors, fn({name, code, desc}) ->
    def code_by_name(unquote(name)), do: unquote(code)

    def format(unquote(code)), do: unquote(to_string(name))

    def description(unquote(code)), do: unquote(desc)
  end

  def format(code) when is_integer(code), do: to_string(code)
  def description(code) when is_integer(code), do: to_string(code)

end

