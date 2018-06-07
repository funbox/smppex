defmodule SMPPEX.Pdu.NetworkErrorCode do
  @moduledoc """
  Module for operating with `deliver_sm` network_error_code parameter.
  """

  @type type_code :: pos_integer()
  @type error_code :: pos_integer()
  @type network_error_code :: <<_::24>>

  @spec encode(type_code, error_code) :: network_error_code

  @doc """
  Converts network_error_code type and error to octet string

  ## Example

      iex(1)> SMPPEX.Pdu.NetworkErrorCode.encode(8,1)
      <<08,00,01>>
  """
  def encode(type_code, error_code) when type_code < 256 and error_code < 65_536 do
    <<type_code::size(8), error_code::size(16)>>
  end

  @spec decode(network_error_code) :: {type_code, error_code}

  @doc """
  Converts octet_string from network_error_code tag to type_code and error_value

  ## Example

      iex(1)> SMPPEX.Pdu.NetworkErrorCode.decode(<<08,00,01>>)
      {8, 1}

  """
  def decode(<<type_code::size(8), error_code::size(16)>>) do
    {type_code, error_code}
  end
end
