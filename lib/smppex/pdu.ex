defmodule SMPPEX.Pdu do
  @moduledoc """
  Module for working with Pdu struct representing parsed SMPP PDU
  """

  alias SMPPEX.Protocol.TlvFormat
  alias SMPPEX.Protocol.CommandNames
  alias SMPPEX.Pdu

  @type t :: %Pdu{
    command_id: non_neg_integer,
    command_status: non_neg_integer,
    sequence_number: non_neg_integer,
    ref: reference,
    mandatory: map,
    optional: map
  }

  defstruct [
    command_id: 0,
    command_status: 0,
    sequence_number: 0,
    ref: nil,
    mandatory: %{},
    optional: %{},
  ]

  @type header :: {non_neg_integer, non_neg_integer, non_neg_integer} | non_neg_integer

  @spec new(header, map, map) :: t

  @doc """
  Construct a new Pdu from header, mandatory fields and optional(TLV) fields.

  Header may be either an integer, then it is treated as command id,
  or a tuple `{command_id, command_status, sequence_number}`

  Each Pdu is created with a unique ref field, by which one can later
  trace Pdu's identity.

  ## Examples

      iex(1)> SMPPEX.Pdu.new(1)
      %SMPPEX.Pdu{command_id: 1, command_status: 0, mandatory: %{}, optional: %{},
      ref: #Reference<0.0.3.215>, sequence_number: 0}
      iex(2)> SMPPEX.Pdu.new({1, 0, 123}, %{system_id: "sid", password: "pass"}, %{})
      %SMPPEX.Pdu{command_id: 1, command_status: 0,
      mandatory: %{password: "pass", system_id: "sid"}, optional: %{},
      ref: #Reference<0.0.3.219>, sequence_number: 123}


  """
  def new(header, mandatory_fields \\ %{}, optional_fields \\ %{}) do
    case header do
      c_id when is_integer(c_id) ->
        %Pdu{
          command_id: c_id,
          ref: make_ref,
          mandatory: mandatory_fields,
          optional: optional_fields
        }
      {c_id, c_status, s_number} ->
        %Pdu{
          command_id: c_id,
          command_status: c_status,
          sequence_number: s_number,
          ref: make_ref,
          mandatory: mandatory_fields,
          optional: optional_fields
        }
    end
  end

  @spec command_name(t) :: atom

  @doc """
  Returns Pdu's symbolic command name as an atom or `:unknown` if Pdu's `command_id`
  do not correspond to any real SMPP command.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(1)
      iex(2)> SMPPEX.Pdu.command_name(pdu)
      :bind_receiver
      iex(3)> pdu = SMPPEX.Pdu.new(1111111)
      iex(4)> SMPPEX.Pdu.command_name(pdu)
      :unknown

  """

  def command_name(pdu) do
    case CommandNames.name_by_id(pdu.command_id) do
      {:ok, name} -> name
      :unknown -> :unknown
    end
  end

  @spec command_id(t) :: non_neg_integer

  @doc """
  Returns Pdu's `command_id`.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(1)
      iex(2)> SMPPEX.Pdu.command_id(pdu)
      1

  """

  def command_id(pdu) do
    pdu.command_id |> to_int
  end

  @spec command_status(t) :: non_neg_integer

  @doc """
  Returns Pdu's `command_status`.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new({1, 4, 123})
      iex(2)> SMPPEX.Pdu.command_status(pdu)
      4

  """

  def command_status(pdu) do
    pdu.command_status |> to_int
  end

  @spec sequence_number(t) :: non_neg_integer

  @doc """
  Returns Pdu's `sequence_number`.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new({1, 4, 123})
      iex(2)> SMPPEX.Pdu.sequence_number(pdu)
      123

  """

  def sequence_number(pdu) do
    pdu.sequence_number |> to_int
  end

  @spec mandatory_field(t, atom) :: term

  @doc """
  Get Pdu mandatory field. If Pdu does not have the field, `nil` is returned.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new({1, 4, 123}, %{system_id: "system_id"})
      iex(2)> SMPPEX.Pdu.mandatory_field(pdu, :system_id)
      "system_id"
      iex(3)> SMPPEX.Pdu.mandatory_field(pdu, :short_message)
      nil

  """

  def mandatory_field(pdu, name) when is_atom(name) do
    pdu.mandatory[name]
  end

  @spec set_mandatory_field(t, atom, any) :: t

  @doc """
  Sets Pdu mandatory field. New Pdu is returned.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new({1, 4, 123}, %{system_id: "system_id"})
      iex(2)> pdu1 = SMPPEX.Pdu.set_mandatory_field(pdu, :password, "pass")
      iex(3)> SMPPEX.Pdu.mandatory_field(pdu1, :password)
      "pass"

  """

  def set_mandatory_field(pdu, name, value) do
    %Pdu{pdu | mandatory: Map.put(pdu.mandatory, name, value)}
  end

  @spec optional_field(t, integer | atom) :: any

  @doc """
  Get Pdu optional(TLV) field by name or by integer id. If Pdu does not have the
  field or field name is unknown, `nil` is returned.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{}, %{0x0424 => "hello"})
      iex(2)> SMPPEX.Pdu.optional_field(pdu, :message_payload)
      "hello"
      iex(3)> SMPPEX.Pdu.optional_field(pdu, 0x0424)
      "hello"
      iex(4)> SMPPEX.Pdu.optional_field(pdu, :receipted_message_id)
      nil
      iex(5)> SMPPEX.Pdu.optional_field(pdu, :unknown_tlv_name)
      nil

  """

  def optional_field(pdu, id) when is_integer(id) do
    case Map.has_key?(pdu.optional, id) do
      true -> pdu.optional[id]
      false -> case TlvFormat.name_by_id(id) do
        {:ok, name} -> pdu.optional[name]
        :unknown -> nil
      end
    end
  end

  def optional_field(pdu, name) when is_atom(name) do
    case Map.has_key?(pdu.optional, name) do
      true -> pdu.optional[name]
      false -> case TlvFormat.id_by_name(name) do
        {:ok, id} -> pdu.optional[id]
        :unknown -> nil
      end
    end
  end

  @spec set_optional_field(t, atom, any) :: t

  @doc """
  Sets Pdu optional field. New Pdu is returned.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4)
      iex(2)> pdu1 = SMPPEX.Pdu.set_optional_field(pdu, :message_payload, "hello")
      iex(3)> SMPPEX.Pdu.optional_field(pdu1, 0x0424)
      "hello"

  """

  def set_optional_field(pdu, name, value) do
    %Pdu{pdu | optional: Map.put(pdu.optional, name, value)}
  end

  @spec field(t, integer | atom) :: any

  @doc """
  Get Pdu mandatory or optional(TLV) field by name or by integer id. If Pdu does not have the
  field or field name is unknown, `nil` is returned.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{short_message: "hi"}, %{0x0424 => "hello"})
      iex(2)> SMPPEX.Pdu.field(pdu, :message_payload)
      "hello"
      iex(3)> SMPPEX.Pdu.field(pdu, 0x0424)
      "hello"
      iex(4)> SMPPEX.Pdu.field(pdu, :short_message)
      "hi"
      iex(5)> SMPPEX.Pdu.field(pdu, :unknown_name)
      nil

  """

  def field(pdu, id) when is_integer(id) do
    optional_field(pdu, id)
  end
  def field(pdu, name) do
    mandatory_field(pdu, name) || optional_field(pdu, name)
  end

  @spec optional_fields(t) :: map

  @doc """
  Get the whole set of optional(TLV) fields as a map.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{short_message: "hi"}, %{0x0424 => "hello"})
      iex(2)> SMPPEX.Pdu.optional_fields(pdu)
      %{0x0424 => "hello"}

  """

  def optional_fields(pdu), do: pdu.optional

  @spec mandatory_fields(t) :: map

  @doc """
  Get the whole set of mandatory fields as a map.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{short_message: "hi"}, %{0x0424 => "hello"})
      iex(2)> SMPPEX.Pdu.mandatory_fields(pdu)
      %{short_message: "hi"}

  """

  def mandatory_fields(pdu), do: pdu.mandatory

  @spec same?(t, t) :: boolean

  @doc """
  Checks if two Pdus are copies of the same Pdu.

  ## Examples

      iex(1)> pdu1 = SMPPEX.Pdu.new(4)
      iex(2)> pdu2 = SMPPEX.Pdu.new(4)
      iex(3)> SMPPEX.Pdu.same?(pdu1, pdu2)
      false
      iex(4)> SMPPEX.Pdu.same?(pdu1, pdu1)
      true

  """

  def same?(pdu1, pdu2), do: pdu1.ref != nil and pdu2.ref != nil and pdu1.ref == pdu2.ref

  @spec resp?(t) :: boolean

  @doc """
  Checks if Pdu is a response Pdu.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4)
      iex(2)> SMPPEX.Pdu.resp?(pdu)
      false
      iex(3)> pdu = SMPPEX.Pdu.new(0x80000004)
      iex(4)> SMPPEX.Pdu.resp?(pdu)
      true

  """

  def resp?(pdu), do: command_id(pdu) > 0x80000000

  @spec success_resp?(t) :: boolean

  @doc """
  Checks if Pdu is a successful response Pdu.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new({0x80000004, 1, 0})
      iex(2)> SMPPEX.Pdu.success_resp?(pdu)
      false
      iex(3)> pdu = SMPPEX.Pdu.new({0x80000004, 0, 0})
      iex(4)> SMPPEX.Pdu.success_resp?(pdu)
      true

  """

  def success_resp?(pdu) do
    resp?(pdu) and command_status(pdu) == 0
  end

  @spec bind?(t) :: boolean

  @doc """
  Checks if Pdu is a bind request.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4)
      iex(2)> SMPPEX.Pdu.bind?(pdu)
      false
      iex(3)> pdu = SMPPEX.Pdu.new(1)
      iex(4)> SMPPEX.Pdu.bind?(pdu)
      true

  """

  def bind?(pdu) do
    command_id = Pdu.command_id(pdu)
    case CommandNames.name_by_id(command_id) do
      {:ok, command_name} -> command_name == :bind_receiver or command_name == :bind_transmitter or command_name == :bind_transceiver
      :unknown -> false
    end
  end

  @spec bind_resp?(t) :: boolean

  @doc """
  Checks if Pdu is a bind response.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(0x80000004)
      iex(2)> SMPPEX.Pdu.bind_resp?(pdu)
      false
      iex(3)> pdu = SMPPEX.Pdu.new(0x80000001)
      iex(4)> SMPPEX.Pdu.bind_resp?(pdu)
      true

  """

  def bind_resp?(pdu) do
    command_id = Pdu.command_id(pdu)
    case CommandNames.name_by_id(command_id) do
      {:ok, command_name} -> command_name == :bind_receiver_resp or command_name == :bind_transmitter_resp or command_name == :bind_transceiver_resp
      :unknown -> false
    end
  end

  @type addr :: {String.t, byte, byte}

  @spec source(t) :: addr

  @doc """
  Returns Pdu's `:source_addr`, `:source_addr_ton` and `:source_addr_npi` fields
  in a tuple.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{source_addr: "from", source_addr_ton: 1, source_addr_npi: 2})
      iex(2)> SMPPEX.Pdu.source(pdu)
      {"from", 1, 2}

  """

  def source(pdu) do
    fields_as_tuple(pdu, [:source_addr, :source_addr_ton, :source_addr_npi])
  end

  @spec dest(t) :: addr

  @doc """
  Returns Pdu's `:destination_addr`, `:dest_addr_ton` and `:dest_addr_npi` fields
  in a tuple.

  ## Examples

      iex(1)> pdu = SMPPEX.Pdu.new(4, %{destination_addr: "to", dest_addr_ton: 1, dest_addr_npi: 2})
      iex(2)> SMPPEX.Pdu.dest(pdu)
      {"to", 1, 2}

  """

  def dest(pdu) do
    fields_as_tuple(pdu, [:destination_addr, :dest_addr_ton, :dest_addr_npi])
  end

  defp fields_as_tuple(pdu, fields) do
    fields
    |> Enum.map(fn field_name -> Pdu.field(pdu, field_name) end)
    |> List.to_tuple
  end

  defp to_int(val) when is_integer(val), do: val
  defp to_int(_), do: 0

end
