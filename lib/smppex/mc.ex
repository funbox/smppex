defmodule SMPPEX.MC do

  alias :ranch, as: Ranch

  @default_transport :ranch_tcp
  @default_acceptor_count 50

  @spec start({module, args :: term}, opts :: Keyword.t) :: {:ok, listener_ref :: Ranch.ref} | {:error, reason :: term}

  def start({_module, _args} = mod_with_args, opts \\ []) do
    acceptor_count = Keyword.get(opts, :acceptor_count, @default_acceptor_count)
    transport = Keyword.get(opts, :transport, @default_transport)
    transport_opts = Keyword.get(opts, :transport_opts, [{:port, 0}])
    mc_opts = Keyword.get(opts, :mc_opts)
    ref = make_ref()

    start_result = Ranch.start_listener(
      ref,
      acceptor_count,
      transport,
      transport_opts,
      TransportSession,
      {Session, [mod_with_args, mc_opts]}
    )

    case start_result do
      {:error, _} = error -> error
      {:ok, _, _} -> {:ok, ref}
      {:ok, _} -> {:ok, ref}
    end

  end

  @spec stop(Ranch.ref) :: :ok

  @doc """
  Stops MC listener and all its sessions.
  """

  def stop(listener) do
    Ranch.stop_listener(listener)
  end

end
