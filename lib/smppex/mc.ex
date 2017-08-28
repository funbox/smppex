defmodule SMPPEX.MC do
  @moduledoc """
  This is a module for launching a TCP listener (or any other listener supported
  by `ranch`, for example, `ssl`) which handles incoming connections with the
  passed `SMPPEX.Session` implementations.

  To start an MC one generally should do the following.

  1. Implement an `SMPPEX.Session` behaviour.

  ```elixir

  defmodule MyMCSession do
    use SMPPEX.Session

    # ...Callback implementation

  end

  ```

  2. Start a listener passing implemented behaviour as a callback module.

  ```elixir

  {:ok, listener} = SMPPEX.MC.start({MyESMESession, some_args},
                                    transport_opts: [port: 2775])
  ```

  The important things to note are:
  * There is no `start_link` method, since started listener is not a standalone
  `GenServer` but a pool of socket acceptors running under `Ranch` supervisor.
  * Each received connection is served with its own process which uses passed
  callback module (`MyESMESession`) for handling connection events.
  """

  alias :ranch, as: Ranch

  @default_transport :ranch_tcp
  @default_acceptor_count 50

  @spec start({module, args :: term}, opts :: Keyword.t) ::
    {:ok, listener_ref :: Ranch.ref} |
    {:error, reason :: term}

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
      SMPPEX.TransportSession,
      {SMPPEX.Session, [mod_with_args, mc_opts]}
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
