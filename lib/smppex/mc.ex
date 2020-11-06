defmodule SMPPEX.MC do
  @moduledoc """
  This is a module for launching a TCP listener (or any other listener supported by `ranch`, for example, `ssl`) which handles incoming connections with the passed `SMPPEX.Session` implementations.

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
  * Each received connection is served with its own process which uses passed callback module (`MyESMESession`) for handling connection events. Each process has his own state initialized by `init` callback receiving `socket`, `transport` and a copy of arguments (`some_args`).
  """

  alias :ranch, as: Ranch

  alias SMPPEX.Session.Defaults

  @default_transport :ranch_tcp
  @default_acceptor_count 50

  @spec start({module, args :: term}, opts :: Keyword.t()) ::
          {:ok, listener_ref :: Ranch.ref()}
          | {:error, reason :: term}

  @doc """
  Starts listener for MC entitiy.

  `module` is the callback module which should implement `SMPPEX.Session` behaviour.
  `args` is the argument passed to the `init` callback each time a new connection is received.
  `opts` is a keyword list of different options:
  * `:transport` is Ranch transport used for TCP connections: either `ranch_tcp` (the default) or `ranch_ssl`;
  * `:transport_opts` is a list of Ranch transport options. The major option is `{:port, port}`. The port is set to `0` by default, which means that the listener will accept connections on a random free port.
  * `:acceptor_count` is the number of Ranch listener acceptors, #{@default_acceptor_count} by default.
  * `:mc_opts` is a keyword list of MC options:
      - `:timer_resolution` is interval of internal `ticks` on which time related events happen, like checking timeouts for pdus, checking SMPP timers, etc. The default is #{
    inspect(Defaults.timer_resolution())
  } ms;
      - `:session_init_limit` is the maximum time for which a session waits an incoming bind request. If no bind request is received within this interval of time, the session stops. The default value is #{
    inspect(Defaults.session_init_limit())
  } ms;
      - `:enquire_link_limit` is value for enquire_link SMPP timer, i.e. the interval of SMPP session inactivity after which enquire_link PDU is send to "ping" the connetion. The default value is #{
    inspect(Defaults.enquire_link_limit())
  } ms;
      - `:enquire_link_resp_limit` is the maximum time for which a session waits for enquire_link PDU response. If the response is not received within this interval of time and no activity from the peer occurs, the session is then considered dead and the session stops. The default value is #{
    inspect(Defaults.enquire_link_resp_limit())
  } ms;
      - `:inactivity_limit` is the maximum time for which a peer is allowed not to send PDUs (which are not response PDUs). If no such PDUs are received within this interval of time, the session stops. The default is #{
    inspect(Defaults.inactivity_limit())
  } ms;
      - `:response_limit` is the maximum time to wait for a response for a previously sent PDU. If the response is not received within this interval, `handle_resp_timeout` callback is triggered for the original pdu. If the response is received later, it is discarded. The default value is #{
    inspect(Defaults.response_limit())
  } ms.
      - `:default_call_timeout` is an integer greater than zero which specifies how many milliseconds to wait for a reply, or the atom :infinity to wait indefinitely.If no reply is received within the specified time, the function call fails and the caller exits. The default value is #{
    inspect(Defaults.default_call_timeout())
  } ms.
  If `:mc_opts` list of options is ommited, all options take their default values.
  The returned value is either `{:ok, ref}` or `{:error, reason}`. The `ref` can be later used
  to stop the whole MC listener and all sessions received by it.
  """
  def start({_module, _args} = mod_with_args, opts \\ []) do
    acceptor_count = Keyword.get(opts, :acceptor_count, @default_acceptor_count)
    transport = Keyword.get(opts, :transport, @default_transport)
    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [{:port, 0}])
      |> normalize_transport_opts(acceptor_count)

    mc_opts = Keyword.get(opts, :mc_opts, [])
    ref = make_ref()

    start_result =
      Ranch.start_listener(
        ref,
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

  defp normalize_transport_opts(opts, acceptor_count) when is_list(opts) do
    %{num_acceptors: acceptor_count, socket_opts: opts}
  end

  defp normalize_transport_opts(opts, acceptor_count) when is_map(opts) do
    Map.put_new(opts, :num_acceptors, acceptor_count)
  end

  @spec stop(Ranch.ref()) :: :ok

  @doc """
  Stops MC listener and all its sessions.
  """

  def stop(listener) do
    Ranch.stop_listener(listener)
  end
end
