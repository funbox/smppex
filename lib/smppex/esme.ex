defmodule SMPPEX.ESME do

  alias SMPPEX.Session
  alias SMPPEX.TransportSession

  @default_transport :ranch_tcp
  @default_timeout 5000

  @spec start_link(host :: term, port :: non_neg_integer, {module, args :: term}, opts :: Keyword.t) :: GenServer.on_start

  def start_link(host, port, {_module, _args} = mod_with_args, opts \\ []) do
    transport = Keyword.get(opts, :transport, @default_transport)
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    sock_opts = [:binary, {:packet, 0}, {:active, :once}]
    esme_opts = Keyword.get(opts, :esme_opts, [])

    case transport.connect(convert_host(host), port, sock_opts, timeout) do
      {:ok, socket} ->
        session_opts = {Session, [mod_with_args, esme_opts]}
        case TransportSession.start_link(socket, transport, session_opts) do
          {:ok, pid} ->
            {:ok, pid}
          {:error, _} = error ->
            transport.close(socket)
            error
        end
      {:error, _} = error -> error
    end
  end

  defp convert_host(host) when is_binary(host), do: to_charlist(host)
  defp convert_host(host), do: host

end
