:ssl.start()

socket_opts = [
  certfile: 'test/support/ssl/localhost.crt',
  keyfile: 'test/support/ssl/cert.key',
  versions: [:'tlsv1.2',:'tlsv1.3'],
  log_level: :debug,
]

default_opts = [{:reuseaddr, true}]

full_opts = socket_opts ++ default_opts

{:ok, listen_socket} = :ssl.listen(33333, full_opts)

[nil]
|> Stream.cycle()
|> Enum.each(fn _ ->
  case :ssl.transport_accept(listen_socket) do
    {:ok, transport_socket} ->
      handshake_res = :ssl.handshake(transport_socket)
      IO.puts("handshake result:")
      IO.inspect(handshake_res)
    err ->
      IO.puts("accept error:")
      IO.inspect(err)
  end
end)
