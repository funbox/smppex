# Related projects

Besides the core `SMPPEX` project there are some auxilary projects.

## smppsend

[smppsend](https://github.com/funbox/smppsend) is a simple CLI utility
for testing SMSC connections. It allows to bind to SMSCs, send submit_sm
PDUs and wait for delivery reports.

## smppex_web

[smppex_web](https://github.com/savonarola/smppex_web) is a simple demonstrational MC (SMPP server)
with web interface.

A public installation is available online at:
* Web: [smppex.rubybox.ru](https://smppex.rubybox.ru).
* SMPP: `smppex.rubybox.ru:2775`

It is useful for manual preliminary testing of SMPP clients.

One can connect to the demonstrational MC with the mentioned `smppssend` tool:

```
./smppsend --host smppex.rubybox.ru --port 2775 --system-id systemid --password pass --bind-mode trx --submit-sm --source-addr from123 --source-addr-npi 1 --source-addr-ton 5 --destination-addr 99999999999 --dest-addr-npi 1 --dest-addr-ton 1 --short-message  HelloHelloHelloHelloHelloHelloHelloHelloHello --data-coding 8 --split-max-bytes 30 --udh-ref 123 --ucs2 --registered-delivery 1 --wait

19:57:33.677 [info]  Connecting to smppex.rubybox.ru:2775

19:57:33.749 [info]  Connected

19:57:33.753 [info]  Binding
pdu: bind_transceiver
  command_id: 9
  command_status: 0 (ok)
  sequence_number: 0
mandatory fields:
  addr_npi: nil
  addr_ton: nil
  address_range: nil
  interface_version: nil
  password: "pass"
  system_id: "systemid"
  system_type: nil
optional fields: []

...
```

## smpp_benchmarks

[smpp_benchmarks](https://github.com/savonarola/smpp_benchmarks) is a bunch of
benchmarks to run against SMPPEX.

## smppex_telemetry

[smppex_telemetry](https://github.com/savonarola/smppex_telemetry) is an advanced
implementation of `SMPPEX.Session` which emits [telemetry](https://github.com/beam-telemetry/telemetry) events.

## smppex_graceful

Sometimes it is important to shutdown one or many SMPP sessions "gracefully",
i.e. to send `unbind` PDU and wait for a response before disconnect.

[smppex_graceful](https://github.com/savonarola/smppex_graceful) contains an example
of how one can organize ESME sessions to achieve this.


