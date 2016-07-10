[![Build Status](https://travis-ci.org/savonarola/smppex.svg?branch=master)](https://travis-ci.org/savonarola/smppex)

Smppex
======

SMPP 3.4 protocol and framework implementation in [Elixir](http://elixir-lang.org)

Under heavy construction.

Running benchmarks
------------------

In asynchronous mode with specified window:

    mix run benchmarks/async.exs 33333 100000 100

    17:35:56.420 [info]  Starting MC on port 33333

    17:35:56.481 [info]  Starting ESME with window 100

    17:35:56.516 [info]  Sending 100000 PDUs...

    17:36:04.205 [info]  All PDUs sent, all resps received, terminating

    17:36:04.205 [info]  ESME stopped

    17:36:04.207 [info]  Completed in 7688ms with avg rate 13007.284079084287 pdu/s

    17:36:04.214 [info]  mc_conn #PID<0.193.0>, socket closed, stopping

In synchronous mode:

    mix run benchmarks/sync.exs 33333 100000

    17:36:12.682 [info]  Starting MC on port 33333

    17:36:12.749 [info]  Starting synchronous ESME

    17:36:12.799 [info]  Sending 100000 PDUs...

    17:36:28.489 [info]  Completed in 15688ms with avg rate 6374.298827129016 pdu/s
