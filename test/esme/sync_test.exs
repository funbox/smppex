defmodule SMPPEX.ESME.SyncTest do
  use ExUnit.Case

  alias SMPPEX.MC
  alias SMPPEX.Pdu
  alias SMPPEX.ESME.Sync, as: ESMESync

  setup do
    {:ok, callback_agent} = Agent.start_link(fn -> [] end)

    callbacks = fn ->
      Agent.get(
        callback_agent,
        &Enum.reverse(&1)
      )
    end

    mc_opts = [
      enquire_link_limit: 1000,
      session_init_limit: :infinity,
      enquire_link_resp_limit: 1000,
      inactivity_limit: 10000,
      response_limit: 2000,
      timer_resolution: 100000
    ]

    port = Support.TCP.Helpers.find_free_port

    mc_with_opts = fn(handler, opts) ->
      case MC.start(
        {Support.Session, {callback_agent, handler}},
        [
          transport_opts: [port: port],
          mc_opts: opts
        ]
      ) do
        {:ok, ref} -> ref
        other -> other
      end
    end

    mc = & mc_with_opts.(&1, mc_opts)

    esme = fn ->
      {:ok, pid} = SMPPEX.ESME.Sync.start_link("127.0.0.1", port)
      pid
    end

    {:ok, mc: mc, mc_with_opts: mc_with_opts, callbacks: callbacks, port: port, esme: esme}
  end

  test "request and response", ctx do
    ctx[:mc].(fn
      {:init, _, _}, st ->
        {:ok, st}
      {:handle_pdu, pdu}, st ->
        {:ok, [SMPPEX.Pdu.Factory.bind_transmitter_resp(0, "sid") |> Pdu.as_reply_to(pdu)], st}
      {:handle_send_pdu_result, _, _}, st -> st
    end)

    esme = ctx[:esme].()
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    assert {:ok, resp} = ESMESync.request(esme, pdu)
    assert :bind_transmitter_resp == Pdu.command_name(resp)
  end

  test "request and stop", ctx do
    Process.flag(:trap_exit, true)

    ctx[:mc].(fn
      {:init, _, _}, st ->
        {:ok, st}
      {:handle_pdu, _pdu}, st ->
        {:stop, :oops, st}
      {:handle_send_pdu_result, _, _}, st -> st
    end)

    esme = ctx[:esme].()
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    assert :stop = ESMESync.request(esme, pdu)
  end

  test "request and timeout", ctx do
    Process.flag(:trap_exit, true)

    ctx[:mc].(fn
      {:init, _, _}, st ->
        {:ok, st}
      {:handle_pdu, _pdu}, st ->
        {:ok, st}
    end)

    esme = ctx[:esme].()
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "password")
    assert :timeout = ESMESync.request(esme, pdu, 50)
  end

  test "request and error", ctx do
    Process.flag(:trap_exit, true)

    ctx[:mc].(fn
      {:init, _, _}, st ->
        {:ok, st}
      {:handle_pdu, _pdu}, st ->
        {:ok, st}
    end)

    esme = ctx[:esme].()
    pdu = SMPPEX.Pdu.Factory.bind_transmitter("system_id", "too_long_password")
    assert {:error, _} = ESMESync.request(esme, pdu)
  end

end
