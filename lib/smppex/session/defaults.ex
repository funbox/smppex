defmodule SMPPEX.Session.Defaults do

  defaults = [
    enquire_link_limit: 30000,
    enquire_link_resp_limit: 30000,
    session_init_limit: 10000,
    inactivity_limit: :infinity,
    response_limit: 60000,
    timer_resolution: 100
  ]

  for {name, value} <- defaults do
    def unquote(name)() do
      unquote(value)
    end
  end
  
end
