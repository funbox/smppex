defmodule Smppex.Mixfile do
  use Mix.Project

  def project do
    [app: :smppex,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps,
     elixirc_paths: ["lib", "test/support"],
     test_coverage: [tool: Coverex.Task],
     dialyzer: [
       flags: ["-Werror_handling", "-Wrace_conditions"]
     ]
   ]
  end

  def application do
    [applications: [:logger, :ranch]]
  end

  defp deps do
    [
      {:coverex, "~> 1.4.1", only: :test},
      {:dialyxir, git: "https://github.com/jeremyjh/dialyxir.git", only: :dev},
      {:credo, "~> 0.1.0", only: :dev},
      {:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:ranch, "~> 1.2"}
    ]
  end
end
