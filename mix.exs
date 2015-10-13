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
       flags: ["-Wunmatched_returns", "-Werror_handling", "-Wrace_conditions"]
     ]
   ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      {:coverex, "~> 1.4.1", only: :test},
      {:dialyxir, git: "https://github.com/jeremyjh/dialyxir.git" }
    ]
  end
end
