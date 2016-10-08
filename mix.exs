defmodule Smppex.Mixfile do
  use Mix.Project

  def project do
    [
      app: :smppex,
      version: "0.1.6",
      elixir: "~> 1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      description: description,
      elixirc_paths: elixirc_paths(Mix.env),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        "coveralls": :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      package: package,
      dialyzer: [
        plt_file: ".local.plt",
        plt_add_deps: true,
        plt_add_apps: [:ssl],
        flags: ["-Werror_handling", "-Wrace_conditions"],
      ],
    ]
  end

  def application do
    [applications: [:logger, :ranch]]
  end

  defp deps do
    [
      {:excoveralls, "~> 0.5", only: :test},
      {:dialyxir, git: "https://github.com/jeremyjh/dialyxir.git", only: :dev},
      {:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:ranch, "~> 1.2"}
    ]
  end

  defp description do
    "SMPP 3.4 protocol and framework implemented in Elixir"
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      name: :smppex,
      files: ["lib", "mix.exs", "README*", "LICENSE"],
      maintainers: ["Ilya Averyanov"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/savonarola/smppex"
      }
    ]
  end


end
