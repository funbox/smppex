defmodule Smppex.Mixfile do
  use Mix.Project

  def project do
    [
      app: :smppex,
      version: "3.0.0",
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      source_url: "https://github.com/funbox/smppex",
      deps: deps(),
      description: description(),
      elixirc_paths: elixirc_paths(Mix.env()),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      package: package(),
      dialyzer: [
        plt_add_deps: true,
        plt_add_apps: [:ssl],
        flags: ["-Werror_handling", "-Wrace_conditions"]
      ]
    ]
  end

  def application do
    [applications: [:logger, :ranch]]
  end

  def ex_doc_version() do
    version = System.version()

    cond do
      version |> Version.match?(">= 1.10.0") ->
        "~> 0.23"

      version |> Version.match?(">= 1.7.0") ->
        "~> 0.22.0"

      true ->
        "~> 0.18.0"
    end
  end

  defp deps do
    [
      {:excoveralls, "~> 0.5", only: :test},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:earmark, "~> 1.4", only: :dev},
      {:ex_doc, ex_doc_version(), only: :dev},
      {:credo, "~> 1.5", only: [:dev, :test], runtime: false},
      {:poison, "~> 3.0", only: :test},
      {:ranch, "~> 2.0"}
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
        "GitHub" => "https://github.com/funbox/smppex"
      }
    ]
  end
end
