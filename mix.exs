defmodule Smppex.Mixfile do
  use Mix.Project

  def project do
    [
      app: :smppex,
      version: "0.1.0",
      elixir: "~> 1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      description: description,
      elixirc_paths: ["lib", "test/support"],
      test_coverage: [tool: Coverex.Task],
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
      {:coverex, "~> 1.4.1", only: :test},
      {:dialyxir, git: "https://github.com/jeremyjh/dialyxir.git", only: :dev},
      {:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:ranch, "~> 1.2"},
      {:dye, "~> 0.4.0"}
    ]
  end

  defp description do
    "SMPP 3.4 protocol and framework implemented in Elixir"
  end

  defp package do
    [
      name: :smppex,
      files: ["lib", "mix.exs", "README*", "LICENSE"],
      maintainers: ["Ilya Averyanov"],
      licenses: ["Apache 2.0"],
      links: %{
        "GitHub" => "https://github.com/savonarola/smppex"
      }
    ]
  end


end
