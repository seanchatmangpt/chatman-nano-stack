defmodule CNSForge.MixProject do
  use Mix.Project

  def project do
    [
      app: :cns_forge,
      version: "1.0.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      description: "CNS Forge - Ecosystem Composer using Ash/Reactor Architecture",
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      dialyzer: [
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"}
      ]
    ]
  end

  def application do
    [
      mod: {CNSForge.ProductionApplication, []},
      extra_applications: [:logger, :runtime_tools, :mnesia, :crypto]
    ]
  end
  
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Core dependencies for Ash.Reactor functionality
      {:ash, "~> 3.0"},
      {:reactor, "~> 0.9"},
      {:jason, "~> 1.4"},
      {:telemetry, "~> 1.2"},
      
      # Database and storage (without yamerl dependency)
      {:ecto, "~> 3.10"},
      {:ecto_sql, "~> 3.10"},
      
      # Testing and validation dependencies
      {:excoveralls, "~> 0.18", only: :test},
      {:ex_unit_notifier, "~> 1.3", only: :test},
      {:benchee, "~> 1.1", only: [:dev, :test]},
      {:benchee_html, "~> 1.0", only: [:dev, :test]},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false}
    ]
  end

  defp aliases do
    [
      # 80/20 APPROACH: Essential aliases only
      setup: ["deps.get"],
      test: ["test"],
      quality: ["format"]
    ]
  end
end