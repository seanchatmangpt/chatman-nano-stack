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
      # 80/20 APPROACH: Absolute minimal dependencies to avoid compilation issues
      # Focus on core business logic without framework overhead
      
      {:jason, "~> 1.4"},  # JSON handling
      {:telemetry, "~> 1.2"}  # Basic telemetry
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