defmodule Dashboard.MixProject do
  use Mix.Project

  def project do
    [
      app: :dashboard,
      version: "1.0.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      
      # Testing
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      
      # Documentation
      name: "CNS Dashboard",
      docs: [
        main: "readme",
        extras: ["README.md"]
      ]
    ]
  end

  def application do
    [
      mod: {Dashboard.Application, []},
      extra_applications: [:logger, :runtime_tools, :observer, :wx]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Phoenix Framework
      {:phoenix, "~> 1.7.10"},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_html, "~> 3.3"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.20.1"},
      
      # Database
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      
      # Telemetry & Monitoring
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:phoenix_live_dashboard, "~> 0.8.2"},
      {:opentelemetry, "~> 1.3"},
      {:opentelemetry_api, "~> 1.2"},
      {:opentelemetry_exporter, "~> 1.6"},
      {:opentelemetry_phoenix, "~> 1.1"},
      {:opentelemetry_ecto, "~> 1.1"},
      
      # JSON
      {:jason, "~> 1.2"},
      {:poison, "~> 5.0"},
      
      # HTTP Client
      {:httpoison, "~> 2.0"},
      {:hackney, "~> 1.18"},
      
      # Utilities
      {:plug_cowboy, "~> 2.5"},
      {:corsica, "~> 1.1"},
      {:uuid, "~> 1.1"},
      
      # BitActor Integration (local path)
      {:bitactor, path: "../bitactor_otp"},
      
      # Testing
      {:excoveralls, "~> 0.18", only: :test},
      {:mox, "~> 1.0", only: :test},
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:stream_data, "~> 0.6", only: :test}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.setup": ["cmd --cd assets npm install"],
      "assets.build": ["cmd --cd assets npm run build"],
      "assets.deploy": ["assets.build", "phx.digest"]
    ]
  end
end