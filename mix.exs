defmodule CNSForge.MixProject do
  use Mix.Project

  def project do
    [
      app: :cns_forge,
      version: "1.0.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      description: "CNS Forge - Ecosystem Composer using Ash/Reactor Architecture"
    ]
  end

  def application do
    [
      mod: {CNSForge.Application, []},
      extra_applications: [:logger, :mnesia, :crypto]
    ]
  end

  defp deps do
    [
      # Ash Framework ecosystem
      {:ash, "~> 3.0"},
      {:ash_postgres, "~> 2.0"},  # Optional: for production PostgreSQL
      {:reactor, "~> 0.8"},       # Reactor for workflow orchestration
      
      # Phoenix for HTTP ingress
      {:phoenix, "~> 1.7.0"},
      {:phoenix_html, "~> 4.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.20.0"},
      {:phoenix_live_dashboard, "~> 0.8.0"},
      
      # Telemetry and observability
      {:telemetry, "~> 1.0"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      
      # JSON handling
      {:jason, "~> 1.2"},
      
      # Development and testing
      {:credo, "~> 1.6", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.27", only: :dev, runtime: false}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind default", "esbuild default"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end