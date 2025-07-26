defmodule AshReactor80_20.MixProject do
  use Mix.Project

  def project do
    [
      app: :ash_reactor_80_20,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [
        ash_reactor_80_20: [
          include_executables_for: [:unix],
          applications: [runtime_tools: :permanent]
        ]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger, :runtime_tools],
      mod: {AshReactor80_20.Application, []}
    ]
  end

  defp deps do
    [
      {:ash, "~> 3.0"},
      {:reactor, "~> 0.8"},
      {:telemetry, "~> 1.2"},
      {:opentelemetry, "~> 1.3"},
      {:opentelemetry_exporter, "~> 1.6"},
      {:opentelemetry_api, "~> 1.2"},
      {:jason, "~> 1.4"}
    ]
  end
end