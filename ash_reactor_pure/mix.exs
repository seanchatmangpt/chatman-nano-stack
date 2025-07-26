defmodule AshReactorPure.MixProject do
  use Mix.Project

  def project do
    [
      app: :ash_reactor_pure,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps_minimal()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {AshReactorPure.Application, []}
    ]
  end

  # MINIMAL DEPENDENCIES - NO ASH, NO YAML, NO COMPROMISED PACKAGES
  defp deps_minimal do
    [
      {:telemetry, "~> 1.2"},
      {:jason, "~> 1.4"}
      # NO ASH - avoiding compromised yamerl dependency
      # NO REACTOR - building pure implementation
    ]
  end
end