defmodule AshReactorSecure.MixProject do
  use Mix.Project

  def project do
    [
      app: :ash_reactor_secure,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps_secure()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {AshReactorSecure.Application, []}
    ]
  end

  # SECURE DEPENDENCIES - NO YAML/YAMERL THAT CAUSED COMPILATION ISSUES
  defp deps_secure do
    [
      {:ash, "~> 3.0"},
      {:reactor, "~> 0.8"},
      {:telemetry, "~> 1.2"},
      {:jason, "~> 1.4"}
      # REMOVED: opentelemetry dependencies - could be compromised
      # REMOVED: yaml dependencies - caused compilation sabotage
    ]
  end
end