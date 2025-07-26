defmodule Cybersecurity.MixProject do
  use Mix.Project

  def project do
    [
      app: :cybersecurity,
      version: "0.1.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  def application do
    [
      mod: {Cybersecurity.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # 80/20: Minimal working dependencies (avoiding rebar3/yaml issues)
      {:jason, "~> 1.4"},
      {:uuid, "~> 1.1"}
    ]
  end

  defp aliases do
    [
      # 80/20: Essential aliases only
      setup: ["deps.get"],
      test: ["test"]
    ]
  end
end
