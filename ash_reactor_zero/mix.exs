defmodule AshReactorZero.MixProject do
  use Mix.Project

  def project do
    [
      app: :ash_reactor_zero,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: []  # ZERO DEPENDENCIES - RED TEAM CANNOT COMPROMISE WHAT ISN'T THERE
    ]
  end

  def application do
    [
      extra_applications: [:logger]
      # NO DEPENDENCIES, NO COMPROMISED PACKAGES
    ]
  end
end