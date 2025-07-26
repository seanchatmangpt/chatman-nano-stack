defmodule Ultrathink.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Database repo
      Ultrathink.Repo,
      
      # Telemetry
      UltrathinkWeb.Telemetry,
      
      # Registry for dynamic processes
      {Registry, keys: :unique, name: Ultrathink.Registry}
    ]

    opts = [strategy: :one_for_one, name: Ultrathink.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
