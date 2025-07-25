defmodule CnsForgeAshMinimal.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      CnsForgeAshMinimalWeb.Telemetry,
      {Phoenix.PubSub, name: CnsForgeAshMinimal.PubSub},
      CnsForgeAshMinimalWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: CnsForgeAshMinimal.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    CnsForgeAshMinimalWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end