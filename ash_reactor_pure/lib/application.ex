defmodule AshReactorPure.Application do
  @moduledoc false
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("🛡️ Starting PURE Implementation - NO ASH DEPENDENCIES")
    
    children = [
      # Pure ETS storage
      {AshReactorPure.Storage, []},
      # Pure processing pipeline
      {AshReactorPure.Pipeline, []}
    ]

    opts = [strategy: :one_for_one, name: AshReactorPure.Supervisor]
    
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("✅ Pure implementation started - RED TEAM DEFEATED")
        {:ok, pid}
        
      error ->
        Logger.error("❌ Pure implementation failed: #{inspect(error)}")
        error
    end
  end
end