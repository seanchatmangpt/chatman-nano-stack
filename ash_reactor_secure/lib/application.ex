defmodule AshReactorSecure.Application do
  @moduledoc false
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("🛡️ Starting SECURE Ash.Reactor - Anti Red Team")
    
    children = [
      # Basic telemetry only - no external collectors
      {Task.Supervisor, name: AshReactorSecure.TaskSupervisor}
    ]

    opts = [strategy: :one_for_one, name: AshReactorSecure.Supervisor]
    
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("✅ Secure Ash.Reactor started successfully")
        {:ok, pid}
        
      error ->
        Logger.error("❌ Failed to start secure application: #{inspect(error)}")
        error
    end
  end
end