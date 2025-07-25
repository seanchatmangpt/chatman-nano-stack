defmodule CnsForgeAshWeb.Router do
  use CnsForgeAshWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  # CNS Forge API routes
  scope "/api/cns-forge", CnsForgeAshWeb do
    pipe_through :api

    # Core CNS Forge endpoints
    post "/register-user", CnsForgeController, :register_user
    get "/workflow-status/:transaction_id", CnsForgeController, :workflow_status
    get "/health", CnsForgeController, :health
    post "/ttl-demo", CnsForgeController, :ttl_demo
  end
end
