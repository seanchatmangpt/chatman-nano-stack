defmodule DashboardWeb.Router do
  use DashboardWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {DashboardWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", DashboardWeb do
    pipe_through :browser

    # Dashboard routes (generated at 2025-07-25T13:12:51.465883)
    live "/", MissionControlLive
    live "/mission-control", MissionControlLive
    live "/bitactor-performance", BitactorPerformanceLive
    live "/forge-factory", ForgeFactoryLive
    live "/semantic", SemanticLive
    live "/business", BusinessLive
    live "/security", SecurityLive
    live "/operations", OperationsLive
    
    # Health check
    get "/health", HealthController, :health
    get "/ready", HealthController, :ready
  end

  scope "/api", DashboardWeb do
    pipe_through :api
    
    # API routes for external integrations
    get "/metrics", MetricsController, :metrics
    get "/status", StatusController, :status
  end
end