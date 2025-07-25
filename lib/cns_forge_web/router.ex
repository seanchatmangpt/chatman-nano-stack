defmodule CNSForgeWeb.Router do
  use CNSForgeWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {CNSForgeWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  # CNS Forge API routes
  scope "/api", CNSForgeWeb do
    pipe_through :api

    # Directive processing endpoint (stimulus:http_request BitActor)
    post "/directive", DirectiveController, :process
    
    # Transaction tracing endpoint for time-travel debugging
    get "/trace/:transaction_id", DirectiveController, :trace
    
    # BitActor mesh status
    get "/mesh/status", MeshController, :status
    get "/mesh/signals", MeshController, :signals
    
    # Telemetry endpoints
    get "/telemetry/metrics", TelemetryController, :metrics
    get "/telemetry/pulse", TelemetryController, :pulse_logs
  end

  # Development routes
  if Application.compile_env(:cns_forge, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: CNSForgeWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end