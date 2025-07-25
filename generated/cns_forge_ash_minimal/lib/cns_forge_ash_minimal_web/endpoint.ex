defmodule CnsForgeAshMinimalWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :cns_forge_ash_minimal

  @session_options [
    store: :cookie,
    key: "_cns_forge_ash_minimal_key",
    signing_salt: "9vgaVxHQ",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]]

  plug Plug.Static,
    at: "/",
    from: :cns_forge_ash_minimal,
    gzip: false,
    only: CnsForgeAshMinimalWeb.static_paths()

  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug CnsForgeAshMinimalWeb.Router
end