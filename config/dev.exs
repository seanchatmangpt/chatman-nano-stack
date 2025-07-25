import Config

# Development configuration for CNS Forge

# Phoenix endpoint configuration
config :cns_forge, CNSForgeWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "cns_forge_dev_secret_key_base_placeholder_change_in_production",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]}
  ]

# Watch static and templates for browser reloading
config :cns_forge, CNSForgeWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/cns_forge_web/(controllers|live|components)/.*(ex|heex)$"
    ]
  ]

# Enable dev routes for dashboard and mailbox
config :cns_forge, dev_routes: true

# Logger configuration for development
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# Disable swoosh api client as it is only required for production adapters
config :swoosh, :api_client, false

# BitActor development configuration
config :cns_forge,
  # Enable verbose telemetry logging
  verbose_telemetry: true,
  
  # Default TTL for development (higher for debugging)
  default_ttl: 16,
  
  # Enable time-travel debugging
  enable_time_travel: true,
  
  # Disable tick budget assertions for development
  enforce_tick_budget: false