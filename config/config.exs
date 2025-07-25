import Config

# CNS Forge Configuration
# Implements the Ash/Reactor architecture for ecosystem composition

# Ash Framework configuration
config :ash, :validate_domain_resource_inclusion?, false
config :ash, :validate_domain_config_inclusion?, false

# CNS Forge domain configuration
config :cns_forge, CNSForge,
  resources: [
    CNSForge.BitActor,
    CNSForge.Signal,
    CNSForge.TelemetryFrame
  ]

# Reactor configuration with CNS Forge middleware
config :reactor,
  middleware: [
    {CNSForge.ReactorMiddleware, []}
  ]

# Phoenix endpoint configuration
config :cns_forge, CNSForgeWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Phoenix.Endpoint.Cowboy2Adapter,
  render_errors: [
    formats: [html: CNSForgeWeb.ErrorHTML, json: CNSForgeWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: CNSForge.PubSub,
  live_view: [signing_salt: "cns_forge_signing_salt"]

# Phoenix LiveView configuration
config :phoenix, :json_library, Jason

# Telemetry configuration for universal observability
# Metrics are defined in CNSForge.Telemetry module to avoid config-time issues
config :cns_forge, CNSForge.Telemetry,
  enabled: true,
  export_interval: 30_000

# Logger configuration
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :transaction_id, :bit_actor_id]

# Mnesia configuration for transactional state management
config :mnesia,
  dir: Path.join(System.tmp_dir(), "cns_forge_mnesia"),
  dc_dump_limit: 40,
  dump_log_write_threshold: 50000

# Environment-specific configurations
import_config "#{config_env()}.exs"