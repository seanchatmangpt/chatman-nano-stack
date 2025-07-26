import Config

# Configure Ash
config :ash, :validate_domain_resource_inclusion?, false
config :ash, :validate_domain_config_inclusion?, false

# Configure the main domain  
# Domain resources are configured in the domain module itself

# Database configuration
config :ultrathink, Ultrathink.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "ultrathink_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# Telemetry configuration
config :telemetry_poller, :default, period: :timer.seconds(5)

# Logger configuration
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Environment-specific configuration
import_config "#{config_env()}.exs"
