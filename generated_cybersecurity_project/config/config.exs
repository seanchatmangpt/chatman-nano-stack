import Config

# 80/20 Reactor Project Configuration
# No Ash dependencies - using plain Reactor with simple structs

# Configure the main domain  
# Domain resources are plain Elixir modules

# 80/20: Using ETS data layer - no database configuration needed

# 80/20: No telemetry configuration needed

# Logger configuration
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Environment-specific configuration
import_config "#{config_env()}.exs"
