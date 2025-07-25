import Config

# Note: Do not include production secrets in this file.
# Instead, use config/runtime.exs for runtime configuration.

config :cns_forge, CNSForgeWeb.Endpoint,
  cache_static_manifest: "priv/static/cache_manifest.json"

# Configures Swoosh API Client
config :swoosh, api_client: Swoosh.ApiClient.Finch, finch_name: CNSForge.Finch

# Do not print debug messages in production
config :logger, level: :info

# Runtime production configuration, including reading
# of environment variables, is done in config/runtime.exs.