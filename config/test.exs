import Config

# Configure your database
config :cns_forge, CNSForge.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "cns_forge_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# We don't run a server during test
config :cns_forge, CNSForgeWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "test_secret_key_base_at_least_64_bytes_long_for_security_purposes",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Disable telemetry events during tests
config :cns_forge, CNSForge.Telemetry,
  enabled: false