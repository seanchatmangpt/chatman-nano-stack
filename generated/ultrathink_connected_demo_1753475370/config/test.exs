import Config

# Database configuration for test
config :ultrathink, Ultrathink.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "ultrathink_test#{System.get_env(\"MIX_TEST_PARTITION\")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
