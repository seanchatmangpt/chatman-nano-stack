import Config

# Database configuration for development
config :ultrathink, Ultrathink.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "ultrathink_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime
