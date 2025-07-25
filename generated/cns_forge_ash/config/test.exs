import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :cns_forge_ash, CnsForgeAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "Www274PEpJV06qF7rik6Bz6rkx3MJtbqV4jtomfF8t6kPcngsQ0QF0ZVtwKSDiQq",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
