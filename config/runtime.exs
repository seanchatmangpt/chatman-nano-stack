import Config

# Runtime configuration for production
if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

  config :cns_forge, CNSForge.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    socket_options: maybe_ipv6

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "example.com"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :cns_forge, CNSForgeWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  # OpenTelemetry configuration
  config :opentelemetry,
    span_processor: :batch,
    traces_exporter: :otlp

  config :opentelemetry_exporter,
    otlp_protocol: :http_protobuf,
    otlp_endpoint: System.get_env("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4317"),
    otlp_headers: System.get_env("OTEL_EXPORTER_OTLP_HEADERS", "") |> parse_headers()

  # Redis configuration for caching
  config :cns_forge, :redis,
    url: System.get_env("REDIS_URL", "redis://localhost:6379")

  # BitActor configuration
  config :cns_forge, :bitactor,
    default_ttl: String.to_integer(System.get_env("BITACTOR_TTL_BUDGET", "8")),
    ring_size: String.to_integer(System.get_env("BITACTOR_RING_SIZE", "1024"))

  # Reactor configuration
  config :cns_forge, :reactor,
    max_concurrency: String.to_integer(System.get_env("REACTOR_MAX_CONCURRENCY", "100")),
    batch_size: String.to_integer(System.get_env("REACTOR_BATCH_SIZE", "50"))
end

defp parse_headers(""), do: []
defp parse_headers(headers) do
  headers
  |> String.split(",")
  |> Enum.map(&String.split(&1, "=", parts: 2))
  |> Enum.map(fn [k, v] -> {String.trim(k), String.trim(v)} end)
end