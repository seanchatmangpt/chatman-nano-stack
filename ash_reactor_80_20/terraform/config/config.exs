import Config

# Base configuration for Ash.Reactor 80/20
config :ash_reactor_80_20,
  ttl_processing_enabled: true,
  reactor_max_concurrency: 10,
  reactor_timeout_ms: 5000

# OpenTelemetry configuration
config :opentelemetry,
  span_processor: :batch,
  traces_exporter: :otlp

config :opentelemetry_exporter,
  otlp_protocol: :grpc,
  otlp_endpoint: System.get_env("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4317"),
  otlp_headers: []

# Logger configuration
config :logger,
  level: String.to_atom(System.get_env("LOG_LEVEL", "info")),
  backends: [:console],
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :trace_id, :span_id]

# Runtime configuration
if config_env() == :prod do
  config :ash_reactor_80_20,
    port: String.to_integer(System.get_env("PORT", "4000"))
end