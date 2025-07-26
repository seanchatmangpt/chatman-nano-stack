# Minimal Ash.Reactor container - 80/20 approach
FROM elixir:1.16-alpine AS builder

# Install build dependencies
RUN apk add --no-cache build-base git

# Set working directory
WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set MIX_ENV
ENV MIX_ENV=prod

# Get dependencies
RUN mix deps.get --only prod && \
    mix deps.compile

# Copy application code
COPY lib lib/
COPY config config/

# Compile application
RUN mix compile

# Create release
RUN mix release

# Runtime stage
FROM alpine:3.19

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs libstdc++ ca-certificates

# Create app user
RUN addgroup -g 1000 app && \
    adduser -u 1000 -G app -s /bin/sh -D app

WORKDIR /app

# Copy release from builder
COPY --from=builder --chown=app:app /app/_build/prod/rel/ash_reactor_80_20 ./

# Set environment
ENV HOME=/app
ENV MIX_ENV=prod
ENV PORT=4000
ENV OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317

USER app

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD /app/bin/ash_reactor_80_20 rpc "IO.puts(:ok)" || exit 1

# Expose port
EXPOSE 4000

# Start application
CMD ["/app/bin/ash_reactor_80_20", "start"]