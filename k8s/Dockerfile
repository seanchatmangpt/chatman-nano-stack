# BitActor Service Dockerfile
# Multi-stage build for optimal size and security

# Build stage
FROM gcc:11-bullseye AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    make \
    cmake \
    python3 \
    python3-pip \
    erlang \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy source files
COPY bitactor_ttl_generator.py .
COPY bitactor_cli.py .
COPY templates/ ./templates/
COPY ontologies/ ./ontologies/

# Install Python dependencies
RUN pip3 install typer rich rdflib jinja2

# Generate BitActor implementation
RUN python3 bitactor_cli.py generate \
    ontologies/bitactor_semantic_core.ttl \
    /build/generated \
    production \
    --no-validate

# Build C implementation
WORKDIR /build/generated
RUN make benchmark

# Runtime stage
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libatomic1 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 bitactor

# Copy built artifacts
COPY --from=builder /build/generated/production_benchmark /usr/local/bin/bitactor
COPY --from=builder /build/generated/production_bitactor.h /usr/local/include/

# Set ownership
RUN chown -R bitactor:bitactor /usr/local/bin/bitactor

# Switch to non-root user
USER bitactor

# Expose metrics port
EXPOSE 9090

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD /usr/local/bin/bitactor --health || exit 1

# Run BitActor service
CMD ["/usr/local/bin/bitactor", "--server", "--port", "9090"]