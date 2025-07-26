# Ash.Reactor 80/20 Implementation

Minimal viable Ash.Reactor deployment for Terraform/K8s using the ultrathink 80/20 approach.

## Overview

This implementation focuses on the 80% that matters:
- ✅ TTL → Ash.Reactor transformation
- ✅ Kubernetes deployment with Terraform
- ✅ OpenTelemetry integration
- ✅ Horizontal scaling
- ✅ Health checks and monitoring

## Quick Start

### Prerequisites
- Docker
- Kubernetes cluster (local or cloud)
- Terraform >= 1.0
- Elixir 1.16+ (for local development)

### Deploy with Terraform

```bash
# Build Docker image
docker build -t ash-reactor-80-20:latest .

# Deploy infrastructure
cd terraform
terraform init
terraform apply
```

### Deploy with kubectl

```bash
# Build Docker image
docker build -t ash-reactor-80-20:latest .

# Deploy OTEL collector first
kubectl apply -f k8s/otel-collector.yaml

# Deploy Ash.Reactor
kubectl apply -f k8s/deployment.yaml
```

### Deploy with script

```bash
chmod +x deploy.sh
./deploy.sh
```

## Architecture

```
┌─────────────────┐     ┌──────────────────┐
│   TTL Input     │────▶│  Ash.Reactor     │
└─────────────────┘     │  Transformer     │
                        └────────┬─────────┘
                                 │
                        ┌────────▼─────────┐
                        │  Ash Resources   │
                        │  - OntologyClass │
                        │  - Workflow      │
                        └────────┬─────────┘
                                 │
                        ┌────────▼─────────┐
                        │ Reactor Workflow │
                        │   Execution      │
                        └────────┬─────────┘
                                 │
                        ┌────────▼─────────┐
                        │ OTEL Collector   │
                        │   & Metrics      │
                        └──────────────────┘
```

## Core Components

### 1. TTL Transformer (`lib/ash_reactor_ttl.ex`)
- Parses TTL content using regex (80/20 approach)
- Creates Ash Resources for ontology classes
- Generates Reactor workflows automatically

### 2. Ash Resources (`lib/ash_reactor_core.ex`)
- `OntologyClass`: Represents TTL classes
- `ReactorWorkflow`: Represents executable workflows
- Uses ETS for simple in-memory storage

### 3. Infrastructure
- **Terraform**: Complete K8s deployment
- **Kubernetes**: Deployment, Service, HPA, NetworkPolicy
- **OTEL**: Full observability with traces and metrics

## Testing

Run validation tests:
```bash
mix test test/validation_test.exs
```

## Monitoring

### Metrics
- Exposed on port 9090 at `/metrics`
- Prometheus-compatible format
- Key metrics:
  - Workflow execution count
  - Processing time
  - Resource usage

### Traces
- Sent to OTEL collector on port 4317
- Trace key operations:
  - TTL parsing
  - Class creation
  - Workflow execution

### Health Checks
- Liveness: `/health` on port 4000
- Readiness: `/ready` on port 4000

## Configuration

Environment variables:
- `OTEL_EXPORTER_OTLP_ENDPOINT`: OTEL collector endpoint
- `LOG_LEVEL`: Logging level (debug, info, warn, error)
- `REACTOR_MAX_CONCURRENCY`: Max concurrent workflows
- `TTL_PROCESSING_ENABLED`: Enable/disable TTL processing

## Scaling

The deployment includes:
- HorizontalPodAutoscaler (2-10 replicas)
- CPU threshold: 70%
- Memory threshold: 80%

## Security

- NetworkPolicy restricts traffic
- Service Account for RBAC
- Pod Security Standards enforced
- No root containers

## Troubleshooting

### Check pod status
```bash
kubectl get pods -n ash-reactor-80-20
kubectl logs -n ash-reactor-80-20 -l app=ash-reactor
```

### Check OTEL collector
```bash
kubectl logs -n opentelemetry -l app=otel-collector
```

### Port forward for local testing
```bash
kubectl port-forward -n ash-reactor-80-20 svc/ash-reactor 4000:80
curl http://localhost:4000/health
```

## 80/20 Philosophy

This implementation follows the 80/20 principle:
- **80% Focus**: Core functionality that MUST work
  - TTL transformation
  - Basic K8s deployment
  - Essential monitoring
- **20% Deferred**: Nice-to-haves for later
  - Advanced security features
  - Complex orchestration
  - Performance optimizations

## Next Steps

1. Connect to production OTEL backend (Jaeger/Zipkin)
2. Add persistent storage (PostgreSQL)
3. Implement authentication
4. Add more sophisticated TTL parsing
5. Create Helm chart for easier deployment