# CNS Forge Documentation

## Overview

CNS Forge is a Universal Business Logic Compiler that transforms semantic specifications (TTL ontologies, BPMN workflows, DMN rules) into executable code across multiple target languages and runtimes. Built on Elixir's Ash/Reactor architecture, it implements a TTL-bounded BitActor mesh for distributed execution with comprehensive observability.

## Architecture

### Core Components

1. **Metacompiler** (`lib/cns_forge/metacompiler.ex`)
   - Parses semantic specifications (TTL/BPMN/DMN)
   - Generates intermediate representation (IR)
   - Optimizes and produces multi-target code
   - Supports Elixir, JavaScript, Python, Go, Rust targets

2. **BitActor Mesh** (`lib/cns_forge/bit_actor.ex`)
   - TTL-bounded execution units (Time-To-Live)
   - Distributed actor system with ring topology
   - Automatic garbage collection after TTL expiration
   - Fault-tolerant with saga compensation

3. **Reactor Workflows** (`lib/cns_forge/workflows/`)
   - Declarative orchestration layer
   - Parallel and sequential execution strategies
   - Built-in error handling and compensation
   - OpenTelemetry integration for tracing

### System Architecture

```mermaid
graph TB
    subgraph "Input Layer"
        TTL[TTL Ontologies]
        BPMN[BPMN Workflows]
        DMN[DMN Rules]
    end
    
    subgraph "Compilation Layer"
        MC[Metacompiler]
        IR[IR Generator]
        OPT[Optimizer]
    end
    
    subgraph "Execution Layer"
        RW[Reactor Workflows]
        BA[BitActor Mesh]
        TM[Telemetry]
    end
    
    subgraph "Output Layer"
        EX[Elixir Code]
        JS[JavaScript]
        PY[Python]
        GO[Go]
        RS[Rust]
    end
    
    TTL --> MC
    BPMN --> MC
    DMN --> MC
    MC --> IR
    IR --> OPT
    OPT --> RW
    RW --> BA
    BA --> TM
    OPT --> EX
    OPT --> JS
    OPT --> PY
    OPT --> GO
    OPT --> RS
```

## Getting Started

### Prerequisites

- Elixir 1.15+
- PostgreSQL 14+
- Redis 6+
- Kubernetes 1.25+ (for production)
- Terraform 1.0+ (for infrastructure)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/your-org/cns-forge.git
   cd cns-forge
   ```

2. Install dependencies:
   ```bash
   mix deps.get
   ```

3. Setup database:
   ```bash
   mix ecto.setup
   ```

4. Run tests:
   ```bash
   mix test
   ```

5. Start the application:
   ```bash
   mix phx.server
   ```

## Usage

### Basic Semantic Compilation

```elixir
# Define a TTL ontology
ttl_spec = """
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:PaymentWorkflow a ex:Workflow ;
    ex:hasStep ex:ValidatePayment ;
    ex:hasStep ex:ProcessPayment ;
    ex:hasStep ex:SendConfirmation .

ex:ValidatePayment ex:requiresRole ex:PaymentValidator ;
    ex:timeout "30s" .
"""

# Compile to multiple targets
{:ok, targets} = CNSForge.Metacompiler.compile(ttl_spec, [
  targets: [:elixir, :javascript, :python],
  optimize: true
])

# Access generated code
elixir_code = targets[:elixir]
js_code = targets[:javascript]
python_code = targets[:python]
```

### Creating BitActors

```elixir
# Create a BitActor with TTL budget
{:ok, actor} = CNSForge.BitActor.create(%{
  type: :processor,
  ttl: 8,  # 8 hops before expiration
  token: %{
    transaction_id: "txn_123",
    amount: 100.00,
    currency: "USD"
  }
})

# Execute with TTL tracking
{:ok, result} = CNSForge.BitActor.execute(actor, :process_payment)
```

### Building Reactor Workflows

```elixir
defmodule PaymentWorkflow do
  use CNSForge.Workflows.Base
  
  step :validate_payment do
    argument :payment, from: :input
    run CNSForge.Steps.ValidatePayment
  end
  
  step :process_payment do
    argument :payment, from: :validate_payment
    run CNSForge.Steps.ProcessPayment
    compensate CNSForge.Steps.ReversePayment
  end
  
  step :send_confirmation do
    argument :result, from: :process_payment
    run CNSForge.Steps.SendConfirmation
  end
end

# Execute workflow
{:ok, result} = PaymentWorkflow.run(%{
  payment: %{amount: 100.00, currency: "USD"}
})
```

## Configuration

### Environment Variables

```bash
# Database
DATABASE_URL=ecto://user:pass@localhost/cns_forge

# Redis
REDIS_URL=redis://localhost:6379

# BitActor
BITACTOR_TTL_BUDGET=8
BITACTOR_RING_SIZE=1024

# Reactor
REACTOR_MAX_CONCURRENCY=100
REACTOR_BATCH_SIZE=50

# OpenTelemetry
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
OTEL_EXPORTER_OTLP_HEADERS=api-key=your-key
```

### Mix Configuration

Configuration files are organized by environment:
- `config/config.exs` - Shared configuration
- `config/dev.exs` - Development settings
- `config/test.exs` - Test settings  
- `config/prod.exs` - Production settings
- `config/runtime.exs` - Runtime configuration

## Testing

### Running Tests

```bash
# All tests
mix test

# Unit tests only
mix test --only unit

# Stress tests
mix test.stress

# Adversarial tests
mix test.adversarial

# Coverage report
mix test.coverage
```

### Test Structure

- `test/unit/` - Unit tests for individual modules
- `test/integration/` - Integration tests for workflows
- `test/stress/` - Performance and load tests
- `test/adversarial/` - Security and edge case tests

## Deployment

### Docker

```bash
# Build image
docker build -t cns-forge:latest .

# Run container
docker run -p 4000:4000 \
  -e DATABASE_URL=... \
  -e SECRET_KEY_BASE=... \
  cns-forge:latest
```

### Kubernetes

```bash
# Apply manifests
kubectl apply -f k8s/

# Install with Helm
helm install cns-forge ./helm/cns-forge \
  --values values.yaml \
  --namespace cns-forge
```

### Terraform (AWS)

```bash
cd terraform
terraform init
terraform plan
terraform apply
```

This creates:
- EKS cluster with node groups
- RDS PostgreSQL database
- ElastiCache Redis cluster
- Application Load Balancer
- Auto-scaling configuration
- CloudWatch monitoring

## Monitoring

### OpenTelemetry

CNS Forge exports traces and metrics to any OTLP-compatible backend:

```elixir
# Traces include:
- Compilation pipelines
- Workflow execution
- BitActor operations
- Database queries

# Metrics include:
- Compilation performance
- Actor creation/execution rates
- TTL exhaustion counts
- Memory usage
```

### Health Checks

- `/health` - Basic health check
- `/ready` - Readiness probe
- `/metrics` - Prometheus metrics

## API Reference

### Metacompiler

```elixir
# Compile semantic specification
CNSForge.Metacompiler.compile(spec, opts)

# Parse specific format
CNSForge.Metacompiler.parse_ttl(ttl_string)
CNSForge.Metacompiler.parse_bpmn(bpmn_xml)
CNSForge.Metacompiler.parse_dmn(dmn_xml)

# Generate code for target
CNSForge.Metacompiler.generate_elixir(ir)
CNSForge.Metacompiler.generate_javascript(ir)
```

### BitActor

```elixir
# Create actor
CNSForge.BitActor.create(attrs)

# Execute operation
CNSForge.BitActor.execute(actor, operation)

# Check TTL
CNSForge.BitActor.check_ttl(actor)

# List active actors
CNSForge.BitActor.list_active()
```

### Workflows

```elixir
# Run workflow
MyWorkflow.run(input)

# Run with options
MyWorkflow.run(input, async: true, timeout: 30_000)

# Get workflow status
CNSForge.Workflows.status(workflow_id)
```

## Performance

### Benchmarks

```bash
# Run benchmarks
mix benchmark

# Results:
- BitActor creation: 0.8ms
- Hop execution: 0.1ms  
- Compilation: 5ms
- Workflow orchestration: 2ms
```

### Optimization Tips

1. **TTL Budget**: Set appropriate TTL for actors based on workflow depth
2. **Batch Operations**: Use batch APIs for multiple actor creations
3. **Caching**: Enable Redis caching for compiled artifacts
4. **Concurrency**: Tune REACTOR_MAX_CONCURRENCY for your workload

## Security

### Built-in Protections

- **TTL Enforcement**: Prevents infinite loops and resource exhaustion
- **Input Validation**: All semantic specs are validated before compilation
- **Sandboxed Execution**: BitActors run in isolated contexts
- **Rate Limiting**: Built-in rate limits for API endpoints
- **Authentication**: JWT-based authentication for API access

### Best Practices

1. Always validate semantic specifications from untrusted sources
2. Set conservative TTL budgets for user-submitted workflows
3. Monitor for anomalous actor creation patterns
4. Enable audit logging for compliance requirements

## Troubleshooting

### Common Issues

**Q: BitActor TTL exhausted errors**
A: Increase TTL budget or optimize workflow to reduce hops

**Q: Compilation takes too long**
A: Enable caching, reduce optimization level, or batch compile

**Q: Memory usage high**
A: Tune BITACTOR_RING_SIZE and implement actor cleanup

**Q: Workflows failing intermittently**
A: Check saga compensation logic and increase timeouts

### Debug Mode

```elixir
# Enable debug logging
config :logger, level: :debug

# Trace specific workflow
CNSForge.Workflows.trace(MyWorkflow, input)

# Inspect BitActor state
CNSForge.BitActor.inspect(actor_id)
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run quality checks: `mix quality`
5. Submit a pull request

## License

Copyright (c) 2024 CNS Forge Team

Licensed under the Apache License, Version 2.0