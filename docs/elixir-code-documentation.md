# CNS Forge Elixir Code Documentation

## Overview

CNS Forge is a sophisticated ecosystem composer built on the Ash/Reactor architecture, implementing a BitActor Mesh pattern for deterministic, observable, and fault-tolerant execution. The system translates high-level directives into atomic, time-limited execution units (BitActors) that communicate through immutable signals.

## Architecture Principles

- **TTL-Driven Execution**: All operations are bounded by a time-to-live (TTL) budget (default: 8 hops)
- **Saga Orchestration**: Atomic transactions with compensation mechanisms
- **Universal Observability**: Complete audit trail via telemetry frames
- **Declarative Resources**: Ash Framework for resource-oriented architecture
- **Signal-Based Communication**: Immutable data flow between BitActors

## Project Structure

```
lib/
├── cns_forge.ex                    # Main domain module
├── cns_forge/
│   ├── application.ex              # Application supervision tree
│   ├── bit_actor.ex                # BitActor resource definition
│   ├── signal.ex                   # Signal resource for routing
│   ├── telemetry_frame.ex          # Observability resource
│   ├── reactor_middleware.ex       # Reactor integration layer
│   └── workflows/                  # Reactor workflow definitions
│       ├── process_directive.ex    # Main directive processing
│       ├── comprehensive_cns_ecosystem.ex
│       ├── trading_semantic_compiler.ex
│       ├── semantic_bitactor_mesh.ex
│       └── cybersecurity_threat_pipeline.ex
└── cns_forge_web/
    ├── router.ex                   # Phoenix router
    └── controllers/
        └── directive_controller.ex # HTTP ingress controller
```

## Core Modules

### CNSForge (lib/cns_forge.ex)

**Purpose**: Main domain module that orchestrates the CNS Forge ecosystem.

**Key Functions**:
- `process_directive/2`: Entry point for directive processing
- Resource definitions for BitActor, Signal, and TelemetryFrame

**Architecture**:
- Uses Ash.Domain for declarative resource management
- Implements 8-hop TTL budget for execution
- Generates cryptographically secure transaction IDs

### CNSForge.Application (lib/cns_forge/application.ex)

**Purpose**: Application supervision tree and system initialization.

**Components**:
- **SignalRegistry**: Registry for signal routing between BitActors
- **MnesiaSetup**: Transactional state management setup
- **TelemetrySupervisor**: Metrics collection and observability
- **BitActorSupervisor**: Dynamic supervisor for BitActor lifecycles
- **Phoenix Endpoint**: HTTP ingress for stimulus BitActors

**Key Features**:
- Mnesia table creation for Ash resources
- Telemetry event handling for pulse logs
- Dynamic BitActor process management
- Saga compensation mechanisms

### CNSForge.BitActor (lib/cns_forge/bit_actor.ex)

**Purpose**: Represents ephemeral, time-limited execution units in the mesh.

**Attributes**:
- `type`: BitActor classification (:stimulus, :decoder, :workflow, :action)
- `transaction_id`: Correlation ID for tracing
- `ttl`: Remaining time-to-live in logical hops
- `token`: Immutable state passed between hops
- `status`: Current execution status
- `result/error`: Output or failure information

**Actions**:
- `create`: Initialize new BitActor
- `execute_hop`: Perform atomic operation and decrement TTL
- `fail`: Mark BitActor as failed with error details
- `expire_ttl`: Handle TTL exhaustion

**Key Features**:
- TTL validation before execution
- Atomic hop execution with telemetry capture
- Blake3 hash generation for integrity
- Automatic status transitions

### CNSForge.Signal (lib/cns_forge/signal.ex)

**Purpose**: Immutable data payload for BitActor communication.

**Attributes**:
- `type`: Signal classification for routing
- `source_actor_id`: Originating BitActor
- `target_actor_type`: Destination BitActor type
- `payload`: Immutable data content
- `ttl`: Inherited TTL from source
- `priority`: Routing priority level
- `routed_at/consumed_at`: Timing metadata

**Actions**:
- `emit`: Create and route new signal
- `mark_routed`: Confirm successful routing
- `mark_consumed`: Record consumption by target

**Key Features**:
- Registry-based high-performance routing
- Priority-based signal dispatch
- Complete timing audit trail
- Immutable payload design

### CNSForge.TelemetryFrame (lib/cns_forge/telemetry_frame.ex)

**Purpose**: Universal observability and time-travel debugging.

**Attributes**:
- `transaction_id`: Correlation ID
- `bit_actor_id`: Source BitActor
- `hop_sequence`: Sequential hop number
- `operation`: Executed operation
- `input_token/output_token`: State before/after
- `ttl_before/ttl_after`: TTL state changes
- `execution_time_us`: Performance metrics
- `blake3_hash`: Integrity verification

**Actions**:
- `capture`: Record BitActor state transition
- `causal_chain`: Retrieve complete transaction history
- `reconstruct_ttl_chain`: Time-travel debugging
- `verify_chain_integrity`: Cryptographic verification

**Key Features**:
- Complete audit trail for every operation
- Time-travel debugging capabilities
- Cryptographic integrity verification
- Performance profiling data

## Web Layer

### CNSForgeWeb.Router (lib/cns_forge_web/router.ex)

**Purpose**: Phoenix router for HTTP ingress and API endpoints.

**Routes**:
- `POST /api/directive`: Directive processing endpoint
- `GET /api/trace/:transaction_id`: Transaction tracing
- `GET /api/mesh/status`: BitActor mesh status
- `GET /api/telemetry/metrics`: System metrics
- `GET /api/telemetry/pulse`: Pulse logs

**Features**:
- JSON API with proper error handling
- Development dashboard integration
- Live telemetry monitoring

### CNSForgeWeb.DirectiveController (lib/cns_forge_web/controllers/directive_controller.ex)

**Purpose**: HTTP ingress controller implementing stimulus BitActor.

**Endpoints**:
- `process/2`: Process directives via Reactor workflow
- `trace/2`: Retrieve complete causal chains

**Features**:
- Transaction ID generation
- Processing time calculation
- Comprehensive error handling
- Time-travel debugging support

## Workflow Layer

### CNSForge.Workflows.ProcessDirective (lib/cns_forge/workflows/process_directive.ex)

**Purpose**: Main Reactor workflow for directive processing.

**Workflow Steps**:
1. **create_stimulus**: Initialize stimulus BitActor
2. **parse_directive**: Decode and parse directive
3. **validate_directive**: Input validation
4. **route_to_workflow**: Determine execution path
5. **execute_business_logic**: Core business operations

**Features**:
- Saga orchestration with compensation
- TTL validation at each step
- Parallel execution where possible
- Comprehensive error handling

### Specialized Workflows

#### Comprehensive CNS Ecosystem (lib/cns_forge/workflows/comprehensive_cns_ecosystem.ex)
- **Purpose**: Orchestrates complex multi-domain operations
- **Size**: 30KB, 688 lines
- **Features**: Cross-domain coordination, advanced saga patterns

#### Trading Semantic Compiler (lib/cns_forge/workflows/trading_semantic_compiler.ex)
- **Purpose**: Financial trading directive processing
- **Size**: 18KB, 554 lines
- **Features**: Market data integration, risk management

#### Semantic BitActor Mesh (lib/cns_forge/workflows/semantic_bitactor_mesh.ex)
- **Purpose**: Semantic reasoning and knowledge graph operations
- **Size**: 22KB, 592 lines
- **Features**: Ontology processing, semantic routing

#### Cybersecurity Threat Pipeline (lib/cns_forge/workflows/cybersecurity_threat_pipeline.ex)
- **Purpose**: Security threat analysis and response
- **Size**: 17KB, 527 lines
- **Features**: Threat detection, automated response

## Configuration

### Mix Configuration (mix.exs)

**Dependencies**:
- **Ash Framework**: Resource-oriented architecture
- **Reactor**: Workflow orchestration
- **Phoenix**: HTTP framework
- **Telemetry**: Observability
- **Mnesia**: Transactional storage

**Development Tools**:
- **Credo**: Code quality analysis
- **Dialyxir**: Static type checking
- **ExDoc**: Documentation generation

### Environment Configuration

#### Development (config/dev.exs)
- Live code reloading
- Detailed logging
- Development dashboard

#### Production (config/prod.exs)
- Optimized performance
- Production telemetry
- Security hardening

## Data Layer

### Mnesia Tables

**bit_actor**:
- Primary key: `id` (UUID)
- Indexes: `transaction_id`, `type`
- Disc copies for persistence

**signal**:
- Primary key: `id` (UUID)
- Indexes: `transaction_id`, `target_actor_type`
- Routing optimization

**telemetry_frame**:
- Primary key: `id` (UUID)
- Indexes: `transaction_id`, `hop_sequence`
- Audit trail optimization

## Telemetry and Observability

### Event Types
- `[:cns_forge, :bit_actor, :hop]`: BitActor execution
- `[:cns_forge, :bit_actor, :ttl_expired]`: TTL exhaustion
- `[:cns_forge, :signal, :routed]`: Signal routing
- `[:cns_forge, :transaction, :completed]`: Transaction completion

### Pulse Logs
Real-time execution traces showing:
- Transaction ID correlation
- Step-by-step execution
- TTL consumption
- Performance metrics

## Security Features

### Cryptographic Integrity
- Blake3 hash generation for all state transitions
- Chain integrity verification
- Tamper-evident audit trails

### Transaction Isolation
- Mnesia transactional guarantees
- Saga compensation mechanisms
- Atomic operation boundaries

## Performance Characteristics

### TTL Budget Management
- Default 8-hop limit per transaction
- Configurable per directive
- Automatic expiration handling

### Signal Routing
- Registry-based high-performance routing
- Priority-based dispatch
- Zero-copy immutable payloads

### Memory Management
- Ephemeral BitActor lifecycle
- Automatic cleanup on completion
- Mnesia garbage collection

## Development Workflow

### Code Quality
- Credo for style and consistency
- Dialyxir for type safety
- Comprehensive test coverage

### Documentation
- ExDoc for API documentation
- Inline documentation for all modules
- Architecture decision records

### Testing
- Unit tests for all resources
- Integration tests for workflows
- Performance benchmarks

## Deployment Considerations

### Production Setup
- Mnesia cluster configuration
- Phoenix endpoint optimization
- Telemetry aggregation

### Monitoring
- Live dashboard integration
- Custom metrics collection
- Alert configuration

### Scaling
- Horizontal scaling via Mnesia clustering
- Load balancing for HTTP ingress
- Signal routing optimization

## Future Enhancements

### Planned Features
- Advanced saga patterns
- Machine learning integration
- Enhanced semantic reasoning
- Cross-cluster coordination

### Performance Optimizations
- AOT compilation integration
- Advanced caching strategies
- Parallel execution optimization

---

*This documentation covers the complete Elixir codebase of CNS Forge, providing a comprehensive understanding of the BitActor Mesh architecture and its implementation.* 