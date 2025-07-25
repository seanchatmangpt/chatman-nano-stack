# CNS Forge Elixir API Reference

## Overview

This document provides a comprehensive API reference for all Elixir modules in the CNS Forge system, including function signatures, types, and usage examples.

## Core Domain Module

### CNSForge

**Module**: `CNSForge`

**Purpose**: Main domain module orchestrating the CNS Forge ecosystem.

#### Functions

##### `process_directive/2`

```elixir
@spec process_directive(String.t(), non_neg_integer()) :: 
  {:ok, map()} | {:error, :ttl_expired | term()}

# Process a directive through the BitActor mesh
CNSForge.process_directive("Create user with premium subscription", 8)
```

**Parameters**:
- `directive` (String.t()): High-level directive to process
- `ttl` (non_neg_integer(), optional): Time-to-live budget (default: 8)

**Returns**:
- `{:ok, result}`: Successful processing with result
- `{:error, :ttl_expired}`: TTL budget exhausted
- `{:error, reason}`: Processing failure

## BitActor Resource

### CNSForge.BitActor

**Module**: `CNSForge.BitActor`

**Purpose**: Represents ephemeral, time-limited execution units.

#### Types

```elixir
@type bit_actor_type :: :stimulus | :decoder | :workflow | :action
@type bit_actor_status :: :pending | :running | :completed | :failed | :ttl_expired
@type token :: map()
@type ttl :: non_neg_integer()
```

#### Functions

##### `create!/1`

```elixir
@spec create!(map()) :: CNSForge.BitActor.t()

# Create a new BitActor
CNSForge.BitActor.create!(%{
  type: :stimulus,
  transaction_id: "abc123",
  token: %{directive: "test"},
  ttl: 8
})
```

**Parameters**:
- `attrs` (map()): BitActor attributes

**Returns**: `CNSForge.BitActor.t()` - Created BitActor

##### `execute_hop!/2`

```elixir
@spec execute_hop!(CNSForge.BitActor.t(), map()) :: CNSForge.BitActor.t()

# Execute one atomic hop
CNSForge.BitActor.execute_hop!(bit_actor, %{
  input_token: %{data: "input"},
  operation: :process_data
})
```

**Parameters**:
- `bit_actor` (CNSForge.BitActor.t()): BitActor to execute
- `attrs` (map()): Execution parameters

**Returns**: `CNSForge.BitActor.t()` - Updated BitActor

##### `fail!/2`

```elixir
@spec fail!(CNSForge.BitActor.t(), map()) :: CNSForge.BitActor.t()

# Mark BitActor as failed
CNSForge.BitActor.fail!(bit_actor, %{
  error_message: "Processing failed"
})
```

**Parameters**:
- `bit_actor` (CNSForge.BitActor.t()): BitActor to fail
- `attrs` (map()): Error details

**Returns**: `CNSForge.BitActor.t()` - Failed BitActor

##### `expire_ttl!/1`

```elixir
@spec expire_ttl!(CNSForge.BitActor.t()) :: CNSForge.BitActor.t()

# Mark BitActor as TTL expired
CNSForge.BitActor.expire_ttl!(bit_actor)
```

**Parameters**:
- `bit_actor` (CNSForge.BitActor.t()): BitActor to expire

**Returns**: `CNSForge.BitActor.t()` - Expired BitActor

## Signal Resource

### CNSForge.Signal

**Module**: `CNSForge.Signal`

**Purpose**: Immutable data payload for BitActor communication.

#### Types

```elixir
@type signal_type :: atom()
@type signal_priority :: :low | :medium | :high | :critical
@type payload :: map()
```

#### Functions

##### `emit!/1`

```elixir
@spec emit!(map()) :: CNSForge.Signal.t()

# Emit a new signal
CNSForge.Signal.emit!(%{
  type: :data_ready,
  source_actor_id: "actor-123",
  transaction_id: "tx-456",
  payload: %{result: "processed"},
  ttl: 7,
  target_actor_type: :workflow,
  priority: :high
})
```

**Parameters**:
- `attrs` (map()): Signal attributes

**Returns**: `CNSForge.Signal.t()` - Created signal

##### `mark_routed!/1`

```elixir
@spec mark_routed!(CNSForge.Signal.t()) :: CNSForge.Signal.t()

# Mark signal as routed
CNSForge.Signal.mark_routed!(signal)
```

**Parameters**:
- `signal` (CNSForge.Signal.t()): Signal to mark

**Returns**: `CNSForge.Signal.t()` - Updated signal

##### `mark_consumed!/1`

```elixir
@spec mark_consumed!(CNSForge.Signal.t()) :: CNSForge.Signal.t()

# Mark signal as consumed
CNSForge.Signal.mark_consumed!(signal)
```

**Parameters**:
- `signal` (CNSForge.Signal.t()): Signal to mark

**Returns**: `CNSForge.Signal.t()` - Updated signal

## Telemetry Frame Resource

### CNSForge.TelemetryFrame

**Module**: `CNSForge.TelemetryFrame`

**Purpose**: Universal observability and time-travel debugging.

#### Functions

##### `capture!/1`

```elixir
@spec capture!(map()) :: CNSForge.TelemetryFrame.t()

# Capture a telemetry frame
CNSForge.TelemetryFrame.capture!(%{
  transaction_id: "tx-123",
  bit_actor_id: "actor-456",
  hop_sequence: 1,
  operation: :process_data,
  input_token: %{data: "input"},
  output_token: %{result: "output"},
  ttl_before: 8,
  ttl_after: 7,
  execution_time_us: 1500,
  status: :success
})
```

**Parameters**:
- `attrs` (map()): Telemetry frame attributes

**Returns**: `CNSForge.TelemetryFrame.t()` - Created frame

##### `causal_chain/1`

```elixir
@spec causal_chain(String.t()) :: [CNSForge.TelemetryFrame.t()] | nil

# Get complete causal chain for transaction
CNSForge.TelemetryFrame.causal_chain("tx-123")
```

**Parameters**:
- `transaction_id` (String.t()): Transaction ID

**Returns**: `[CNSForge.TelemetryFrame.t()]` - Causal chain frames

##### `reconstruct_ttl_chain/1`

```elixir
@spec reconstruct_ttl_chain(String.t()) :: map()

# Reconstruct TTL chain for debugging
CNSForge.TelemetryFrame.reconstruct_ttl_chain("tx-123")
```

**Parameters**:
- `transaction_id` (String.t()): Transaction ID

**Returns**: `map()` - TTL chain reconstruction

##### `verify_chain_integrity/1`

```elixir
@spec verify_chain_integrity(String.t()) :: boolean()

# Verify chain cryptographic integrity
CNSForge.TelemetryFrame.verify_chain_integrity("tx-123")
```

**Parameters**:
- `transaction_id` (String.t()): Transaction ID

**Returns**: `boolean()` - Integrity verification result

## Application Supervision

### CNSForge.Application

**Module**: `CNSForge.Application`

**Purpose**: Application supervision tree and system initialization.

#### Callbacks

##### `start/2`

```elixir
@callback start(Application.start_type(), term()) :: 
  {:ok, pid()} | {:error, term()}

# Application start callback
```

##### `config_change/3`

```elixir
@callback config_change([{atom(), term()}], map(), [atom()]) :: :ok

# Configuration change callback
```

### CNSForge.BitActorSupervisor

**Module**: `CNSForge.BitActorSupervisor`

**Purpose**: Dynamic supervisor for BitActor lifecycles.

#### Functions

##### `spawn_bit_actor/4`

```elixir
@spec spawn_bit_actor(atom(), String.t(), map(), non_neg_integer()) :: 
  {:ok, pid()} | {:error, term()}

# Spawn a new BitActor process
CNSForge.BitActorSupervisor.spawn_bit_actor(:stimulus, "tx-123", %{}, 8)
```

**Parameters**:
- `type` (atom()): BitActor type
- `transaction_id` (String.t()): Transaction ID
- `token` (map()): Initial token
- `ttl` (non_neg_integer()): TTL budget

**Returns**:
- `{:ok, pid()}`: Successfully spawned process
- `{:error, reason}`: Spawn failure

## Web Layer

### CNSForgeWeb.Router

**Module**: `CNSForgeWeb.Router`

**Purpose**: Phoenix router for HTTP ingress and API endpoints.

#### Routes

```elixir
# Directive processing
POST "/api/directive" → CNSForgeWeb.DirectiveController.process/2

# Transaction tracing
GET "/api/trace/:transaction_id" → CNSForgeWeb.DirectiveController.trace/2

# Mesh status
GET "/api/mesh/status" → CNSForgeWeb.MeshController.status/2
GET "/api/mesh/signals" → CNSForgeWeb.MeshController.signals/2

# Telemetry endpoints
GET "/api/telemetry/metrics" → CNSForgeWeb.TelemetryController.metrics/2
GET "/api/telemetry/pulse" → CNSForgeWeb.TelemetryController.pulse_logs/2
```

### CNSForgeWeb.DirectiveController

**Module**: `CNSForgeWeb.DirectiveController`

**Purpose**: HTTP ingress controller implementing stimulus BitActor.

#### Functions

##### `process/2`

```elixir
@spec process(Plug.Conn.t(), map()) :: Plug.Conn.t()

# Process directive via HTTP
def process(conn, %{"directive" => directive} = params) do
  ttl = Map.get(params, "ttl", 8)
  # ... processing logic
end
```

**Parameters**:
- `conn` (Plug.Conn.t()): Phoenix connection
- `params` (map()): Request parameters

**Returns**: `Plug.Conn.t()` - Response connection

##### `trace/2`

```elixir
@spec trace(Plug.Conn.t(), map()) :: Plug.Conn.t()

# Trace transaction
def trace(conn, %{"transaction_id" => transaction_id}) do
  # ... tracing logic
end
```

**Parameters**:
- `conn` (Plug.Conn.t()): Phoenix connection
- `params` (map()): Request parameters

**Returns**: `Plug.Conn.t()` - Response connection

## Workflow Modules

### CNSForge.Workflows.ProcessDirective

**Module**: `CNSForge.Workflows.ProcessDirective`

**Purpose**: Main Reactor workflow for directive processing.

#### Workflow Steps

```elixir
# Step 1: Create stimulus BitActor
step :create_stimulus do
  argument :directive_token, input(:directive_token)
  run fn args, _context -> ... end
end

# Step 2: Parse directive
step :parse_directive do
  argument :stimulus, result(:create_stimulus)
  run fn args, _context -> ... end
  compensate fn _error, args, _context -> ... end
end

# Step 3: Validate directive
step :validate_directive do
  argument :parsed_result, result(:parse_directive)
  run fn args, _context -> ... end
end

# Step 4: Route to workflow
step :route_to_workflow do
  argument :validated_result, result(:validate_directive)
  run fn args, _context -> ... end
end

# Step 5: Execute business logic
step :execute_business_logic do
  argument :workflow_result, result(:route_to_workflow)
  run fn args, _context -> ... end
end
```

### CNSForge.Workflows.ComprehensiveCNSEcosystem

**Module**: `CNSForge.Workflows.ComprehensiveCNSEcosystem`

**Purpose**: Orchestrates complete CNS ecosystem using all ontologies.

#### Workflow Steps

```elixir
# Step 1: Discover ontology universe
step :discover_ontology_universe do
  argument :universe_config, input(:ontology_universe)
  argument :ttl, input(:ttl)
  run fn %{universe_config: config, ttl: ttl} -> ... end
end

# Step 2: Process ontology universe (Map)
map :process_ontology_universe do
  source result(:discover_ontology_universe, [:discovered_files])
  batch_size 25
  allow_async? true
  return :domain_bitactor_mesh
  
  step :parse_and_classify_ontology do
    argument :ontology_file, element(:process_ontology_universe)
    run fn %{ontology_file: file, ttl: ttl} -> ... end
  end
  
  switch :compile_domain_mesh do
    on result(:parse_and_classify_ontology)
    
    matches? &(&1.domain == :cybersecurity) do
      compose :compile_cybersecurity_mesh, CNSForge.Workflows.CybersecurityThreatPipeline
    end
    
    matches? &(&1.domain == :trading) do
      compose :compile_trading_mesh, CNSForge.Workflows.TradingSemanticCompiler
    end
  end
end
```

### CNSForge.Workflows.TradingSemanticCompiler

**Module**: `CNSForge.Workflows.TradingSemanticCompiler`

**Purpose**: Compiles trading domain ontologies into executable workflows.

#### Workflow Steps

```elixir
# Step 1: Extract trading patterns
step :extract_trading_patterns do
  argument :data, input(:semantic_data)
  argument :ttl, input(:ttl)
  run fn %{data: data, ttl: ttl} -> ... end
end

# Step 2: Compile instrument BitActors (Map)
map :compile_instrument_bitactors do
  source result(:extract_trading_patterns, [:trading_patterns, :instruments])
  allow_async? true
  return :instrument_bitactor
  
  step :create_instrument_bitactor do
    argument :instrument, element(:compile_instrument_bitactors)
    run fn %{instrument: inst, ttl: ttl} -> ... end
  end
end

# Step 3: Compile strategy BitActors (Switch)
switch :compile_strategy_bitactors do
  on result(:extract_trading_patterns, [:trading_patterns, :strategies])
  
  matches? &has_algorithmic_strategies?/1 do
    map :compile_algorithmic_strategies do
      source result(:extract_trading_patterns, [:trading_patterns, :strategies])
      allow_async? true
      
      step :create_algo_strategy_bitactor do
        argument :strategy, element(:compile_algorithmic_strategies)
        run fn %{strategy: strat} -> ... end
      end
    end
  end
end
```

### CNSForge.Workflows.CybersecurityThreatPipeline

**Module**: `CNSForge.Workflows.CybersecurityThreatPipeline`

**Purpose**: End-to-end cybersecurity threat detection and response.

#### Workflow Steps

```elixir
# Step 1: Extract threat feeds (Streaming)
step :extract_threat_feeds do
  argument :sources, input(:threat_feed_sources)
  argument :ttl, input(:ttl)
  max_retries 3
  compensate fn reason, _args, _context, _options -> ... end
end

# Step 2: Validate threat quality
step :validate_threat_quality do
  argument :raw_threats, result(:extract_threat_feeds)
  argument :ttl, input(:ttl)
  where fn %{raw_threats: threats} -> length(threats.items) > 0 end
end

# Step 3: Process threat batch (Map)
map :process_threat_batch do
  source result(:extract_threat_feeds, [:threat_stream])
  batch_size 100
  allow_async? true
  return :classify_threat
  
  step :parse_threat_indicators do
    argument :threat_data, element(:process_threat_batch)
    run fn %{threat_data: data, ttl: ttl} -> ... end
  end
  
  step :enrich_threat_intel do
    argument :parsed_threat, result(:parse_threat_indicators)
    run &enrich_with_misp_feed/1
    max_retries 2
    compensate fn _reason, _args, _context, _options -> :ok end
  end
  
  switch :classify_threat do
    on result(:enrich_threat_intel)
    
    matches? &(&1.ioc_type == :malware_hash) do
      step :classify_malware do
        argument :threat, result(:enrich_threat_intel)
        run fn %{threat: threat} -> ... end
      end
    end
  end
end
```

## Middleware Modules

### CNSForge.ReactorMiddleware

**Module**: `CNSForge.ReactorMiddleware`

**Purpose**: Custom middleware for CNS Forge specific functionality.

#### Functions

##### `before_action/3`

```elixir
@spec before_action(Reactor.step(), map(), Reactor.context()) :: 
  {:ok, Reactor.step()} | {:error, term()}

# Pre-execution middleware
```

##### `after_action/3`

```elixir
@spec after_action(Reactor.step(), map(), Reactor.context()) :: 
  {:ok, Reactor.step()} | {:error, term()}

# Post-execution middleware
```

### CNSForge.SemanticMiddleware

**Module**: `CNSForge.SemanticMiddleware`

**Purpose**: Semantic reasoning middleware for ontology processing.

#### Functions

##### `parse_ontology/1`

```elixir
@spec parse_ontology(String.t()) :: {:ok, map()} | {:error, term()}

# Parse TTL/RDF ontology
```

##### `validate_semantic/1`

```elixir
@spec validate_semantic(map()) :: {:ok, map()} | {:error, term()}

# Validate semantic consistency
```

## Configuration

### Mix Configuration

**File**: `mix.exs`

```elixir
defmodule CNSForge.MixProject do
  use Mix.Project
  
  def project do
    [
      app: :cns_forge,
      version: "1.0.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end
  
  def application do
    [
      mod: {CNSForge.Application, []},
      extra_applications: [:logger, :mnesia, :crypto]
    ]
  end
  
  defp deps do
    [
      {:ash, "~> 3.0"},
      {:ash_postgres, "~> 2.0"},
      {:reactor, "~> 0.8"},
      {:phoenix, "~> 1.7.0"},
      {:telemetry, "~> 1.0"},
      {:jason, "~> 1.2"},
      {:credo, "~> 1.6", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.27", only: :dev, runtime: false}
    ]
  end
end
```

## Error Handling

### Error Types

```elixir
@type ttl_expired_error :: {:error, :ttl_expired}
@type processing_error :: {:error, {:processing_failed, String.t()}}
@type validation_error :: {:error, {:validation_failed, [String.t()]}}
@type routing_error :: {:error, {:routing_failed, String.t()}}
```

### Error Handling Patterns

```elixir
# TTL validation
case ttl do
  ttl when ttl <= 0 -> {:error, :ttl_expired}
  _ -> {:ok, ttl}
end

# Saga compensation
compensate fn _error, args, _context ->
  CNSForge.BitActor.fail!(args.stimulus, %{
    error_message: "Processing failed"
  })
end

# Graceful degradation
compensate fn _reason, _args, _context, _options ->
  # Continue without enrichment on external service failure
  :ok
end
```

## Telemetry Events

### Event Types

```elixir
# BitActor events
[:cns_forge, :bit_actor, :hop]
[:cns_forge, :bit_actor, :ttl_expired]
[:cns_forge, :bit_actor, :compensate]
[:cns_forge, :bit_actor, :undo]

# Signal events
[:cns_forge, :signal, :routed]
[:cns_forge, :signal, :dropped]

# Transaction events
[:cns_forge, :transaction, :completed]
```

### Event Handlers

```elixir
def handle_event([:cns_forge, :bit_actor, :hop], measurements, metadata, _config) do
  IO.puts("""
  [PULSE] #{metadata.transaction_id} | #{metadata.step_name} | TTL: #{measurements.ttl_remaining} | #{measurements.execution_time_us}μs
  """)
end
```

## Performance Considerations

### TTL Management

```elixir
# TTL validation pattern
if ttl <= 0 do
  {:error, :ttl_expired}
else
  # Continue processing
  {:ok, %{result: result, ttl: ttl - 1}}
end
```

### Parallel Processing

```elixir
# Map step with parallel processing
map :process_items do
  source input(:items)
  allow_async? true
  batch_size 25
  
  step :process_item do
    argument :item, element(:process_items)
    run fn %{item: item} -> ... end
  end
end
```

### Streaming Processing

```elixir
# Streaming with backpressure handling
map :process_stream do
  source input(:data_stream)
  batch_size 100
  allow_async? true
  
  step :process_chunk do
    argument :chunk, element(:process_stream)
    run fn %{chunk: chunk} -> ... end
  end
end
```

---

*This API reference provides comprehensive coverage of all Elixir modules in the CNS Forge system, including function signatures, types, and usage examples.* 