# CNS Forge API Reference

## Core Modules

### CNSForge.Metacompiler

The metacompiler transforms semantic specifications into executable code.

#### Functions

##### compile/2

```elixir
@spec compile(String.t() | map(), keyword()) :: 
  {:ok, %{atom() => String.t()}} | {:error, term()}

compile(semantic_spec, opts \\ [])
```

Compiles a semantic specification into multiple target languages.

**Parameters:**
- `semantic_spec` - TTL string, BPMN XML, DMN XML, or parsed map
- `opts` - Compilation options:
  - `:targets` - List of target languages (default: `[:elixir]`)
  - `:optimize` - Enable optimization (default: `true`)
  - `:cache` - Enable caching (default: `true`)
  - `:timeout` - Compilation timeout in ms (default: `30_000`)

**Returns:**
- `{:ok, %{elixir: code, javascript: code, ...}}` - Success with generated code
- `{:error, reason}` - Compilation failure

**Example:**
```elixir
{:ok, targets} = CNSForge.Metacompiler.compile(ttl_spec, 
  targets: [:elixir, :javascript],
  optimize: true
)
```

##### parse_ttl/1

```elixir
@spec parse_ttl(String.t()) :: {:ok, map()} | {:error, term()}

parse_ttl(ttl_string)
```

Parses a TTL (Turtle) ontology into internal representation.

**Example:**
```elixir
{:ok, parsed} = CNSForge.Metacompiler.parse_ttl("""
  @prefix ex: <http://example.org/> .
  ex:Workflow a ex:Process .
""")
```

##### generate_intermediate_representation/1

```elixir
@spec generate_intermediate_representation(map()) :: {:ok, map()} | {:error, term()}

generate_intermediate_representation(parsed_spec)
```

Converts parsed specification into intermediate representation (IR).

### CNSForge.BitActor

BitActor implements TTL-bounded execution units.

#### Functions

##### create/1

```elixir
@spec create(map()) :: {:ok, BitActor.t()} | {:error, term()}

create(attrs)
```

Creates a new BitActor with specified attributes.

**Parameters:**
- `attrs` - Actor attributes:
  - `:type` - Actor type (`:stimulus`, `:sensor`, `:motor`, `:processor`)
  - `:ttl` - Time-to-live budget (default: `8`)
  - `:token` - Initial token data
  - `:transaction_id` - Unique transaction identifier

**Example:**
```elixir
{:ok, actor} = CNSForge.BitActor.create(%{
  type: :processor,
  ttl: 10,
  token: %{data: "payload"},
  transaction_id: "txn_123"
})
```

##### execute/2

```elixir
@spec execute(BitActor.t() | String.t(), atom()) :: 
  {:ok, BitActor.t()} | {:error, term()}

execute(actor_or_id, operation)
```

Executes an operation on a BitActor, consuming TTL.

**Parameters:**
- `actor_or_id` - BitActor struct or ID
- `operation` - Operation to execute

**Returns:**
- `{:ok, updated_actor}` - Success with updated actor
- `{:error, :ttl_exhausted}` - TTL budget exhausted
- `{:error, reason}` - Other failure

##### hop/2

```elixir
@spec hop(BitActor.t(), map()) :: {:ok, BitActor.t()} | {:error, term()}

hop(actor, next_token)
```

Performs a hop operation, decreasing TTL and updating token.

##### check_ttl/1

```elixir
@spec check_ttl(BitActor.t()) :: {:ok, integer()} | {:error, :ttl_exhausted}

check_ttl(actor)
```

Checks remaining TTL budget.

##### list_active/1

```elixir
@spec list_active(keyword()) :: [BitActor.t()]

list_active(opts \\ [])
```

Lists all active BitActors.

**Options:**
- `:type` - Filter by actor type
- `:min_ttl` - Minimum TTL threshold
- `:transaction_id` - Filter by transaction

### CNSForge.Workflows

Base module for defining Reactor workflows.

#### Macros

##### step/2

```elixir
step name, do: block
```

Defines a workflow step.

**Example:**
```elixir
step :validate do
  argument :input, from: :input
  run MyValidator
  compensate MyReverser
end
```

##### workflow/2

```elixir
workflow name, do: block
```

Defines a complete workflow.

#### Functions

##### run/2

```elixir
@spec run(module(), map(), keyword()) :: {:ok, term()} | {:error, term()}

run(workflow_module, input, opts \\ [])
```

Executes a workflow with given input.

**Options:**
- `:async` - Run asynchronously (default: `false`)
- `:timeout` - Execution timeout in ms (default: `60_000`)
- `:trace` - Enable tracing (default: `true`)

### CNSForge.ReactorBuilder

Dynamically builds Reactor workflows from specifications.

#### Functions

##### build_from_spec/1

```elixir
@spec build_from_spec(map()) :: {:ok, module()} | {:error, term()}

build_from_spec(workflow_spec)
```

Builds a Reactor workflow module from specification.

**Example:**
```elixir
spec = %{
  name: "PaymentFlow",
  steps: [
    %{name: :validate, module: ValidateStep},
    %{name: :process, module: ProcessStep, depends_on: [:validate]}
  ]
}

{:ok, module} = CNSForge.ReactorBuilder.build_from_spec(spec)
```

##### compile_workflow/2

```elixir
@spec compile_workflow(String.t(), map()) :: {:ok, module()} | {:error, term()}

compile_workflow(name, spec)
```

Compiles a workflow specification into an executable module.

### CNSForge.Telemetry

Telemetry integration for observability.

#### Functions

##### setup/0

```elixir
@spec setup() :: :ok

setup()
```

Sets up telemetry handlers for all CNS Forge events.

##### emit_event/3

```elixir
@spec emit_event(list(atom()), map(), map()) :: :ok

emit_event(event_name, measurements, metadata)
```

Emits a telemetry event.

**Example:**
```elixir
CNSForge.Telemetry.emit_event(
  [:cns_forge, :bitactor, :hop],
  %{duration: 100},
  %{actor_id: "123", ttl: 7}
)
```

### CNSForge.Security

Security utilities and guards.

#### Functions

##### validate_spec/1

```elixir
@spec validate_spec(String.t() | map()) :: :ok | {:error, term()}

validate_spec(spec)
```

Validates a semantic specification for security issues.

##### sanitize_code/2

```elixir
@spec sanitize_code(String.t(), atom()) :: String.t()

sanitize_code(code, language)
```

Sanitizes generated code for safe execution.

## Ash Resources

### CNSForge.BitActor (Ash Resource)

```elixir
defmodule CNSForge.BitActor do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshStateMachine]

  attributes do
    uuid_primary_key :id
    attribute :type, :atom
    attribute :transaction_id, :string
    attribute :ttl, :integer, default: 8
    attribute :token, :map
    attribute :state, :atom, default: :active
    timestamps()
  end

  actions do
    defaults [:read, :destroy]
    
    create :create do
      accept [:type, :transaction_id, :ttl, :token]
    end
    
    update :hop do
      accept [:ttl, :token]
      change decrement(:ttl)
    end
    
    update :expire do
      change set_attribute(:state, :expired)
    end
  end

  calculations do
    calculate :ttl_percentage, :float do
      calculation fn records, _ ->
        Enum.map(records, fn r -> 
          (r.ttl / 8.0) * 100
        end)
      end
    end
  end
end
```

## Phoenix Endpoints

### Health Check

```
GET /health
```

Returns system health status.

**Response:**
```json
{
  "status": "healthy",
  "version": "1.0.0",
  "timestamp": "2024-01-15T10:30:00Z"
}
```

### Metrics

```
GET /metrics
```

Returns Prometheus-formatted metrics.

### API Endpoints

#### Compile Semantic Specification

```
POST /api/v1/compile
```

**Request Body:**
```json
{
  "spec": "@prefix ex: <http://example.org/> ...",
  "format": "ttl",
  "targets": ["elixir", "javascript"],
  "optimize": true
}
```

**Response:**
```json
{
  "status": "success",
  "targets": {
    "elixir": "defmodule Workflow do...",
    "javascript": "class Workflow {..."
  },
  "compilation_time_ms": 45
}
```

#### Create BitActor

```
POST /api/v1/actors
```

**Request Body:**
```json
{
  "type": "processor",
  "ttl": 10,
  "token": {"data": "value"},
  "transaction_id": "txn_123"
}
```

**Response:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "type": "processor",
  "ttl": 10,
  "state": "active",
  "created_at": "2024-01-15T10:30:00Z"
}
```

#### Execute Workflow

```
POST /api/v1/workflows/{workflow_name}/execute
```

**Request Body:**
```json
{
  "input": {
    "amount": 100.00,
    "currency": "USD"
  },
  "options": {
    "async": false,
    "timeout": 30000
  }
}
```

**Response:**
```json
{
  "workflow_id": "wf_123",
  "status": "completed",
  "result": {...},
  "execution_time_ms": 250,
  "trace_id": "abc123"
}
```

## WebSocket API

### Actor Events

```javascript
// Connect to WebSocket
const socket = new Phoenix.Socket("/socket", {})
socket.connect()

// Join actor channel
const channel = socket.channel("actors:lobby", {})

channel.on("actor:created", payload => {
  console.log("New actor:", payload)
})

channel.on("actor:expired", payload => {
  console.log("Actor expired:", payload)
})

channel.join()
  .receive("ok", resp => console.log("Joined"))
  .receive("error", resp => console.log("Join failed"))
```

### Workflow Events

```javascript
const workflowChannel = socket.channel("workflows:updates", {})

workflowChannel.on("workflow:started", payload => {
  console.log("Workflow started:", payload)
})

workflowChannel.on("workflow:completed", payload => {
  console.log("Workflow completed:", payload)
})

workflowChannel.on("workflow:failed", payload => {
  console.log("Workflow failed:", payload)
})
```

## Error Codes

| Code | Description | Resolution |
|------|-------------|------------|
| TTL_EXHAUSTED | BitActor TTL budget exhausted | Increase TTL or optimize workflow |
| INVALID_SPEC | Semantic specification invalid | Check spec syntax and schema |
| COMPILATION_FAILED | Code generation failed | Review spec for unsupported features |
| ACTOR_NOT_FOUND | BitActor ID not found | Verify actor ID and state |
| WORKFLOW_TIMEOUT | Workflow execution timeout | Increase timeout or optimize steps |
| RATE_LIMITED | API rate limit exceeded | Implement backoff and retry |
| INVALID_TARGET | Unsupported target language | Use supported targets |
| CACHE_ERROR | Caching operation failed | Check Redis connection |

## Rate Limits

| Endpoint | Rate Limit | Window |
|----------|------------|--------|
| /api/v1/compile | 100 requests | 1 minute |
| /api/v1/actors | 1000 requests | 1 minute |
| /api/v1/workflows/*/execute | 500 requests | 1 minute |
| WebSocket connections | 100 connections | Per IP |

## Authentication

### API Key Authentication

```bash
curl -X POST https://api.cnsforge.com/api/v1/compile \
  -H "Authorization: Bearer YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"spec": "...", "targets": ["elixir"]}'
```

### JWT Authentication

```javascript
const token = jwt.sign(
  { sub: "user123", exp: Math.floor(Date.now() / 1000) + 3600 },
  process.env.JWT_SECRET
)

fetch("/api/v1/actors", {
  headers: {
    "Authorization": `Bearer ${token}`,
    "Content-Type": "application/json"
  },
  method: "POST",
  body: JSON.stringify({...})
})
```

## Versioning

The API follows semantic versioning. The current version is `v1`.

Breaking changes will result in a new API version (e.g., `v2`). Previous versions will be maintained for at least 6 months after a new version is released.

Version is specified in the URL path:
- Current: `/api/v1/*`
- Future: `/api/v2/*`