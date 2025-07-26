# BitActor Distributed System Specification
# 🚀 SWARM 80/20: DSPy → BitActor transformation
# Generated from DSPy reasoning modules

# BitActor Definitions
## AssetActor

**Actor Type**: Reasoning Actor
**Signature**: AssetSignature
**Mailbox Capacity**: 1000
**Supervision**: :permanent

### State
```elixir
%{
  signature: "AssetSignature",
  reasoning_cache: %{},
  request_count: 0,
  last_request: nil
}
```

### Messages
- `{:reason, context, query, from}` - Perform reasoning
- `{:get_stats, from}` - Get actor statistics
- `{:clear_cache, from}` - Clear reasoning cache

### Behaviors
- Caches reasoning results for performance
- Implements timeout after 5 seconds
- Restarts on failure with exponential backoff

## ThreatActor

**Actor Type**: Reasoning Actor
**Signature**: ThreatSignature
**Mailbox Capacity**: 1000
**Supervision**: :permanent

### State
```elixir
%{
  signature: "ThreatSignature",
  reasoning_cache: %{},
  request_count: 0,
  last_request: nil
}
```

### Messages
- `{:reason, context, query, from}` - Perform reasoning
- `{:get_stats, from}` - Get actor statistics
- `{:clear_cache, from}` - Clear reasoning cache

### Behaviors
- Caches reasoning results for performance
- Implements timeout after 5 seconds
- Restarts on failure with exponential backoff

## VulnerabilityActor

**Actor Type**: Reasoning Actor
**Signature**: VulnerabilitySignature
**Mailbox Capacity**: 1000
**Supervision**: :permanent

### State
```elixir
%{
  signature: "VulnerabilitySignature",
  reasoning_cache: %{},
  request_count: 0,
  last_request: nil
}
```

### Messages
- `{:reason, context, query, from}` - Perform reasoning
- `{:get_stats, from}` - Get actor statistics
- `{:clear_cache, from}` - Clear reasoning cache

### Behaviors
- Caches reasoning results for performance
- Implements timeout after 5 seconds
- Restarts on failure with exponential backoff

## SecurityControlActor

**Actor Type**: Reasoning Actor
**Signature**: SecurityControlSignature
**Mailbox Capacity**: 1000
**Supervision**: :permanent

### State
```elixir
%{
  signature: "SecurityControlSignature",
  reasoning_cache: %{},
  request_count: 0,
  last_request: nil
}
```

### Messages
- `{:reason, context, query, from}` - Perform reasoning
- `{:get_stats, from}` - Get actor statistics
- `{:clear_cache, from}` - Clear reasoning cache

### Behaviors
- Caches reasoning results for performance
- Implements timeout after 5 seconds
- Restarts on failure with exponential backoff


# Message Protocol Specification

## Request Messages

```elixir
# Reasoning request
defmodule ReasoningRequest do
  defstruct [:actor, :context, :query, :timeout, :request_id]
end

# Reasoning response  
defmodule ReasoningResponse do
  defstruct [:request_id, :result, :reasoning, :actor, :duration_ms]
end

# Error response
defmodule ReasoningError do
  defstruct [:request_id, :error, :actor, :retry_after]
end
```

## Message Flow

```mermaid
sequenceDiagram
    Client->>Router: ReasoningRequest
    Router->>Actor: {:reason, context, query, from}
    Actor-->>Actor: Process reasoning
    Actor->>Router: {:ok, result}
    Router->>Client: ReasoningResponse
```


# Supervisor Tree Specification

```elixir
defmodule BitActorSupervisor do
  use Supervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    children = [
      # Router actor for message distribution
      {BitActorRouter, name: :router},
      
      # Reasoning actors
      {AssetActor, name: :asset_actor},
      {ThreatActor, name: :threat_actor},
      {VulnerabilityActor, name: :vulnerability_actor},
      {SecurityControlActor, name: :securitycontrol_actor},
    ]
    
    opts = [strategy: :one_for_one, max_restarts: 10, max_seconds: 60]
    Supervisor.init(children, opts)
  end
end
```

## Supervision Strategy

- **Strategy**: `:one_for_one` - Restart only failed actors
- **Max Restarts**: 10 within 60 seconds
- **Restart**: `:permanent` - Always restart on failure
- **Shutdown**: 5000ms graceful shutdown


# Router Actor Specification

The BitActorRouter distributes reasoning requests to appropriate actors.

```elixir
defmodule BitActorRouter do
  use GenServer
  
  # Client API
  def reason(type, context, query, timeout \\ 5000) do
    request = %ReasoningRequest{
      actor: type,
      context: context,
      query: query,
      timeout: timeout,
      request_id: generate_request_id()
    }
    
    GenServer.call(__MODULE__, {:route, request}, timeout)
  end
  
  # Server callbacks
  def handle_call({:route, request}, from, state) do
    actor = select_actor(request.actor)
    
    case GenServer.call(actor, {:reason, request.context, request.query}, request.timeout) do
      {:ok, result} ->
        response = %ReasoningResponse{
          request_id: request.request_id,
          result: result.info,
          reasoning: result.reasoning,
          actor: actor,
          duration_ms: calculate_duration()
        }
        {:reply, {:ok, response}, state}
        
      {:error, reason} ->
        error = %ReasoningError{
          request_id: request.request_id,
          error: reason,
          actor: actor,
          retry_after: calculate_retry_delay()
        }
        {:reply, {:error, error}, state}
    end
  end
  
  defp select_actor(type) do
    # Actor selection logic
    case String.downcase(type) do
      "asset" -> :asset_actor
      "threat" -> :threat_actor
      "vulnerability" -> :vulnerability_actor
      "securitycontrol" -> :securitycontrol_actor
      _ -> :unknown_actor
    end
  end
end
```


# Example Usage

```elixir
# Start the BitActor system
{:ok, _sup} = BitActorSupervisor.start_link([])

# Perform reasoning through router
{:ok, response} = BitActorRouter.reason(
  "asset",
  "In a cybersecurity context", 
  "What are the key characteristics?"
)

IO.puts("Result: #{response.result}")
IO.puts("Reasoning: #{response.reasoning}")
IO.puts("Duration: #{response.duration_ms}ms")

# Direct actor communication
GenServer.call(:asset_actor, {:get_stats, self()})

# Distributed reasoning across multiple actors
tasks = [
    Task.async(fn -> BitActorRouter.reason("asset", "context", "query") end),
    Task.async(fn -> BitActorRouter.reason("threat", "context", "query") end),
    Task.async(fn -> BitActorRouter.reason("vulnerability", "context", "query") end),
]

results = Task.await_many(tasks, 10000)
```

## Deployment Options

1. **Single Node**: All actors on one BEAM VM
2. **Distributed**: Actors across multiple nodes
3. **Kubernetes**: Each actor type as a pod
4. **Edge**: Actors near data sources

