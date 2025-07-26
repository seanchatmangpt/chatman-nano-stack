defmodule CnsForge.DSPyToBitActorTransformer do
  @moduledoc """
  ⚡ SWARM 80/20: Transform DSPy modules to BitActor distributed system
  Converts reasoning modules to distributed actors with message passing
  """
  
  @doc """
  Transform DSPy Python code to BitActor specifications
  """
  def transform(dspy_code) when is_binary(dspy_code) do
    # Extract DSPy components
    signatures = extract_signatures(dspy_code)
    modules = extract_modules(dspy_code)
    
    # Generate BitActor system
    bitactor_spec = generate_bitactor_system(signatures, modules)
    
    {:ok, bitactor_spec}
  end
  
  # Extract signature definitions from DSPy code
  defp extract_signatures(dspy_code) do
    ~r/class (\w+)Signature\(dspy\.Signature\):/
    |> Regex.scan(dspy_code)
    |> Enum.map(fn [_, name] -> 
      %{
        name: name,
        type: :signature,
        fields: extract_signature_fields(dspy_code, name)
      }
    end)
  end
  
  # Extract module definitions
  defp extract_modules(dspy_code) do
    ~r/class (\w+)Module\(dspy\.Module\):/
    |> Regex.scan(dspy_code)
    |> Enum.map(fn [_, name] -> 
      %{
        name: name,
        type: :module,
        signature: "#{name}Signature"
      }
    end)
  end
  
  # Extract fields from a signature
  defp extract_signature_fields(dspy_code, signature_name) do
    # Simple extraction - in real implementation would parse Python AST
    %{
      inputs: ["context", "query"],
      outputs: ["#{String.downcase(signature_name)}_info", "reasoning"]
    }
  end
  
  # Generate complete BitActor system
  defp generate_bitactor_system(signatures, modules) do
    """
    # BitActor Distributed System Specification
    # 🚀 SWARM 80/20: DSPy → BitActor transformation
    # Generated from DSPy reasoning modules
    
    #{generate_actor_definitions(modules)}
    
    #{generate_message_protocol(signatures)}
    
    #{generate_supervisor_tree(modules)}
    
    #{generate_router_actor(modules)}
    
    #{generate_example_usage(modules)}
    """
  end
  
  # Generate actor definitions
  defp generate_actor_definitions(modules) do
    actors = modules
    |> Enum.map(fn module ->
      """
      ## #{module.name}Actor
      
      **Actor Type**: Reasoning Actor
      **Signature**: #{module.signature}
      **Mailbox Capacity**: 1000
      **Supervision**: :permanent
      
      ### State
      ```elixir
      %{
        signature: "#{module.signature}",
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
      """
    end)
    |> Enum.join("\n")
    
    "# BitActor Definitions\n" <> actors
  end
  
  # Generate message protocol
  defp generate_message_protocol(signatures) do
    """
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
    """
  end
  
  # Generate supervisor tree
  defp generate_supervisor_tree(modules) do
    """
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
    #{generate_supervisor_children(modules)}
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
    """
  end
  
  defp generate_supervisor_children(modules) do
    modules
    |> Enum.map(fn module ->
      "      {#{module.name}Actor, name: :#{String.downcase(module.name)}_actor},"
    end)
    |> Enum.join("\n")
  end
  
  # Generate router actor
  defp generate_router_actor(modules) do
    """
    # Router Actor Specification
    
    The BitActorRouter distributes reasoning requests to appropriate actors.
    
    ```elixir
    defmodule BitActorRouter do
      use GenServer
      
      # Client API
      def reason(type, context, query, timeout \\\\ 5000) do
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
    #{generate_actor_selection(modules)}
          _ -> :unknown_actor
        end
      end
    end
    ```
    """
  end
  
  defp generate_actor_selection(modules) do
    modules
    |> Enum.map(fn module ->
      "      \"#{String.downcase(module.name)}\" -> :#{String.downcase(module.name)}_actor"
    end)
    |> Enum.join("\n")
  end
  
  # Generate example usage
  defp generate_example_usage(modules) do
    first_module = List.first(modules)
    
    if first_module do
      """
      # Example Usage
      
      ```elixir
      # Start the BitActor system
      {:ok, _sup} = BitActorSupervisor.start_link([])
      
      # Perform reasoning through router
      {:ok, response} = BitActorRouter.reason(
        "#{String.downcase(first_module.name)}",
        "In a cybersecurity context", 
        "What are the key characteristics?"
      )
      
      IO.puts("Result: \#{response.result}")
      IO.puts("Reasoning: \#{response.reasoning}")
      IO.puts("Duration: \#{response.duration_ms}ms")
      
      # Direct actor communication
      GenServer.call(:#{String.downcase(first_module.name)}_actor, {:get_stats, self()})
      
      # Distributed reasoning across multiple actors
      tasks = [
    #{generate_parallel_tasks(modules)}
      ]
      
      results = Task.await_many(tasks, 10000)
      ```
      
      ## Deployment Options
      
      1. **Single Node**: All actors on one BEAM VM
      2. **Distributed**: Actors across multiple nodes
      3. **Kubernetes**: Each actor type as a pod
      4. **Edge**: Actors near data sources
      """
    else
      "# No modules found"
    end
  end
  
  defp generate_parallel_tasks(modules) do
    modules
    |> Enum.take(3)
    |> Enum.map(fn module ->
      "    Task.async(fn -> BitActorRouter.reason(\"#{String.downcase(module.name)}\", \"context\", \"query\") end),"
    end)
    |> Enum.join("\n")
  end
end