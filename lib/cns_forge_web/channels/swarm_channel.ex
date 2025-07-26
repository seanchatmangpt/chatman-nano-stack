defmodule CnsForgeWeb.SwarmChannel do
  @moduledoc """
  Phoenix Channel for Swarm Pipeline WebSocket communication.
  Connects Nuxt UI to Elixir backend for real-time telemetry and control.
  """
  
  use CnsForgeWeb, :channel
  
  alias CnsForge.{
    UltrathinkPipelineSwarm,
    WorkingPipelineSwarm,
    UltrathinkPermutationSwarm
  }
  
  @impl true
  def join("swarm:pipeline", _payload, socket) do
    # Subscribe to telemetry events
    :telemetry.attach(
      "swarm-channel-#{socket.assigns.socket_id}",
      [:ultrathink, :pipeline, :stage, :stop],
      &handle_telemetry_event/4,
      %{socket: socket}
    )
    
    send(self(), :after_join)
    {:ok, socket}
  end
  
  @impl true
  def handle_info(:after_join, socket) do
    # Send initial state
    push(socket, "initial_state", %{
      strategies: available_strategies(),
      stages: pipeline_stages(),
      connected: true
    })
    
    {:noreply, socket}
  end
  
  # Handle pipeline execution request from UI
  @impl true
  def handle_in("execute_pipeline", %{"data" => input}, socket) do
    Task.start(fn ->
      # Convert JS input to Elixir format
      elixir_input = %{
        domain: String.to_atom(input["domain"]),
        entities: input["entities"],
        complexity: input["complexity"],
        priority: String.to_atom(input["priority"])
      }
      
      # Execute pipeline with selected strategy
      strategy = Map.get(input, "strategy", "adaptive")
      
      result = case strategy do
        "permutation" ->
          UltrathinkPermutationSwarm.ultrathink_permute(elixir_input, :adaptive)
        _ ->
          WorkingPipelineSwarm.ultrathink_connect(elixir_input)
      end
      
      # Send result back to UI
      push(socket, "execution_result", %{
        success: true,
        result: format_result(result),
        duration: Map.get(result, :total_duration, 0),
        optimizations: length(Map.get(result, :optimizations, []))
      })
    end)
    
    {:reply, {:ok, %{status: "executing"}}, socket}
  end
  
  # Handle telemetry subscription
  @impl true
  def handle_in("subscribe_telemetry", _payload, socket) do
    socket = assign(socket, :telemetry_enabled, true)
    {:reply, {:ok, %{subscribed: true}}, socket}
  end
  
  # Handle strategy selection
  @impl true
  def handle_in("select_strategy", %{"strategy" => strategy}, socket) do
    socket = assign(socket, :selected_strategy, strategy)
    
    # Send strategy analysis
    analysis = analyze_strategy(strategy)
    push(socket, "strategy_analysis", analysis)
    
    {:reply, {:ok, analysis}, socket}
  end
  
  # Handle permutation matrix request
  @impl true
  def handle_in("get_permutations", %{"params" => params}, socket) do
    permutations = generate_permutations(params)
    
    push(socket, "permutation_update", %{
      permutations: permutations,
      optimal: find_optimal_permutation(permutations)
    })
    
    {:reply, {:ok, %{count: length(permutations)}}, socket}
  end
  
  # Private Functions
  
  defp handle_telemetry_event(_event_name, measurements, metadata, %{socket: socket}) do
    if Map.get(socket.assigns, :telemetry_enabled, false) do
      push(socket, "telemetry_update", %{
        type: "metrics",
        data: %{
          stage: metadata[:stage],
          duration: measurements[:duration],
          ttl_remaining: measurements[:ttl_remaining],
          correlation_id: metadata[:correlation_id],
          timestamp: DateTime.utc_now()
        }
      })
    end
  end
  
  defp available_strategies do
    [
      %{id: "linear", name: "Linear Sequential", efficiency: 0.6},
      %{id: "skip_optimization", name: "Skip Optimization (80/20)", efficiency: 0.9},
      %{id: "parallel_merge", name: "Parallel Merge", efficiency: 0.8},
      %{id: "emergence_guided", name: "Emergence Guided", efficiency: 0.85},
      %{id: "domain_specific", name: "Domain Specific", efficiency: 0.7},
      %{id: "complexity_branch", name: "Complexity Branch", efficiency: 0.75}
    ]
  end
  
  defp pipeline_stages do
    ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"]
  end
  
  defp format_result(result) do
    %{
      swarm_analysis: Map.get(result, :swarm_analysis, %{}),
      pipeline_metrics: Map.get(result, :pipeline_metrics, %{}),
      optimizations: Map.get(result, :optimizations, []),
      execution_path: extract_execution_path(result)
    }
  end
  
  defp extract_execution_path(result) do
    case result do
      %{execution_result: %{path: path}} -> path
      %{result: %{executed_stages: stages}} -> stages
      _ -> pipeline_stages()
    end
  end
  
  defp analyze_strategy(strategy) do
    %{
      strategy: strategy,
      characteristics: get_strategy_characteristics(strategy),
      recommended_for: get_strategy_recommendations(strategy),
      performance_profile: get_performance_profile(strategy)
    }
  end
  
  defp get_strategy_characteristics(strategy) do
    case strategy do
      "linear" -> 
        ["Sequential execution", "Full coverage", "Predictable"]
      "skip_optimization" ->
        ["80/20 principle", "Minimal stages", "Maximum efficiency"]
      "parallel_merge" ->
        ["Concurrent paths", "Fork-join pattern", "Reduced latency"]
      "emergence_guided" ->
        ["AI-driven", "Dynamic adaptation", "Learning patterns"]
      _ ->
        ["Standard execution"]
    end
  end
  
  defp get_strategy_recommendations(strategy) do
    case strategy do
      "skip_optimization" -> 
        ["Low complexity tasks", "Time-critical operations", "Resource constraints"]
      "parallel_merge" ->
        ["Multi-core systems", "Independent stages", "High throughput needs"]
      "emergence_guided" ->
        ["Complex decisions", "Pattern recognition", "Adaptive workflows"]
      _ ->
        ["General purpose"]
    end
  end
  
  defp get_performance_profile(strategy) do
    %{
      latency: get_strategy_latency(strategy),
      throughput: get_strategy_throughput(strategy),
      resource_usage: get_strategy_resources(strategy)
    }
  end
  
  defp get_strategy_latency(strategy) do
    case strategy do
      "skip_optimization" -> "Very Low"
      "parallel_merge" -> "Low"
      "linear" -> "Medium"
      _ -> "Variable"
    end
  end
  
  defp get_strategy_throughput(strategy) do
    case strategy do
      "parallel_merge" -> "Very High"
      "skip_optimization" -> "High"
      "linear" -> "Medium"
      _ -> "Variable"
    end
  end
  
  defp get_strategy_resources(strategy) do
    case strategy do
      "parallel_merge" -> "High"
      "linear" -> "Medium"
      "skip_optimization" -> "Low"
      _ -> "Variable"
    end
  end
  
  defp generate_permutations(params) do
    complexity = Map.get(params, "complexity", 0.5)
    domain = Map.get(params, "domain", "generic")
    
    # Generate various permutation strategies
    base_permutations = [
      generate_linear_permutation(),
      generate_skip_permutation(),
      generate_parallel_permutation(),
      generate_adaptive_permutation(complexity),
      generate_domain_permutation(domain)
    ]
    
    # Add emergence-guided variations
    emergence_variations = generate_emergence_variations(base_permutations, complexity)
    
    base_permutations ++ emergence_variations
  end
  
  defp generate_linear_permutation do
    %{
      id: "perm-linear",
      strategy: "Linear Sequential",
      path: pipeline_stages(),
      efficiency: 0.6,
      stage_count: 8
    }
  end
  
  defp generate_skip_permutation do
    critical_stages = ["typer", "turtle", "ash", "k8s"]
    %{
      id: "perm-skip",
      strategy: "Skip Optimization",
      path: critical_stages,
      efficiency: 0.9,
      stage_count: 4
    }
  end
  
  defp generate_parallel_permutation do
    %{
      id: "perm-parallel",
      strategy: "Parallel Merge",
      path: %{
        branch_a: ["typer", "turtle", "ash", "reactor"],
        branch_b: ["typer", "turtle", "ttl2dspy", "bitactor", "erlang"],
        merge: "k8s"
      },
      efficiency: 0.8,
      stage_count: 6
    }
  end
  
  defp generate_adaptive_permutation(complexity) do
    path = if complexity > 0.7 do
      pipeline_stages()
    else
      ["typer", "turtle", "ash", "k8s"]
    end
    
    %{
      id: "perm-adaptive",
      strategy: "Adaptive Branch",
      path: path,
      efficiency: 0.75,
      stage_count: length(path),
      complexity_threshold: complexity
    }
  end
  
  defp generate_domain_permutation(domain) do
    path = case domain do
      "cybersecurity" -> 
        ["typer", "turtle", "ttl2dspy", "bitactor", "ash", "reactor", "erlang", "k8s"]
      "finance" ->
        ["typer", "turtle", "ash", "reactor", "erlang", "k8s"]
      "healthcare" ->
        ["typer", "turtle", "ash", "k8s"]
      _ ->
        pipeline_stages()
    end
    
    %{
      id: "perm-domain-#{domain}",
      strategy: "Domain: #{domain}",
      path: path,
      efficiency: calculate_domain_efficiency(domain),
      stage_count: length(path),
      domain: domain
    }
  end
  
  defp generate_emergence_variations(base_permutations, complexity) do
    # Generate AI-driven variations based on patterns
    Enum.map(1..3, fn i ->
      %{
        id: "perm-emergence-#{i}",
        strategy: "Emergence Pattern #{i}",
        path: generate_emergence_path(i, complexity),
        efficiency: 0.8 + (i * 0.05),
        stage_count: 5 + i,
        ai_generated: true
      }
    end)
  end
  
  defp generate_emergence_path(pattern_id, complexity) do
    base = ["typer", "turtle", "k8s"]
    
    additional = case pattern_id do
      1 -> if complexity > 0.5, do: ["ash"], else: []
      2 -> ["ttl2dspy", "reactor"]
      3 -> ["ash", "ttl2dspy", "bitactor"]
      _ -> []
    end
    
    Enum.uniq(base ++ additional)
  end
  
  defp calculate_domain_efficiency(domain) do
    case domain do
      "healthcare" -> 0.9  # Minimal transformation preferred
      "finance" -> 0.7     # Balance of security and speed
      "cybersecurity" -> 0.6  # Comprehensive analysis required
      _ -> 0.7
    end
  end
  
  defp find_optimal_permutation(permutations) do
    permutations
    |> Enum.max_by(& &1.efficiency)
    |> Map.get(:id)
  end
end