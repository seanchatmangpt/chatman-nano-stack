defmodule CnsForge.UnifiedPipelineAPI do
  @moduledoc """
  🎯 ULTRATHINK SWARM 80/20: Unified Pipeline API
  Single interface for ALL transformation approaches
  typer > turtle > ttl2dspy > BitActor > Erlang > Ash > Reactor > k8s
  """
  
  alias CnsForge.{TypedOntology, MasterPipelineOrchestrator}
  alias CnsForge.{PipelinePermutations, ParallelPipelineExecutor}
  alias CnsForge.{BypassTransformers, UltrathinkPermutationOrchestrator}
  
  require Logger
  
  @doc """
  🚀 MAIN API: transform/2 - Universal transformation function
  Automatically selects optimal approach based on input and options
  
  ## Examples
      # Simple transformation (auto-optimized)
      {:ok, result} = UnifiedPipelineAPI.transform(ontology)
      
      # Speed-optimized transformation
      {:ok, result} = UnifiedPipelineAPI.transform(ontology, mode: :speed)
      
      # Custom transformation
      {:ok, result} = UnifiedPipelineAPI.transform(ontology, 
        mode: :custom, 
        approaches: [:bypass, :parallel],
        outputs: [:k8s, :ash]
      )
  """
  def transform(ontology = %TypedOntology{}, opts \\ []) do
    Logger.info("🎯 UNIFIED API: Starting transformation")
    
    # Parse options
    mode = Keyword.get(opts, :mode, :auto)
    requirements = build_requirements_from_opts(opts)
    
    # Route to appropriate transformation approach
    case mode do
      :auto -> 
        MasterPipelineOrchestrator.execute_optimal(ontology, requirements)
        
      :speed -> 
        MasterPipelineOrchestrator.execute_for_speed(ontology)
        
      :comprehensive -> 
        MasterPipelineOrchestrator.execute_comprehensive(ontology)
        
      :custom -> 
        custom_config = build_custom_config_from_opts(opts)
        MasterPipelineOrchestrator.execute_custom(ontology, custom_config)
        
      :existing -> 
        execute_existing_only(ontology, opts)
        
      :permutations -> 
        execute_permutations_only(ontology, opts)
        
      specific_approach when is_atom(specific_approach) ->
        execute_specific_approach(specific_approach, ontology, opts)
    end
  end
  
  @doc """
  ⚡ SPEED API: Fastest possible transformation
  """
  def transform_fast(ontology = %TypedOntology{}, target \\ :k8s) do
    case target do
      :k8s -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
      :ash -> BypassTransformers.typer_to_ash_speed_bypass(ontology)
      :reactor -> BypassTransformers.typer_to_reactor_smart_bypass(ontology)
      :dspy -> BypassTransformers.typer_to_dspy_flow_bypass(ontology)
      :all -> BypassTransformers.execute_bypass_chain(ontology, [:k8s, :ash, :reactor, :dspy])
    end
  end
  
  @doc """
  🔄 COMPREHENSIVE API: All transformation approaches
  """
  def transform_comprehensive(ontology = %TypedOntology{}) do
    MasterPipelineOrchestrator.execute_comprehensive(ontology)
  end
  
  @doc """
  🎛️ CUSTOM API: User-defined transformation
  """
  def transform_custom(ontology = %TypedOntology{}, config) do
    MasterPipelineOrchestrator.execute_custom(ontology, config)
  end
  
  @doc """
  📊 ANALYSIS API: Analyze ontology and recommend approach
  """
  def analyze_and_recommend(ontology = %TypedOntology{}, requirements \\ %{}) do
    # Analyze ontology characteristics
    analysis = %{
      size: %{
        classes: length(ontology.classes),
        properties: length(ontology.properties),
        namespaces: length(ontology.namespaces)
      },
      complexity: calculate_complexity(ontology),
      recommended_approaches: recommend_approaches(ontology, requirements)
    }
    
    {:ok, analysis}
  end
  
  @doc """
  🔗 BRIDGE API: Connect existing components with new permutations
  """
  def bridge(existing_component, permutation, ontology = %TypedOntology{}) do
    MasterPipelineOrchestrator.bridge_existing_with_permutation(
      existing_component, 
      permutation, 
      ontology
    )
  end
  
  @doc """
  📈 BENCHMARK API: Compare all approaches
  """
  def benchmark(ontology = %TypedOntology{}, approaches \\ :all) do
    Logger.info("📈 UNIFIED API: Starting benchmark")
    
    # Define benchmark approaches
    benchmark_approaches = case approaches do
      :all -> [:ultra_bypass, :speed_bypass, :smart_bypass, :parallel_full, :existing_traditional]
      :fast -> [:ultra_bypass, :speed_bypass, :smart_bypass]
      :comprehensive -> [:parallel_full, :orchestrated_comprehensive, :existing_traditional]
      list when is_list(list) -> list
    end
    
    # Execute benchmarks with timing
    benchmark_results = Enum.map(benchmark_approaches, fn approach ->
      {time_us, result} = :timer.tc(fn ->
        execute_benchmark_approach(approach, ontology)
      end)
      
      time_ms = div(time_us, 1000)
      
      %{
        approach: approach,
        duration_ms: time_ms,
        result: result,
        success: match?({:ok, _}, result),
        performance_score: calculate_performance_score(result, time_ms)
      }
    end)
    
    # Analyze results
    analysis = analyze_benchmark_results(benchmark_results)
    
    {:ok, %{
      benchmark_results: benchmark_results,
      analysis: analysis,
      fastest: find_fastest(benchmark_results),
      most_comprehensive: find_most_comprehensive(benchmark_results),
      recommended: recommend_from_benchmark(benchmark_results)
    }}
  end
  
  @doc """
  🎯 ROUTE API: Intelligently route based on requirements
  """
  def route(ontology = %TypedOntology{}, requirements) do
    # Intelligent routing based on requirements
    route_decision = make_routing_decision(ontology, requirements)
    
    Logger.info("🎯 ROUTING: Selected #{route_decision.approach} (confidence: #{route_decision.confidence})")
    
    # Execute selected approach
    result = execute_routed_approach(route_decision.approach, ontology, requirements)
    
    {:ok, %{
      route_decision: route_decision,
      result: result,
      routing_success: true
    }}
  end
  
  @doc """
  🔍 INSPECT API: Inspect available transformation paths
  """
  def inspect_paths do
    %{
      existing_approaches: [
        :traditional_pipeline,
        :ttl_ash_reactor_transformer,
        :pipeline_connector
      ],
      bypass_approaches: [
        :ultra_bypass_k8s,
        :speed_bypass_ash,
        :smart_bypass_reactor,
        :flow_bypass_dspy,
        :bypass_chain
      ],
      parallel_approaches: [
        :full_parallel,
        :optimized_parallel,
        :adaptive_parallel,
        :maximum_parallel
      ],
      orchestrated_approaches: [
        :optimal_orchestration,
        :speed_orchestration,
        :adaptive_orchestration,
        :comprehensive_orchestration,
        :custom_orchestration
      ],
      hybrid_approaches: [
        :existing_enhanced,
        :hybrid_optimal,
        :permutation_primary,
        :conservative_integration
      ]
    }
  end
  
  @doc """
  📋 STATUS API: Get system status and capabilities
  """
  def status do
    %{
      api_version: "1.0.0",
      available_transformations: 25,
      system_resources: %{
        cores: System.schedulers_online(),
        memory_mb: estimate_available_memory()
      },
      pipeline_stages: [
        "typer", "turtle", "ttl2dspy", "BitActor", 
        "Erlang", "Ash", "Reactor", "k8s"
      ],
      bypass_capabilities: [
        "Skip any intermediate stage",
        "Direct transformations",
        "Multi-target output"
      ],
      parallel_capabilities: [
        "Concurrent stage execution",
        "Adaptive load balancing", 
        "Stream processing"
      ]
    }
  end
  
  # Helper Functions
  
  defp build_requirements_from_opts(opts) do
    %{
      speed_priority: Keyword.get(opts, :speed_priority, :medium),
      output_formats: Keyword.get(opts, :outputs, [:ash, :reactor]),
      existing_code_preference: Keyword.get(opts, :existing_preference, :balanced),
      innovation_tolerance: Keyword.get(opts, :innovation, :medium),
      resource_constraints: Keyword.get(opts, :resources, :medium)
    }
  end
  
  defp build_custom_config_from_opts(opts) do
    %{
      existing_approaches: Keyword.get(opts, :existing, []),
      permutation_approaches: Keyword.get(opts, :approaches, []),
      execution_style: Keyword.get(opts, :execution, :parallel),
      required_outputs: Keyword.get(opts, :outputs, [])
    }
  end
  
  defp execute_existing_only(ontology, opts) do
    # Execute only existing approaches
    approach = Keyword.get(opts, :existing_approach, :traditional)
    
    case approach do
      :traditional -> execute_traditional_pipeline(ontology)
      :connector -> execute_connector_pipeline()
      _ -> execute_traditional_pipeline(ontology)
    end
  end
  
  defp execute_permutations_only(ontology, opts) do
    # Execute only new permutation approaches
    permutation_type = Keyword.get(opts, :permutation_type, :optimal)
    
    case permutation_type do
      :bypass -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
      :parallel -> ParallelPipelineExecutor.execute_full_parallel(ontology)
      :orchestrated -> UltrathinkPermutationOrchestrator.orchestrate_optimal(ontology)
      :optimal -> UltrathinkPermutationOrchestrator.orchestrate_optimal(ontology)
    end
  end
  
  defp execute_specific_approach(approach, ontology, opts) do
    case approach do
      :ultra_bypass -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
      :speed_bypass -> BypassTransformers.typer_to_ash_speed_bypass(ontology)
      :smart_bypass -> BypassTransformers.typer_to_reactor_smart_bypass(ontology)
      :parallel_full -> ParallelPipelineExecutor.execute_full_parallel(ontology)
      :orchestrated -> UltrathinkPermutationOrchestrator.orchestrate_optimal(ontology, build_requirements_from_opts(opts))
      _ -> {:error, {:unknown_approach, approach}}
    end
  end
  
  defp calculate_complexity(ontology) do
    class_count = length(ontology.classes)
    property_count = length(ontology.properties)
    relationship_count = length(ontology.relationships)
    
    total_score = class_count + (property_count * 0.5) + (relationship_count * 0.8)
    
    cond do
      total_score <= 10 -> :simple
      total_score <= 25 -> :moderate
      total_score <= 50 -> :complex
      true -> :enterprise
    end
  end
  
  defp recommend_approaches(ontology, requirements) do
    complexity = calculate_complexity(ontology)
    speed_priority = Map.get(requirements, :speed_priority, :medium)
    
    case {complexity, speed_priority} do
      {:simple, :high} -> [:ultra_bypass, :speed_bypass]
      {:simple, _} -> [:speed_bypass, :smart_bypass, :traditional]
      {:moderate, :high} -> [:parallel_optimized, :smart_bypass]
      {:moderate, _} -> [:orchestrated_optimal, :parallel_full]
      {:complex, :high} -> [:parallel_full, :orchestrated_comprehensive]
      {:complex, _} -> [:orchestrated_comprehensive, :hybrid_optimal]
      {:enterprise, _} -> [:orchestrated_comprehensive, :parallel_full, :hybrid_optimal]
    end
  end
  
  defp execute_benchmark_approach(approach, ontology) do
    case approach do
      :ultra_bypass -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
      :speed_bypass -> BypassTransformers.typer_to_ash_speed_bypass(ontology)
      :smart_bypass -> BypassTransformers.typer_to_reactor_smart_bypass(ontology)
      :parallel_full -> ParallelPipelineExecutor.execute_full_parallel(ontology)
      :existing_traditional -> execute_traditional_pipeline(ontology)
      :orchestrated_comprehensive -> UltrathinkPermutationOrchestrator.orchestrate_comprehensive(ontology)
      _ -> {:error, {:unknown_benchmark_approach, approach}}
    end
  end
  
  defp calculate_performance_score(result, time_ms) do
    # Calculate performance score based on result quality and time
    base_score = case result do
      {:ok, _} -> 100
      {:error, _} -> 0
      _ -> 50
    end
    
    # Time penalty (higher time = lower score)
    time_penalty = min(time_ms / 100, 50)  # Max 50 point penalty
    
    max(base_score - time_penalty, 0)
  end
  
  defp analyze_benchmark_results(results) do
    successful_results = Enum.filter(results, & &1.success)
    
    %{
      total_approaches: length(results),
      successful_approaches: length(successful_results),
      success_rate: length(successful_results) / length(results),
      average_duration: calculate_average_duration(results),
      speed_leader: find_speed_leader(successful_results),
      reliability_leader: find_reliability_leader(results)
    }
  end
  
  defp calculate_average_duration(results) do
    if length(results) > 0 do
      total_time = Enum.sum(Enum.map(results, & &1.duration_ms))
      div(total_time, length(results))
    else
      0
    end
  end
  
  defp find_speed_leader(successful_results) do
    case Enum.min_by(successful_results, & &1.duration_ms, fn -> nil end) do
      nil -> :none
      result -> result.approach
    end
  end
  
  defp find_reliability_leader(results) do
    # Find approach with best combination of success and performance
    case Enum.max_by(results, & &1.performance_score, fn -> nil end) do
      nil -> :none
      result -> result.approach
    end
  end
  
  defp find_fastest(results) do
    successful_results = Enum.filter(results, & &1.success)
    case Enum.min_by(successful_results, & &1.duration_ms, fn -> nil end) do
      nil -> nil
      result -> %{approach: result.approach, duration_ms: result.duration_ms}
    end
  end
  
  defp find_most_comprehensive(results) do
    # Find approach that generates most comprehensive output
    case Enum.find(results, fn r -> 
      String.contains?(to_string(r.approach), "comprehensive") and r.success
    end) do
      nil -> nil
      result -> %{approach: result.approach, performance_score: result.performance_score}
    end
  end
  
  defp recommend_from_benchmark(results) do
    # Recommend best overall approach
    successful_results = Enum.filter(results, & &1.success)
    
    case Enum.max_by(successful_results, & &1.performance_score, fn -> nil end) do
      nil -> :none
      result -> %{
        approach: result.approach,
        reason: "Best balance of speed and reliability",
        performance_score: result.performance_score
      }
    end
  end
  
  defp make_routing_decision(ontology, requirements) do
    # Intelligent routing logic
    complexity = calculate_complexity(ontology)
    speed_priority = Map.get(requirements, :speed_priority, :medium)
    outputs_needed = Map.get(requirements, :outputs, [:ash])
    
    # Decision matrix
    {approach, confidence} = case {complexity, speed_priority, length(outputs_needed)} do
      {:simple, :high, 1} -> {:ultra_bypass, 0.95}
      {:simple, :high, _} -> {:speed_bypass, 0.9}
      {:simple, _, _} -> {:smart_bypass, 0.85}
      {:moderate, :high, _} -> {:parallel_optimized, 0.8}
      {:moderate, _, _} -> {:orchestrated_optimal, 0.85}
      {:complex, _, _} -> {:orchestrated_comprehensive, 0.9}
      {:enterprise, _, _} -> {:hybrid_optimal, 0.85}
    end
    
    %{
      approach: approach,
      confidence: confidence,
      reasoning: generate_routing_reasoning(complexity, speed_priority, outputs_needed),
      alternatives: suggest_alternatives(approach)
    }
  end
  
  defp generate_routing_reasoning(complexity, speed_priority, outputs_needed) do
    "Selected based on complexity: #{complexity}, speed priority: #{speed_priority}, outputs needed: #{length(outputs_needed)}"
  end
  
  defp suggest_alternatives(primary_approach) do
    case primary_approach do
      :ultra_bypass -> [:speed_bypass, :smart_bypass]
      :speed_bypass -> [:ultra_bypass, :smart_bypass]
      :smart_bypass -> [:speed_bypass, :orchestrated_optimal]
      :parallel_optimized -> [:orchestrated_optimal, :hybrid_optimal]
      :orchestrated_optimal -> [:parallel_optimized, :hybrid_optimal]
      :orchestrated_comprehensive -> [:hybrid_optimal, :parallel_full]
      :hybrid_optimal -> [:orchestrated_comprehensive, :orchestrated_optimal]
      _ -> []
    end
  end
  
  defp execute_routed_approach(approach, ontology, requirements) do
    case approach do
      :ultra_bypass -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
      :speed_bypass -> BypassTransformers.typer_to_ash_speed_bypass(ontology)
      :smart_bypass -> BypassTransformers.typer_to_reactor_smart_bypass(ontology)
      :parallel_optimized -> ParallelPipelineExecutor.execute_optimized_pipeline(ontology)
      :orchestrated_optimal -> UltrathinkPermutationOrchestrator.orchestrate_optimal(ontology, requirements)
      :orchestrated_comprehensive -> UltrathinkPermutationOrchestrator.orchestrate_comprehensive(ontology)
      :hybrid_optimal -> MasterPipelineOrchestrator.execute_optimal(ontology, requirements)
      _ -> {:error, {:unknown_routed_approach, approach}}
    end
  end
  
  defp execute_traditional_pipeline(ontology) do
    # Execute traditional pipeline using existing code
    try do
      alias CnsForge.{TurtleGenerator, TTLAshReactorTransformer}
      ttl = TurtleGenerator.generate(ontology)
      TTLAshReactorTransformer.transform_ttl(ttl)
    rescue
      e -> {:error, e}
    end
  end
  
  defp execute_connector_pipeline do
    # Execute using existing pipeline connector
    try do
      alias CnsForge.PipelineConnector
      {:ok, PipelineConnector.execute_full_pipeline()}
    rescue
      e -> {:error, e}
    end
  end
  
  defp estimate_available_memory do
    # Simplified memory estimation
    try do
      case :memsup.get_system_memory_data() do
        data when is_list(data) ->
          available = Keyword.get(data, :available_memory, 4_000_000_000)
          div(available, 1_000_000)  # Convert to MB
        _ -> 4096  # Fallback: 4GB
      end
    rescue
      _ -> 4096
    end
  end
end