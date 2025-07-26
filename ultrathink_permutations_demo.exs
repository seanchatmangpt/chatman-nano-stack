#!/usr/bin/env elixir

# ULTRATHINK SWARM 80/20: Permutations & Combinations Demo
# typer > turtle > ttl2dspy > BitActor > Erlang > Ash > Reactor > k8s

Code.require_file("lib/cns_forge/typed_ontology.ex")
Code.require_file("lib/cns_forge/turtle_generator.ex")
Code.require_file("lib/cns_forge/ttl_to_dspy_simple.ex")
Code.require_file("lib/cns_forge/simple_transformers.ex")
Code.require_file("lib/cns_forge/pipeline_permutations.ex")
Code.require_file("lib/cns_forge/parallel_pipeline_executor.ex")
Code.require_file("lib/cns_forge/bypass_transformers.ex")
Code.require_file("lib/cns_forge/ultrathink_permutation_orchestrator.ex")

defmodule UltrathinkPermutationsDemo do
  alias CnsForge.{TypedOntology, TurtleGenerator}
  alias CnsForge.{PipelinePermutations, ParallelPipelineExecutor}
  alias CnsForge.{BypassTransformers, UltrathinkPermutationOrchestrator}
  
  def run_complete_demo do
    IO.puts("🚀 ULTRATHINK SWARM 80/20: PERMUTATIONS & COMBINATIONS DEMO")
    IO.puts("=" |> String.duplicate(80))
    
    demo_ontology = create_demo_ontology()
    
    IO.puts("📊 Demo Ontology: #{length(demo_ontology.classes)} classes, #{length(demo_ontology.properties)} properties")
    IO.puts("")
    
    # Run all permutation categories
    run_basic_permutations(demo_ontology)
    run_parallel_permutations(demo_ontology)
    run_bypass_permutations(demo_ontology)
    run_orchestrated_permutations(demo_ontology)
    
    IO.puts("🎯 DEMO COMPLETED: All permutation categories demonstrated")
  end
  
  defp create_demo_ontology do
    TypedOntology.new()
    |> TypedOntology.add_namespace(:cyber, "http://cybersecurity.org/")
    |> TypedOntology.add_class("Asset", :cyber, description: "Cybersecurity asset")
    |> TypedOntology.add_class("Threat", :cyber, description: "Security threat")
    |> TypedOntology.add_class("Vulnerability", :cyber, description: "Security vulnerability")
    |> TypedOntology.add_class("SecurityControl", :cyber, description: "Security control measure")
    |> TypedOntology.add_property("exploits", :cyber, "cyber:Threat", "cyber:Vulnerability")
    |> TypedOntology.add_property("protects", :cyber, "cyber:SecurityControl", "cyber:Asset")
    |> TypedOntology.add_relationship("cyber:Threat", "cyber:exploits", "cyber:Vulnerability")
    |> TypedOntology.add_relationship("cyber:SecurityControl", "cyber:protects", "cyber:Asset")
  end
  
  defp run_basic_permutations(ontology) do
    IO.puts("🔄 TESTING BASIC PERMUTATIONS")
    IO.puts("-" |> String.duplicate(40))
    
    # Test direct typer → ash
    test_permutation("Direct TypedOntology → Ash", fn ->
      PipelinePermutations.typer_to_ash_direct(ontology)
    end)
    
    # Test parallel dspy + bitactor
    ttl = TurtleGenerator.generate(ontology)
    test_permutation("Parallel DSPy + BitActor", fn ->
      PipelinePermutations.parallel_dspy_bitactor(ttl)
    end)
    
    # Test ultra bypass typer → reactor
    test_permutation("Ultra Bypass: TypedOntology → Reactor", fn ->
      PipelinePermutations.typer_to_reactor_direct(ontology)
    end)
    
    # Test TTL → k8s direct
    test_permutation("Direct TTL → Kubernetes", fn ->
      PipelinePermutations.ttl_to_k8s_direct(ttl)
    end)
    
    # Test multi-path convergence
    test_permutation("Multi-path Convergence", fn ->
      PipelinePermutations.multi_path_convergence(ontology)
    end)
    
    IO.puts("")
  end
  
  defp run_parallel_permutations(ontology) do
    IO.puts("⚡ TESTING PARALLEL PERMUTATIONS")
    IO.puts("-" |> String.duplicate(40))
    
    # Test full parallel execution
    test_permutation("Full Parallel Pipeline", fn ->
      ParallelPipelineExecutor.execute_full_parallel(ontology)
    end)
    
    # Test optimized pipeline
    test_permutation("Optimized Parallel Pipeline", fn ->
      ParallelPipelineExecutor.execute_optimized_pipeline(ontology)
    end)
    
    # Test adaptive pipeline
    test_permutation("Adaptive Parallel Pipeline", fn ->
      ParallelPipelineExecutor.execute_adaptive_pipeline(ontology)
    end)
    
    # Test maximum parallel
    test_permutation("Maximum Parallel Execution", fn ->
      ParallelPipelineExecutor.execute_maximum_parallel(ontology)
    end)
    
    IO.puts("")
  end
  
  defp run_bypass_permutations(ontology) do
    IO.puts("🔀 TESTING BYPASS PERMUTATIONS")
    IO.puts("-" |> String.duplicate(40))
    
    # Test ultra bypass to k8s
    test_permutation("Ultra Bypass: TypedOntology → Kubernetes", fn ->
      BypassTransformers.typer_to_k8s_ultra_bypass(ontology)
    end)
    
    # Test speed bypass to ash
    test_permutation("Speed Bypass: TypedOntology → Ash", fn ->
      BypassTransformers.typer_to_ash_speed_bypass(ontology)
    end)
    
    # Test smart bypass to reactor
    test_permutation("Smart Bypass: TypedOntology → Reactor", fn ->
      BypassTransformers.typer_to_reactor_smart_bypass(ontology)
    end)
    
    # Test flow bypass to dspy
    test_permutation("Flow Bypass: TypedOntology → DSPy", fn ->
      BypassTransformers.typer_to_dspy_flow_bypass(ontology)
    end)
    
    # Test bypass chain
    test_permutation("Bypass Chain: Multiple Targets", fn ->
      BypassTransformers.execute_bypass_chain(ontology, [:k8s, :ash, :reactor])
    end)
    
    IO.puts("")
  end
  
  defp run_orchestrated_permutations(ontology) do
    IO.puts("🎯 TESTING ORCHESTRATED PERMUTATIONS")
    IO.puts("-" |> String.duplicate(40))
    
    # Test optimal orchestration
    test_permutation("Optimal Orchestration", fn ->
      UltrathinkPermutationOrchestrator.orchestrate_optimal(ontology, %{
        speed_priority: :high,
        output_formats: [:k8s, :ash],
        deployment_target: :kubernetes
      })
    end)
    
    # Test speed orchestration
    test_permutation("Speed-Optimized Orchestration", fn ->
      UltrathinkPermutationOrchestrator.orchestrate_for_speed(ontology)
    end)
    
    # Test adaptive orchestration
    test_permutation("Adaptive Orchestration", fn ->
      UltrathinkPermutationOrchestrator.orchestrate_adaptive(ontology, [])
    end)
    
    # Test comprehensive orchestration
    test_permutation("Comprehensive Orchestration", fn ->
      UltrathinkPermutationOrchestrator.orchestrate_comprehensive(ontology)
    end)
    
    # Test custom orchestration
    test_permutation("Custom Orchestration", fn ->
      UltrathinkPermutationOrchestrator.orchestrate_custom(ontology, %{
        selected_permutations: [:ultra_bypass, :speed_bypass],
        required_outputs: [:k8s, :ash]
      })
    end)
    
    IO.puts("")
  end
  
  defp test_permutation(name, test_fn) do
    IO.write("  #{name}... ")
    
    start_time = System.monotonic_time(:millisecond)
    
    try do
      case test_fn.() do
        {:ok, result} -> 
          end_time = System.monotonic_time(:millisecond)
          duration = end_time - start_time
          
          # Analyze result
          analysis = analyze_result(result)
          
          IO.puts("✅ SUCCESS (#{duration}ms)")
          IO.puts("    #{analysis}")
          
        {:error, reason} -> 
          IO.puts("❌ FAILED: #{inspect(reason)}")
          
        result -> 
          IO.puts("⚠️  UNEXPECTED: #{inspect(result)}")
      end
    rescue
      e -> 
        IO.puts("💥 CRASHED: #{inspect(e)}")
    end
  end
  
  defp analyze_result(result) when is_map(result) do
    keys = Map.keys(result)
    key_count = length(keys)
    
    # Extract key information
    bypass_info = case Map.get(result, :bypass) do
      nil -> ""
      bypasses when is_list(bypasses) -> "bypassed #{length(bypasses)} stages"
      bypass -> "bypass: #{bypass}"
    end
    
    performance_info = case Map.get(result, :performance_gain) do
      nil -> case Map.get(result, :performance) do
        nil -> ""
        perf -> "perf: #{perf}"
      end
      gain -> "gain: #{gain}"
    end
    
    output_info = case Map.get(result, :resources) do
      nil -> case Map.get(result, :k8s_manifest) do
        nil -> case Map.get(result, :reactor) do
          nil -> ""
          _ -> "reactor generated"
        end
        _ -> "k8s manifest generated"
      end
      resources when is_list(resources) -> "#{length(resources)} resources"
      _ -> "resources generated"
    end
    
    parts = [bypass_info, performance_info, output_info]
    |> Enum.filter(fn part -> part != "" end)
    
    if length(parts) > 0 do
      Enum.join(parts, ", ")
    else
      "#{key_count} result keys"
    end
  end
  
  defp analyze_result(result) do
    "result: #{inspect(result)}"
  end
  
  def benchmark_all_permutations do
    IO.puts("📊 BENCHMARKING ALL PERMUTATIONS")
    IO.puts("=" |> String.duplicate(60))
    
    ontology = create_demo_ontology()
    
    permutations = [
      {"Direct Ash", fn -> PipelinePermutations.typer_to_ash_direct(ontology) end},
      {"Direct Reactor", fn -> PipelinePermutations.typer_to_reactor_direct(ontology) end},
      {"Ultra Bypass K8s", fn -> BypassTransformers.typer_to_k8s_ultra_bypass(ontology) end},
      {"Speed Bypass Ash", fn -> BypassTransformers.typer_to_ash_speed_bypass(ontology) end},
      {"Smart Bypass Reactor", fn -> BypassTransformers.typer_to_reactor_smart_bypass(ontology) end},
      {"Parallel Optimized", fn -> ParallelPipelineExecutor.execute_optimized_pipeline(ontology) end}
    ]
    
    benchmark_results = Enum.map(permutations, fn {name, test_fn} ->
      {duration, result} = :timer.tc(test_fn)
      duration_ms = div(duration, 1000)
      
      success = case result do
        {:ok, _} -> true
        _ -> false
      end
      
      %{
        name: name,
        duration_ms: duration_ms,
        success: success,
        result: result
      }
    end)
    
    # Sort by duration (fastest first)
    sorted_results = Enum.sort_by(benchmark_results, & &1.duration_ms)
    
    IO.puts("📈 BENCHMARK RESULTS (fastest first):")
    IO.puts("")
    
    Enum.each(sorted_results, fn %{name: name, duration_ms: duration, success: success} ->
      status = if success, do: "✅", else: "❌"
      IO.puts("  #{status} #{String.pad_trailing(name, 25)} #{duration}ms")
    end)
    
    IO.puts("")
    fastest = List.first(sorted_results)
    IO.puts("🏆 FASTEST: #{fastest.name} (#{fastest.duration_ms}ms)")
    
    success_count = Enum.count(benchmark_results, & &1.success)
    IO.puts("📊 SUCCESS RATE: #{success_count}/#{length(benchmark_results)} (#{round(success_count/length(benchmark_results)*100)}%)")
  end
  
  def generate_permutation_report do
    IO.puts("📋 PERMUTATION CAPABILITIES REPORT")
    IO.puts("=" |> String.duplicate(50))
    
    report = %{
      basic_permutations: [
        "Direct TypedOntology → Ash (skip TTL)",
        "Parallel DSPy + BitActor from TTL",
        "Ultra bypass TypedOntology → Reactor",
        "Direct TTL → Kubernetes",
        "Multi-path convergence at Ash"
      ],
      parallel_permutations: [
        "Full parallel pipeline execution",
        "Optimized parallel with intelligent path selection", 
        "Adaptive parallel based on system load",
        "Maximum parallel for high-performance systems",
        "Stream-based parallel for continuous processing"
      ],
      bypass_permutations: [
        "Ultra bypass: TypedOntology → Kubernetes (skip everything)",
        "Speed bypass: TypedOntology → Ash (skip TTL parsing)",
        "Smart bypass: TypedOntology → Reactor (intelligent optimization)",
        "Flow bypass: TypedOntology → DSPy (skip TTL generation)",
        "Chain bypass: Multiple targets simultaneously"
      ],
      orchestrated_permutations: [
        "Optimal orchestration with automatic strategy selection",
        "Speed-optimized for maximum performance",
        "Adaptive with learning from feedback",
        "Comprehensive executing all valuable permutations",
        "Custom with user-defined permutation selection"
      ]
    }
    
    Enum.each(report, fn {category, permutations} ->
      IO.puts("#{category |> to_string() |> String.upcase() |> String.replace("_", " ")}:")
      Enum.each(permutations, fn perm ->
        IO.puts("  • #{perm}")
      end)
      IO.puts("")
    end)
    
    IO.puts("🎯 TOTAL CAPABILITIES:")
    total_permutations = Enum.reduce(report, 0, fn {_, perms}, acc -> acc + length(perms) end)
    IO.puts("  • #{total_permutations} different permutation strategies")
    IO.puts("  • Automatic optimal path selection")
    IO.puts("  • Performance optimization based on ontology size")
    IO.puts("  • Resource-aware execution strategies")
    IO.puts("  • Learning and adaptation capabilities")
  end
end

# Run the complete demonstration
UltrathinkPermutationsDemo.run_complete_demo()

IO.puts("")
UltrathinkPermutationsDemo.benchmark_all_permutations()

IO.puts("")
UltrathinkPermutationsDemo.generate_permutation_report()