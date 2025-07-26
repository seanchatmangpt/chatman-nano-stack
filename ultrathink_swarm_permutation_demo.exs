#!/usr/bin/env elixir
# 🔄 UltraThink Swarm 80/20 Permutation & Combination Demo
# Shows different ways to connect existing code components

Code.require_file("lib/cns_forge/typed_ontology.ex")
Code.require_file("lib/cns_forge/turtle_generator.ex")
Code.require_file("lib/cns_forge/ttl_to_dspy_simple.ex")
Code.require_file("lib/cns_forge/dspy_to_bitactor_transformer.ex")
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")
Code.require_file("lib/cns_forge/ultrathink_swarm_permutation_orchestrator.ex")

defmodule UltraThinkSwarmPermutationDemo do
  @moduledoc """
  Demonstrates different permutations and combinations of the UltraThink Swarm 80/20 pipeline
  """
  
  def run do
    IO.puts """
    ╔═══════════════════════════════════════════════════╗
    ║  🔄 UltraThink Swarm 80/20 Permutations Demo     ║
    ║                                                   ║
    ║  Exploring New Combinations of Existing Code:    ║
    ║  typer > turtle > ttl2dspy > BitActor >          ║
    ║  Erlang > Ash > Reactor > k8s                    ║
    ╚═══════════════════════════════════════════════════╝
    """
    
    # Start the permutation orchestrator
    {:ok, _pid} = CnsForge.UltraThinkSwarmPermutationOrchestrator.start_link()
    
    # Test data for demonstrations
    test_data = %{
      critical_types: [
        %{name: "ThreatActor", attributes: ["id", "name", "tactics"]},
        %{name: "Vulnerability", attributes: ["id", "severity", "cvss_score"]},
        %{name: "Asset", attributes: ["id", "type", "value"]}
      ],
      relationships: [
        %{source: "ThreatActor", target: "Vulnerability", predicate: "exploits"},
        %{source: "Vulnerability", target: "Asset", predicate: "affects"}
      ]
    }
    
    # Demo different permutation patterns
    demo_linear_pattern(test_data)
    demo_parallel_pattern(test_data) 
    demo_diamond_pattern(test_data)
    demo_hybrid_pattern(test_data)
    demo_adaptive_pattern(test_data)
    demo_mesh_pattern(test_data)
    
    # Compare all patterns
    compare_all_patterns(test_data)
    
    # Generate permutation metrics
    generate_permutation_metrics()
  end
  
  defp demo_linear_pattern(test_data) do
    IO.puts "\n🔗 Demo 1: Linear Pattern (Original Flow)"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :linear) do
      {:ok, result} ->
        IO.puts "✅ Linear pattern executed successfully!"
        IO.puts "   Stages: #{inspect(result[:stages_executed])}"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Linear pattern failed: #{reason}"
    end
  end
  
  defp demo_parallel_pattern(test_data) do
    IO.puts "\n🔀 Demo 2: Parallel Split Pattern"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    IO.puts "   Flow: typer → [turtle|bitactor] → [ttl2dspy|erlang] → merge → ash → reactor → k8s"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :parallel_split) do
      {:ok, result} ->
        IO.puts "✅ Parallel pattern executed successfully!"
        IO.puts "   Branches executed: #{result[:branches_executed]}"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Parallel pattern failed: #{reason}"
    end
  end
  
  defp demo_diamond_pattern(test_data) do
    IO.puts "\n💎 Demo 3: Diamond Pattern"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    IO.puts "   Flow: typer → [turtle & bitactor] → [ttl2dspy & erlang] → merge → ash → reactor → k8s"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :diamond) do
      {:ok, result} ->
        IO.puts "✅ Diamond pattern executed successfully!"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Diamond pattern failed: #{reason}"
    end
  end
  
  defp demo_hybrid_pattern(test_data) do
    IO.puts "\n🔧 Demo 4: Hybrid Pattern"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    IO.puts "   Flow: typer → turtle → [parallel branches] → cross-connect → reactor → k8s"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :hybrid) do
      {:ok, result} ->
        IO.puts "✅ Hybrid pattern executed successfully!"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Hybrid pattern failed: #{reason}"
    end
  end
  
  defp demo_adaptive_pattern(test_data) do
    IO.puts "\n🤖 Demo 5: Adaptive Routing Pattern"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    IO.puts "   Flow: Dynamic routing based on input characteristics"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :adaptive) do
      {:ok, result} ->
        IO.puts "✅ Adaptive pattern executed successfully!"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Adaptive pattern failed: #{reason}"
    end
  end
  
  defp demo_mesh_pattern(test_data) do
    IO.puts "\n🕸️  Demo 6: Mesh Pattern (Full Interconnect)"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    IO.puts "   Flow: Components can connect to any other component"
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.execute_permutation(test_data, :mesh) do
      {:ok, result} ->
        IO.puts "✅ Mesh pattern executed successfully!"
        IO.puts "   Execution order: #{inspect(result[:execution_order])}"
        IO.puts "   Pattern type: #{result[:pattern_type]}"
        
      {:error, reason} ->
        IO.puts "❌ Mesh pattern failed: #{reason}"
    end
  end
  
  defp compare_all_patterns(test_data) do
    IO.puts "\n📊 Demo 7: Comparative Analysis of All Patterns"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    patterns = [:linear, :parallel_split, :diamond, :hybrid, :adaptive, :mesh]
    
    case CnsForge.UltraThinkSwarmPermutationOrchestrator.compare_permutations(test_data, patterns) do
      {:ok, results} ->
        IO.puts "✅ Comparative analysis completed!"
        IO.puts "\n📈 Performance Comparison:"
        
        results
        |> Enum.sort_by(& &1.duration_us)
        |> Enum.with_index(1)
        |> Enum.each(fn {result, rank} ->
          status = if result.success, do: "✅", else: "❌"
          IO.puts "   #{rank}. #{result.pattern}: #{result.duration_us}µs #{status}"
        end)
        
        fastest = Enum.min_by(results, & &1.duration_us)
        slowest = Enum.max_by(results, & &1.duration_us)
        
        IO.puts "\n🏆 Fastest: #{fastest.pattern} (#{fastest.duration_us}µs)"
        IO.puts "🐌 Slowest: #{slowest.pattern} (#{slowest.duration_us}µs)"
        
        speedup = slowest.duration_us / fastest.duration_us
        IO.puts "⚡ Speedup: #{Float.round(speedup, 2)}x"
        
      {:error, reason} ->
        IO.puts "❌ Comparative analysis failed: #{reason}"
    end
  end
  
  defp generate_permutation_metrics() do
    IO.puts "\n📊 Permutation Metrics & Insights"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    patterns = CnsForge.UltraThinkSwarmPermutationOrchestrator.get_patterns()
    
    IO.puts "Available Permutation Patterns: #{length(patterns)}"
    IO.puts "Pattern Types:"
    
    Enum.each(patterns, fn pattern ->
      description = case pattern do
        :linear -> "Sequential execution through all stages"
        :parallel_split -> "Parallel branches with merge points"
        :diamond -> "Fan-out, parallel processing, fan-in"  
        :reverse_flow -> "Reverse pipeline execution order"
        :hybrid -> "Mixed sequential and parallel execution"
        :adaptive -> "Dynamic routing based on input"
        :mesh -> "Full interconnection between components"
      end
      
      IO.puts "  • #{pattern}: #{description}"
    end)
    
    IO.puts "\n🔍 Key Insights:"
    IO.puts "  • Parallel patterns reduce latency for independent operations"
    IO.puts "  • Diamond pattern maximizes throughput with synchronization"
    IO.puts "  • Adaptive routing optimizes for different input types"
    IO.puts "  • Mesh topology provides maximum flexibility"
    IO.puts "  • Linear pattern ensures predictable execution order"
    
    IO.puts "\n🚀 Optimization Opportunities:"
    IO.puts "  • Use parallel patterns for CPU-intensive workloads"
    IO.puts "  • Apply adaptive routing for mixed input scenarios"
    IO.puts "  • Leverage mesh topology for complex dependencies"
    IO.puts "  • Choose linear for simple, reliable processing"
  end
  
  defp show_permutation_flow_diagram do
    IO.puts """
    
    🔄 Permutation Flow Diagrams:
    
    Linear:     [typer] → [turtle] → [ttl2dspy] → [bitactor] → [erlang] → [ash] → [reactor] → [k8s]
    
    Parallel:   [typer] → [turtle|bitactor] → [ttl2dspy|erlang] → [merge] → [ash] → [reactor] → [k8s]
    
    Diamond:    [typer] ┌─ [turtle] ──┐
                        │             ├─ [merge] → [ash] → [reactor] → [k8s]  
                        └─ [bitactor] ─┘
    
    Adaptive:   [typer] → [decision] → [optimal_path] → [k8s]
    
    Mesh:       [Any] ←→ [Any] ←→ [Any] ←→ [Any]
                  ↕       ↕       ↕       ↕
                [Any] ←→ [Any] ←→ [Any] ←→ [Any]
    """
  end
end

# Run the permutation demo
UltraThinkSwarmPermutationDemo.run()