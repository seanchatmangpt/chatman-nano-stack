#!/usr/bin/env elixir

# Test Ultrathink Permutation Swarm - All Strategies

Code.compile_file("lib/cns_forge/ultrathink_permutation_swarm.ex")

defmodule PermutationSwarmTest do
  def run do
    IO.puts("🔄 ULTRATHINK PERMUTATION SWARM - COMPREHENSIVE TEST\n")
    
    # Test different input scenarios
    test_scenarios = [
      %{
        name: "High Complexity Cybersecurity",
        input: %{
          domain: :cybersecurity,
          entities: ["ThreatActor", "Malware", "Vulnerability", "Attack", "Defense", "Incident", "IOC", "Campaign"],
          complexity: 0.95,
          priority: :critical
        }
      },
      %{
        name: "Medium Finance System", 
        input: %{
          domain: :finance,
          entities: ["Transaction", "Account", "Risk", "Compliance"],
          complexity: 0.6,
          priority: :high
        }
      },
      %{
        name: "Simple Healthcare Data",
        input: %{
          domain: :healthcare,
          entities: ["Patient", "Treatment"],
          complexity: 0.3,
          priority: :medium
        }
      }
    ]
    
    # Test all permutation strategies
    strategies = [
      :linear,
      :parallel_merge,
      :adaptive_branch, 
      :skip_optimization,
      :iterative_loop,
      :complexity_route,
      :domain_specific,
      :emergence_guided
    ]
    
    IO.puts("🧪 Testing #{length(test_scenarios)} scenarios with #{length(strategies)} strategies\n")
    
    # Run comprehensive tests
    results = Enum.flat_map(test_scenarios, fn scenario ->
      IO.puts("📊 SCENARIO: #{scenario.name}")
      IO.puts("   Domain: #{scenario.input.domain}")
      IO.puts("   Entities: #{length(scenario.input.entities)}")
      IO.puts("   Complexity: #{scenario.input.complexity}")
      IO.puts("")
      
      # Test adaptive strategy selection first
      adaptive_result = CnsForge.UltrathinkPermutationSwarm.ultrathink_permute(
        scenario.input, 
        :adaptive
      )
      
      IO.puts("   🧠 Adaptive Strategy Selected: #{adaptive_result.selected_strategy}")
      
      # Test all specific strategies
      strategy_results = Enum.map(strategies, fn strategy ->
        IO.puts("   🔧 Testing strategy: #{strategy}")
        
        result = CnsForge.UltrathinkPermutationSwarm.ultrathink_permute(
          scenario.input,
          strategy
        )
        
        execution_result = result.execution_result
        path_length = case execution_result do
          %{path: path} -> length(path)
          %{selected_route: route} -> length(route)
          %{specialized_route: route} -> length(route)
          %{critical_stages: stages} -> length(stages)
          %{emergent_path: path} -> length(path)
          _ -> 8  # Default full pipeline
        end
        
        IO.puts("     ⚡ Duration: #{result.permutation_analysis.total_duration}ms")
        IO.puts("     📏 Path Length: #{path_length} stages")
        IO.puts("     🎯 Stages Executed: #{result.permutation_analysis.stages_executed}")
        
        %{
          scenario: scenario.name,
          strategy: strategy,
          duration: result.permutation_analysis.total_duration,
          path_length: path_length,
          stages_executed: result.permutation_analysis.stages_executed,
          swarm_decisions: result.swarm_intelligence.total_decisions,
          result: result
        }
      end)
      
      IO.puts("")
      strategy_results
    end)
    
    # Analyze all results
    analyze_comprehensive_results(results)
  end
  
  defp analyze_comprehensive_results(results) do
    IO.puts(String.duplicate("=", 80))
    IO.puts("🔍 COMPREHENSIVE PERMUTATION ANALYSIS")
    IO.puts(String.duplicate("=", 80))
    
    # Group by strategy
    by_strategy = Enum.group_by(results, & &1.strategy)
    
    IO.puts("\n📊 STRATEGY PERFORMANCE COMPARISON:\n")
    
    Enum.each(by_strategy, fn {strategy, strategy_results} ->
      avg_duration = Enum.sum(Enum.map(strategy_results, & &1.duration)) / length(strategy_results)
      avg_path_length = Enum.sum(Enum.map(strategy_results, & &1.path_length)) / length(strategy_results)
      total_decisions = Enum.sum(Enum.map(strategy_results, & &1.swarm_decisions))
      
      IO.puts("🔧 #{String.upcase(to_string(strategy))}:")
      IO.puts("   • Avg Duration: #{Float.round(avg_duration, 1)}ms")
      IO.puts("   • Avg Path Length: #{Float.round(avg_path_length, 1)} stages")
      IO.puts("   • Total Swarm Decisions: #{total_decisions}")
      IO.puts("   • Efficiency Score: #{calculate_efficiency_score(avg_duration, avg_path_length)}")
      IO.puts("")
    end)
    
    # Find optimal strategies
    IO.puts("🏆 OPTIMAL STRATEGY RANKINGS:")
    
    strategy_scores = Enum.map(by_strategy, fn {strategy, strategy_results} ->
      avg_duration = Enum.sum(Enum.map(strategy_results, & &1.duration)) / length(strategy_results)
      avg_path_length = Enum.sum(Enum.map(strategy_results, & &1.path_length)) / length(strategy_results)
      
      efficiency = calculate_efficiency_score(avg_duration, avg_path_length)
      {strategy, efficiency}
    end)
    |> Enum.sort_by(fn {_strategy, score} -> score end, :desc)
    
    Enum.with_index(strategy_scores, 1) |> Enum.each(fn {{strategy, score}, rank} ->
      medal = case rank do
        1 -> "🥇"
        2 -> "🥈" 
        3 -> "🥉"
        _ -> "#{rank}."
      end
      
      IO.puts("   #{medal} #{String.upcase(to_string(strategy))}: #{Float.round(score, 3)}")
    end)
    
    # Scenario-specific insights
    IO.puts("\n🎯 SCENARIO-SPECIFIC INSIGHTS:")
    
    by_scenario = Enum.group_by(results, & &1.scenario)
    
    Enum.each(by_scenario, fn {scenario, scenario_results} ->
      best_result = Enum.min_by(scenario_results, & &1.duration)
      worst_result = Enum.max_by(scenario_results, & &1.duration)
      
      IO.puts("\n📈 #{scenario}:")
      IO.puts("   • Best Strategy: #{best_result.strategy} (#{best_result.duration}ms)")
      IO.puts("   • Worst Strategy: #{worst_result.strategy} (#{worst_result.duration}ms)")
      IO.puts("   • Performance Variance: #{worst_result.duration - best_result.duration}ms")
      
      # Find most used path lengths
      path_lengths = Enum.map(scenario_results, & &1.path_length)
      common_length = Enum.frequencies(path_lengths) |> Enum.max_by(fn {_length, count} -> count end) |> elem(0)
      IO.puts("   • Most Common Path Length: #{common_length} stages")
    end)
    
    # Swarm intelligence analysis
    IO.puts("\n🧠 SWARM INTELLIGENCE PATTERNS:")
    
    total_decisions = Enum.sum(Enum.map(results, & &1.swarm_decisions))
    adaptive_results = Enum.filter(results, fn r -> 
      r.result.selected_strategy != :adaptive  # These were using adaptive selection
    end)
    
    IO.puts("   • Total Swarm Decisions Made: #{total_decisions}")
    IO.puts("   • Average Decisions Per Execution: #{Float.round(total_decisions / length(results), 1)}")
    
    if length(adaptive_results) > 0 do
      strategy_selections = Enum.map(adaptive_results, fn r -> r.result.selected_strategy end)
      popular_selections = Enum.frequencies(strategy_selections)
      
      IO.puts("   • Adaptive Strategy Selections:")
      Enum.each(popular_selections, fn {strategy, count} ->
        percentage = Float.round(count / length(adaptive_results) * 100, 1)
        IO.puts("     - #{strategy}: #{count} times (#{percentage}%)")
      end)
    end
    
    # Performance distribution
    IO.puts("\n⚡ PERFORMANCE DISTRIBUTION:")
    
    all_durations = Enum.map(results, & &1.duration)
    min_duration = Enum.min(all_durations)
    max_duration = Enum.max(all_durations)
    avg_duration = Enum.sum(all_durations) / length(all_durations)
    
    IO.puts("   • Fastest Execution: #{min_duration}ms")
    IO.puts("   • Slowest Execution: #{max_duration}ms")
    IO.puts("   • Average Duration: #{Float.round(avg_duration, 1)}ms")
    IO.puts("   • Performance Range: #{max_duration - min_duration}ms")
    
    # Path diversity analysis
    IO.puts("\n🔄 PATH DIVERSITY ANALYSIS:")
    
    all_path_lengths = Enum.map(results, & &1.path_length)
    path_distribution = Enum.frequencies(all_path_lengths)
    
    IO.puts("   • Path Length Distribution:")
    Enum.each(path_distribution, fn {length, count} ->
      percentage = Float.round(count / length(results) * 100, 1)
      IO.puts("     - #{length} stages: #{count} executions (#{percentage}%)")
    end)
    
    # 80/20 effectiveness
    IO.puts("\n🎯 80/20 OPTIMIZATION EFFECTIVENESS:")
    
    skip_optimization_results = Enum.filter(results, fn r -> r.strategy == :skip_optimization end)
    
    if length(skip_optimization_results) > 0 do
      avg_skip_duration = Enum.sum(Enum.map(skip_optimization_results, & &1.duration)) / length(skip_optimization_results)
      avg_skip_path = Enum.sum(Enum.map(skip_optimization_results, & &1.path_length)) / length(skip_optimization_results)
      
      stage_reduction = (8 - avg_skip_path) / 8 * 100
      
      IO.puts("   • Average Skip Optimization Duration: #{Float.round(avg_skip_duration, 1)}ms")
      IO.puts("   • Average Stages Reduced: #{Float.round(8 - avg_skip_path, 1)} (#{Float.round(stage_reduction, 1)}%)")
      IO.puts("   • Efficiency Gain: #{Float.round(stage_reduction, 1)}% fewer stages")
    end
    
    IO.puts("\n✨ PERMUTATION SWARM ANALYSIS COMPLETE!")
    IO.puts("The swarm explored #{length(results)} different execution paths across multiple strategies.")
  end
  
  defp calculate_efficiency_score(duration, path_length) do
    # Higher score = better efficiency (lower duration, appropriate path length)
    # Normalize duration (assume 100ms is baseline)
    duration_score = max(0, 1 - (duration / 100.0))
    
    # Path length score (sweet spot around 5-6 stages)
    path_score = case path_length do
      x when x <= 4 -> 0.8  # Too short, might miss important steps
      x when x <= 6 -> 1.0  # Optimal range
      x when x <= 8 -> 0.9  # Full pipeline, good but not optimized
      _ -> 0.7  # Too long
    end
    
    (duration_score + path_score) / 2.0
  end
end

PermutationSwarmTest.run()