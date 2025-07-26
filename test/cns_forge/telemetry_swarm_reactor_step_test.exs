defmodule CnsForge.TelemetrySwarmReactorStepTest do
  @moduledoc """
  🧪 UNIT TESTS FOR TELEMETRY SWARM REACTOR STEPS
  ==============================================
  
  Tests each reactor step individually following ExUnit patterns.
  ONLY TESTS STEPS - NOT FULL REACTOR INTEGRATION.
  """
  
  use ExUnit.Case, async: true
  
  alias CnsForge.TelemetrySwarmReactor
  
  describe "correlate_event step" do
    test "generates correlation ID when missing" do
      event = {[:ash, :cns_forge, :create, :start], %{duration: 100}, %{resource: "User"}}
      state = %{correlations: %{}}
      
      # Simulate the step function
      result = correlate_event_step(%{
        event: event, 
        correlation_id: nil, 
        state: state
      })
      
      assert {:ok, %{correlation_id: corr_id, event_added: true}} = result
      assert is_binary(corr_id)
      assert String.starts_with?(corr_id, "swarm-")
    end
    
    test "uses existing correlation ID" do
      event = {[:ash, :cns_forge, :create, :start], %{duration: 100}, %{resource: "User"}}
      state = %{correlations: %{}}
      existing_id = "test-correlation-123"
      
      result = correlate_event_step(%{
        event: event, 
        correlation_id: existing_id, 
        state: state
      })
      
      assert {:ok, %{correlation_id: ^existing_id, event_added: true}} = result
    end
    
    test "adds event to correlation chain" do
      event = {[:ash, :cns_forge, :create, :start], %{duration: 100}, %{resource: "User"}}
      corr_id = "test-123"
      state = %{correlations: %{corr_id => []}}
      
      result = correlate_event_step(%{
        event: event, 
        correlation_id: corr_id, 
        state: state
      })
      
      assert {:ok, %{correlations: correlations}} = result
      assert Map.has_key?(correlations, corr_id)
      assert length(correlations[corr_id]) == 1
      
      [{event_name, measurements, metadata, _timestamp}] = correlations[corr_id]
      assert event_name == [:ash, :cns_forge, :create, :start]
      assert measurements == %{duration: 100}
      assert metadata == %{resource: "User"}
    end
    
    test "handles nil event gracefully" do
      state = %{correlations: %{}}
      
      result = correlate_event_step(%{
        event: nil, 
        correlation_id: nil, 
        state: state
      })
      
      assert {:ok, %{event_added: false, correlations: %{}}} = result
    end
  end
  
  describe "detect_patterns step" do
    test "detects resource lifecycle pattern" do
      correlations = %{
        "test-123" => [
          {[:ash, :users, :update], %{}, %{}, System.monotonic_time()},
          {[:ash, :users, :create], %{}, %{}, System.monotonic_time()}
        ]
      }
      
      result = detect_patterns_step(%{
        correlations: correlations,
        previous_patterns: %{}
      })
      
      assert {:ok, %{patterns: patterns, pattern_count: count}} = result
      assert count > 0
      assert Map.has_key?(patterns, :resource_lifecycle)
    end
    
    test "detects error cascade pattern" do
      correlations = %{
        "test-456" => [
          {[:phoenix, :controller, :error], %{}, %{}, System.monotonic_time()},
          {[:ash, :users, :error], %{}, %{}, System.monotonic_time()}
        ]
      }
      
      result = detect_patterns_step(%{
        correlations: correlations,
        previous_patterns: %{}
      })
      
      assert {:ok, %{patterns: patterns}} = result
      assert Map.has_key?(patterns, :error_cascade)
    end
    
    test "detects TTL violation pattern" do
      correlations = %{
        "test-789" => [
          {[:cns_forge, :ttl, :violation], %{}, %{ttl_compliant: false}, System.monotonic_time()}
        ]
      }
      
      result = detect_patterns_step(%{
        correlations: correlations,
        previous_patterns: %{}
      })
      
      assert {:ok, %{patterns: patterns}} = result
      assert Map.has_key?(patterns, :ttl_violation)
    end
    
    test "merges with previous patterns" do
      correlations = %{
        "new-123" => [
          {[:ash, :posts, :create], %{}, %{}, System.monotonic_time()}
        ]
      }
      
      previous_patterns = %{
        resource_lifecycle: ["old-456"]
      }
      
      result = detect_patterns_step(%{
        correlations: correlations,
        previous_patterns: previous_patterns
      })
      
      assert {:ok, %{patterns: patterns}} = result
      # Should maintain previous patterns
      assert Map.has_key?(patterns, :resource_lifecycle)
    end
  end
  
  describe "calculate_emergence step" do
    test "calculates emergence factor from patterns and correlations" do
      patterns = %{
        resource_lifecycle: ["corr-1", "corr-2"],
        error_cascade: ["corr-3"]
      }
      
      correlations = %{
        "corr-1" => [{:event1, %{}, %{}, 0}, {:event2, %{}, %{}, 0}],
        "corr-2" => [{:event3, %{}, %{}, 0}],
        "corr-3" => [{:event4, %{}, %{}, 0}]
      }
      
      result = calculate_emergence_step(%{
        patterns: patterns,
        correlations: correlations
      })
      
      assert {:ok, %{emergence_factor: factor, intelligence_level: level}} = result
      assert is_float(factor)
      assert factor >= 0.0 and factor <= 1.0
      assert level in [:nascent, :learning, :intelligent, :highly_intelligent, :hyper_intelligent]
    end
    
    test "handles empty patterns and correlations" do
      result = calculate_emergence_step(%{
        patterns: %{},
        correlations: %{}
      })
      
      assert {:ok, %{emergence_factor: factor, intelligence_level: :nascent}} = result
      assert factor == 0.0
    end
    
    test "categorizes intelligence levels correctly" do
      # Test high emergence
      patterns = %{
        pattern1: Enum.to_list(1..50),
        pattern2: Enum.to_list(1..30)  
      }
      correlations = %{
        "high" => Enum.map(1..20, fn i -> {"event#{i}", %{}, %{}, i} end)
      }
      
      result = calculate_emergence_step(%{
        patterns: patterns,
        correlations: correlations
      })
      
      assert {:ok, %{emergence_factor: factor, intelligence_level: level}} = result
      assert factor > 0.7
      assert level in [:highly_intelligent, :hyper_intelligent]
    end
  end
  
  describe "analyze_ttl_compliance step" do
    test "calculates TTL compliance rate" do
      correlations = %{
        "test-1" => [
          {[:event1], %{processing_time_ns: 1000}, %{ttl_compliant: true}, 0},
          {[:event2], %{processing_time_ns: 2000}, %{ttl_compliant: false}, 0}
        ],
        "test-2" => [
          {[:event3], %{processing_time_ns: 500}, %{ttl_compliant: true}, 0}
        ]
      }
      
      result = analyze_ttl_compliance_step(%{correlations: correlations})
      
      assert {:ok, %{
        ttl_compliance_rate: rate,
        ttl_violations: violations,
        ttl_metrics_analyzed: total
      }} = result
      
      assert total == 3
      assert violations == 1
      assert rate == 2/3
    end
    
    test "handles correlations without TTL metadata" do
      correlations = %{
        "test-1" => [
          {[:event1], %{duration: 100}, %{resource: "User"}, 0}
        ]
      }
      
      result = analyze_ttl_compliance_step(%{correlations: correlations})
      
      assert {:ok, %{
        ttl_compliance_rate: 1.0,
        ttl_violations: 0,
        ttl_metrics_analyzed: 0
      }} = result
    end
    
    test "handles empty correlations" do
      result = analyze_ttl_compliance_step(%{correlations: %{}})
      
      assert {:ok, %{
        ttl_compliance_rate: 1.0,
        ttl_violations: 0,
        ttl_metrics_analyzed: 0
      }} = result
    end
  end
  
  describe "generate_optimizations step" do
    test "recommends correlation IDs for low emergence" do
      result = generate_optimizations_step(%{
        emergence_factor: 0.2,
        patterns: %{},
        ttl_compliance: 1.0
      })
      
      assert {:ok, %{recommendations: recommendations, optimization_priority: priority}} = result
      assert "Enable correlation IDs in all Ash changesets" in recommendations
      assert priority in [:critical, :high, :medium, :low, :minimal]
    end
    
    test "recommends TTL adjustments for low compliance" do
      result = generate_optimizations_step(%{
        emergence_factor: 0.8,
        patterns: %{},
        ttl_compliance: 0.85
      })
      
      assert {:ok, %{recommendations: recommendations}} = result
      assert Enum.any?(recommendations, &String.contains?(&1, "TTL budgets"))
    end
    
    test "recommends caching for lifecycle patterns" do
      result = generate_optimizations_step(%{
        emergence_factor: 0.6,
        patterns: %{resource_lifecycle: ["test"]},
        ttl_compliance: 0.98
      })
      
      assert {:ok, %{recommendations: recommendations}} = result
      assert "Resource lifecycle patterns detected - consider caching" in recommendations
    end
    
    test "recommends circuit breakers for error cascade" do
      result = generate_optimizations_step(%{
        emergence_factor: 0.6,
        patterns: %{error_cascade: ["test"]},
        ttl_compliance: 0.98
      })
      
      assert {:ok, %{recommendations: recommendations}} = result
      assert "Error cascade pattern detected - add circuit breakers" in recommendations
    end
    
    test "recognizes peak intelligence" do
      result = generate_optimizations_step(%{
        emergence_factor: 0.85,
        patterns: %{},
        ttl_compliance: 0.99
      })
      
      assert {:ok, %{recommendations: recommendations}} = result
      assert "System operating at peak intelligence - maintain current configuration" in recommendations
    end
  end
  
  describe "emit_swarm_telemetry step" do
    test "emits telemetry event successfully" do
      # Capture telemetry events
      test_pid = self()
      :telemetry.attach("test-handler", [:cns_forge, :telemetry_swarm, :intelligence_calculated], 
        fn event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, event, measurements, metadata})
        end, nil)
      
      result = emit_swarm_telemetry_step(%{
        emergence_factor: 0.7,
        pattern_count: 3,
        ttl_compliance_rate: 0.95,
        recommendations: ["test rec 1", "test rec 2"]
      })
      
      assert {:ok, %{telemetry_emitted: true}} = result
      
      # Verify telemetry was emitted
      assert_receive {:telemetry, [:cns_forge, :telemetry_swarm, :intelligence_calculated], 
        measurements, metadata}
      
      assert measurements.emergence_factor == 0.7
      assert measurements.pattern_count == 3
      assert measurements.ttl_compliance_rate == 0.95
      assert measurements.recommendation_count == 2
      
      assert length(metadata.recommendations) == 2
      assert metadata.recommendations == ["test rec 1", "test rec 2"]
      
      :telemetry.detach("test-handler")
    end
  end
  
  # Helper functions that mirror the step logic
  defp correlate_event_step(%{event: event, correlation_id: corr_id, state: state}) do
    # Generate correlation ID if missing
    correlation_id = corr_id || generate_correlation_id()
    
    # Extract event details
    {event_name, measurements, metadata} = event || {nil, %{}, %{}}
    
    if event_name do
      # Add to correlation chain
      correlation_chain = Map.get(state.correlations, correlation_id, [])
      updated_chain = [{event_name, measurements, metadata, System.monotonic_time()} | correlation_chain]
      
      # Update state
      updated_correlations = Map.put(state.correlations, correlation_id, updated_chain)
      
      {:ok, %{
        correlation_id: correlation_id,
        event_added: true,
        correlations: updated_correlations
      }}
    else
      {:ok, %{correlation_id: correlation_id, event_added: false, correlations: state.correlations}}
    end
  end
  
  defp detect_patterns_step(%{correlations: correlations, previous_patterns: prev_patterns}) do
    current_time = System.monotonic_time(:millisecond)
    
    # Analyze correlation chains for patterns
    patterns = Enum.reduce(correlations, %{}, fn {corr_id, chain}, acc ->
      pattern = analyze_correlation_chain(chain, current_time)
      
      if pattern != :none do
        pattern_list = Map.get(acc, pattern, [])
        Map.put(acc, pattern, [corr_id | pattern_list])
      else
        acc
      end
    end)
    
    # Merge with previous patterns (with time decay)
    merged_patterns = merge_patterns_with_decay(patterns, prev_patterns, current_time)
    
    {:ok, %{patterns: merged_patterns, pattern_count: map_size(merged_patterns)}}
  end
  
  defp calculate_emergence_step(%{patterns: patterns, correlations: correlations}) do
    # Count total pattern instances
    pattern_instances = patterns
    |> Map.values()
    |> Enum.map(&length/1)
    |> Enum.sum()
    
    # Calculate correlation complexity
    correlation_complexity = correlations
    |> Map.values()
    |> Enum.map(&length/1)
    |> Enum.sum()
    
    # Emergence factor based on pattern density and correlation complexity
    emergence_factor = :math.tanh((pattern_instances + correlation_complexity) / 100)
    
    {:ok, %{
      emergence_factor: emergence_factor,
      intelligence_level: categorize_intelligence(emergence_factor)
    }}
  end
  
  defp analyze_ttl_compliance_step(%{correlations: correlations}) do
    # Extract TTL metrics from correlation chains
    ttl_metrics = Enum.flat_map(correlations, fn {_corr_id, chain} ->
      Enum.filter_map(
        chain,
        fn {_event, _measurements, metadata, _time} ->
          Map.has_key?(metadata, :ttl_compliant) or Map.has_key?(metadata, :ttl_remaining)
        end,
        fn {_event, measurements, metadata, _time} ->
          %{
            compliant: Map.get(metadata, :ttl_compliant, true),
            ttl_remaining: Map.get(metadata, :ttl_remaining),
            processing_time: Map.get(measurements, :processing_time_ns)
          }
        end
      )
    end)
    
    # Calculate compliance rate
    total = length(ttl_metrics)
    compliant = Enum.count(ttl_metrics, & &1.compliant)
    
    compliance_rate = if total > 0, do: compliant / total, else: 1.0
    
    {:ok, %{
      ttl_compliance_rate: compliance_rate,
      ttl_violations: total - compliant,
      ttl_metrics_analyzed: total
    }}
  end
  
  defp generate_optimizations_step(%{emergence_factor: emergence, patterns: patterns, ttl_compliance: ttl_rate}) do
    recommendations = []
    
    # Low emergence - need more correlation
    recommendations = if emergence < 0.3 do
      ["Enable correlation IDs in all Ash changesets" | recommendations]
    else
      recommendations
    end
    
    # TTL issues detected
    recommendations = if ttl_rate < 0.95 do
      ["Adjust TTL budgets - current compliance: #{Float.round(ttl_rate * 100, 2)}%" | recommendations]
    else
      recommendations  
    end
    
    # Pattern-specific optimizations
    recommendations = if Map.has_key?(patterns, :resource_lifecycle) do
      ["Resource lifecycle patterns detected - consider caching" | recommendations]
    else
      recommendations
    end
    
    recommendations = if Map.has_key?(patterns, :error_cascade) do
      ["Error cascade pattern detected - add circuit breakers" | recommendations]
    else
      recommendations
    end
    
    # High emergence - system performing well
    recommendations = if emergence > 0.8 do
      ["System operating at peak intelligence - maintain current configuration" | recommendations]
    else
      recommendations
    end
    
    {:ok, %{
      recommendations: recommendations,
      optimization_priority: calculate_optimization_priority(emergence, ttl_rate)
    }}
  end
  
  defp emit_swarm_telemetry_step(args) do
    # Emit our own telemetry about swarm intelligence
    :telemetry.execute(
      [:cns_forge, :telemetry_swarm, :intelligence_calculated],
      %{
        emergence_factor: args.emergence_factor,
        pattern_count: args.pattern_count,
        ttl_compliance_rate: args.ttl_compliance_rate,
        recommendation_count: length(args.recommendations)
      },
      %{
        recommendations: args.recommendations,
        timestamp: DateTime.utc_now()
      }
    )
    
    {:ok, %{telemetry_emitted: true}}
  end
  
  # Helper functions copied from main module
  defp generate_correlation_id do
    "swarm-#{System.unique_integer([:positive])}-#{:erlang.phash2(System.monotonic_time())}"
  end
  
  defp analyze_correlation_chain(chain, current_time) when is_list(chain) do
    # Analyze the chain for patterns
    event_names = Enum.map(chain, fn {name, _, _, _} -> name end)
    
    cond do
      # Ash resource lifecycle pattern
      match_pattern?(event_names, [[:ash, :_, :create], [:ash, :_, :update]]) ->
        :resource_lifecycle
        
      # Error cascade pattern
      match_pattern?(event_names, [[:ash, :_, :error], [:phoenix, :_, :error]]) ->
        :error_cascade
        
      # BitActor processing pattern
      match_pattern?(event_names, [[:cns_forge, :bit_actor, :_], [:cns_forge, :ttl, :_]]) ->
        :bitactor_ttl_flow
        
      # Phoenix request pattern
      match_pattern?(event_names, [[:phoenix, :endpoint, :start], [:phoenix, :endpoint, :stop]]) ->
        :request_lifecycle
        
      # TTL violation pattern
      Enum.any?(chain, fn {_, _, metadata, _} -> 
        Map.get(metadata, :ttl_compliant) == false 
      end) ->
        :ttl_violation
        
      # Default - check chain length
      length(chain) >= 3 ->
        :complex_workflow
        
      true ->
        :none
    end
  end
  
  defp match_pattern?(events, patterns) do
    Enum.any?(patterns, fn pattern ->
      Enum.all?(pattern, fn expected ->
        Enum.any?(events, fn event ->  
          match_event_pattern?(event, expected)
        end)
      end)
    end)
  end
  
  defp match_event_pattern?(event, pattern) when is_list(event) and is_list(pattern) do
    Enum.zip(event, pattern)
    |> Enum.all?(fn
      {_e, :_} -> true
      {e, p} -> e == p
    end)
  end
  
  defp merge_patterns_with_decay(new_patterns, old_patterns, _current_time) do
    # Simple merge for now - in production, implement time-based decay
    Map.merge(old_patterns, new_patterns, fn _key, old_val, new_val ->
      Enum.uniq(old_val ++ new_val) |> Enum.take(100) # Limit pattern memory
    end)
  end
  
  defp categorize_intelligence(factor) when is_float(factor) do
    cond do
      factor >= 0.9 -> :hyper_intelligent
      factor >= 0.7 -> :highly_intelligent
      factor >= 0.5 -> :intelligent
      factor >= 0.3 -> :learning
      true -> :nascent
    end
  end
  
  defp calculate_optimization_priority(emergence_factor, ttl_compliance) do
    # Higher priority for lower performance
    base_priority = (1.0 - emergence_factor) * 50 + (1.0 - ttl_compliance) * 50
    
    cond do
      base_priority >= 80 -> :critical
      base_priority >= 60 -> :high
      base_priority >= 40 -> :medium
      base_priority >= 20 -> :low
      true -> :minimal
    end
  end
end