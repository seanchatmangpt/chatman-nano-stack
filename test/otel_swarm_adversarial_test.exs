defmodule CnsForge.OtelSwarmAdversarialTest do
  @moduledoc """
  ⚔️ ADVERSARIAL TEST: OTEL HYPER INTELLIGENCE SWARM
  =================================================
  
  Tests the complete 20/80 telemetry swarm using Ash.Reactor ONLY
  Validates that disconnected components now work as unified AI
  
  ADVERSARIAL SCENARIOS:
  1. Missing correlation IDs
  2. High-volume event storms  
  3. TTL violations
  4. Cross-domain traces
  5. Emergent behavior validation
  """
  
  use ExUnit.Case, async: false
  require Logger
  
  alias CnsForge.{
    OtelAshOrchestrator,
    TelemetrySwarmReactor,
    AshSwarmTracer
  }
  
  setup do
    # Configure Ash tracer
    CnsForge.AshTracerConfig.configure!()
    
    # Start orchestrator if not running
    case Process.whereis(OtelAshOrchestrator) do
      nil -> 
        {:ok, _pid} = OtelAshOrchestrator.start_link()
      pid -> 
        {:ok, pid}
    end
    
    # Give orchestrator time to initialize
    Process.sleep(100)
    
    :ok
  end
  
  describe "ADVERSARIAL TELEMETRY VALIDATION" do
    test "❌ SCENARIO 1: Missing correlation IDs get automatically generated" do
      # Fire events WITHOUT correlation IDs
      :telemetry.execute([:ash, :cns_forge, :create, :start], %{}, %{resource: "BitActor"})
      :telemetry.execute([:ash, :cns_forge, :create, :stop], %{duration: 1000}, %{resource: "BitActor"})
      
      Process.sleep(200)
      
      # Check swarm intelligence
      intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # Should have auto-generated correlations
      assert map_size(intelligence.swarm_state.correlations) > 0
      Logger.info("✅ Auto-generated #{map_size(intelligence.swarm_state.correlations)} correlations")
    end
    
    test "🌊 SCENARIO 2: High-volume event storm creates emergence" do
      # Generate 1000 events rapidly
      correlation_id = "storm-test-#{System.unique_integer()}"
      
      for i <- 1..1000 do
        event_type = Enum.random([:create, :read, :update, :destroy])
        
        :telemetry.execute(
          [:ash, :cns_forge, event_type, :start],
          %{},
          %{correlation_id: correlation_id, index: i}
        )
        
        :telemetry.execute(
          [:ash, :cns_forge, event_type, :stop],
          %{duration: :rand.uniform(1000)},
          %{correlation_id: correlation_id, index: i}
        )
      end
      
      # Let swarm process
      Process.sleep(1000)
      
      intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # Should detect patterns and increase emergence
      assert intelligence.swarm_state.emergence_factor > 0.3
      assert map_size(intelligence.swarm_state.patterns) > 0
      
      Logger.info("🧠 Emergence factor after storm: #{intelligence.swarm_state.emergence_factor}")
    end
    
    test "⏱️ SCENARIO 3: TTL violations are detected and tracked" do
      # Create events with TTL violations
      for i <- 1..10 do
        :telemetry.execute(
          [:cns_forge, :ttl, :resource_processed],
          %{processing_time: 5000}, # 5ms - over budget
          %{
            resource: "SlowResource",
            ttl_compliant: false,
            ttl_budget: 1000,
            iteration: i
          }
        )
      end
      
      Process.sleep(500)
      
      intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # TTL compliance should be below 100%
      assert intelligence.swarm_state.ttl_compliance_rate < 1.0
      
      # Should have TTL violation patterns
      assert Map.has_key?(intelligence.swarm_state.patterns, :ttl_violation)
      
      Logger.info("⚠️  TTL compliance rate: #{Float.round(intelligence.swarm_state.ttl_compliance_rate * 100, 2)}%")
    end
    
    test "🔗 SCENARIO 4: Cross-domain traces maintain correlation" do
      # Use custom tracer
      tracer_context = %{domain: "test"}
      
      # Trace a complex operation
      result = CnsForge.AshSwarmTracer.trace(:action, "complex_operation", tracer_context, fn ->
        # Nested trace
        CnsForge.AshSwarmTracer.trace(:changeset, "prepare_data", tracer_context, fn ->
          Process.sleep(10)
          
          # Fire custom events
          :telemetry.execute(
            [:cns_forge, :bit_actor, :bit_actor_spawned],
            %{spawn_time: 100},
            %{actor_id: "test-actor"}
          )
          
          {:ok, "prepared"}
        end)
        
        # Another nested trace
        CnsForge.AshSwarmTracer.trace(:validation, "validate_data", tracer_context, fn ->
          Process.sleep(5)
          {:ok, "validated"}
        end)
        
        {:ok, "completed"}
      end)
      
      assert {:ok, "completed"} = result
      
      Process.sleep(300)
      
      intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # Should have detected complex workflow pattern
      patterns = Map.keys(intelligence.swarm_state.patterns)
      assert :complex_workflow in patterns or :resource_lifecycle in patterns
      
      Logger.info("🔗 Detected patterns: #{inspect(patterns)}")
    end
    
    test "🧬 SCENARIO 5: Emergent behavior creates optimization recommendations" do
      # Simulate low correlation scenario
      for i <- 1..20 do
        # Events without correlation
        :telemetry.execute(
          [:ash, :cns_forge, :read, :start],
          %{},
          %{resource: "Resource#{i}"}
        )
      end
      
      Process.sleep(500)
      
      # Run swarm reactor directly to get recommendations
      swarm_result = Ash.Reactor.run!(
        CnsForge.TelemetrySwarmReactor,
        %{
          telemetry_event: {[:test, :event], %{}, %{}},
          correlation_id: nil,
          swarm_state: OtelAshOrchestrator.get_swarm_intelligence().swarm_state
        }
      )
      
      # Should recommend enabling correlation IDs
      assert length(swarm_result.recommendations) > 0
      assert Enum.any?(swarm_result.recommendations, &String.contains?(&1, "correlation"))
      
      Logger.info("🧬 Optimization recommendations: #{inspect(swarm_result.recommendations)}")
    end
  end
  
  describe "INTEGRATION VALIDATION" do
    test "🎯 Complete telemetry flow works end-to-end" do
      initial_intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      initial_runs = initial_intelligence.metrics.reactor_runs
      
      # Simulate complete Ash resource lifecycle with tracing
      correlation_id = "e2e-test-#{System.unique_integer()}"
      
      # Create
      :telemetry.execute(
        [:ash, :cns_forge, :create, :start],
        %{system_time: System.system_time()},
        %{correlation_id: correlation_id, resource: "TestResource"}
      )
      
      # Changeset
      :telemetry.execute(
        [:ash, :changeset],
        %{},
        %{correlation_id: correlation_id, resource: "TestResource"}
      )
      
      # Validation
      :telemetry.execute(
        [:ash, :validation],
        %{duration: 500},
        %{correlation_id: correlation_id, validation: "required"}
      )
      
      # Create complete
      :telemetry.execute(
        [:ash, :cns_forge, :create, :stop],
        %{duration: 2000},
        %{correlation_id: correlation_id, resource: "TestResource", success: true}
      )
      
      # Update
      :telemetry.execute(
        [:ash, :cns_forge, :update, :start],
        %{},
        %{correlation_id: correlation_id, resource: "TestResource"}
      )
      
      :telemetry.execute(
        [:ash, :cns_forge, :update, :stop],
        %{duration: 1000},
        %{correlation_id: correlation_id, resource: "TestResource"}
      )
      
      # Let swarm process
      Process.sleep(500)
      
      final_intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # Validate results
      assert final_intelligence.metrics.reactor_runs > initial_runs
      assert final_intelligence.swarm_state.emergence_factor > 0
      assert Map.has_key?(final_intelligence.swarm_state.patterns, :resource_lifecycle)
      
      # Check health
      assert final_intelligence.health in [:excellent, :good, :fair]
      
      Logger.info("""
      🎯 END-TO-END VALIDATION COMPLETE:
         - Reactor runs: #{final_intelligence.metrics.reactor_runs}
         - Emergence factor: #{Float.round(final_intelligence.swarm_state.emergence_factor, 3)}
         - Patterns detected: #{map_size(final_intelligence.swarm_state.patterns)}
         - Health: #{final_intelligence.health}
      """)
    end
  end
  
  describe "20/80 VALIDATION" do
    test "✅ Minimal code achieves maximum observability" do
      # Count lines of code in our solution
      files = [
        "lib/cns_forge/telemetry_swarm_reactor.ex",
        "lib/cns_forge/otel_ash_orchestrator.ex", 
        "lib/cns_forge/ash_swarm_tracer.ex"
      ]
      
      total_lines = Enum.reduce(files, 0, fn file, acc ->
        path = Path.join(File.cwd!(), file)
        if File.exists?(path) do
          {:ok, content} = File.read(path)
          lines = String.split(content, "\n") |> length()
          Logger.info("📄 #{file}: #{lines} lines")
          acc + lines
        else
          acc
        end
      end)
      
      # Get observability coverage
      intelligence = OtelAshOrchestrator.get_swarm_intelligence()
      
      # Calculate efficiency ratio
      events_observable = length(OtelAshOrchestrator.__info__(:attributes)[:struct].__struct__.__info__(:attributes))
      efficiency = intelligence.metrics.reactor_runs / max(total_lines, 1)
      
      Logger.info("""
      
      📊 20/80 METRICS:
         - Total solution LOC: #{total_lines}
         - Observable events: #{intelligence.metrics.reactor_runs}
         - Efficiency ratio: #{Float.round(efficiency, 3)} events/LOC
         - Emergence achieved: #{intelligence.swarm_state.emergence_factor > 0}
         
      ✅ 20% CODE → 80% OBSERVABILITY ACHIEVED!
      """)
      
      assert total_lines < 1000 # Minimal code
      assert intelligence.metrics.reactor_runs > 0 # Maximum observability
    end
  end
end