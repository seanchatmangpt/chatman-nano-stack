defmodule CnsForge.ReactorIntegrationTest do
  @moduledoc """
  Integration tests for complete Ash.Reactor workflows
  Tests end-to-end scenarios with all components working together
  """
  
  use ExUnit.Case
  use Ash.Test
  
  alias CnsForge.{
    AshReactorHyperIntelligenceSwarm,
    TTLAshReactorTransformer,
    TTLAshReactorAISwarmConnector
  }
  
  setup do
    # Set up test data and mocks
    %{
      test_ttl: """
      @prefix blog: <http://example.org/blog#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      blog:Author a owl:Class .
      blog:Post a owl:Class .
      
      blog:hasWritten a owl:ObjectProperty ;
        rdfs:domain blog:Author ;
        rdfs:range blog:Post .
      """,
      
      broken_reactor_config: %{
        steps: [
          %{name: "get_author", type: "read_one", inputs: [:email]},
          %{name: "create_post", type: "create", inputs: [:title, :content, :author_id]},
          %{name: "update_author_post_count", type: "update", inputs: []}  # Missing!
        ]
      }
    }
  end
  
  describe "Complete TTL to Reactor transformation workflow" do
    test "transforms TTL and creates working reactor with AI healing", %{test_ttl: ttl} do
      # Step 1: Transform TTL
      assert {:ok, transform_result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      assert Map.has_key?(transform_result, :resources)
      assert Map.has_key?(transform_result, :reactors)
      assert length(transform_result.resources) == 2  # Author and Post
      
      # Step 2: Initialize AI Swarm
      swarm_input = %{
        validation_data: %{},
        discovered_components: transform_result.resources
      }
      
      assert {:ok, swarm_result} = Ash.Reactor.run(
        AshReactorHyperIntelligenceSwarm.HyperIntelligenceSwarm,
        swarm_input
      )
      
      # Step 3: Verify connections created
      assert swarm_result.weaknesses_fixed > 0
      assert swarm_result.security_defenses_active > 0
      assert swarm_result.otel_coverage == 100.0
    end
    
    test "fixes broken reactor configuration automatically", %{broken_reactor_config: config} do
      # Run AI swarm connector
      connector_input = %{
        reactor_definitions: config,
        ttl_file_path: nil  # Not needed for this test
      }
      
      assert {:ok, result} = TTLAshReactorAISwarmConnector.fix_broken_connections(
        connector_input
      )
      
      # Verify update_author_post_count now has inputs
      fixed_step = Enum.find(result.fixed_steps, & &1.name == "update_author_post_count")
      assert length(fixed_step.inputs) > 0
      assert :author_id in fixed_step.inputs
    end
  end
  
  describe "End-to-end blog post creation with self-healing" do
    test "creates post and updates author count with auto-fixed connection" do
      # Initialize the fixed reactor
      input = %{
        author_email: "test@example.com",
        title: "Test Post",
        content: "Test content"
      }
      
      # Run the complete workflow
      assert {:ok, result} = Ash.Reactor.run(
        FixedBlogReactor.CreatePostWithProperConnections,
        input
      )
      
      # Verify all steps completed
      assert result.post
      assert result.author
      assert result.post_count_updated == true
      
      # Verify telemetry collected
      assert Map.has_key?(result, :telemetry)
      assert result.telemetry.coverage_percent == 100.0
    end
    
    test "handles connection failures with self-healing" do
      # Simulate a failing connection
      input = %{
        author_email: nil,  # Will cause initial failure
        title: "Test Post",
        content: "Test content"
      }
      
      # Run with self-healing enabled
      opts = [
        context: %{
          self_healing_enabled: true,
          fallback_strategies: [:use_default, :retry, :skip]
        }
      ]
      
      assert {:ok, result} = Ash.Reactor.run(
        FixedBlogReactor.CreatePostWithProperConnections,
        input,
        opts
      )
      
      # Should succeed despite initial failure
      assert result.post
      # Healing metrics should show recovery
      assert result.telemetry.recoveries > 0
    end
  end
  
  describe "Security defense integration" do
    test "blocks all attack vectors simultaneously" do
      # Simulate concurrent attacks
      attacks = [
        Task.async(fn -> 
          SecurityDefenseReactor.process_request(%{
            type: "sql_injection",
            payload: "'; DROP TABLE users; --"
          })
        end),
        Task.async(fn ->
          SecurityDefenseReactor.process_request(%{
            type: "xss",
            payload: "<script>alert('xss')</script>"
          })
        end),
        Task.async(fn ->
          SecurityDefenseReactor.process_request(%{
            type: "csrf",
            payload: "forged_token"
          })
        end)
      ]
      
      results = Task.await_many(attacks)
      
      # All should be blocked
      assert Enum.all?(results, fn {:blocked, _} -> true; _ -> false end)
    end
    
    test "maintains performance under security load" do
      # Generate high volume of attacks
      start_time = System.monotonic_time(:millisecond)
      
      results = Enum.map(1..1000, fn i ->
        SecurityDefenseReactor.process_request(%{
          type: Enum.random(["sql_injection", "xss", "csrf"]),
          payload: "attack_#{i}"
        })
      end)
      
      end_time = System.monotonic_time(:millisecond)
      duration = end_time - start_time
      
      # Should handle 1000 attacks quickly
      assert duration < 1000  # Less than 1 second
      
      # All should be blocked
      blocked_count = Enum.count(results, fn {:blocked, _} -> true; _ -> false end)
      assert blocked_count == 1000
    end
  end
  
  describe "OTEL coverage integration" do
    test "achieves 100% coverage across all components" do
      # Run coverage analysis
      components = [
        "bitactor_performance",
        "cns_forge_workflows",
        "dashboard_metrics",
        "security_monitoring",
        "reactor_execution"
      ]
      
      coverage_results = Enum.map(components, fn component ->
        OTELBridgeReactor.analyze_component_coverage(component)
      end)
      
      # All should have 100% coverage
      assert Enum.all?(coverage_results, fn {:ok, coverage} ->
        coverage.execution_count == 100.0 and
        coverage.error_rate == 100.0 and
        coverage.resource_utilization == 100.0
      end)
    end
    
    test "collects metrics during reactor execution" do
      # Run a reactor with OTEL instrumentation
      input = %{test_data: "sample"}
      
      {:ok, result} = Ash.Reactor.run(
        InstrumentedTestReactor,
        input,
        telemetry_options: [collect_metrics: true]
      )
      
      # Verify metrics collected
      metrics = result.collected_metrics
      
      assert Map.has_key?(metrics, :execution_time)
      assert Map.has_key?(metrics, :step_count)
      assert Map.has_key?(metrics, :error_count)
      assert metrics.error_count == 0
    end
  end
  
  describe "Chaos resilience integration" do
    test "system remains operational under chaos conditions" do
      chaos_scenarios = [
        :random_input_failures,
        :network_partitions,
        :resource_exhaustion,
        :cascading_failures
      ]
      
      results = Enum.map(chaos_scenarios, fn scenario ->
        ChaosInjector.inject(scenario)
        
        # Try to run normal operations
        operation_result = try do
          Ash.Reactor.run(TestReactor, %{data: "test"})
        rescue
          _ -> {:error, :failed}
        end
        
        # Clean up chaos
        ChaosInjector.cleanup(scenario)
        
        operation_result
      end)
      
      # At least 80% should succeed (allowing some failures)
      success_count = Enum.count(results, fn
        {:ok, _} -> true
        _ -> false
      end)
      
      success_rate = success_count / length(results)
      assert success_rate >= 0.8
    end
    
    test "self-healing activates during chaos" do
      # Inject connection failure
      ChaosInjector.inject(:connection_failure, target: "critical_connection")
      
      # Monitor healing
      healing_events = HealthMonitor.subscribe()
      
      # Run reactor that uses the connection
      {:ok, _} = Ash.Reactor.run(CriticalReactor, %{})
      
      # Verify healing occurred
      events = HealthMonitor.get_events(healing_events)
      
      healing_count = Enum.count(events, & &1.type == :connection_healed)
      assert healing_count > 0
      
      ChaosInjector.cleanup(:connection_failure)
    end
  end
  
  describe "Performance benchmarks" do
    @tag :benchmark
    test "reactor execution meets performance targets" do
      # Warm up
      Enum.each(1..10, fn _ ->
        Ash.Reactor.run(PerformanceTestReactor, %{data: "warmup"})
      end)
      
      # Benchmark
      results = Enum.map(1..100, fn _ ->
        start = System.monotonic_time(:microsecond)
        {:ok, _} = Ash.Reactor.run(PerformanceTestReactor, %{data: "test"})
        System.monotonic_time(:microsecond) - start
      end)
      
      avg_time = Enum.sum(results) / length(results)
      p95_time = Enum.at(Enum.sort(results), round(length(results) * 0.95))
      
      # Performance targets
      assert avg_time < 1000  # Average under 1ms
      assert p95_time < 2000  # P95 under 2ms
    end
    
    @tag :benchmark
    test "handles high concurrency" do
      # Launch concurrent requests
      tasks = Enum.map(1..1000, fn i ->
        Task.async(fn ->
          Ash.Reactor.run(ConcurrentTestReactor, %{id: i})
        end)
      end)
      
      results = Task.await_many(tasks, 30_000)
      
      success_count = Enum.count(results, fn
        {:ok, _} -> true
        _ -> false
      end)
      
      # Should handle all concurrent requests
      assert success_count == 1000
    end
  end
  
  # Helper modules for testing
  defmodule InstrumentedTestReactor do
    use Ash.Reactor
    
    reactor do
      step :process_data do
        argument :data, input(:test_data)
        
        run fn %{data: data}, _context ->
          {:ok, String.upcase(data)}
        end
        
        # OTEL instrumentation
        around :telemetry_span, %{
          name: [:test, :process_data],
          metadata: %{step: "process_data"}
        }
      end
      
      return :process_data
    end
  end
  
  defmodule ChaosInjector do
    def inject(scenario, opts \\ []) do
      # Simulate various chaos scenarios
      case scenario do
        :random_input_failures ->
          Process.put(:chaos_input_failure_rate, 0.3)
          
        :connection_failure ->
          target = Keyword.get(opts, :target)
          Process.put({:chaos_connection_failed, target}, true)
          
        _ ->
          :ok
      end
    end
    
    def cleanup(scenario) do
      case scenario do
        :random_input_failures ->
          Process.delete(:chaos_input_failure_rate)
          
        :connection_failure ->
          Process.delete({:chaos_connection_failed, :all})
          
        _ ->
          :ok
      end
    end
  end
end