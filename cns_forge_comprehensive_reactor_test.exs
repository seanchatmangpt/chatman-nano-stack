defmodule CNSForge.ComprehensiveReactorTest do
  @moduledoc """
  Comprehensive End-to-End Testing Suite for CNS Forge Reactor Infrastructure
  
  This test suite implements the complete testing strategy from the reactor testing guide,
  leveraging all existing CNS Forge assets for full validation.
  
  Test Categories:
  1. Unit Testing Individual Steps
  2. Integration Testing Complete Reactors
  3. Error Handling and Compensation Logic
  4. Concurrent Execution Testing
  5. TTL-Driven Workflow Validation
  6. BitActor Mesh Integration
  7. OTEL Telemetry Validation
  """
  
  use ExUnit.Case, async: false
  use Mimic
  
  alias CNSForge.EndToEndForexReactor
  alias CNSForge.Domain
  alias CNSForge.DirectiveParser
  alias CNSForge.SemanticOntology
  alias CNSForge.BitActorGenerator
  
  # Setup comprehensive test environment
  setup_all do
    # Initialize Mnesia for testing
    :mnesia.create_schema([node()])
    :mnesia.start()
    
    # Initialize ETS tables for CNS Forge
    :ets.new(:cns_forge_bit_actors, [:named_table, :public, :set])
    :ets.new(:cns_forge_pulse_logs, [:named_table, :public, :ordered_set])
    :ets.new(:cns_forge_workflows, [:named_table, :public, :set])
    
    # Initialize telemetry for comprehensive monitoring
    :telemetry.attach_many(
      "cns-forge-test-handler",
      [
        [:cns_forge, :reactor, :step, :start],
        [:cns_forge, :reactor, :step, :stop],
        [:cns_forge, :bitactor, :execution, :complete],
        [:cns_forge, :ttl, :decremented],
        [:cns_forge, :pulse, :logged]
      ],
      &__MODULE__.handle_telemetry/4,
      %{}
    )
    
    :ok
  end
  
  setup do
    # Reset state for each test
    :ets.delete_all_objects(:cns_forge_bit_actors)
    :ets.delete_all_objects(:cns_forge_pulse_logs)
    :ets.delete_all_objects(:cns_forge_workflows)
    
    # Mock Python bridge for directive parsing
    CNSForge.PythonBridge
    |> stub(:call_directive_parser, fn params ->
      {:ok, %CNSForge.ParsedDirective{
        directive_id: UUID.uuid4(),
        original_text: params.directive_text,
        intent_category: "forex_trading",
        target_metrics: %{latency_ns: 42, uptime_percentage: 99.999},
        constraints: %{risk_tolerance: "medium"},
        generated_ttl: generate_mock_ttl(params),
        confidence_score: 0.95,
        parsed_at: DateTime.utc_now()
      }}
    end)
    
    # Mock TTL loader
    CNSForge.TTLLoader
    |> stub(:load_ontology, fn _path, _queries ->
      {:ok, %{
        semantic_graph: generate_mock_semantic_graph(),
        sparql_queries: load_mock_sparql_queries(),
        performance_metadata: %{load_time_ms: 150}
      }}
    end)
    
    # Mock quantum semantic compiler
    CNSForge.QuantumSemanticCompiler
    |> stub(:generate_bitactor, fn params ->
      {:ok, %{
        c_implementation: generate_mock_c_code(params),
        erlang_nif: generate_mock_nif_code(params),
        metadata: %{
          estimated_latency_ns: params.latency_target_ns,
          memory_usage_bytes: 1024,
          optimization_level: params.optimization_level
        }
      }}
    end)
    
    %{
      test_forex_pair: "EUR/USD",
      test_directive: "achieve 42ns latency with 99.999% uptime",
      test_risk_params: %{
        max_position_size: 1000000,
        stop_loss_percentage: 2.0,
        max_daily_loss: 50000
      },
      test_market_conditions: %{
        volatility: "medium",
        liquidity: "high",
        trend: "bullish"
      }
    }
  end
  
  # ============================================================================
  # 1. Unit Testing Individual Steps
  # ============================================================================
  
  describe "Unit Testing - Individual Reactor Steps" do
    test "DirectiveParser.parse_directive validates input and produces structured output" do
      arguments = %{
        directive_text: "achieve ultra-low latency trading with 42ns execution",
        domain_context: "forex_trading",
        target_ontology: "production_forex_trading.ttl"
      }
      
      assert {:ok, result} = DirectiveParser.parse_directive(arguments, %{}, [])
      
      # Validate structured output
      assert %CNSForge.ParsedDirective{} = result
      assert result.directive_id != nil
      assert result.original_text == arguments.directive_text
      assert result.intent_category == "forex_trading"
      assert result.target_metrics.latency_ns == 42
      assert result.confidence_score >= 0.8
      assert is_binary(result.generated_ttl)
      
      # Verify telemetry event was emitted
      assert_receive {:telemetry_event, [:cns_forge, :directive, :parsed], %{latency_ns: 42}}
    end
    
    test "DirectiveParser handles invalid input gracefully" do
      invalid_arguments = %{
        directive_text: "",  # Empty directive
        domain_context: "unknown_domain",
        target_ontology: "nonexistent.ttl"
      }
      
      CNSForge.PythonBridge
      |> expect(:call_directive_parser, fn _params ->
        {:error, "Invalid directive: empty text"}
      end)
      
      assert {:error, reason} = DirectiveParser.parse_directive(invalid_arguments, %{}, [])
      assert reason =~ "Invalid directive"
    end
    
    test "BitActorGenerator produces optimized C code and Erlang NIF" do
      ontology = %{semantic_graph: generate_mock_semantic_graph()}
      performance_requirements = %{latency_ns: 42, throughput_ops_sec: 1000000}
      
      arguments = %{
        ontology: ontology,
        performance_requirements: performance_requirements,
        latency_target_ns: 42,
        optimization_level: "O3"
      }
      
      assert {:ok, result} = BitActorGenerator.generate_bitactor(arguments, %{}, [])
      
      # Validate generated BitActor
      assert %CNSForge.GeneratedBitActor{} = result
      assert is_binary(result.c_implementation)
      assert is_binary(result.erlang_nif_wrapper)
      assert result.estimated_latency_ns <= 42
      assert "O3" in result.optimization_flags
      assert "cache_aligned" in result.optimization_flags
      assert "simd_optimized" in result.optimization_flags
      
      # Verify C code contains expected optimizations
      assert result.c_implementation =~ "#pragma GCC optimize"
      assert result.c_implementation =~ "rdtsc()"
      assert result.c_implementation =~ "TTL_BUDGET"
    end
  end
  
  # ============================================================================
  # 2. Integration Testing Complete Reactors
  # ============================================================================
  
  describe "Integration Testing - Complete Reactor Workflows" do
    test "EndToEndForexReactor executes complete workflow successfully", context do
      inputs = %{
        forex_pair: context.test_forex_pair,
        trading_directive: context.test_directive,
        risk_parameters: context.test_risk_params,
        news_sentiment: %{sentiment: "neutral", confidence: 0.8},
        market_conditions: context.test_market_conditions
      }
      
      # Execute the complete end-to-end workflow
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Validate complete workflow execution
      assert %{
        trade_result: trade_result,
        performance_metrics: performance_metrics,
        system_optimizations: optimizations,
        latency_achieved_ns: latency,
        uptime_percentage: uptime,
        semantic_compliance: compliance
      } = result
      
      # Verify trading execution was successful
      assert trade_result != nil
      assert performance_metrics != nil
      assert is_integer(latency)
      assert latency <= 50  # Should meet near-target latency
      assert uptime >= 99.9
      assert compliance.valid? == true
      
      # Verify system optimizations were applied
      assert optimizations.latency_improvement_percent >= 0
      assert optimizations.optimization_applied == true
    end
    
    test "Reactor handles workflow failure with proper compensation", context do
      # Setup failure in trade execution step
      CNSForge.TradeExecution
      |> expect(:execute_trade, fn _inputs, _context, _opts ->
        {:error, :insufficient_liquidity}
      end)
      |> expect(:compensate_trade_execution, fn _reason, _inputs, _context, _opts ->
        # Compensation should rollback any partial state
        {:ok, %{compensated: true, rollback_complete: true}}
      end)
      
      inputs = %{
        forex_pair: context.test_forex_pair,
        trading_directive: context.test_directive,
        risk_parameters: context.test_risk_params,
        news_sentiment: %{sentiment: "negative", confidence: 0.9},
        market_conditions: %{volatility: "high", liquidity: "low"}
      }
      
      # Workflow should fail but compensation should execute
      assert {:error, _reason} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Verify compensation was called
      assert_receive {:compensation_executed, :trade_execution}
    end
  end
  
  # ============================================================================
  # 3. Error Handling and Compensation Logic Testing
  # ============================================================================
  
  describe "Error Handling and Compensation Logic" do
    test "BitActorGenerator cleanup compensation removes generated files" do
      # Setup file generation
      generation_metadata = %{
        generated_files: ["/tmp/bitactor_test.c", "/tmp/bitactor_test.nif"],
        temp_directory: "/tmp/cns_forge_test"
      }
      
      CNSForge.FileManager
      |> expect(:cleanup_generated_files, fn metadata ->
        # Verify cleanup is called with correct metadata
        assert metadata == generation_metadata
        {:ok, %{files_removed: 2, directories_cleaned: 1}}
      end)
      
      arguments = %{generation_metadata: generation_metadata}
      assert {:ok, result} = BitActorGenerator.cleanup_generated_code(arguments, %{}, [])
      assert result.cleaned_up == true
    end
    
    test "Reactor transaction rollback triggers all undo operations" do
      # Setup multiple steps with undo operations
      CNSForge.RiskAssessment
      |> stub(:assess_risk, fn _inputs, _context, _opts ->
        {:ok, %{risk_level: "acceptable", confidence: 0.85}}
      end)
      |> expect(:undo_risk_assessment, fn _result, _inputs, _context, _opts ->
        {:ok, %{risk_assessment_undone: true}}
      end)
      
      CNSForge.TradeExecution
      |> stub(:execute_trade, fn _inputs, _context, _opts ->
        {:ok, %{trade_id: "T123456", status: "executed"}}
      end)
      |> expect(:compensate_trade_execution, fn _reason, _inputs, _context, _opts ->
        {:ok, %{trade_cancelled: true, refund_issued: true}}
      end)
      
      CNSForge.PerformanceMonitor
      |> stub(:monitor_execution, fn _inputs, _context, _opts ->
        # Simulate monitoring failure that triggers rollback
        {:error, :monitoring_system_failure}
      end)
      
      inputs = %{
        forex_pair: "GBP/USD",
        trading_directive: "execute high-frequency arbitrage",
        risk_parameters: %{max_risk: "low"},
        market_conditions: %{stability: "high"}
      }
      
      # Transaction should fail and trigger all undo operations
      assert {:error, _reason} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Verify all undo operations were called
      assert_receive {:undo_executed, :risk_assessment}
      assert_receive {:compensation_executed, :trade_execution}
    end
  end
  
  # ============================================================================
  # 4. Concurrent Execution Testing
  # ============================================================================
  
  describe "Concurrent Execution Testing" do
    test "Multiple reactor workflows execute concurrently without interference" do
      # Create multiple concurrent workflow executions
      inputs_list = [
        %{forex_pair: "EUR/USD", trading_directive: "scalping_42ns", risk_parameters: %{}},
        %{forex_pair: "GBP/USD", trading_directive: "arbitrage_50ns", risk_parameters: %{}},
        %{forex_pair: "USD/JPY", trading_directive: "momentum_60ns", risk_parameters: %{}}
      ]
      
      # Execute all workflows concurrently
      tasks = Enum.map(inputs_list, fn inputs ->
        Task.async(fn ->
          Reactor.run(EndToEndForexReactor, inputs, %{}, async?: true)
        end)
      end)
      
      # Wait for all tasks to complete
      results = Task.await_many(tasks, 30_000)
      
      # Verify all workflows completed successfully
      Enum.each(results, fn result ->
        assert {:ok, workflow_result} = result
        assert workflow_result.latency_achieved_ns <= 100
        assert workflow_result.uptime_percentage >= 99.0
      end)
      
      # Verify no resource contention occurred
      pulse_logs = :ets.tab2list(:cns_forge_pulse_logs)
      assert length(pulse_logs) >= 15  # Each workflow should generate multiple logs
      
      # Verify transaction IDs are unique
      transaction_ids = Enum.map(pulse_logs, fn {_, log} -> log.transaction_id end)
      assert length(Enum.uniq(transaction_ids)) == 3
    end
    
    test "Reactor handles backpressure and resource limits gracefully" do
      # Simulate high load scenario
      concurrent_workflows = 50
      
      inputs = %{
        forex_pair: "EUR/USD",
        trading_directive: "high_load_test",
        risk_parameters: %{max_concurrent: 10}
      }
      
      # Create many concurrent executions
      tasks = for i <- 1..concurrent_workflows do
        Task.async(fn ->
          workflow_inputs = Map.put(inputs, :workflow_id, i)
          Reactor.run(EndToEndForexReactor, workflow_inputs, %{}, async?: true)
        end)
      end
      
      # Some should succeed, some may be throttled
      results = Task.await_many(tasks, 60_000)
      
      successful_results = Enum.filter(results, &match?({:ok, _}, &1))
      
      # Should have some successful executions
      assert length(successful_results) >= 5
      
      # System should remain stable
      bit_actors = :ets.tab2list(:cns_forge_bit_actors)
      assert length(bit_actors) <= 100  # Resource limit respected
    end
  end
  
  # ============================================================================
  # 5. TTL-Driven Workflow Validation
  # ============================================================================
  
  describe "TTL-Driven Workflow Validation" do
    test "TTL decrements correctly through each workflow step" do
      inputs = %{
        forex_pair: "EUR/USD",
        trading_directive: "ttl_validation_test",
        risk_parameters: %{},
        initial_ttl: 8
      }
      
      # Track TTL decrementation through telemetry
      ttl_events = []
      
      :telemetry.attach(
        "ttl-tracker",
        [:cns_forge, :ttl, :decremented],
        fn _event, measurements, metadata, _config ->
          send(self(), {:ttl_event, measurements, metadata})
        end,
        %{}
      )
      
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Collect all TTL events
      ttl_events = collect_ttl_events([])
      
      # Verify TTL decremented through workflow
      assert length(ttl_events) >= 5
      
      # Verify TTL sequence is correct
      ttl_values = Enum.map(ttl_events, & &1.measurements.current_ttl)
      assert ttl_values == Enum.sort(ttl_values, :desc)  # Should be decreasing
      
      # Verify final TTL is within bounds
      final_ttl = List.last(ttl_values)
      assert final_ttl >= 0
      assert final_ttl < 8
      
      :telemetry.detach("ttl-tracker")
    end
    
    test "Workflow terminates when TTL reaches zero" do
      inputs = %{
        forex_pair: "EUR/USD",
        trading_directive: "ttl_expiration_test",
        risk_parameters: %{},
        initial_ttl: 1  # Very low TTL to force expiration
      }
      
      # Mock slow operation to trigger TTL expiration
      CNSForge.TradeExecution
      |> stub(:execute_trade, fn _inputs, _context, _opts ->
        Process.sleep(100)  # Simulate slow operation
        {:ok, %{trade_id: "SLOW123"}}
      end)
      
      # Workflow should fail due to TTL expiration
      assert {:error, reason} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      assert reason == :ttl_expired
      
      # Verify TTL expiration was logged
      pulse_logs = :ets.tab2list(:cns_forge_pulse_logs)
      expiration_logs = Enum.filter(pulse_logs, fn {_, log} -> 
        log.event_type == "ttl_expired" 
      end)
      assert length(expiration_logs) >= 1
    end
  end
  
  # ============================================================================
  # 6. BitActor Mesh Integration Testing
  # ============================================================================
  
  describe "BitActor Mesh Integration" do
    test "BitActors are created and managed correctly throughout workflow" do
      inputs = %{
        forex_pair: "EUR/USD",
        trading_directive: "bitactor_mesh_test",
        risk_parameters: %{}
      }
      
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Verify BitActors were created
      bit_actors = :ets.tab2list(:cns_forge_bit_actors)
      assert length(bit_actors) >= 5
      
      # Verify BitActor types match CNS Forge specification
      actor_types = Enum.map(bit_actors, fn {_, actor} -> actor.actor_type end)
      expected_types = [:directive_parser, :semantic_validator, :risk_assessor, :trade_executor, :monitor]
      
      Enum.each(expected_types, fn type ->
        assert type in actor_types, "Missing BitActor type: #{type}"
      end)
      
      # Verify all BitActors have valid state
      Enum.each(bit_actors, fn {_, actor} ->
        assert actor.hops_processed > 0
        assert actor.status in [:active, :completed, :terminated]
        assert is_binary(actor.transaction_id)
        assert actor.created_at != nil
      end)
    end
    
    test "BitActor mesh handles signal routing correctly" do
      inputs = %{
        forex_pair: "GBP/USD",
        trading_directive: "signal_routing_test",
        risk_parameters: %{}
      }
      
      # Track signal routing through telemetry
      :telemetry.attach(
        "signal-router",
        [:cns_forge, :signal, :routed],
        fn _event, measurements, metadata, _config ->
          send(self(), {:signal_routed, measurements, metadata})
        end,
        %{}
      )
      
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Verify signals were routed between BitActors
      signal_events = collect_signal_events([])
      assert length(signal_events) >= 10
      
      # Verify signal routing preserves transaction context
      transaction_ids = Enum.map(signal_events, & &1.metadata.transaction_id)
      assert length(Enum.uniq(transaction_ids)) == 1  # All signals same transaction
      
      :telemetry.detach("signal-router")
    end
  end
  
  # ============================================================================
  # 7. OTEL Telemetry Validation
  # ============================================================================
  
  describe "OpenTelemetry Integration" do
    test "Comprehensive telemetry data is collected throughout workflow" do
      inputs = %{
        forex_pair: "USD/JPY",
        trading_directive: "otel_telemetry_test",
        risk_parameters: %{}
      }
      
      # Enable OTEL span collection
      :opentelemetry.set_default_tracer({:otel_tracer_default, :cns_forge})
      
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Verify OTEL spans were created
      spans = :otel_batch_processor.force_flush()
      assert length(spans) >= 10
      
      # Verify span hierarchy represents workflow structure
      root_spans = Enum.filter(spans, fn span -> span.parent_span_id == undefined end)
      assert length(root_spans) == 1
      
      # Verify span attributes contain CNS Forge metadata
      workflow_span = List.first(root_spans)
      assert workflow_span.attributes["cns_forge.workflow"] == "EndToEndForexReactor"
      assert workflow_span.attributes["cns_forge.forex_pair"] == "USD/JPY"
      assert workflow_span.attributes["cns_forge.ttl_initial"] != nil
      
      # Verify performance metrics are captured
      performance_spans = Enum.filter(spans, fn span ->
        span.name =~ "performance"
      end)
      assert length(performance_spans) >= 1
      
      performance_span = List.first(performance_spans)
      assert performance_span.attributes["latency_ns"] != nil
      assert performance_span.attributes["throughput_ops_sec"] != nil
    end
    
    test "OTEL metrics are exported correctly" do
      inputs = %{
        forex_pair: "EUR/GBP",
        trading_directive: "otel_metrics_test",
        risk_parameters: %{}
      }
      
      # Setup OTEL metrics collection
      :otel_metrics.set_default_meter(:cns_forge_meter)
      
      assert {:ok, result} = Reactor.run(EndToEndForexReactor, inputs, %{}, async?: false)
      
      # Force metrics export
      :otel_metrics.force_flush()
      
      # Verify key metrics were recorded
      recorded_metrics = :otel_metrics.get_all_metrics()
      metric_names = Enum.map(recorded_metrics, & &1.name)
      
      expected_metrics = [
        "cns_forge.workflow.duration",
        "cns_forge.bitactor.execution_time",
        "cns_forge.ttl.hops_remaining", 
        "cns_forge.trade.latency_ns",
        "cns_forge.system.memory_usage"
      ]
      
      Enum.each(expected_metrics, fn metric ->
        assert metric in metric_names, "Missing metric: #{metric}"
      end)
    end
  end
  
  # ============================================================================
  # Helper Functions
  # ============================================================================
  
  def handle_telemetry(event, measurements, metadata, _config) do
    # Store telemetry events for test verification
    case event do
      [:cns_forge, :reactor, :step, :stop] ->
        send(self(), {:telemetry_event, event, measurements})
        
      [:cns_forge, :ttl, :decremented] ->
        send(self(), {:ttl_event, measurements, metadata})
        
      [:cns_forge, :signal, :routed] ->
        send(self(), {:signal_routed, measurements, metadata})
        
      [:cns_forge, :pulse, :logged] ->
        # Store pulse log in ETS for verification
        :ets.insert(:cns_forge_pulse_logs, {
          {metadata.transaction_id, metadata.sequence_number},
          %{
            event_type: metadata.event_type,
            transaction_id: metadata.transaction_id,
            sequence_number: metadata.sequence_number,
            timestamp: DateTime.utc_now(),
            metadata: metadata
          }
        })
        
      _ ->
        :ok
    end
  end
  
  defp generate_mock_ttl(%{directive_text: directive}) do
    """
    @prefix cns: <http://cns-forge.com/ontology#> .
    @prefix forex: <http://cns-forge.com/forex#> .
    
    forex:TradingDirective a cns:Directive ;
        cns:originalText "#{directive}" ;
        cns:targetLatency 42 ;
        cns:targetUptime 99.999 ;
        cns:optimizationLevel "O3" .
    """
  end
  
  defp generate_mock_semantic_graph do
    %{
      triples: [
        {"forex:EUR_USD", "rdf:type", "forex:CurrencyPair"},
        {"forex:TradingEngine", "cns:targetLatency", "42"},
        {"forex:RiskProfile", "cns:maxExposure", "1000000"}
      ],
      prefixes: %{
        "cns" => "http://cns-forge.com/ontology#",
        "forex" => "http://cns-forge.com/forex#"
      }
    }
  end
  
  defp load_mock_sparql_queries do
    [
      """
      SELECT ?latency WHERE {
        ?directive cns:targetLatency ?latency .
      }
      """,
      """
      SELECT ?pair WHERE {
        ?pair rdf:type forex:CurrencyPair .
      }
      """
    ]
  end
  
  defp generate_mock_c_code(%{latency_target_ns: latency}) do
    """
    // Ultra-low latency BitActor implementation
    // Generated for #{latency}ns target latency
    
    #include "bitactor.h"
    #pragma GCC optimize("O3")
    
    static inline uint64_t rdtsc() {
        return __rdtsc();
    }
    
    #define TTL_BUDGET #{div(latency, 8)}
    
    typedef struct {
        uint64_t ttl_hops;
        uint64_t transaction_id;
        uint8_t payload[512];
    } forex_token_t;
    
    int execute_trade(forex_token_t* token) {
        uint64_t start_cycle = rdtsc();
        
        // Ultra-low latency trading logic
        if (token->ttl_hops == 0) {
            return -1; // TTL expired
        }
        
        token->ttl_hops--;
        
        uint64_t end_cycle = rdtsc();
        return (end_cycle - start_cycle) <= TTL_BUDGET ? 0 : -1;
    }
    """
  end
  
  defp generate_mock_nif_code(%{latency_target_ns: latency}) do
    """
    // Erlang NIF wrapper for ultra-low latency BitActor
    // Target latency: #{latency}ns
    
    #include "erl_nif.h"
    #include "bitactor_generated.h"
    
    static ERL_NIF_TERM execute_trade_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        // NIF implementation for #{latency}ns target
        return enif_make_atom(env, "ok");
    }
    
    static ErlNifFunc nif_funcs[] = {
        {"execute_trade", 1, execute_trade_nif}
    };
    
    ERL_NIF_INIT(bitactor_forex, nif_funcs, NULL, NULL, NULL, NULL)
    """
  end
  
  defp collect_ttl_events(events) do
    receive do
      {:ttl_event, measurements, metadata} ->
        collect_ttl_events([%{measurements: measurements, metadata: metadata} | events])
    after
      100 -> Enum.reverse(events)
    end
  end
  
  defp collect_signal_events(events) do
    receive do
      {:signal_routed, measurements, metadata} ->
        collect_signal_events([%{measurements: measurements, metadata: metadata} | events])
    after
      100 -> Enum.reverse(events)
    end
  end
end