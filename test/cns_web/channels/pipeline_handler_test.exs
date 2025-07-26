defmodule CnsWeb.Channels.PipelineHandlerTest do
  @moduledoc """
  Comprehensive unit tests for PipelineHandler with 80/20 optimization.
  Tests critical path execution, optimization strategies, and performance.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.Channels.PipelineHandler
  alias Cns.Swarm.{Pipeline, Optimizer, Metrics}
  
  import Mox
  
  setup :verify_on_exit!
  
  setup do
    # Mock dependencies
    Mox.defmock(MockPipeline, for: Pipeline)
    Mox.defmock(MockOptimizer, for: Optimizer)
    Mox.defmock(MockMetrics, for: Metrics)
    
    # Create test socket with 80/20 optimization
    socket = %Phoenix.Socket{
      assigns: %{
        swarm_id: "test-swarm-123",
        optimization_mode: "80_20",
        current_user: %{id: 1, role: "admin"},
        critical_stages: ["k8s", "reactor", "ash", "turtle", "typer"]
      }
    }
    
    {:ok, %{socket: socket}}
  end
  
  describe "execute/3 - 80/20 Pipeline Execution" do
    test "executes only critical stages in 80/20 mode", %{socket: socket} do
      payload = %{
        "strategy" => "80_20",
        "data" => %{"input" => "test_data"}
      }
      
      # Mock pipeline execution
      MockPipeline
      |> expect(:execute, fn stages, _payload, opts ->
        assert stages == ["k8s", "reactor", "ash", "turtle", "typer"]
        assert opts[:strategy] == "80_20"
        
        {:ok, %{
          stages_executed: stages,
          skipped_stages: ["erlang", "bitactor", "ttl2dspy"],
          duration: 1500,
          estimated_full_duration: 3500,
          current_stage: "completed",
          progress: 100,
          metrics: %{cpu: 45, memory: 60, latency: 120, throughput: 85}
        }}
      end)
      
      {:reply, {:ok, result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
      
      # Validate 80/20 optimization results
      assert result.executed_stages == ["k8s", "reactor", "ash", "turtle", "typer"]
      assert result.skipped_stages == ["erlang", "bitactor", "ttl2dspy"]
      assert result.optimization_savings.stages_skipped == 3
      assert result.optimization_savings.time_saved == 2000
      assert result.optimization_savings.efficiency_gain > 37.0
      
      # Validate critical metrics are present
      assert result.critical_metrics.cpu == 45
      assert result.critical_metrics.memory == 60
    end
    
    test "executes all stages in full mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "full")}
      
      payload = %{
        "strategy" => "full",
        "data" => %{"input" => "test_data"}
      }
      
      MockPipeline
      |> expect(:execute, fn stages, _payload, opts ->
        assert length(stages) == 8  # All pipeline stages
        assert opts[:strategy] == "full"
        
        {:ok, %{
          stages_executed: stages,
          skipped_stages: [],
          duration: 3500,
          estimated_full_duration: 3500,
          current_stage: "completed",
          progress: 100,
          metrics: %{cpu: 75, memory: 80, latency: 250, throughput: 60}
        }}
      end)
      
      {:reply, {:ok, result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
      
      # Should not have optimization savings in full mode
      assert result.skipped_stages == []
      assert result.optimization_savings.stages_skipped == 0
    end
    
    test "handles pipeline execution errors", %{socket: socket} do
      payload = %{
        "strategy" => "80_20",
        "data" => %{"input" => "invalid_data"}
      }
      
      MockPipeline
      |> expect(:execute, fn _stages, _payload, _opts ->
        {:error, "reactor", "Step validation failed"}
      end)
      
      {:reply, {:error, error}, _socket} = PipelineHandler.execute(payload, %{}, socket)
      
      assert error.stage == "reactor"
      assert error.reason == "Step validation failed"
    end
    
    test "applies custom stage selection", %{socket: socket} do
      payload = %{
        "strategy" => "custom",
        "stages" => ["k8s", "ash", "typer"],
        "data" => %{"input" => "test_data"}
      }
      
      MockPipeline
      |> expect(:execute, fn stages, _payload, _opts ->
        assert stages == ["k8s", "ash", "typer"]
        
        {:ok, %{
          stages_executed: stages,
          skipped_stages: ["reactor", "turtle", "erlang", "bitactor", "ttl2dspy"],
          duration: 1000,
          estimated_full_duration: 3500,
          current_stage: "completed",
          progress: 100,
          metrics: %{cpu: 30, memory: 45, latency: 80, throughput: 95}
        }}
      end)
      
      {:reply, {:ok, result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
      
      assert result.executed_stages == ["k8s", "ash", "typer"]
      assert length(result.skipped_stages) == 5
    end
  end
  
  describe "optimize/3 - Auto-Optimization" do
    test "analyzes and applies auto-optimization", %{socket: socket} do
      payload = %{"type" => "auto"}
      
      current_metrics = %{
        cpu_usage: 85,
        memory_usage: 70,
        latency: 250,
        error_rate: 2,
        stage_performance: %{
          "k8s" => %{duration: 500, errors: 0},
          "reactor" => %{duration: 300, errors: 1},
          "ash" => %{duration: 200, errors: 0},
          "erlang" => %{duration: 150, errors: 0},
          "bitactor" => %{duration: 100, errors: 0},
          "ttl2dspy" => %{duration: 80, errors: 0},
          "turtle" => %{duration: 120, errors: 0},
          "typer" => %{duration: 180, errors: 0}
        }
      }
      
      MockMetrics
      |> expect(:get_current, fn swarm_id ->
        assert swarm_id == "test-swarm-123"
        current_metrics
      end)
      
      MockOptimizer
      |> expect(:analyze_and_optimize, fn metrics, type ->
        assert metrics == current_metrics
        assert type == "auto"
        
        {:ok, %{
          type: "auto",
          stages: ["k8s", "reactor", "ash", "turtle", "typer"],
          skip_stages: ["erlang", "bitactor", "ttl2dspy"],
          expected_improvement: 45,
          thresholds: %{cpu: 80, memory: 75, latency: 200}
        }}
      end)
      
      {:reply, {:ok, optimization}, _socket} = PipelineHandler.optimize(payload, %{}, socket)
      
      assert optimization.type == "auto"
      assert optimization.expected_improvement == 45
      assert "erlang" in optimization.skip_stages
      assert "bitactor" in optimization.skip_stages
      assert "ttl2dspy" in optimization.skip_stages
    end
    
    test "handles optimization analysis errors", %{socket: socket} do
      payload = %{"type" => "invalid_type"}
      
      MockMetrics
      |> expect(:get_current, fn _swarm_id ->
        %{cpu_usage: 50, memory_usage: 40}
      end)
      
      MockOptimizer
      |> expect(:analyze_and_optimize, fn _metrics, _type ->
        {:error, "Invalid optimization type"}
      end)
      
      {:reply, {:error, error}, _socket} = PipelineHandler.optimize(payload, %{}, socket)
      
      assert error.reason == "Invalid optimization type"
    end
  end
  
  describe "status/3 - Pipeline Status" do
    test "returns 80/20 filtered status", %{socket: socket} do
      full_status = %{
        stages: %{
          "k8s" => %{health: 95, latency: 100, error_rate: 1},
          "reactor" => %{health: 90, latency: 150, error_rate: 2},
          "ash" => %{health: 88, latency: 120, error_rate: 1},
          "erlang" => %{health: 85, latency: 80, error_rate: 0},
          "bitactor" => %{health: 82, latency: 60, error_rate: 0},
          "ttl2dspy" => %{health: 80, latency: 50, error_rate: 0},
          "turtle" => %{health: 92, latency: 110, error_rate: 1},
          "typer" => %{health: 94, latency: 130, error_rate: 2}
        }
      }
      
      MockPipeline
      |> expect(:get_status, fn swarm_id ->
        assert swarm_id == "test-swarm-123"
        full_status
      end)
      
      {:reply, {:ok, status}, _socket} = PipelineHandler.status(%{}, %{}, socket)
      
      # Should only include critical stages
      critical_stages = Map.keys(status.critical_stages)
      assert "k8s" in critical_stages
      assert "reactor" in critical_stages
      assert "ash" in critical_stages
      assert "turtle" in critical_stages
      assert "typer" in critical_stages
      
      # Should not include non-critical stages
      refute "erlang" in critical_stages
      refute "bitactor" in critical_stages
      refute "ttl2dspy" in critical_stages
      
      # Should calculate 80/20 health
      assert status.overall_health > 0
      assert is_list(status.bottlenecks)
      assert is_list(status.optimization_opportunities)
    end
    
    test "returns full status in non-80/20 mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "full")}
      
      full_status = %{
        stages: %{
          "k8s" => %{health: 95, latency: 100, error_rate: 1},
          "reactor" => %{health: 90, latency: 150, error_rate: 2}
        }
      }
      
      MockPipeline
      |> expect(:get_status, fn _swarm_id -> full_status end)
      
      {:reply, {:ok, status}, _socket} = PipelineHandler.status(%{}, %{}, socket)
      
      # Should return full status without 80/20 filtering
      assert status == full_status
    end
  end
  
  describe "handle_in/4 - Event Delegation" do
    test "handles stage events", %{socket: socket} do
      event = "k8s:deploy"
      payload = %{"action" => "deploy", "image" => "test:latest"}
      bindings = %{}
      
      {:reply, {:ok, result}, _socket} = PipelineHandler.handle_in(event, payload, bindings, socket)
      
      assert result.stage == "k8s"
      assert result.action == "deploy"
    end
    
    test "handles optimization events", %{socket: socket} do
      event = "optimize:suggest"
      payload = %{"context" => %{"cpu_high" => true}}
      bindings = %{}
      
      MockOptimizer
      |> expect(:suggest_optimizations, fn swarm_id, context ->
        assert swarm_id == "test-swarm-123"
        assert context.cpu_high == true
        
        [
          %{
            id: "reduce-stages",
            title: "Reduce Non-Critical Stages",
            impact: 40,
            effort: "low"
          }
        ]
      end)
      
      {:reply, {:ok, suggestions}, _socket} = PipelineHandler.handle_in(event, payload, bindings, socket)
      
      assert is_list(suggestions)
      assert hd(suggestions).id == "reduce-stages"
    end
    
    test "handles metrics events", %{socket: socket} do
      event = "metrics:cpu"
      payload = %{"options" => %{"window" => "5m"}}
      bindings = %{}
      
      MockMetrics
      |> expect(:get_by_type, fn swarm_id, metric_type, options ->
        assert swarm_id == "test-swarm-123"
        assert metric_type == "cpu"
        assert options.window == "5m"
        
        %{
          average: 65,
          max: 85,
          min: 45,
          p95: 80
        }
      end)
      
      {:reply, {:ok, metrics}, _socket} = PipelineHandler.handle_in(event, payload, bindings, socket)
      
      assert metrics.average == 65
      assert metrics.p95 == 80
    end
    
    test "handles unknown events", %{socket: socket} do
      event = "unknown:action"
      payload = %{}
      bindings = %{}
      
      {:reply, {:error, error}, _socket} = PipelineHandler.handle_in(event, payload, bindings, socket)
      
      assert error.reason == "Unknown pipeline event: unknown:action"
    end
  end
  
  describe "80/20 Optimization Logic" do
    test "calculates correct efficiency gains", %{socket: socket} do
      result = %{
        stages_executed: ["k8s", "reactor", "ash", "turtle", "typer"],
        skipped_stages: ["erlang", "bitactor", "ttl2dspy"],
        duration: 1500,
        estimated_full_duration: 3500
      }
      
      savings = PipelineHandler.calculate_savings(result)
      
      assert savings.stages_skipped == 3
      assert savings.time_saved == 2000
      assert_in_delta savings.efficiency_gain, 37.5, 0.1
    end
    
    test "identifies bottlenecks correctly", %{socket: socket} do
      status = %{
        stages: %{
          "k8s" => %{health: 95, latency: 500, error_rate: 8},  # High latency & errors
          "reactor" => %{health: 90, latency: 150, error_rate: 2},
          "ash" => %{health: 60, latency: 300, error_rate: 6},  # High latency & errors
          "turtle" => %{health: 95, latency: 100, error_rate: 1}
        }
      }
      
      bottlenecks = PipelineHandler.identify_bottlenecks(status)
      
      # Should identify k8s and ash as bottlenecks
      bottleneck_stages = Enum.map(bottlenecks, & &1.stage)
      assert "k8s" in bottleneck_stages
      assert "ash" in bottleneck_stages
      refute "reactor" in bottleneck_stages
      refute "turtle" in bottleneck_stages
      
      # Should be sorted by impact
      assert hd(bottlenecks).impact >= List.last(bottlenecks).impact
    end
    
    test "finds optimization opportunities", %{socket: socket} do
      status = %{
        stages: %{
          "k8s" => %{health: 95, latency: 100, error_rate: 1, utilization: 85},
          "erlang" => %{health: 80, latency: 50, error_rate: 0, utilization: 15},  # Low utilization
          "bitactor" => %{health: 82, latency: 60, error_rate: 0, utilization: 10},  # Low utilization
          "turtle" => %{health: 92, latency: 110, error_rate: 1, utilization: 75}
        }
      }
      
      opportunities = PipelineHandler.find_optimization_opportunities(status)
      
      # Should identify low-utilization non-critical stages
      stage_names = Enum.map(opportunities, & &1.stage)
      assert "erlang" in stage_names
      assert "bitactor" in stage_names
      refute "k8s" in stage_names  # Critical stage
      refute "turtle" in stage_names  # Critical stage
      
      # Should include recommendations
      erlang_opp = Enum.find(opportunities, &(&1.stage == "erlang"))
      assert erlang_opp.recommendation == "Consider removing from pipeline"
      assert erlang_opp.potential_saving == "85%"
    end
  end
  
  describe "Performance and Metrics" do
    test "tracks execution metrics", %{socket: socket} do
      result = %{
        duration: 1500,
        stages_executed: ["k8s", "reactor", "ash"],
        metrics: %{cpu: 65, memory: 70}
      }
      
      MockMetrics
      |> expect(:record_execution, fn swarm_id, metrics ->
        assert swarm_id == "test-swarm-123"
        assert metrics.duration == 1500
        assert metrics.stages_executed == ["k8s", "reactor", "ash"]
        assert metrics.optimization_mode == "80_20"
        assert metrics.success == true
      end)
      
      PipelineHandler.track_execution(socket, result)
    end
    
    test "extracts critical metrics", %{socket: socket} do
      result = %{
        metrics: %{
          cpu: 65,
          memory: 70,
          latency: 120,
          throughput: 85,
          non_critical_metric: 99
        }
      }
      
      critical_metrics = PipelineHandler.extract_critical_metrics(result)
      
      assert critical_metrics.cpu == 65
      assert critical_metrics.memory == 70
      assert critical_metrics.latency == 120
      assert critical_metrics.throughput == 85
      refute Map.has_key?(critical_metrics, :non_critical_metric)
    end
  end
  
  describe "Error Handling and Edge Cases" do
    test "handles empty stages list", %{socket: socket} do
      payload = %{
        "strategy" => "custom",
        "stages" => [],
        "data" => %{"input" => "test"}
      }
      
      MockPipeline
      |> expect(:execute, fn stages, _payload, _opts ->
        assert stages == ["k8s", "reactor", "ash", "turtle", "typer"]  # Should default to critical
        {:ok, %{stages_executed: stages, duration: 1000}}
      end)
      
      {:reply, {:ok, _result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
    end
    
    test "handles invalid optimization mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "invalid")}
      
      payload = %{
        "strategy" => "80_20",
        "data" => %{"input" => "test"}
      }
      
      # Should default to critical stages for unknown optimization mode
      MockPipeline
      |> expect(:execute, fn stages, _payload, _opts ->
        assert stages == ["k8s", "reactor", "ash", "turtle", "typer"]
        {:ok, %{stages_executed: stages, duration: 1000}}
      end)
      
      {:reply, {:ok, _result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
    end
    
    test "handles missing socket assigns", %{socket: socket} do
      socket = %{socket | assigns: %{swarm_id: "test"}}  # Missing optimization_mode
      
      payload = %{"strategy" => "80_20"}
      
      # Should handle gracefully
      MockPipeline
      |> expect(:execute, fn _stages, _payload, _opts ->
        {:ok, %{stages_executed: [], duration: 0}}
      end)
      
      {:reply, {:ok, _result}, _socket} = PipelineHandler.execute(payload, %{}, socket)
    end
  end
end