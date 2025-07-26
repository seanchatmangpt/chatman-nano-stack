defmodule CnsWeb.Channels.PipelineHandlerTest do
  @moduledoc """
  Unit tests for PipelineHandler with 80/20 optimization validation.
  Tests pipeline execution, monitoring, and stage management.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: true
  
  alias CnsWeb.Channels.PipelineHandler
  alias CnsWeb.UserSocket
  
  setup do
    socket = socket(UserSocket, "user_id", %{
      current_user: %{id: "test_user", role: "operator"},
      optimization_mode: "80_20",
      connector_id: "test_connector_123"
    })
    
    %{socket: socket}
  end
  
  describe "pipeline execution" do
    test "executes pipeline with 80/20 optimization", %{socket: socket} do
      payload = %{
        "domain" => "cybersecurity",
        "optimization_strategy" => "skip_non_critical",
        "stages" => ["typer", "turtle", "ash", "reactor", "k8s"],
        "priority" => "high"
      }
      
      result = PipelineHandler.execute(payload, %{}, socket)
      
      assert {:reply, {:ok, %{execution_id: execution_id}}, _socket} = result
      assert is_binary(execution_id)
    end
    
    test "filters critical stages in 80/20 mode", %{socket: socket} do
      payload = %{
        "domain" => "cybersecurity",
        "optimization_strategy" => "skip_non_critical"
      }
      
      critical_stages = PipelineHandler.get_critical_stages("skip_non_critical")
      
      assert "typer" in critical_stages
      assert "turtle" in critical_stages
      assert "ash" in critical_stages
      assert "reactor" in critical_stages
      assert "k8s" in critical_stages
      
      # Non-critical stages should not be included
      refute "ttl2dspy" in critical_stages
      refute "erlang" in critical_stages
    end
    
    test "uses minimal path for healthcare domain", %{socket: socket} do
      payload = %{
        "domain" => "healthcare",
        "optimization_strategy" => "minimal_path"
      }
      
      minimal_stages = PipelineHandler.get_critical_stages("minimal_path")
      
      assert length(minimal_stages) == 3
      assert "typer" in minimal_stages
      assert "turtle" in minimal_stages
      assert "k8s" in minimal_stages
    end
    
    test "calculates estimated duration correctly", %{socket: socket} do
      stages = ["typer", "turtle", "ash", "reactor", "k8s"]
      
      # Skip non-critical strategy should be faster
      skip_duration = PipelineHandler.calculate_estimated_duration(stages, "skip_non_critical")
      full_duration = PipelineHandler.calculate_estimated_duration(stages, "full_pipeline")
      
      assert skip_duration < full_duration
      assert skip_duration > 0
    end
  end
  
  describe "pipeline optimization" do
    test "applies skip_non_critical optimization", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_123",
        "type" => "skip_non_critical"
      }
      
      result = PipelineHandler.optimize(payload, %{}, socket)
      
      assert {:reply, {:ok, optimization_result}, _socket} = result
      assert Map.has_key?(optimization_result, :stages_affected)
      assert Map.has_key?(optimization_result, :time_saved)
    end
    
    test "applies parallel_execution optimization", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_124",
        "type" => "parallel_execution"
      }
      
      result = PipelineHandler.optimize(payload, %{}, socket)
      
      assert {:reply, {:ok, optimization_result}, _socket} = result
      assert optimization_result.stages_affected > 0
    end
    
    test "creates optimization plan for parallel execution" do
      mock_execution = %{
        pending_stages: ["ash", "reactor", "ttl2dspy", "bitactor"]
      }
      
      {:ok, plan} = PipelineHandler.create_optimization_plan(mock_execution, "parallel_execution")
      
      assert plan.parallel_stages != []
      assert length(plan.parallel_stages) > 0
    end
    
    test "creates optimization plan for cached bypass" do
      mock_execution = %{
        pending_stages: ["ttl2dspy", "erlang", "ash", "reactor"]
      }
      
      {:ok, plan} = PipelineHandler.create_optimization_plan(mock_execution, "cached_bypass")
      
      assert "ttl2dspy" in plan.cache_stages
      assert "erlang" in plan.cache_stages
    end
  end
  
  describe "pipeline monitoring" do
    test "gets pipeline status with filtering", %{socket: socket} do
      payload = %{"execution_id" => "test_exec_125"}
      
      result = PipelineHandler.status(payload, %{}, socket)
      
      assert {:reply, {:ok, status}, _socket} = result
      assert Map.has_key?(status, :execution_id)
      assert Map.has_key?(status, :status)
    end
    
    test "filters status for 80/20 mode", %{socket: socket} do
      mock_status = %{
        execution_id: "test_exec_126",
        status: "running",
        progress: 0.6,
        stages: [
          %{name: "typer", status: "completed", critical: true},
          %{name: "ttl2dspy", status: "completed", critical: false},
          %{name: "ash", status: "running", critical: true},
          %{name: "erlang", status: "pending", critical: false}
        ],
        estimated_completion: DateTime.utc_now()
      }
      
      filtered_status = PipelineHandler.filter_critical_status(mock_status)
      
      # Should only include critical stages
      critical_stage_names = Enum.map(filtered_status.stages, & &1.name)
      assert "typer" in critical_stage_names
      assert "ash" in critical_stage_names
      refute "ttl2dspy" in critical_stage_names
      refute "erlang" in critical_stage_names
    end
    
    test "subscribes to execution monitoring", %{socket: socket} do
      payload = %{"execution_id" => "test_exec_127"}
      
      result = PipelineHandler.handle_monitor_event("subscribe", payload, socket)
      
      assert {:reply, {:ok, %{subscribed: true}}, _socket} = result
    end
  end
  
  describe "stage event handling" do
    test "handles stage start events", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_128",
        "stage" => "ash"
      }
      
      result = PipelineHandler.handle_stage_event("ash", "start", payload, socket)
      
      assert {:noreply, _socket} = result
      # Should broadcast stage started event
    end
    
    test "handles stage completion events", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_129", 
        "stage" => "reactor",
        "duration" => 150,
        "outputs" => [%{"type" => "reactor_workflow", "count" => 2}]
      }
      
      result = PipelineHandler.handle_stage_event("reactor", "complete", payload, socket)
      
      assert {:noreply, _socket} = result
    end
    
    test "handles stage errors gracefully", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_130",
        "stage" => "ash",
        "error" => "Resource validation failed"
      }
      
      result = PipelineHandler.handle_stage_event("ash", "error", payload, socket)
      
      assert {:noreply, _socket} = result
    end
    
    test "gets correct stage duration" do
      assert PipelineHandler.get_stage_duration("typer") == 50
      assert PipelineHandler.get_stage_duration("ash") == 150
      assert PipelineHandler.get_stage_duration("k8s") == 70
      assert PipelineHandler.get_stage_duration("unknown") == 50
    end
  end
  
  describe "metrics and performance" do
    test "handles performance metrics events", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_131"
      }
      
      result = PipelineHandler.handle_metrics_event("performance", payload, socket)
      
      assert {:reply, {:ok, metrics}, _socket} = result
      assert is_map(metrics)
    end
    
    test "handles optimization metrics events", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_132"
      }
      
      result = PipelineHandler.handle_metrics_event("optimization", payload, socket)
      
      assert {:reply, {:ok, metrics}, _socket} = result
    end
    
    test "handles stage timing metrics", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_133"
      }
      
      result = PipelineHandler.handle_metrics_event("stage_timing", payload, socket)
      
      assert {:reply, {:ok, metrics}, _socket} = result
    end
    
    test "tracks optimization metrics", %{socket: socket} do
      optimization_result = %{
        optimization_type: "skip_non_critical",
        stages_affected: ["ttl2dspy", "erlang"],
        time_saved: 180,
        efficiency_gain: 0.25,
        execution_id: "test_exec_134"
      }
      
      # Should execute without error
      PipelineHandler.track_optimization_metrics(optimization_result, socket)
    end
  end
  
  describe "event parsing and delegation" do
    test "parses stage events correctly" do
      assert {:stage_event, "ash", "start"} = 
        PipelineHandler.parse_pipeline_event("stage:ash:start")
      
      assert {:stage_event, "reactor", "complete"} = 
        PipelineHandler.parse_pipeline_event("stage:reactor:complete")
    end
    
    test "parses monitor events correctly" do
      assert {:monitor_event, "subscribe"} = 
        PipelineHandler.parse_pipeline_event("monitor:subscribe")
      
      assert {:monitor_event, "unsubscribe"} = 
        PipelineHandler.parse_pipeline_event("monitor:unsubscribe")
    end
    
    test "parses metrics events correctly" do
      assert {:metrics_event, "performance"} = 
        PipelineHandler.parse_pipeline_event("metrics:performance")
      
      assert {:metrics_event, "optimization"} = 
        PipelineHandler.parse_pipeline_event("metrics:optimization")
    end
    
    test "handles unknown events" do
      assert {:unknown, "unknown:event:format"} = 
        PipelineHandler.parse_pipeline_event("unknown:event:format")
    end
  end
  
  describe "delegated event handling" do
    test "handles delegated pipeline events", %{socket: socket} do
      result = PipelineHandler.handle_in("stage:ash:start", %{
        "execution_id" => "test_exec_135"
      }, %{}, socket)
      
      assert {:noreply, _socket} = result
    end
    
    test "handles delegated monitor events", %{socket: socket} do
      result = PipelineHandler.handle_in("monitor:subscribe", %{
        "execution_id" => "test_exec_136"
      }, %{}, socket)
      
      assert {:reply, {:ok, %{subscribed: true}}, _socket} = result
    end
    
    test "handles delegated metrics events", %{socket: socket} do
      result = PipelineHandler.handle_in("metrics:performance", %{
        "execution_id" => "test_exec_137"
      }, %{}, socket)
      
      assert {:reply, {:ok, _metrics}, _socket} = result
    end
  end
  
  describe "80/20 optimization validation" do
    test "demonstrates performance improvement with optimization" do
      # Test execution times for different strategies
      stages = ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"]
      
      full_duration = PipelineHandler.calculate_estimated_duration(stages, "full_pipeline")
      optimized_duration = PipelineHandler.calculate_estimated_duration(stages, "skip_non_critical")
      parallel_duration = PipelineHandler.calculate_estimated_duration(stages, "parallel_execution")
      
      # Optimized strategies should be faster
      assert optimized_duration < full_duration
      assert parallel_duration < full_duration
      
      # Should achieve at least 20% improvement
      improvement_ratio = (full_duration - optimized_duration) / full_duration
      assert improvement_ratio >= 0.2
    end
    
    test "maintains critical functionality in optimization", %{socket: socket} do
      # Execute with minimal optimization
      payload = %{
        "domain" => "cybersecurity",
        "optimization_strategy" => "minimal_path",
        "stages" => ["typer", "turtle", "k8s"]
      }
      
      result = PipelineHandler.execute(payload, %{}, socket)
      
      assert {:reply, {:ok, %{execution_id: _execution_id}}, _socket} = result
      
      # Should still execute successfully with minimal stages
    end
    
    test "applies domain-specific optimizations correctly" do
      # Cybersecurity domain should include more stages
      cybersec_stages = PipelineHandler.get_critical_stages("skip_non_critical")
      
      # Healthcare domain should be more minimal
      healthcare_stages = PipelineHandler.get_critical_stages("minimal_path")
      
      assert length(cybersec_stages) > length(healthcare_stages)
      
      # Both should include essential stages
      assert "typer" in cybersec_stages
      assert "typer" in healthcare_stages
      assert "k8s" in cybersec_stages  
      assert "k8s" in healthcare_stages
    end
    
    test "optimization preserves stage dependencies" do
      mock_execution = %{
        pending_stages: ["ash", "reactor", "k8s"]
      }
      
      {:ok, plan} = PipelineHandler.create_optimization_plan(mock_execution, "skip_non_critical")
      
      # Should not skip stages that other stages depend on
      # k8s depends on reactor and ash, so they should not be skipped
      assert plan.skip_stages == []
    end
  end
  
  describe "error handling and resilience" do
    test "handles execution errors gracefully", %{socket: socket} do
      payload = %{
        "domain" => "invalid_domain",
        "optimization_strategy" => "unknown_strategy"
      }
      
      result = PipelineHandler.execute(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason}}, _socket} = result
    end
    
    test "handles optimization errors", %{socket: socket} do
      payload = %{
        "execution_id" => "nonexistent_execution",
        "type" => "invalid_optimization"
      }
      
      result = PipelineHandler.optimize(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason}}, _socket} = result
    end
    
    test "handles status query for nonexistent execution", %{socket: socket} do
      payload = %{"execution_id" => "nonexistent_execution"}
      
      result = PipelineHandler.status(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason}}, _socket} = result
    end
  end
  
  describe "telemetry integration" do
    test "emits telemetry events for optimization" do
      # Set up telemetry handler
      test_pid = self()
      
      :telemetry.attach("test_pipeline_optimization", 
        [:cns, :pipeline, :optimization], 
        fn _event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, measurements, metadata})
        end, 
        nil
      )
      
      socket = socket(UserSocket, "user_id", %{
        current_user: %{id: "test_user"},
        connector_id: "test_connector"
      })
      
      optimization_result = %{
        optimization_type: "skip_non_critical",
        stages_affected: ["ttl2dspy"],
        time_saved: 100,
        efficiency_gain: 0.2,
        execution_id: "test_exec"
      }
      
      PipelineHandler.track_optimization_metrics(optimization_result, socket)
      
      # Should receive telemetry event
      assert_receive {:telemetry, measurements, metadata}, 1000
      
      assert measurements.time_saved_ms == 100
      assert metadata.connector_id == "test_connector"
      assert metadata.execution_id == "test_exec"
      
      :telemetry.detach("test_pipeline_optimization")
    end
  end
end