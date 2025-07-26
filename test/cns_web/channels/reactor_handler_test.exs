defmodule CnsWeb.Channels.ReactorHandlerTest do
  @moduledoc """
  Comprehensive unit tests for ReactorHandler with 80/20 optimization.
  Tests step execution, workflow management, and stage-specific handlers.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.Channels.ReactorHandler
  alias Cns.Swarm.{Reactor, Workflow, StepOptimizer}
  
  import Mox
  
  setup :verify_on_exit!
  
  setup do
    # Mock dependencies
    Mox.defmock(MockReactor, for: Reactor)
    Mox.defmock(MockWorkflow, for: Workflow)
    Mox.defmock(MockStepOptimizer, for: StepOptimizer)
    
    # Create test socket with 80/20 optimization
    socket = %Phoenix.Socket{
      assigns: %{
        swarm_id: "test-swarm-123",
        optimization_mode: "80_20",
        current_user: %{id: 1, role: "admin"},
        workflow_tasks: %{}
      }
    }
    
    {:ok, %{socket: socket}}
  end
  
  describe "execute_step/3 - Reactor Step Execution" do
    test "executes step with 80/20 optimization", %{socket: socket} do
      payload = %{
        "step" => "validate",
        "data" => %{"input" => "test_data"}
      }
      
      MockReactor
      |> expect(:execute_step, fn step_name, step_data, opts ->
        assert step_name == "validate"
        assert step_data == %{"input" => "test_data"}
        
        # Should include 80/20 optimization options
        assert opts[:skip_non_critical] == true
        assert opts[:parallel_execution] == true
        assert opts[:batch_size] == 20
        
        {:ok, %{
          step: "validate",
          duration: 250,
          output: %{"validated" => true, "errors" => []},
          metrics: %{cpu: 45, memory: 60}
        }}
      end)
      
      {:reply, {:ok, result}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
      
      assert result.step == "validate"
      assert result.duration == 250
      assert result.output["validated"] == true
    end
    
    test "executes step without optimization in full mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "full")}
      
      payload = %{
        "step" => "transform",
        "data" => %{"input" => "test_data"}
      }
      
      MockReactor
      |> expect(:execute_step, fn step_name, step_data, opts ->
        assert step_name == "transform"
        
        # Should not include 80/20 optimization options
        assert opts == []
        
        {:ok, %{
          step: "transform",
          duration: 500,
          output: %{"transformed" => true},
          metrics: %{cpu: 75, memory: 80}
        }}
      end)
      
      {:reply, {:ok, result}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
      
      assert result.step == "transform"
      assert result.duration == 500
    end
    
    test "handles step execution errors", %{socket: socket} do
      payload = %{
        "step" => "invalid_step",
        "data" => %{}
      }
      
      MockReactor
      |> expect(:execute_step, fn _step, _data, _opts ->
        {:error, "Step not found"}
      end)
      
      {:reply, {:error, error}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
      
      assert error.step == "invalid_step"
      assert error.reason == "Step not found"
    end
    
    test "broadcasts critical step completions", %{socket: socket} do
      payload = %{
        "step" => "validate",  # Critical step
        "data" => %{"input" => "test"}
      }
      
      MockReactor
      |> expect(:execute_step, fn _step, _data, _opts ->
        {:ok, %{
          step: "validate",
          duration: 200,
          output: %{"result" => "success"},
          metrics: %{}
        }}
      end)
      
      {:reply, {:ok, _result}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
      
      # Should broadcast critical step completion
      assert_broadcast "reactor:step:completed", broadcast_data
      assert broadcast_data.step == "validate"
      assert broadcast_data.duration == 200
    end
    
    test "skips broadcast for non-critical steps", %{socket: socket} do
      payload = %{
        "step" => "non_critical_step",
        "data" => %{"input" => "test"}
      }
      
      MockReactor
      |> expect(:execute_step, fn _step, _data, _opts ->
        {:ok, %{
          step: "non_critical_step",
          duration: 100,
          output: %{"result" => "success"},
          metrics: %{}
        }}
      end)
      
      {:reply, {:ok, _result}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
      
      # Should not broadcast non-critical step completion
      refute_broadcast "reactor:step:completed", _
    end
  end
  
  describe "step_status/3 - Step Status" do
    test "returns filtered status in 80/20 mode", %{socket: socket} do
      payload = %{"step" => "validate"}
      
      full_status = %{
        state: "running",
        progress: 75,
        duration: 1500,
        error_count: 2,
        throughput: 45,
        detailed_metrics: %{
          memory_usage: 65,
          cpu_usage: 70,
          network_io: 30,
          disk_io: 25
        }
      }
      
      MockReactor
      |> expect(:get_step_status, fn step_name ->
        assert step_name == "validate"
        {:ok, full_status}
      end)
      
      {:reply, {:ok, status}, _socket} = ReactorHandler.step_status(payload, %{}, socket)
      
      # Should filter to critical status info only
      assert status.state == "running"
      assert status.progress == 75
      assert status.critical_metrics.duration == 1500
      assert status.critical_metrics.error_count == 2
      assert status.critical_metrics.throughput == 45
      
      # Should not include detailed metrics
      refute Map.has_key?(status, :detailed_metrics)
    end
    
    test "returns full status in full mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "full")}
      
      payload = %{"step" => "transform"}
      
      full_status = %{
        state: "completed",
        progress: 100,
        detailed_metrics: %{cpu: 45, memory: 60}
      }
      
      MockReactor
      |> expect(:get_step_status, fn _step -> {:ok, full_status} end)
      
      {:reply, {:ok, status}, _socket} = ReactorHandler.step_status(payload, %{}, socket)
      
      # Should return full status
      assert status == full_status
    end
  end
  
  describe "create_workflow/3 - Workflow Creation" do
    test "creates optimized workflow in 80/20 mode", %{socket: socket} do
      workflow_def = %{
        "name" => "test-workflow",
        "steps" => [
          %{"name" => "step1", "critical" => true, "impact" => 90},
          %{"name" => "step2", "critical" => false, "impact" => 10},
          %{"name" => "step3", "critical" => true, "impact" => 85}
        ]
      }
      
      payload = %{"workflow" => workflow_def}
      
      MockWorkflow
      |> expect(:create, fn optimized_workflow ->
        # Should be optimized for 80/20
        assert optimized_workflow.optimization.mode == "80_20"
        assert optimized_workflow.optimization.skip_non_critical == true
        assert optimized_workflow.optimization.parallel_execution == true
        
        # Should only include critical steps
        critical_steps = Enum.filter(optimized_workflow.steps, & &1["critical"])
        assert length(critical_steps) == 2
        
        {:ok, %{
          id: "workflow-123",
          name: "test-workflow",
          optimization: optimized_workflow.optimization,
          steps: optimized_workflow.steps
        }}
      end)
      
      {:reply, {:ok, workflow}, _socket} = ReactorHandler.create_workflow(payload, %{}, socket)
      
      assert workflow.id == "workflow-123"
      assert workflow.name == "test-workflow"
      
      # Should broadcast workflow creation
      assert_broadcast "workflow:created", broadcast_data
      assert broadcast_data.id == "workflow-123"
      assert broadcast_data.optimized == true
    end
    
    test "creates unoptimized workflow in full mode", %{socket: socket} do
      socket = %{socket | assigns: Map.put(socket.assigns, :optimization_mode, "full")}
      
      workflow_def = %{
        "name" => "full-workflow",
        "steps" => [
          %{"name" => "step1", "critical" => true},
          %{"name" => "step2", "critical" => false}
        ]
      }
      
      payload = %{"workflow" => workflow_def}
      
      MockWorkflow
      |> expect(:create, fn unoptimized_workflow ->
        # Should not be optimized
        assert unoptimized_workflow == workflow_def
        
        {:ok, %{
          id: "workflow-456",
          name: "full-workflow",
          steps: unoptimized_workflow.steps
        }}
      end)
      
      {:reply, {:ok, workflow}, _socket} = ReactorHandler.create_workflow(payload, %{}, socket)
      
      assert workflow.id == "workflow-456"
      
      # Should broadcast with optimized: false
      assert_broadcast "workflow:created", broadcast_data
      assert broadcast_data.optimized == false
    end
  end
  
  describe "execute_workflow/3 - Async Workflow Execution" do
    test "starts async workflow execution", %{socket: socket} do
      payload = %{
        "workflow_id" => "workflow-123",
        "input" => %{"data" => "test"}
      }
      
      # Mock the task supervisor
      :meck.new(Task.Supervisor, [:passthrough])
      :meck.expect(Task.Supervisor, :async_nolink, fn _supervisor, _fun ->
        %Task{ref: make_ref(), pid: self()}
      end)
      
      MockWorkflow
      |> expect(:execute, fn workflow_id, input_data, opts ->
        assert workflow_id == "workflow-123"
        assert input_data == %{"data" => "test"}
        assert opts[:optimization_mode] == "80_20"
        
        {:ok, %{status: "completed", duration: 2000}}
      end)
      
      {:reply, {:ok, response}, updated_socket} = ReactorHandler.execute_workflow(payload, %{}, socket)
      
      assert response.workflow_id == "workflow-123"
      assert response.status == "started"
      
      # Should store task reference
      assert Map.has_key?(updated_socket.assigns.workflow_tasks, "workflow-123")
      
      :meck.unload(Task.Supervisor)
    end
  end
  
  describe "apply_optimization/3 - Reactor Optimization" do
    test "applies auto optimization", %{socket: socket} do
      payload = %{"type" => "auto"}
      
      MockStepOptimizer
      |> expect(:optimize_reactor, fn optimization_type ->
        assert optimization_type == "auto"
        
        {:ok, %{
          type: "auto",
          removed_steps: ["step2", "step4"],
          parallel_steps: ["step1", "step3"],
          expected_improvement: 65
        }}
      end)
      
      MockReactor
      |> expect(:apply_optimization, fn optimization ->
        assert optimization.type == "auto"
        :ok
      end)
      
      {:reply, {:ok, optimization}, _socket} = ReactorHandler.apply_optimization(payload, %{}, socket)
      
      assert optimization.type == "auto"
      assert optimization.expected_improvement == 65
      assert "step2" in optimization.removed_steps
      
      # Should broadcast optimization
      assert_broadcast "optimization:reactor:applied", broadcast_data
      assert broadcast_data.type == "auto"
      assert broadcast_data.expected_improvement == 65
    end
    
    test "handles optimization errors", %{socket: socket} do
      payload = %{"type" => "invalid"}
      
      MockStepOptimizer
      |> expect(:optimize_reactor, fn _type ->
        {:error, "Invalid optimization type"}
      end)
      
      {:reply, {:error, error}, _socket} = ReactorHandler.apply_optimization(payload, %{}, socket)
      
      assert error.reason == "Invalid optimization type"
    end
  end
  
  describe "analyze_optimization/3 - Optimization Analysis" do
    test "analyzes optimization opportunities", %{socket: socket} do
      MockStepOptimizer
      |> expect(:analyze_reactor, fn ->
        %{
          critical_path: ["step1", "step3", "step5"],
          bottlenecks: [
            %{step: "step3", latency: 500, impact: 80},
            %{step: "step1", latency: 300, impact: 60}
          ],
          removable_steps: ["step2", "step4"],
          parallel_candidates: ["step1", "step3"],
          time_saving_percentage: 45
        }
      end)
      
      {:reply, {:ok, analysis}, _socket} = ReactorHandler.analyze_optimization(%{}, %{}, socket)
      
      assert analysis.critical_path == ["step1", "step3", "step5"]
      assert length(analysis.bottlenecks) == 2  # Should limit to top 3
      assert analysis.removable_steps == ["step2", "step4"]
      assert analysis.potential_time_saving == "45%"
      
      # Should include recommendations
      assert is_list(analysis.recommendations)
      assert length(analysis.recommendations) <= 5
    end
  end
  
  describe "Stage Handlers" do
    test "K8sStageHandler handles deploy action", %{socket: socket} do
      payload = %{
        "services" => [
          %{"name" => "service1", "traffic_percentage" => 80},
          %{"name" => "service2", "traffic_percentage" => 5}
        ],
        "replicas" => 5
      }
      
      {:reply, {:ok, result}, _socket} = ReactorHandler.K8sStageHandler.handle_in(
        "deploy", payload, %{}, socket
      )
      
      assert result.status == "deployed"
      
      # In 80/20 mode, should filter services and optimize replicas
      if socket.assigns.optimization_mode == "80_20" do
        assert result.params["replicas"] == 4  # ceil(5 * 0.8)
      end
    end
    
    test "K8sStageHandler handles scaling", %{socket: socket} do
      payload = %{"max_replicas" => 20}
      
      {:reply, {:ok, result}, _socket} = ReactorHandler.K8sStageHandler.handle_in(
        "scale", payload, %{}, socket
      )
      
      assert result.status == "scaling"
      
      # Should optimize scaling params in 80/20 mode
      if socket.assigns.optimization_mode == "80_20" do
        assert result.params.target_cpu == 80
        assert result.params.scale_down_delay == 300
      end
    end
    
    test "AshStageHandler handles resource creation", %{socket: socket} do
      payload = %{
        "resource" => "User",
        "data" => %{
          "email" => "test@example.com",
          "name" => "Test User",
          "optional_field" => "value"
        }
      }
      
      MockAsh = Mox.defmock(MockAsh, for: Cns.Ash)
      
      MockAsh
      |> expect(:create_resource, fn resource, data, opts ->
        assert resource == "User"
        
        # In 80/20 mode, should only validate critical fields
        if socket.assigns.optimization_mode == "80_20" do
          assert opts[:validations] == [:required_fields, :type_check]
        end
        
        {:ok, %{id: 123, email: data["email"], name: data["name"]}}
      end)
      
      {:reply, {:ok, result}, _socket} = ReactorHandler.AshStageHandler.handle_in(
        "resource:create", payload, %{}, socket
      )
      
      assert result.id == 123
      assert result.email == "test@example.com"
    end
    
    test "AshStageHandler handles resource queries", %{socket: socket} do
      payload = %{
        "resource" => "User",
        "filter" => %{"active" => true},
        "options" => %{"limit" => 1000}
      }
      
      MockAsh = Mox.defmock(MockAsh, for: Cns.Ash)
      
      MockAsh
      |> expect(:query_resource, fn resource, filter, opts ->
        assert resource == "User"
        assert filter == %{"active" => true}
        
        # In 80/20 mode, should optimize query
        if socket.assigns.optimization_mode == "80_20" do
          assert opts[:select] == [:id, :email, :name, :active]
          assert opts[:limit] == 100
          assert opts[:preload] == []
        end
        
        {:ok, [%{id: 1, email: "user1@test.com"}]}
      end)
      
      {:reply, {:ok, results}, _socket} = ReactorHandler.AshStageHandler.handle_in(
        "resource:query", payload, %{}, socket
      )
      
      assert is_list(results)
      assert hd(results).id == 1
    end
  end
  
  describe "Workflow Optimization" do
    test "optimizes workflow steps for 80/20", %{socket: socket} do
      steps = [
        %{"name" => "step1", "critical" => true, "impact" => 90, "timeout" => 5000},
        %{"name" => "step2", "critical" => false, "impact" => 10, "timeout" => 3000},
        %{"name" => "step3", "critical" => true, "impact" => 85, "timeout" => 4000}
      ]
      
      optimized_steps = ReactorHandler.optimize_workflow_steps(steps)
      
      # Should only include critical steps
      assert length(optimized_steps) == 2
      
      critical_names = Enum.map(optimized_steps, & &1["name"])
      assert "step1" in critical_names
      assert "step3" in critical_names
      refute "step2" in critical_names
      
      # Should add optimization hints
      step1 = Enum.find(optimized_steps, &(&1["name"] == "step1"))
      assert step1["batch_size"] == 20
      assert step1["timeout"] == 10000  # Critical step gets 2x timeout
      assert step1["retry_strategy"] == "exponential_backoff"
    end
    
    test "calculates optimal timeouts", %{socket: socket} do
      critical_step = %{"timeout" => 5000, "critical" => true}
      normal_step = %{"timeout" => 3000, "critical" => false}
      
      critical_timeout = ReactorHandler.calculate_optimal_timeout(critical_step)
      normal_timeout = ReactorHandler.calculate_optimal_timeout(normal_step)
      
      assert critical_timeout == 10000  # 2x for critical
      assert normal_timeout == 3000     # Same for normal
    end
  end
  
  describe "Error Handling and Edge Cases" do
    test "handles missing step name", %{socket: socket} do
      payload = %{"data" => %{"input" => "test"}}  # Missing "step"
      
      assert_raise KeyError, fn ->
        ReactorHandler.execute_step(payload, %{}, socket)
      end
    end
    
    test "handles invalid workflow definition", %{socket: socket} do
      payload = %{
        "workflow" => %{
          "name" => "invalid-workflow"
          # Missing "steps"
        }
      }
      
      MockWorkflow
      |> expect(:create, fn _workflow ->
        {:error, "Invalid workflow definition"}
      end)
      
      {:reply, {:error, error}, _socket} = ReactorHandler.create_workflow(payload, %{}, socket)
      
      assert error.reason == "Invalid workflow definition"
    end
    
    test "handles reactor execution with no optimization mode", %{socket: socket} do
      socket = %{socket | assigns: Map.delete(socket.assigns, :optimization_mode)}
      
      payload = %{"step" => "test", "data" => %{}}
      
      MockReactor
      |> expect(:execute_step, fn _step, _data, opts ->
        # Should have empty options when no optimization mode
        assert opts == []
        {:ok, %{step: "test", duration: 100, output: %{}, metrics: %{}}}
      end)
      
      {:reply, {:ok, _result}, _socket} = ReactorHandler.execute_step(payload, %{}, socket)
    end
    
    test "handles workflow task cleanup on error", %{socket: socket} do
      socket = %{socket | assigns: %{socket.assigns | workflow_tasks: %{"workflow-1" => %Task{}}}}
      
      # Should handle gracefully when workflow tasks exist
      payload = %{"workflow_id" => "workflow-2", "input" => %{}}
      
      :meck.new(Task.Supervisor, [:passthrough])
      :meck.expect(Task.Supervisor, :async_nolink, fn _supervisor, _fun ->
        %Task{ref: make_ref(), pid: self()}
      end)
      
      MockWorkflow
      |> expect(:execute, fn _id, _input, _opts ->
        {:ok, %{status: "completed"}}
      end)
      
      {:reply, {:ok, _response}, updated_socket} = ReactorHandler.execute_workflow(payload, %{}, socket)
      
      # Should add new task while preserving existing ones
      assert Map.has_key?(updated_socket.assigns.workflow_tasks, "workflow-1")
      assert Map.has_key?(updated_socket.assigns.workflow_tasks, "workflow-2")
      
      :meck.unload(Task.Supervisor)
    end
  end
end