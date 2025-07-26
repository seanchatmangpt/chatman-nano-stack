defmodule CnsWeb.Channels.ReactorHandlerTest do
  @moduledoc """
  Unit tests for ReactorHandler with 80/20 optimization validation.
  Tests Ash Reactor workflow management, step execution, and optimization.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: true
  
  alias CnsWeb.Channels.ReactorHandler
  alias CnsWeb.UserSocket
  
  setup do
    socket = socket(UserSocket, "user_id", %{
      current_user: %{id: "test_user", role: "operator"},
      optimization_mode: "80_20",
      reactor_preferences: %{
        parallel_execution: true,
        step_timeout: 30000
      }
    })
    
    %{socket: socket}
  end
  
  describe "workflow creation" do
    test "creates reactor workflow with optimization", %{socket: socket} do
      payload = %{
        "name" => "cybersecurity_workflow",
        "steps" => [
          %{
            "name" => "parse_input",
            "type" => "ash_action",
            "critical" => true
          },
          %{
            "name" => "generate_resources", 
            "type" => "ash_create",
            "critical" => true
          },
          %{
            "name" => "validate_output",
            "type" => "ash_validate",
            "critical" => false
          }
        ],
        "optimization_mode" => "80_20"
      }
      
      result = ReactorHandler.create_workflow(payload, %{}, socket)
      
      assert {:reply, {:ok, %{workflow_id: workflow_id}}, _socket} = result
      assert is_binary(workflow_id)
    end
    
    test "optimizes workflow steps based on 80/20 principle", %{socket: socket} do
      payload = %{
        "name" => "optimized_workflow",
        "steps" => [
          %{"name" => "critical_step_1", "type" => "ash_action", "critical" => true},
          %{"name" => "critical_step_2", "type" => "ash_create", "critical" => true},
          %{"name" => "optional_step_1", "type" => "ash_validate", "critical" => false},
          %{"name" => "optional_step_2", "type" => "ash_destroy", "critical" => false}
        ],
        "optimization_mode" => "80_20"
      }
      
      result = ReactorHandler.optimize_workflow(payload, %{}, socket)
      
      assert {:reply, {:ok, optimized_workflow}, _socket} = result
      
      # Should prioritize critical steps
      critical_steps = Enum.filter(optimized_workflow.steps, & &1.critical)
      assert length(critical_steps) >= 2
      
      # Should have optimized execution order
      assert optimized_workflow.execution_strategy == "critical_first"
    end
    
    test "validates workflow structure", %{socket: socket} do
      invalid_payload = %{
        "name" => "",
        "steps" => []
      }
      
      result = ReactorHandler.validate_workflow(invalid_payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason}}, _socket} = result
    end
  end
  
  describe "workflow execution" do
    test "executes workflow with step monitoring", %{socket: socket} do
      payload = %{
        "workflow_id" => "test_workflow_123",
        "execution_mode" => "optimized",
        "inputs" => %{
          "domain" => "cybersecurity",
          "resource_count" => 5
        }
      }
      
      result = ReactorHandler.execute_workflow(payload, %{}, socket)
      
      assert {:reply, {:ok, %{execution_id: execution_id}}, _socket} = result
      assert is_binary(execution_id)
    end
    
    test "executes individual step with progress tracking", %{socket: socket} do
      payload = %{
        "step_name" => "generate_ash_resource",
        "step_type" => "ash_create",
        "inputs" => %{
          "name" => "CybersecurityAlert",
          "attributes" => %{
            "severity" => "high",
            "type" => "intrusion_detection"
          }
        },
        "workflow_id" => "test_workflow"
      }
      
      result = ReactorHandler.execute_step(payload, %{}, socket)
      
      assert {:reply, {:ok, %{step_completed: true}}, _socket} = result
    end
    
    test "handles step failure and recovery", %{socket: socket} do
      payload = %{
        "step_name" => "failing_step",
        "step_type" => "ash_action",
        "simulate_failure" => true,
        "workflow_id" => "test_workflow"
      }
      
      result = ReactorHandler.execute_step(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason, retry_available: true}}, _socket} = result
    end
    
    test "supports parallel step execution", %{socket: socket} do
      payload = %{
        "workflow_id" => "parallel_workflow",
        "parallel_steps" => [
          %{"name" => "step_1", "type" => "ash_create"},
          %{"name" => "step_2", "type" => "ash_create"},
          %{"name" => "step_3", "type" => "ash_validate"}
        ]
      }
      
      result = ReactorHandler.execute_parallel_steps(payload, %{}, socket)
      
      assert {:reply, {:ok, %{parallel_execution_id: execution_id}}, _socket} = result
      assert is_binary(execution_id)
    end
  end
  
  describe "ash resource management" do
    test "generates ash resources from reactor steps", %{socket: socket} do
      payload = %{
        "resource_name" => "ThreatDetection",
        "attributes" => [
          %{"name" => "threat_level", "type" => "atom", "values" => ["low", "medium", "high"]},
          %{"name" => "source_ip", "type" => "string"},
          %{"name" => "timestamp", "type" => "datetime"}
        ],
        "actions" => [
          %{"name" => "create", "type" => "create"},
          %{"name" => "update_threat_level", "type" => "update"}
        ]
      }
      
      result = ReactorHandler.generate_ash_resource(payload, %{}, socket)
      
      assert {:reply, {:ok, %{resource_generated: true}}, _socket} = result
    end
    
    test "validates ash resource configuration", %{socket: socket} do
      payload = %{
        "resource_name" => "TestResource",
        "validation_rules" => [
          %{"field" => "name", "rule" => "required"},
          %{"field" => "email", "rule" => "format", "pattern" => "email"}
        ]
      }
      
      result = ReactorHandler.validate_ash_resource(payload, %{}, socket)
      
      assert {:reply, {:ok, %{validation_passed: true}}, _socket} = result
    end
    
    test "applies resource optimizations", %{socket: socket} do
      payload = %{
        "resource_name" => "PerformanceOptimizedResource",
        "optimization_settings" => %{
          "eager_loading" => true,
          "connection_pooling" => true,
          "caching_strategy" => "aggressive"
        }
      }
      
      result = ReactorHandler.optimize_ash_resource(payload, %{}, socket)
      
      assert {:reply, {:ok, %{optimizations_applied: true}}, _socket} = result
    end
  end
  
  describe "workflow monitoring" do
    test "monitors workflow execution status", %{socket: socket} do
      payload = %{
        "workflow_id" => "monitored_workflow_456"
      }
      
      result = ReactorHandler.get_workflow_status(payload, %{}, socket)
      
      assert {:reply, {:ok, status}, _socket} = result
      assert Map.has_key?(status, :workflow_id)
      assert Map.has_key?(status, :status)
      assert Map.has_key?(status, :progress)
    end
    
    test "tracks step-level metrics", %{socket: socket} do
      payload = %{
        "execution_id" => "test_execution_789"
      }
      
      result = ReactorHandler.get_step_metrics(payload, %{}, socket)
      
      assert {:reply, {:ok, metrics}, _socket} = result
      assert Map.has_key?(metrics, :total_steps)
      assert Map.has_key?(metrics, :completed_steps) 
      assert Map.has_key?(metrics, :failed_steps)
      assert Map.has_key?(metrics, :average_duration)
    end
    
    test "generates workflow performance report", %{socket: socket} do
      payload = %{
        "workflow_id" => "report_workflow",
        "time_range" => "24h"
      }
      
      result = ReactorHandler.generate_performance_report(payload, %{}, socket)
      
      assert {:reply, {:ok, report}, _socket} = result
      assert Map.has_key?(report, :total_executions)
      assert Map.has_key?(report, :success_rate)
      assert Map.has_key?(report, :average_duration)
      assert Map.has_key?(report, :optimization_gains)
    end
  end
  
  describe "80/20 workflow optimization" do
    test "identifies critical workflow paths", %{socket: socket} do
      payload = %{
        "workflow_id" => "analysis_workflow",
        "execution_history" => [
          %{"step" => "parse_input", "frequency" => 95, "success_rate" => 0.98},
          %{"step" => "validate_data", "frequency" => 85, "success_rate" => 0.92},
          %{"step" => "optional_enrichment", "frequency" => 20, "success_rate" => 0.75}
        ]
      }
      
      result = ReactorHandler.identify_critical_paths(payload, %{}, socket)
      
      assert {:reply, {:ok, analysis}, _socket} = result
      assert length(analysis.critical_steps) >= 2
      assert analysis.optimization_potential > 0
    end
    
    test "applies 80/20 optimization to workflow", %{socket: socket} do
      payload = %{
        "workflow_id" => "optimization_target",
        "optimization_strategy" => "skip_non_critical"
      }
      
      result = ReactorHandler.apply_80_20_optimization(payload, %{}, socket)
      
      assert {:reply, {:ok, optimization_result}, _socket} = result
      assert optimization_result.steps_optimized > 0
      assert optimization_result.estimated_time_savings > 0
    end
    
    test "measures optimization effectiveness", %{socket: socket} do
      # Baseline execution
      baseline_payload = %{
        "workflow_id" => "effectiveness_test",
        "optimization_mode" => "full"
      }
      
      ReactorHandler.execute_workflow(baseline_payload, %{}, socket)
      
      # Optimized execution
      optimized_payload = %{
        "workflow_id" => "effectiveness_test", 
        "optimization_mode" => "80_20"
      }
      
      result = ReactorHandler.measure_optimization_effectiveness(optimized_payload, %{}, socket)
      
      assert {:reply, {:ok, effectiveness}, _socket} = result
      assert effectiveness.performance_improvement >= 0.2 # 20% improvement
    end
  end
  
  describe "dynamic step generation" do
    test "generates steps based on domain requirements", %{socket: socket} do
      payload = %{
        "domain" => "cybersecurity",
        "requirements" => [
          "threat_detection",
          "incident_response", 
          "compliance_reporting"
        ]
      }
      
      result = ReactorHandler.generate_domain_steps(payload, %{}, socket)
      
      assert {:reply, {:ok, %{steps_generated: steps}}, _socket} = result
      assert length(steps) >= 3
      
      # Should include domain-specific steps
      step_names = Enum.map(steps, & &1.name)
      assert Enum.any?(step_names, &String.contains?(&1, "threat"))
    end
    
    test "adapts steps based on execution feedback", %{socket: socket} do
      payload = %{
        "workflow_id" => "adaptive_workflow",
        "feedback" => %{
          "step_performance" => %{
            "slow_step" => %{"avg_duration" => 5000, "timeout_rate" => 0.1},
            "fast_step" => %{"avg_duration" => 100, "timeout_rate" => 0.0}
          }
        }
      }
      
      result = ReactorHandler.adapt_workflow_steps(payload, %{}, socket)
      
      assert {:reply, {:ok, %{adaptations_applied: true}}, _socket} = result
    end
  end
  
  describe "integration with ash framework" do
    test "creates ash domain resources", %{socket: socket} do
      payload = %{
        "domain_name" => "CybersecurityDomain",
        "resources" => [
          %{"name" => "Alert", "type" => "resource"},
          %{"name" => "Threat", "type" => "resource"},
          %{"name" => "Asset", "type" => "resource"}
        ]
      }
      
      result = ReactorHandler.create_ash_domain(payload, %{}, socket)
      
      assert {:reply, {:ok, %{domain_created: true}}, _socket} = result
    end
    
    test "executes ash actions through reactor", %{socket: socket} do
      payload = %{
        "resource" => "CybersecurityAlert",
        "action" => "create",
        "inputs" => %{
          "severity" => "high",
          "description" => "Suspicious network activity"
        }
      }
      
      result = ReactorHandler.execute_ash_action(payload, %{}, socket)
      
      assert {:reply, {:ok, %{action_executed: true}}, _socket} = result
    end
    
    test "handles ash changeset validation", %{socket: socket} do
      payload = %{
        "resource" => "ThreatDetection",
        "changeset" => %{
          "threat_level" => "critical",
          "source_ip" => "192.168.1.100",
          "invalid_field" => "should_fail"
        }
      }
      
      result = ReactorHandler.validate_ash_changeset(payload, %{}, socket)
      
      # Should handle validation errors gracefully
      assert {:reply, response, _socket} = result
      assert elem(response, 0) in [:ok, :error]
    end
  end
  
  describe "error handling and resilience" do
    test "handles step execution timeouts", %{socket: socket} do
      payload = %{
        "step_name" => "long_running_step",
        "timeout_ms" => 100,
        "simulate_timeout" => true
      }
      
      result = ReactorHandler.execute_step_with_timeout(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: "timeout"}}, _socket} = result
    end
    
    test "implements step retry logic", %{socket: socket} do
      payload = %{
        "step_name" => "flaky_step",
        "max_retries" => 3,
        "retry_delay_ms" => 100
      }
      
      result = ReactorHandler.execute_step_with_retry(payload, %{}, socket)
      
      # Should attempt retries on failure
      assert {:reply, response, _socket} = result
      assert elem(response, 0) in [:ok, :error]
    end
    
    test "handles workflow rollback", %{socket: socket} do
      payload = %{
        "workflow_id" => "rollback_test",
        "execution_id" => "failed_execution",
        "rollback_strategy" => "compensating_actions"
      }
      
      result = ReactorHandler.rollback_workflow(payload, %{}, socket)
      
      assert {:reply, {:ok, %{rollback_completed: true}}, _socket} = result
    end
  end
  
  describe "telemetry integration" do
    test "emits telemetry events for workflow execution", %{socket: socket} do
      # Set up telemetry handler
      test_pid = self()
      
      :telemetry.attach("test_reactor_workflow",
        [:cns, :reactor, :workflow, :executed],
        fn _event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, measurements, metadata})
        end,
        nil
      )
      
      ReactorHandler.execute_workflow(%{
        "workflow_id" => "telemetry_test",
        "inputs" => %{}
      }, %{}, socket)
      
      # Should receive telemetry event
      assert_receive {:telemetry, measurements, metadata}, 1000
      
      assert measurements.execution_time_ms > 0
      assert metadata.workflow_id == "telemetry_test"
      
      :telemetry.detach("test_reactor_workflow")
    end
  end
end