defmodule CnsWeb.ErrorInjectionTest do
  @moduledoc """
  Adversarial Error Injection Tests for Ultrathink Swarm 80/20 System
  Tests system resilience, fault tolerance, and graceful degradation
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  
  describe "network failure simulation" do
    test "handles WebSocket connection drops gracefully" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:connection_test")
      
      # Start pipeline execution
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "turtle", "ash"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Simulate connection drop
      leave(socket)
      
      # System should handle disconnection gracefully
      # Pipeline should continue or fail gracefully
      
      # Reconnect and check status
      {:ok, _, new_socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:reconnection_test")
      
      ref = push(new_socket, "pipeline:status", %{"execution_id" => execution_id})
      
      # Should either show completed, failed, or recovery status
      assert_reply ref, :ok, %{status: status}
      assert status in ["completed", "failed", "recovering", "not_found"]
    end
    
    test "handles intermittent connectivity issues" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:intermittent_test")
      
      # Simulate intermittent connection by rapid disconnect/reconnect
      for _i <- 1..5 do
        leave(socket)
        
        {:ok, _, socket} =
          UserSocket
          |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
          |> subscribe_and_join(SwarmChannel, "swarm:intermittent_test")
        
        Process.sleep(100)
      end
      
      # System should remain stable
      ref = push(socket, "swarm:health_check", %{})
      assert_reply ref, :ok, %{status: "healthy"}
    end
    
    test "handles message loss and duplication" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:message_reliability_test")
      
      # Send duplicate messages
      duplicate_payload = %{
        "execution_id" => "duplicate_test_123",
        "operation" => "start_pipeline",
        "idempotency_key" => "unique_key_456"
      }
      
      ref1 = push(socket, "pipeline:execute", duplicate_payload)
      ref2 = push(socket, "pipeline:execute", duplicate_payload)
      ref3 = push(socket, "pipeline:execute", duplicate_payload)
      
      # Should handle duplicates gracefully
      assert_reply ref1, :ok, %{execution_id: execution_id1}
      assert_reply ref2, :ok, %{execution_id: execution_id2}
      assert_reply ref3, :ok, %{execution_id: execution_id3}
      
      # Should be same execution or properly deduplicated
      assert execution_id1 == execution_id2
      assert execution_id2 == execution_id3
    end
  end
  
  describe "resource exhaustion scenarios" do
    test "handles memory pressure gracefully" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:memory_pressure_test")
      
      # Simulate memory pressure by requesting large allocations
      memory_intensive_payload = %{
        "operation" => "memory_intensive_task",
        "data_size" => "100MB",
        "concurrent_requests" => 50
      }
      
      ref = push(socket, "system:simulate_pressure", memory_intensive_payload)
      
      # Should either complete with degraded performance or fail gracefully
      assert_reply ref, response
      assert elem(response, 0) in [:ok, :error]
      
      if elem(response, 0) == :error do
        assert elem(response, 1).reason == "Resource exhaustion"
      end
    end
    
    test "handles CPU throttling under load" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:cpu_throttle_test")
      
      # Start multiple concurrent executions
      tasks = for i <- 1..20 do
        Task.async(fn ->
          ref = push(socket, "pipeline:execute", %{
            "execution_id" => "cpu_test_#{i}",
            "stages" => ["typer", "turtle", "ash", "reactor"],
            "optimization_strategy" => "parallel_execution"
          })
          
          receive do
            {:reply, _ref, response, _socket} -> response
          after
            10000 -> {:error, %{reason: "timeout"}}
          end
        end)
      end
      
      results = Task.await_many(tasks, 15000)
      
      # Some should succeed, some may fail gracefully
      successes = Enum.count(results, &match?({:ok, _}, &1))
      failures = Enum.count(results, &match?({:error, _}, &1))
      
      # Should handle load without complete failure
      assert successes > 0
      
      # Failures should be graceful with appropriate error messages
      graceful_failures = Enum.count(results, fn
        {:error, %{reason: reason}} when reason in ["Resource unavailable", "System overloaded", "Rate limit exceeded"] -> true
        _ -> false
      end)
      
      assert graceful_failures == failures
    end
    
    test "handles disk space exhaustion" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:disk_space_test")
      
      # Simulate disk space exhaustion
      ref = push(socket, "system:simulate_disk_full", %{
        "write_intensive_operation" => true,
        "large_log_generation" => true
      })
      
      assert_reply ref, response
      
      if elem(response, 0) == :error do
        assert elem(response, 1).reason == "Disk space exhausted"
      end
      
      # System should recover gracefully
      ref = push(socket, "system:health_check", %{})
      assert_reply ref, :ok, %{status: status}
      assert status in ["healthy", "degraded", "recovering"]
    end
  end
  
  describe "database failure scenarios" do
    test "handles database connection failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:db_failure_test")
      
      # Simulate database failure
      ref = push(socket, "system:simulate_db_failure", %{
        "failure_type" => "connection_lost",
        "duration" => 5000
      })
      
      assert_reply ref, :ok, %{simulation_started: true}
      
      # Try operations during database failure
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "ash"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      # Should either queue operation or fail gracefully
      assert_reply ref, response
      
      case response do
        {:ok, %{queued: true}} -> :ok
        {:error, %{reason: "Database unavailable"}} -> :ok
        _ -> flunk("Unexpected response during database failure")
      end
    end
    
    test "handles database corruption scenarios" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:db_corruption_test")
      
      # Simulate data corruption
      ref = push(socket, "system:simulate_corruption", %{
        "corruption_type" => "partial_data_loss",
        "affected_tables" => ["executions", "metrics"]
      })
      
      assert_reply ref, :ok, %{corruption_simulated: true}
      
      # System should detect and handle corruption
      ref = push(socket, "pipeline:status", %{
        "execution_id" => "potentially_corrupted_execution"
      })
      
      assert_reply ref, response
      
      case response do
        {:ok, %{status: "data_recovery_needed"}} -> :ok
        {:error, %{reason: "Data integrity error"}} -> :ok
        _ -> flunk("Should detect data corruption")
      end
    end
    
    test "handles transaction deadlocks" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:deadlock_test")
      
      # Create conditions for deadlock
      deadlock_payload = %{
        "concurrent_operations" => [
          %{"operation" => "update_metrics", "resource" => "execution_1"},
          %{"operation" => "update_status", "resource" => "execution_2"}
        ],
        "create_deadlock" => true
      }
      
      ref = push(socket, "system:simulate_deadlock", deadlock_payload)
      
      # Should resolve deadlock gracefully
      assert_reply ref, response, 10000
      
      case response do
        {:ok, %{deadlock_resolved: true}} -> :ok
        {:error, %{reason: "Transaction deadlock", retry_available: true}} -> :ok
        _ -> flunk("Should handle deadlock gracefully")
      end
    end
  end
  
  describe "component failure injection" do
    test "handles ash resource generation failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:ash_failure_test")
      
      # Inject failure in ash stage
      failure_payload = %{
        "stage" => "ash",
        "failure_type" => "resource_generation_error",
        "failure_probability" => 1.0
      }
      
      ref = push(socket, "system:inject_failure", failure_payload)
      assert_reply ref, :ok, %{failure_injected: true}
      
      # Execute pipeline with ash failure
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "ash", "reactor"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should receive failure notification
      assert_broadcast "pipeline:stage_failed", %{
        execution_id: ^execution_id,
        stage: "ash",
        error: error,
        recovery_attempted: true
      }
      
      assert String.contains?(error, "resource_generation_error")
    end
    
    test "handles reactor workflow failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:reactor_failure_test")
      
      # Create workflow with injected failure
      workflow_payload = %{
        "name" => "failure_test_workflow",
        "steps" => [
          %{"name" => "failing_step", "type" => "ash_action", "inject_failure" => true},
          %{"name" => "recovery_step", "type" => "ash_validate", "critical" => false}
        ]
      }
      
      ref = push(socket, "reactor:workflow:create", workflow_payload)
      assert_reply ref, :ok, %{workflow_id: workflow_id}
      
      # Execute workflow
      ref = push(socket, "reactor:workflow:execute", %{
        "workflow_id" => workflow_id
      })
      
      # Should handle step failure gracefully
      assert_reply ref, response
      
      case response do
        {:ok, %{completed_with_failures: true}} -> :ok
        {:error, %{reason: "Workflow execution failed", partial_results: _}} -> :ok
        _ -> flunk("Should handle workflow failure gracefully")
      end
    end
    
    test "handles k8s deployment failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:k8s_failure_test")
      
      # Simulate k8s cluster unavailability
      k8s_failure_payload = %{
        "stage" => "k8s",
        "failure_type" => "cluster_unreachable",
        "error_message" => "connection refused"
      }
      
      ref = push(socket, "system:inject_failure", k8s_failure_payload)
      assert_reply ref, :ok, %{failure_injected: true}
      
      # Execute pipeline with k8s failure
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "ash", "k8s"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should attempt recovery or graceful degradation
      assert_broadcast "pipeline:stage_failed", %{
        execution_id: ^execution_id,
        stage: "k8s",
        recovery_strategy: recovery_strategy
      }
      
      assert recovery_strategy in ["retry", "skip", "fallback"]
    end
  end
  
  describe "notification system failures" do
    test "handles notification channel failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:notification_failure_test")
      
      # Inject failure in notification system
      ref = push(socket, "system:inject_notification_failure", %{
        "channel" => "error_alerts",
        "failure_type" => "delivery_failure"
      })
      
      assert_reply ref, :ok, %{failure_injected: true}
      
      # Try to send critical notification
      ref = push(socket, "notifications:send", %{
        "level" => "critical",
        "channel" => "error_alerts",
        "message" => "Critical system alert"
      })
      
      # Should handle delivery failure gracefully
      assert_reply ref, response
      
      case response do
        {:ok, %{queued_for_retry: true}} -> :ok
        {:error, %{reason: "Notification delivery failed", will_retry: true}} -> :ok
        _ -> flunk("Should handle notification failure gracefully")
      end
    end
    
    test "handles notification queue overflow" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:notification_overflow_test")
      
      # Fill notification queue to capacity
      for i <- 1..1000 do
        push(socket, "notifications:send", %{
          "level" => "info",
          "channel" => "performance_metrics",
          "message" => "Queue overflow test #{i}"
        })
      end
      
      # Additional notification should handle overflow
      ref = push(socket, "notifications:send", %{
        "level" => "critical",
        "channel" => "error_alerts",
        "message" => "Critical alert during overflow"
      })
      
      # Critical notifications should still be processed
      assert_reply ref, :ok
    end
  end
  
  describe "swarm intelligence failures" do
    test "handles learning model corruption" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:learning_failure_test")
      
      # Corrupt learning model
      ref = push(socket, "system:corrupt_learning_model", %{
        "corruption_type" => "weight_corruption",
        "severity" => "high"
      })
      
      assert_reply ref, :ok, %{model_corrupted: true}
      
      # Try to use swarm intelligence
      ref = push(socket, "swarm:optimize", %{
        "target" => "pipeline",
        "strategy" => "80_20"
      })
      
      # Should fall back to rule-based optimization
      assert_reply ref, :ok, %{
        fallback_mode: true,
        reason: "Learning model unavailable"
      }
    end
    
    test "handles recommendation generation failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:recommendation_failure_test")
      
      # Inject failure in recommendation engine
      ref = push(socket, "system:inject_recommendation_failure", %{
        "failure_type" => "algorithm_error",
        "error_rate" => 1.0
      })
      
      assert_reply ref, :ok, %{failure_injected: true}
      
      # Request recommendations
      ref = push(socket, "swarm:recommendations", %{
        "pipeline_state" => %{"active_stages" => ["ash"]},
        "metrics" => %{"avg_duration" => 400}
      })
      
      # Should provide default recommendations or error gracefully
      assert_reply ref, response
      
      case response do
        {:ok, %{recommendations: [], fallback_used: true}} -> :ok
        {:error, %{reason: "Recommendation engine unavailable"}} -> :ok
        _ -> flunk("Should handle recommendation failure gracefully")
      end
    end
  end
  
  describe "cascading failure scenarios" do
    test "handles multiple simultaneous component failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:cascading_failure_test")
      
      # Inject multiple failures simultaneously
      failures = [
        %{"component" => "database", "failure_type" => "connection_timeout"},
        %{"component" => "notification_system", "failure_type" => "queue_overflow"},
        %{"component" => "learning_model", "failure_type" => "corruption"},
        %{"component" => "k8s_cluster", "failure_type" => "unreachable"}
      ]
      
      ref = push(socket, "system:inject_cascading_failures", %{
        "failures" => failures,
        "propagation_delay" => 100
      })
      
      assert_reply ref, :ok, %{cascading_failures_injected: true}
      
      # System should maintain core functionality
      ref = push(socket, "system:health_check", %{})
      assert_reply ref, :ok, %{
        status: status,
        degraded_components: degraded,
        core_functionality: core_status
      }
      
      assert status in ["degraded", "critical", "emergency"]
      assert is_list(degraded)
      assert core_status in ["operational", "limited"]
    end
    
    test "validates system recovery after cascading failures" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:recovery_test")
      
      # Inject and then clear failures
      ref = push(socket, "system:inject_cascading_failures", %{
        "failures" => [
          %{"component" => "ash", "failure_type" => "temporary_error"}
        ]
      })
      
      assert_reply ref, :ok
      
      # Clear failures and trigger recovery
      ref = push(socket, "system:clear_failures", %{})
      assert_reply ref, :ok, %{failures_cleared: true}
      
      # Wait for recovery
      Process.sleep(2000)
      
      # Verify system recovery
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "ash"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      assert_reply ref, :ok, %{execution_id: _execution_id}
    end
  end
  
  describe "timeout and hanging scenarios" do
    test "handles stage execution timeouts" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:timeout_test")
      
      # Configure stage to hang
      ref = push(socket, "system:configure_hang", %{
        "stage" => "reactor",
        "hang_duration" => 30000,  # 30 seconds
        "timeout_threshold" => 5000  # 5 seconds
      })
      
      assert_reply ref, :ok, %{hang_configured: true}
      
      # Execute pipeline with hanging stage
      ref = push(socket, "pipeline:execute", %{
        "stages" => ["typer", "reactor"],
        "optimization_strategy" => "skip_non_critical"
      })
      
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should receive timeout notification
      assert_broadcast "pipeline:stage_timeout", %{
        execution_id: ^execution_id,
        stage: "reactor",
        timeout_action: action
      }, 10000
      
      assert action in ["kill", "skip", "retry"]
    end
    
    test "handles infinite loops in processing" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{current_user: %{id: "test_user", role: "operator"}})
        |> subscribe_and_join(SwarmChannel, "swarm:infinite_loop_test")
      
      # Inject infinite loop condition
      ref = push(socket, "system:inject_infinite_loop", %{
        "component" => "swarm_intelligence",
        "loop_type" => "recommendation_generation"
      })
      
      assert_reply ref, :ok, %{infinite_loop_injected: true}
      
      # Request operation that would trigger loop
      ref = push(socket, "swarm:recommendations", %{
        "trigger_loop" => true
      })
      
      # Should timeout and break the loop
      assert_reply ref, :error, %{
        reason: "Operation timeout",
        loop_detected: true
      }, 10000
    end
  end
end