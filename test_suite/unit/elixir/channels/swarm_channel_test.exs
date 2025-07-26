defmodule CnsWeb.SwarmChannelTest do
  @moduledoc """
  Unit tests for SwarmChannel with 80/20 optimization validation.
  Tests channel routing, authentication, and optimization features.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: true
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Phoenix.ChannelTest
  
  @default_payload %{
    "optimization" => "80_20",
    "domain" => "cybersecurity"
  }
  
  setup do
    {:ok, _, socket} =
      UserSocket
      |> socket("user_id", %{
        current_user: %{id: "test_user", role: "operator"},
        optimization_mode: "80_20"
      })
      |> subscribe_and_join(SwarmChannel, "swarm:test_pipeline", @default_payload)
    
    %{socket: socket}
  end
  
  describe "channel join" do
    test "joins with valid swarm topic", %{socket: socket} do
      assert socket.assigns.swarm_id == "test_pipeline"
      assert socket.assigns.optimization_mode == "80_20"
    end
    
    test "sets critical stages for cybersecurity domain", %{socket: socket} do
      expected_critical = ["k8s", "reactor", "ash", "turtle", "typer"]
      assert socket.assigns.critical_stages == expected_critical
    end
    
    test "tracks presence on join", %{socket: socket} do
      presence_list = CnsWeb.Presence.list(socket)
      assert Map.has_key?(presence_list, socket.assigns.swarm_id)
    end
    
    test "rejects invalid topic format" do
      {:ok, socket} = connect(UserSocket, %{
        current_user: %{id: "test_user", role: "operator"}
      })
      
      assert {:error, %{reason: "Invalid topic"}} = 
        subscribe_and_join(socket, SwarmChannel, "invalid:topic:format")
    end
  end
  
  describe "80/20 optimization filtering" do
    test "allows critical events through filter", %{socket: socket} do
      critical_payload = %{"critical" => true, "priority" => "high"}
      
      ref = push(socket, "test_event", critical_payload)
      
      # Should not be filtered out
      assert_reply ref, :ok
    end
    
    test "filters non-critical events in 80_20 mode", %{socket: socket} do
      non_critical_payload = %{"priority" => "low", "level" => "debug"}
      
      ref = push(socket, "test_event", non_critical_payload)
      
      # Should be filtered out (no reply expected)
      refute_reply ref, :error
    end
    
    test "allows all events in non-optimization mode" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "test_user", role: "operator"},
          optimization_mode: "full"
        })
        |> subscribe_and_join(SwarmChannel, "swarm:test_pipeline", %{
          "optimization" => "full"
        })
      
      non_critical_payload = %{"priority" => "low", "level" => "debug"}
      ref = push(socket, "test_event", non_critical_payload)
      
      # Should not be filtered in full mode
      assert_reply ref, :ok
    end
  end
  
  describe "pipeline execution" do
    test "executes pipeline with 80/20 optimization", %{socket: socket} do
      payload = %{
        "stages" => ["typer", "turtle", "ash", "reactor", "k8s"],
        "optimization_strategy" => "skip_non_critical",
        "domain" => "cybersecurity"
      }
      
      ref = push(socket, "pipeline:execute", payload)
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      assert is_binary(execution_id)
      
      # Should broadcast pipeline started event
      assert_broadcast "pipeline:started", %{
        execution_id: ^execution_id,
        stages: stages,
        strategy: "skip_non_critical"
      }
      
      # Should only include critical stages
      critical_stages = ["typer", "turtle", "ash", "reactor", "k8s"]
      assert Enum.all?(stages, &(&1 in critical_stages))
    end
    
    test "validates critical stages in 80/20 mode", %{socket: socket} do
      payload = %{
        "stage" => "ttl2dspy",  # Non-critical stage
        "action" => "execute"
      }
      
      ref = push(socket, "pipeline:stage_action", payload)
      assert_reply ref, :error, %{reason: "Non-critical stage in 80/20 mode"}
    end
    
    test "allows non-critical stages in full mode" do
      {:ok, _, socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "test_user", role: "operator"},
          optimization_mode: "full"
        })
        |> subscribe_and_join(SwarmChannel, "swarm:test_pipeline", %{
          "optimization" => "full"
        })
      
      payload = %{
        "stage" => "ttl2dspy",
        "action" => "execute"
      }
      
      ref = push(socket, "pipeline:stage_action", payload)
      assert_reply ref, :ok
    end
  end
  
  describe "notification handling" do
    test "subscribes to critical channels only in 80/20 mode", %{socket: socket} do
      payload = %{
        "channels" => ["error_alerts", "ash_resources", "reactor_workflows", "debug_logs"]
      }
      
      ref = push(socket, "notifications:subscribe", payload)
      assert_reply ref, :ok, %{subscribed: subscribed_channels}
      
      # Should filter to critical channels only
      critical_channels = ["error_alerts", "ash_resources", "reactor_workflows"]
      assert Enum.all?(subscribed_channels, &(&1 in critical_channels))
      refute "debug_logs" in subscribed_channels
    end
    
    test "filters notifications by priority", %{socket: socket} do
      # Subscribe first
      push(socket, "notifications:subscribe", %{
        "channels" => ["error_alerts"]
      })
      
      # Critical notification should go through
      critical_notification = %{
        "level" => "critical",
        "channel" => "error_alerts",
        "message" => "Critical error"
      }
      
      ref = push(socket, "notifications:send", critical_notification)
      assert_reply ref, :ok
      
      # Low priority should be filtered
      low_priority_notification = %{
        "level" => "info", 
        "channel" => "error_alerts",
        "message" => "Info message"
      }
      
      ref = push(socket, "notifications:send", low_priority_notification)
      # Should be silently filtered (no reply)
      refute_reply ref, :ok
    end
  end
  
  describe "reactor workflow management" do
    test "creates reactor workflow with optimization", %{socket: socket} do
      payload = %{
        "name" => "cybersecurity_workflow",
        "steps" => [
          %{"name" => "parse_input", "type" => "ash_action"},
          %{"name" => "generate_resources", "type" => "ash_create"},
          %{"name" => "validate_output", "type" => "ash_validate"}
        ],
        "optimization_mode" => "80_20"
      }
      
      ref = push(socket, "reactor:workflow:create", payload)
      assert_reply ref, :ok, %{workflow_id: workflow_id}
      
      assert is_binary(workflow_id)
      
      # Should broadcast workflow created
      assert_broadcast "reactor:workflow:created", %{
        workflow_id: ^workflow_id,
        optimized: true
      }
    end
    
    test "executes reactor step with monitoring", %{socket: socket} do
      payload = %{
        "step_name" => "generate_ash_resource",
        "step_type" => "ash_create",
        "inputs" => %{"name" => "TestResource"},
        "workflow_id" => "test_workflow"
      }
      
      ref = push(socket, "reactor:step:execute", payload)
      assert_reply ref, :ok
      
      # Should broadcast step completion
      assert_broadcast "reactor:step:completed", %{
        workflow_id: "test_workflow",
        step_name: "generate_ash_resource"
      }
    end
  end
  
  describe "swarm intelligence" do
    test "applies optimization recommendations", %{socket: socket} do
      payload = %{
        "target" => "pipeline",
        "strategy" => "80_20",
        "constraints" => %{"max_duration" => 500}
      }
      
      ref = push(socket, "swarm:optimize", payload)
      assert_reply ref, :ok, %{improvements: improvements}
      
      assert is_map(improvements)
      
      # Should broadcast optimization complete
      assert_broadcast "swarm:optimization_complete", %{
        target: "pipeline",
        estimated_gains: _gains
      }
    end
    
    test "learns from execution patterns", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_123",
        "domain" => "cybersecurity",
        "metrics" => %{"duration" => 350, "efficiency" => 0.85},
        "outcomes" => %{"success" => true, "errors" => []}
      }
      
      ref = push(socket, "swarm:learn", payload)
      assert_reply ref, :ok, %{
        patterns_learned: patterns_count,
        confidence_score: confidence
      }
      
      assert is_integer(patterns_count)
      assert is_float(confidence)
      assert confidence >= 0.0 and confidence <= 1.0
    end
    
    test "generates contextual recommendations", %{socket: socket} do
      payload = %{
        "pipeline_state" => %{"active_stages" => ["ash", "reactor"]},
        "metrics" => %{"avg_duration" => 400, "success_rate" => 0.9}
      }
      
      ref = push(socket, "swarm:recommendations", payload)
      assert_reply ref, :ok, %{
        recommendations: recommendations,
        confidence: confidence
      }
      
      assert is_list(recommendations)
      assert is_float(confidence)
    end
  end
  
  describe "telemetry and metrics" do
    test "reports pipeline metrics", %{socket: socket} do
      payload = %{
        "execution_id" => "test_exec_456",
        "pipeline_metrics" => %{
          "duration" => 280,
          "stages_completed" => 5,
          "optimizations_applied" => 2
        }
      }
      
      ref = push(socket, "report_metrics", payload)
      assert_reply ref, :ok
      
      # Should broadcast telemetry update
      assert_broadcast "telemetry_update", %{
        metrics: metrics,
        timestamp: _timestamp
      }
      
      assert metrics["duration"] == 280
    end
    
    test "throttles telemetry events", %{socket: socket} do
      # Send many telemetry events rapidly
      for i <- 1..150 do
        push(socket, "telemetry:stream", %{"metric" => "cpu_usage", "value" => i})
      end
      
      # Should hit rate limit
      ref = push(socket, "telemetry:stream", %{"metric" => "cpu_usage", "value" => 151})
      assert_reply ref, :error, %{reason: "Rate limit exceeded"}
    end
  end
  
  describe "swarm control" do
    test "pauses and resumes swarm", %{socket: socket} do
      # Pause swarm
      ref = push(socket, "swarm:control", %{"action" => "pause"})
      assert_reply ref, :ok
      
      assert socket.assigns.paused == true
      assert_broadcast "swarm:paused", %{swarm_id: "test_pipeline"}
      
      # Resume swarm
      ref = push(socket, "swarm:control", %{"action" => "resume"})
      assert_reply ref, :ok
      
      assert_broadcast "swarm:resumed", %{swarm_id: "test_pipeline"}
    end
    
    test "resets swarm state", %{socket: socket} do
      ref = push(socket, "swarm:control", %{"action" => "reset"})
      assert_reply ref, :ok
      
      # Should reset to critical stages
      assert socket.assigns.active_stages == socket.assigns.critical_stages
    end
    
    test "rejects unknown control actions", %{socket: socket} do
      ref = push(socket, "swarm:control", %{"action" => "unknown_action"})
      assert_reply ref, :error, %{reason: "Unknown action"}
    end
  end
  
  describe "stage-specific handlers" do
    test "delegates to ash stage handler", %{socket: socket} do
      payload = %{
        "name" => "CybersecurityAlert",
        "attributes" => %{"severity" => "high", "type" => "intrusion"}
      }
      
      ref = push(socket, "stage:ash:resource:create", payload)
      assert_reply ref, :ok
      
      # Should receive ash resource created broadcast
      assert_broadcast "ash:resource_created", %{
        name: "CybersecurityAlert"
      }
    end
    
    test "delegates to k8s stage handler", %{socket: socket} do
      payload = %{
        "name" => "cybersecurity-app",
        "replicas" => 3,
        "image" => "cybersec:latest"
      }
      
      ref = push(socket, "stage:k8s:manifest:generate", payload)
      assert_reply ref, :ok, %{manifest_generated: true}
      
      # Should receive k8s manifest ready
      assert_broadcast "k8s:manifest_ready", %{
        resource_type: "deployment"
      }
    end
    
    test "delegates to reactor stage handler", %{socket: socket} do
      payload = %{
        "steps" => [
          %{"name" => "validate_input", "type" => "validation"},
          %{"name" => "process_data", "type" => "computation"}
        ]
      }
      
      ref = push(socket, "stage:reactor:workflow:validate", payload)
      assert_reply ref, :ok, %{valid: true, step_count: 2}
    end
  end
  
  describe "authentication and authorization" do
    test "requires authentication for protected operations" do
      # Create unauthenticated socket
      {:ok, socket} = connect(UserSocket, %{})
      
      {:ok, _, socket} = subscribe_and_join(socket, SwarmChannel, "swarm:test")
      
      ref = push(socket, "admin:reset_all", %{})
      assert_reply ref, :error, %{reason: "Authentication required"}
    end
    
    test "checks permissions for reactor management", %{socket: socket} do
      # Operator role should not have admin access
      ref = push(socket, "optimize:mode:set", %{"mode" => "performance"})
      assert_reply ref, :error, %{reason: "Admin access required"}
    end
  end
  
  describe "performance and optimization validation" do
    test "demonstrates 80/20 optimization effectiveness" do
      # Test with 80/20 mode
      {:ok, _, optimized_socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "test_user", role: "operator"},
          optimization_mode: "80_20"
        })
        |> subscribe_and_join(SwarmChannel, "swarm:optimized", %{
          "optimization" => "80_20"
        })
      
      # Test with full mode
      {:ok, _, full_socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "test_user", role: "operator"},
          optimization_mode: "full"
        })
        |> subscribe_and_join(SwarmChannel, "swarm:full", %{
          "optimization" => "full"
        })
      
      # Execute same pipeline on both
      pipeline_payload = %{
        "stages" => ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"],
        "domain" => "cybersecurity"
      }
      
      # Optimized should complete faster
      start_time = :os.system_time(:millisecond)
      
      ref_optimized = push(optimized_socket, "pipeline:execute", pipeline_payload)
      assert_reply ref_optimized, :ok, %{execution_id: optimized_id}
      
      optimized_time = :os.system_time(:millisecond) - start_time
      
      start_time = :os.system_time(:millisecond)
      
      ref_full = push(full_socket, "pipeline:execute", pipeline_payload)
      assert_reply ref_full, :ok, %{execution_id: full_id}
      
      full_time = :os.system_time(:millisecond) - start_time
      
      # 80/20 optimization should be faster (or at least not significantly slower)
      assert optimized_time <= full_time * 1.2  # Allow 20% margin
    end
    
    test "maintains critical functionality while optimizing", %{socket: socket} do
      # Execute minimal critical path
      payload = %{
        "stages" => ["typer", "turtle", "ash", "k8s"],
        "optimization_strategy" => "minimal_path"
      }
      
      ref = push(socket, "pipeline:execute", payload)
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should still produce valid outputs
      assert_broadcast "pipeline:started", %{
        execution_id: ^execution_id,
        stages: stages
      }
      
      # Essential stages should be present
      essential_stages = ["typer", "turtle", "k8s"]
      assert Enum.all?(essential_stages, &(&1 in stages))
    end
  end
  
  describe "error handling and resilience" do
    test "handles channel errors gracefully", %{socket: socket} do
      # Send malformed payload
      ref = push(socket, "pipeline:execute", %{"invalid" => "payload"})
      assert_reply ref, :error, %{reason: _reason}
      
      # Channel should remain functional
      ref = push(socket, "swarm:control", %{"action" => "pause"})
      assert_reply ref, :ok
    end
    
    test "recovers from stage failures", %{socket: socket} do
      payload = %{
        "stage" => "ash",
        "action" => "execute",
        "force_failure" => true  # Test parameter
      }
      
      ref = push(socket, "pipeline:stage_action", payload)
      
      # Should handle failure gracefully
      assert_reply ref, :error, %{reason: _reason}
      
      # Should still be able to continue with other operations
      ref = push(socket, "swarm:control", %{"action" => "pause"})
      assert_reply ref, :ok
    end
  end
end