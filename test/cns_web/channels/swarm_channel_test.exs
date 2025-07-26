defmodule CnsWeb.SwarmChannelTest do
  @moduledoc """
  Test suite for SwarmChannel with 80/20 optimization.
  Validates ultrathink swarm channel functionality across the entire stack.
  """
  
  use CnsWeb.ChannelCase
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Accounts, Swarm}
  
  setup do
    # Create test user with swarm access
    user = Accounts.create_test_user(%{
      email: "swarm@test.com",
      role: "admin",
      active: true
    })
    
    # Create test swarm
    swarm = Swarm.create_test_swarm(%{
      name: "test-swarm",
      optimization_mode: "80_20"
    })
    
    # Create socket with authentication
    {:ok, socket} = connect(UserSocket, %{
      "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
    })
    
    {:ok, %{user: user, swarm: swarm, socket: socket}}
  end
  
  describe "channel join" do
    test "joins swarm channel with 80/20 optimization", %{socket: socket, swarm: swarm} do
      {:ok, response, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      assert response.status == "connected"
      assert response.swarm_id == swarm.id
      assert socket.assigns.optimization_mode == "80_20"
      assert socket.assigns.critical_stages == ["k8s", "reactor", "ash", "turtle", "typer"]
    end
    
    test "rejects invalid swarm topic", %{socket: socket} do
      assert {:error, %{reason: "Invalid topic"}} = 
        subscribe_and_join(socket, SwarmChannel, "invalid:topic")
    end
  end
  
  describe "pipeline execution with 80/20 optimization" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "executes critical stages only in 80/20 mode", %{socket: socket} do
      # Execute pipeline with 80/20 optimization
      ref = push(socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "data" => %{"input" => "test"}
      })
      
      assert_reply ref, :ok, response
      
      # Should only execute critical stages
      assert response.executed_stages == ["k8s", "reactor", "ash", "turtle", "typer"]
      assert response.skipped_stages == ["erlang", "bitactor", "ttl2dspy"]
      assert response.optimization_savings.efficiency_gain > 30
    end
    
    test "applies optimization strategies", %{socket: socket} do
      ref = push(socket, "pipeline:optimize", %{
        "type" => "auto"
      })
      
      assert_reply ref, :ok, optimization
      
      assert optimization.type == "auto"
      assert is_list(optimization.stages)
      assert optimization.expected_improvement > 0
      
      # Should broadcast optimization application
      assert_broadcast "optimization:applied", broadcast_data
      assert broadcast_data.type == "auto"
    end
  end
  
  describe "reactor step execution" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "executes reactor steps with optimization", %{socket: socket} do
      ref = push(socket, "reactor:step:execute", %{
        "step" => "validate",
        "data" => %{"input" => "test_data"}
      })
      
      assert_reply ref, :ok, result
      
      assert result.step == "validate"
      assert result.duration > 0
      assert is_map(result.output)
      
      # Should broadcast step completion for critical steps
      assert_broadcast "reactor:step:completed", broadcast_data
      assert broadcast_data.step == "validate"
    end
    
    test "creates optimized workflows", %{socket: socket} do
      workflow_def = %{
        "name" => "test-workflow",
        "steps" => [
          %{"name" => "step1", "critical" => true, "impact" => 90},
          %{"name" => "step2", "critical" => false, "impact" => 10}
        ]
      }
      
      ref = push(socket, "reactor:workflow:create", %{
        "workflow" => workflow_def
      })
      
      assert_reply ref, :ok, workflow
      
      assert workflow.name == "test-workflow"
      assert workflow.optimization.mode == "80_20"
      assert workflow.optimization.skip_non_critical == true
      
      # Should broadcast workflow creation
      assert_broadcast "workflow:created", _broadcast_data
    end
  end
  
  describe "notification filtering" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "subscribes to critical notifications only in 80/20 mode", %{socket: socket} do
      ref = push(socket, "notifications:subscribe", %{
        "types" => ["all"],
        "level" => "info"
      })
      
      assert_reply ref, :ok, subscription
      
      # Should filter to critical types only in 80/20 mode
      assert subscription.types == ["error", "critical", "security", "performance", "deployment"]
      assert subscription.level == "critical"
      assert subscription.batch_enabled == true
    end
    
    test "handles critical notifications immediately", %{socket: socket} do
      ref = push(socket, "notifications:critical:system_failure", %{
        "title" => "System Failure",
        "message" => "Critical system failure detected",
        "type" => "critical"
      })
      
      assert_reply ref, :noreply
      
      # Should receive immediate critical notification
      assert_push "notification:critical", notification
      assert notification.level == "critical"
      assert notification.title == "System Failure"
    end
    
    test "batches non-critical notifications", %{socket: socket} do
      notifications = [
        %{"level" => "info", "message" => "Info 1"},
        %{"level" => "warning", "message" => "Warning 1"},
        %{"level" => "critical", "message" => "Critical 1"}
      ]
      
      ref = push(socket, "notifications:batch:process", %{
        "notifications" => notifications
      })
      
      assert_reply ref, :ok, batch_results
      
      # Should filter and prioritize
      assert batch_results.critical_count == 1
      assert batch_results.count <= 20  # 80/20 limit
    end
  end
  
  describe "telemetry streaming" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "subscribes to critical metrics only", %{socket: socket} do
      ref = push(socket, "telemetry:subscribe", %{
        "metrics" => ["all"],
        "mode" => "real_time"
      })
      
      assert_reply ref, :ok, response
      
      # Should filter to critical metrics in 80/20 mode
      config = response.config
      assert config.metrics == ["cpu", "memory", "latency", "error_rate", "throughput"]
      assert config.mode == "aggregated"
      assert config.sample_rate == 0.2
      assert config.compression == true
    end
    
    test "streams critical telemetry data", %{socket: socket} do
      ref = push(socket, "telemetry:stream:critical", %{})
      
      assert_reply ref, :noreply
      
      # Should start critical stream
      assert_push "telemetry:critical:started", stream_data
      assert stream_data.metrics == ["cpu", "memory", "latency", "error_rate", "throughput"]
    end
    
    test "detects performance patterns", %{socket: socket} do
      ref = push(socket, "telemetry:pattern:detect", %{
        "patterns" => ["anomaly", "threshold"],
        "window" => "5m"
      })
      
      assert_reply ref, :ok, patterns
      
      # Should return only high-confidence patterns in 80/20 mode
      assert is_list(patterns)
      assert Enum.all?(patterns, fn pattern ->
        pattern.confidence >= 0.8 or pattern.type in ["threshold_breach", "error_spike"]
      end)
    end
  end
  
  describe "optimization controls" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "sets optimization mode", %{socket: socket} do
      ref = push(socket, "optimize:mode:set", %{
        "mode" => "full"
      })
      
      assert_reply ref, :ok, config
      
      assert config.mode == "full"
      
      # Should broadcast mode change
      assert_broadcast "optimization:mode:changed", broadcast_data
      assert broadcast_data.mode == "full"
    end
    
    test "generates optimization report", %{socket: socket} do
      ref = push(socket, "optimize:report:generate", %{
        "type" => "summary",
        "range" => "24h"
      })
      
      assert_reply ref, :ok, report
      
      assert is_number(report.optimization_score)
      assert is_list(report.key_improvements)
      assert is_list(report.critical_issues)
      assert is_list(report.next_actions)
    end
  end
  
  describe "80/20 filtering behavior" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "filters non-critical events in 80/20 mode", %{socket: socket} do
      # Non-critical event should be silently dropped
      ref = push(socket, "pipeline:erlang:process", %{
        "action" => "monitor",
        "priority" => "low"
      })
      
      # Should not get a reply for non-critical events
      refute_reply ref, _
    end
    
    test "allows critical events through filter", %{socket: socket} do
      # Critical event should be processed
      ref = push(socket, "pipeline:k8s:deploy", %{
        "action" => "deploy",
        "critical" => true
      })
      
      assert_reply ref, :ok, _response
    end
    
    test "broadcasts only critical updates", %{socket: socket} do
      ref = push(socket, "broadcast:optimized", %{
        "message" => "System update",
        "priority" => "high"
      })
      
      assert_reply ref, :ok
      
      # Should broadcast critical update
      assert_broadcast "update:critical", broadcast_data
      assert broadcast_data.priority == "high"
    end
  end
  
  describe "error handling" do
    setup %{socket: socket, swarm: swarm} do
      {:ok, _, socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      {:ok, %{socket: socket}}
    end
    
    test "handles invalid pipeline stage", %{socket: socket} do
      ref = push(socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "stages" => ["invalid_stage"]
      })
      
      assert_reply ref, :error, %{reason: "Non-critical stage in 80/20 mode"}
    end
    
    test "handles rate limiting", %{socket: socket} do
      # Send many requests to trigger rate limit
      for _i <- 1..200 do
        push(socket, "telemetry:stream:critical", %{})
      end
      
      ref = push(socket, "telemetry:stream:critical", %{})
      assert_reply ref, :error, %{reason: "Telemetry rate limit exceeded"}
    end
  end
end