defmodule CnsWeb.ChannelIntegrationTest do
  @moduledoc """
  Phoenix Channel E2E Integration Tests for Swarm Communication
  Tests real channel communication between frontend and backend
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Phoenix.ChannelTest
  
  setup do
    {:ok, _, socket} =
      UserSocket
      |> socket("user_id", %{
        current_user: %{id: "integration_test_user", role: "operator"},
        optimization_mode: "80_20"
      })
      |> subscribe_and_join(SwarmChannel, "swarm:integration_test", %{
        "optimization" => "80_20",
        "domain" => "cybersecurity"
      })
    
    %{socket: socket}
  end
  
  describe "end-to-end pipeline execution" do
    test "executes complete pipeline workflow through channels", %{socket: socket} do
      # Start pipeline execution
      execution_payload = %{
        "stages" => ["typer", "turtle", "ash", "reactor", "k8s"],
        "optimization_strategy" => "skip_non_critical",
        "domain" => "cybersecurity",
        "priority" => "high"
      }
      
      ref = push(socket, "pipeline:execute", execution_payload)
      assert_reply ref, :ok, %{execution_id: execution_id}
      assert is_binary(execution_id)
      
      # Should receive pipeline started event
      assert_broadcast "pipeline:started", %{
        execution_id: ^execution_id,
        stages: stages,
        strategy: "skip_non_critical"
      }
      
      # Should only include critical stages in 80/20 mode
      critical_stages = ["typer", "turtle", "ash", "reactor", "k8s"]
      assert Enum.all?(stages, &(&1 in critical_stages))
      
      # Should receive stage completion events
      assert_broadcast "pipeline:stage_completed", %{
        execution_id: ^execution_id,
        stage: stage_name
      }
      assert stage_name in critical_stages
      
      # Should receive final completion
      assert_broadcast "pipeline:completed", %{
        execution_id: ^execution_id,
        status: "completed",
        optimization_applied: true
      }
    end
    
    test "filters non-critical stages in 80/20 optimization mode", %{socket: socket} do
      # Try to execute non-critical stage
      stage_payload = %{
        "stage" => "ttl2dspy",  # Non-critical stage
        "action" => "execute"
      }
      
      ref = push(socket, "pipeline:stage_action", stage_payload)
      assert_reply ref, :error, %{reason: "Non-critical stage in 80/20 mode"}
    end
    
    test "handles parallel execution optimization", %{socket: socket} do
      parallel_payload = %{
        "stages" => ["ash", "reactor", "k8s"],
        "optimization_strategy" => "parallel_execution",
        "domain" => "cybersecurity"
      }
      
      ref = push(socket, "pipeline:execute", parallel_payload)
      assert_reply ref, :ok, %{execution_id: execution_id}
      
      # Should broadcast parallel execution started
      assert_broadcast "pipeline:parallel_execution_started", %{
        execution_id: ^execution_id,
        parallel_groups: parallel_groups
      }
      
      assert is_list(parallel_groups)
      assert length(parallel_groups) > 0
    end
  end
  
  describe "notification channel integration" do
    test "subscribes to and receives notifications with 80/20 filtering", %{socket: socket} do
      # Subscribe to notification channels
      subscription_payload = %{
        "channels" => ["error_alerts", "ash_resources", "reactor_workflows", "debug_logs"]
      }
      
      ref = push(socket, "notifications:subscribe", subscription_payload)
      assert_reply ref, :ok, %{subscribed: subscribed_channels}
      
      # Should filter to critical channels only in 80/20 mode
      critical_channels = ["error_alerts", "ash_resources", "reactor_workflows"]
      assert Enum.all?(subscribed_channels, &(&1 in critical_channels))
      refute "debug_logs" in subscribed_channels
      
      # Send critical notification
      critical_notification = %{
        "level" => "critical",
        "channel" => "error_alerts",
        "message" => "Integration test critical alert"
      }
      
      ref = push(socket, "notifications:send", critical_notification)
      assert_reply ref, :ok
      
      # Should receive notification broadcast
      assert_broadcast "notification", %{
        level: "critical",
        channel: "error_alerts",
        message: "Integration test critical alert"
      }
      
      # Send non-critical notification (should be filtered)
      low_priority_notification = %{
        "level" => "info",
        "channel" => "debug_logs",
        "message" => "Debug information"
      }
      
      ref = push(socket, "notifications:send", low_priority_notification)
      # Should be filtered out (no reply or broadcast expected)
      refute_reply ref, :ok, 1000
    end
    
    test "handles notification batching for high-frequency events", %{socket: socket} do
      # Configure batching for performance metrics
      batching_config = %{
        "channel" => "performance_metrics",
        "batch_size" => 5,
        "timeout_ms" => 1000
      }
      
      ref = push(socket, "notifications:configure_batching", batching_config)
      assert_reply ref, :ok, %{batching_enabled: true}
      
      # Send multiple notifications rapidly
      for i <- 1..7 do
        notification = %{
          "channel" => "performance_metrics",
          "level" => "info",
          "message" => "Metric update #{i}"
        }
        push(socket, "notifications:add_to_batch", notification)
      end
      
      # Should receive batch processed event
      assert_broadcast "notifications:batch_processed", %{
        channel: "performance_metrics",
        batch_size: batch_size,
        items_processed: items_count
      }
      
      assert batch_size == 5
      assert items_count >= 5
    end
  end
  
  describe "reactor workflow integration" do
    test "creates and executes reactor workflows through channels", %{socket: socket} do
      # Create reactor workflow
      workflow_payload = %{
        "name" => "integration_test_workflow",
        "steps" => [
          %{
            "name" => "parse_cybersecurity_data",
            "type" => "ash_action",
            "critical" => true
          },
          %{
            "name" => "generate_threat_resources",
            "type" => "ash_create",
            "critical" => true
          },
          %{
            "name" => "validate_compliance",
            "type" => "ash_validate",
            "critical" => false
          }
        ],
        "optimization_mode" => "80_20"
      }
      
      ref = push(socket, "reactor:workflow:create", workflow_payload)
      assert_reply ref, :ok, %{workflow_id: workflow_id}
      assert is_binary(workflow_id)
      
      # Should broadcast workflow created
      assert_broadcast "reactor:workflow:created", %{
        workflow_id: ^workflow_id,
        optimized: true,
        critical_steps: critical_step_count
      }
      
      # Should prioritize critical steps
      assert critical_step_count == 2
      
      # Execute reactor step
      step_payload = %{
        "step_name" => "generate_threat_resources",
        "step_type" => "ash_create",
        "inputs" => %{
          "resource_name" => "ThreatDetection",
          "attributes" => %{
            "severity" => "high",
            "source" => "integration_test"
          }
        },
        "workflow_id" => workflow_id
      }
      
      ref = push(socket, "reactor:step:execute", step_payload)
      assert_reply ref, :ok, %{step_completed: true}
      
      # Should broadcast step completion
      assert_broadcast "reactor:step:completed", %{
        workflow_id: ^workflow_id,
        step_name: "generate_threat_resources",
        outputs: outputs
      }
      
      assert is_list(outputs)
    end
    
    test "generates ash resources through reactor workflow", %{socket: socket} do
      resource_payload = %{
        "resource_name" => "IntegrationTestAlert",
        "attributes" => [
          %{"name" => "severity", "type" => "atom", "values" => ["low", "medium", "high", "critical"]},
          %{"name" => "message", "type" => "string"},
          %{"name" => "timestamp", "type" => "datetime"},
          %{"name" => "source_system", "type" => "string"}
        ],
        "actions" => [
          %{"name" => "create", "type" => "create"},
          %{"name" => "update_severity", "type" => "update"},
          %{"name" => "resolve", "type" => "update"}
        ]
      }
      
      ref = push(socket, "reactor:generate_ash_resource", resource_payload)
      assert_reply ref, :ok, %{resource_generated: true, resource_name: "IntegrationTestAlert"}
      
      # Should broadcast resource creation event
      assert_broadcast "ash:resource_created", %{
        name: "IntegrationTestAlert",
        attributes_count: 4,
        actions_count: 3
      }
    end
  end
  
  describe "swarm intelligence integration" do
    test "applies swarm optimization and learns from results", %{socket: socket} do
      # Trigger swarm optimization
      optimization_payload = %{
        "target" => "pipeline",
        "strategy" => "80_20",
        "current_metrics" => %{
          "avg_duration" => 450,
          "success_rate" => 0.85,
          "bottlenecks" => ["ash", "reactor"]
        },
        "constraints" => %{
          "max_duration" => 400,
          "min_success_rate" => 0.9
        }
      }
      
      ref = push(socket, "swarm:optimize", optimization_payload)
      assert_reply ref, :ok, %{improvements: improvements}
      
      assert Map.has_key?(improvements, :estimated_gains)
      assert Map.has_key?(improvements, :recommended_changes)
      
      # Should broadcast optimization completion
      assert_broadcast "swarm:optimization_complete", %{
        target: "pipeline",
        estimated_gains: gains
      }
      
      assert is_map(gains)
      
      # Record learning from execution
      learning_payload = %{
        "execution_id" => "integration_learning_test",
        "domain" => "cybersecurity",
        "metrics" => %{
          "duration" => 380,
          "efficiency" => 0.92,
          "success_rate" => 0.95
        },
        "outcomes" => %{
          "success" => true,
          "errors" => [],
          "user_satisfaction" => 0.88
        },
        "context" => %{
          "optimization_applied" => true,
          "strategy_used" => "skip_non_critical"
        }
      }
      
      ref = push(socket, "swarm:learn", learning_payload)
      assert_reply ref, :ok, %{
        patterns_learned: patterns_count,
        confidence_score: confidence
      }
      
      assert is_integer(patterns_count)
      assert patterns_count > 0
      assert is_float(confidence)
      assert confidence >= 0.0 and confidence <= 1.0
    end
    
    test "generates contextual recommendations based on current state", %{socket: socket} do
      recommendation_payload = %{
        "pipeline_state" => %{
          "active_stages" => ["ash", "reactor"],
          "queued_stages" => ["k8s"],
          "failed_stages" => [],
          "average_stage_duration" => 120
        },
        "system_metrics" => %{
          "cpu_usage" => 0.75,
          "memory_usage" => 0.60,
          "network_latency" => 25,
          "concurrent_executions" => 3
        },
        "user_context" => %{
          "experience_level" => "advanced",
          "time_pressure" => "medium",
          "quality_requirements" => "high"
        }
      }
      
      ref = push(socket, "swarm:recommendations", recommendation_payload)
      assert_reply ref, :ok, %{
        recommendations: recommendations,
        confidence: confidence
      }
      
      assert is_list(recommendations)
      assert length(recommendations) > 0
      assert is_float(confidence)
      
      # Recommendations should be contextual
      recommendation_types = Enum.map(recommendations, & &1.type)
      assert "optimization" in recommendation_types or "performance" in recommendation_types
    end
  end
  
  describe "telemetry and metrics integration" do
    test "reports and tracks pipeline metrics through channels", %{socket: socket} do
      metrics_payload = %{
        "execution_id" => "integration_metrics_test",
        "pipeline_metrics" => %{
          "total_duration" => 385,
          "stages_completed" => 5,
          "stages_skipped" => 3,
          "optimizations_applied" => 2,
          "efficiency_score" => 0.88
        },
        "stage_metrics" => [
          %{"stage" => "typer", "duration" => 45, "success" => true},
          %{"stage" => "turtle", "duration" => 60, "success" => true},
          %{"stage" => "ash", "duration" => 120, "success" => true},
          %{"stage" => "reactor", "duration" => 90, "success" => true},
          %{"stage" => "k8s", "duration" => 70, "success" => true}
        ]
      }
      
      ref = push(socket, "report_metrics", metrics_payload)
      assert_reply ref, :ok
      
      # Should broadcast telemetry update
      assert_broadcast "telemetry_update", %{
        metrics: updated_metrics,
        timestamp: timestamp
      }
      
      assert updated_metrics["total_duration"] == 385
      assert updated_metrics["efficiency_score"] == 0.88
      assert is_integer(timestamp)
    end
    
    test "handles rate limiting for high-frequency telemetry", %{socket: socket} do
      # Send many telemetry events rapidly
      for i <- 1..50 do
        telemetry_event = %{
          "metric" => "cpu_usage",
          "value" => rem(i, 100),
          "timestamp" => System.system_time(:millisecond)
        }
        
        push(socket, "telemetry:stream", telemetry_event)
      end
      
      # Additional event should hit rate limit
      ref = push(socket, "telemetry:stream", %{
        "metric" => "cpu_usage",
        "value" => 99
      })
      
      assert_reply ref, :error, %{reason: "Rate limit exceeded"}
    end
  end
  
  describe "error handling and resilience" do
    test "handles channel errors gracefully without breaking functionality", %{socket: socket} do
      # Send malformed request
      malformed_payload = %{
        "invalid_field" => "test",
        "missing_required" => true
      }
      
      ref = push(socket, "pipeline:execute", malformed_payload)
      assert_reply ref, :error, %{reason: _reason}
      
      # Channel should remain functional
      valid_payload = %{
        "stages" => ["typer", "turtle", "ash"],
        "optimization_strategy" => "minimal_path",
        "domain" => "cybersecurity"
      }
      
      ref = push(socket, "pipeline:execute", valid_payload)
      assert_reply ref, :ok, %{execution_id: _execution_id}
    end
    
    test "maintains service during connection disruptions", %{socket: socket} do
      # Simulate temporary disconnection by leaving and rejoining
      leave(socket)
      
      {:ok, _, new_socket} =
        UserSocket
        |> socket("user_id", %{
          current_user: %{id: "integration_test_user", role: "operator"},
          optimization_mode: "80_20"
        })
        |> subscribe_and_join(SwarmChannel, "swarm:reconnection_test", %{
          "optimization" => "80_20"
        })
      
      # Should be able to execute pipeline after reconnection
      ref = push(new_socket, "pipeline:execute", %{
        "stages" => ["typer", "ash", "k8s"],
        "optimization_strategy" => "minimal_path"
      })
      
      assert_reply ref, :ok, %{execution_id: _execution_id}
    end
  end
  
  describe "80/20 optimization validation" do
    test "demonstrates measurable performance improvement through channel communication", %{socket: socket} do
      # Execute baseline (full pipeline)
      baseline_payload = %{
        "stages" => ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"],
        "optimization_strategy" => "adaptive_routing",
        "domain" => "cybersecurity"
      }
      
      ref = push(socket, "pipeline:execute", baseline_payload)
      assert_reply ref, :ok, %{execution_id: baseline_execution_id}
      
      assert_broadcast "pipeline:completed", %{
        execution_id: ^baseline_execution_id,
        duration: baseline_duration
      }
      
      # Execute optimized (80/20)
      optimized_payload = %{
        "stages" => ["typer", "turtle", "ash", "reactor", "k8s"],
        "optimization_strategy" => "skip_non_critical",
        "domain" => "cybersecurity"
      }
      
      ref = push(socket, "pipeline:execute", optimized_payload)
      assert_reply ref, :ok, %{execution_id: optimized_execution_id}
      
      assert_broadcast "pipeline:completed", %{
        execution_id: ^optimized_execution_id,
        duration: optimized_duration,
        optimization_applied: true
      }
      
      # Should show significant improvement
      improvement_ratio = (baseline_duration - optimized_duration) / baseline_duration
      assert improvement_ratio >= 0.2  # At least 20% improvement
    end
    
    test "validates 80/20 principle in notification filtering", %{socket: socket} do
      # Subscribe to all channels
      ref = push(socket, "notifications:subscribe", %{
        "channels" => ["error_alerts", "ash_resources", "reactor_workflows", "debug_logs", "performance_metrics"]
      })
      
      assert_reply ref, :ok, %{subscribed: subscribed_channels}
      
      # Should filter to critical channels (approximately 20% handling 80% of value)
      critical_channels = ["error_alerts", "ash_resources", "reactor_workflows"]
      
      # All subscribed channels should be critical in 80/20 mode
      assert Enum.all?(subscribed_channels, &(&1 in critical_channels))
      assert length(subscribed_channels) <= 3  # Should be limited set
    end
  end
end