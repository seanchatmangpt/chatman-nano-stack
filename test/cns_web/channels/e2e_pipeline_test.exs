defmodule CnsWeb.Channels.E2EPipelineTest do
  @moduledoc """
  End-to-end tests for the complete ultrathink 80/20 swarm pipeline.
  Tests full pipeline execution from typer through k8s with optimization.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Accounts, Swarm, Pipeline, Reactor, Telemetry}
  
  @pipeline_stages ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"]
  @critical_stages ["typer", "turtle", "ash", "reactor", "k8s"]
  @non_critical_stages ["ttl2dspy", "bitactor", "erlang"]
  
  setup_all do
    # Start test supervision tree
    start_supervised!({Phoenix.PubSub, name: CnsWeb.PubSub})
    start_supervised!({Registry, keys: :unique, name: CnsWeb.ChannelRegistry})
    
    :ok
  end
  
  setup do
    # Create test user with admin privileges
    user = %{
      id: 1,
      email: "e2e@test.com",
      role: "admin",
      active: true
    }
    
    # Create test swarm
    swarm = %{
      id: "e2e-swarm-123",
      name: "E2E Test Swarm",
      optimization_mode: "80_20"
    }
    
    # Create authenticated socket
    {:ok, socket} = connect(UserSocket, %{
      "token" => Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id),
      "optimization" => "80_20"
    })
    
    {:ok, %{user: user, swarm: swarm, socket: socket}}
  end
  
  describe "Complete 80/20 Pipeline Execution" do
    test "executes full pipeline with 80/20 optimization", %{socket: socket, swarm: swarm} do
      # Join swarm channel
      {:ok, join_response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      assert join_response.status == "connected"
      assert join_response.swarm_id == swarm.id
      
      # Execute complete pipeline
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{
          "content" => "test ontology data",
          "format" => "ttl",
          "validation_rules" => ["syntax", "semantics"]
        },
        "metadata" => %{
          "source" => "e2e_test",
          "priority" => "high"
        }
      })
      
      # Should receive immediate acknowledgment
      assert_reply pipeline_ref, :ok, initial_response
      assert initial_response.executed_stages == @critical_stages
      assert initial_response.skipped_stages == @non_critical_stages
      
      # Should receive progress broadcasts for each critical stage
      Enum.each(@critical_stages, fn stage ->
        assert_broadcast "execution:progress", progress_data
        assert progress_data.stage in @critical_stages
        assert is_number(progress_data.progress)
        assert is_map(progress_data.metrics)
      end)
      
      # Should receive final completion broadcast
      assert_broadcast "execution:completed", completion_data
      assert completion_data.status == "success"
      assert completion_data.total_duration < 2000  # Should be faster due to optimization
      assert completion_data.optimization_savings.efficiency_gain > 30
      
      # Validate optimization metrics
      assert completion_data.stages_executed == 5  # Critical stages only
      assert completion_data.stages_skipped == 3   # Non-critical stages
      assert completion_data.optimization_savings.time_saved > 1000
    end
    
    test "executes pipeline with real-time telemetry streaming", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Subscribe to telemetry
      telemetry_ref = push(channel_socket, "telemetry:subscribe", %{
        "metrics" => ["cpu", "memory", "latency", "throughput"],
        "mode" => "real_time"
      })
      
      assert_reply telemetry_ref, :ok, telemetry_response
      assert telemetry_response.config.metrics == ["cpu", "memory", "latency", "error_rate", "throughput"]
      assert telemetry_response.config.compression == true
      
      # Execute pipeline
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{"test" => "telemetry_data"}
      })
      
      assert_reply pipeline_ref, :ok, _response
      
      # Should receive telemetry data during execution
      assert_receive %Phoenix.Socket.Message{
        event: "telemetry:data",
        payload: telemetry_data
      }
      
      # Validate telemetry data structure
      assert is_map(telemetry_data)
      assert Map.has_key?(telemetry_data, "cpu")
      assert Map.has_key?(telemetry_data, "memory")
      assert Map.has_key?(telemetry_data, "latency")
      
      # Should receive pattern detection results
      assert_receive %Phoenix.Socket.Message{
        event: "telemetry:pattern",
        payload: pattern_data
      }
      
      assert pattern_data.confidence >= 0.8
      assert pattern_data.type in ["performance", "optimization", "bottleneck"]
    end
    
    test "handles critical notifications during pipeline execution", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Subscribe to critical notifications
      notification_ref = push(channel_socket, "notifications:subscribe", %{
        "types" => ["all"],
        "level" => "critical"
      })
      
      assert_reply notification_ref, :ok, subscription
      # Should filter to critical types only in 80/20 mode
      assert subscription.level == "critical"
      assert subscription.batch_enabled == true
      
      # Execute pipeline that will generate notifications
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{"simulate_errors" => true}
      })
      
      assert_reply pipeline_ref, :ok, _response
      
      # Should receive critical notifications immediately
      assert_receive %Phoenix.Socket.Message{
        event: "notification:critical",
        payload: critical_notification
      }
      
      assert critical_notification.level == "critical"
      assert critical_notification.type in ["error", "security", "performance"]
      assert is_binary(critical_notification.message)
      
      # Should receive batch updates for non-critical notifications
      assert_receive %Phoenix.Socket.Message{
        event: "notification:batch",
        payload: batch_data
      }
      
      assert batch_data.critical_count >= 1
      assert batch_data.count <= 20  # 80/20 batch limit
    end
  end
  
  describe "Multi-Channel Coordination" do
    test "coordinates between pipeline and reactor channels", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Create workflow first
      workflow_ref = push(channel_socket, "reactor:workflow:create", %{
        "workflow" => %{
          "name" => "e2e-test-workflow",
          "steps" => [
            %{"name" => "validate", "critical" => true, "impact" => 95},
            %{"name" => "transform", "critical" => true, "impact" => 85},
            %{"name" => "optional_step", "critical" => false, "impact" => 15}
          ]
        }
      })
      
      assert_reply workflow_ref, :ok, workflow
      assert workflow.name == "e2e-test-workflow"
      assert workflow.optimization.mode == "80_20"
      
      # Execute workflow through reactor
      workflow_exec_ref = push(channel_socket, "reactor:workflow:execute", %{
        "workflow_id" => workflow.id,
        "input" => %{"data" => "e2e_test_data"}
      })
      
      assert_reply workflow_exec_ref, :ok, exec_response
      assert exec_response.status == "started"
      
      # Execute pipeline that uses the workflow
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "workflow_id" => workflow.id,
        "input_data" => %{"use_workflow" => true}
      })
      
      assert_reply pipeline_ref, :ok, pipeline_response
      
      # Should coordinate between channels
      assert pipeline_response.workflow_integration == true
      assert pipeline_response.reactor_steps_executed >= 2
      
      # Should receive coordination broadcasts
      assert_broadcast "workflow:reactor:coordination", coord_data
      assert coord_data.workflow_id == workflow.id
      assert coord_data.pipeline_stage in @critical_stages
    end
    
    test "handles optimization across multiple channels", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Apply global optimization
      global_opt_ref = push(channel_socket, "optimize:mode:set", %{
        "mode" => "80_20"
      })
      
      assert_reply global_opt_ref, :ok, opt_config
      assert opt_config.mode == "80_20"
      
      # Should broadcast to all channels
      assert_broadcast "optimization:mode:changed", mode_change
      assert mode_change.mode == "80_20"
      
      # Apply reactor-specific optimization
      reactor_opt_ref = push(channel_socket, "reactor:optimize:apply", %{
        "type" => "auto"
      })
      
      assert_reply reactor_opt_ref, :ok, reactor_optimization
      assert reactor_optimization.type == "auto"
      
      # Execute pipeline with coordinated optimization
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "apply_reactor_optimization" => true,
        "input_data" => %{"test" => "coordination"}
      })
      
      assert_reply pipeline_ref, :ok, coordinated_response
      
      # Should show coordinated optimization benefits
      assert coordinated_response.reactor_optimization_applied == true
      assert coordinated_response.total_optimization_gain > 50
      assert coordinated_response.optimization_savings.efficiency_gain > 40
    end
  end
  
  describe "Error Recovery and Resilience" do
    test "recovers from stage failures gracefully", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Execute pipeline with simulated failures
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{
          "simulate_failure" => true,
          "failing_stage" => "reactor"
        }
      })
      
      assert_reply pipeline_ref, :ok, response
      
      # Should attempt recovery
      assert_broadcast "execution:error", error_data
      assert error_data.stage == "reactor"
      assert is_binary(error_data.reason)
      
      # Should broadcast recovery attempt
      assert_broadcast "execution:recovery", recovery_data
      assert recovery_data.stage == "reactor"
      assert recovery_data.attempt > 0
      
      # Should eventually succeed or provide graceful degradation
      assert_broadcast "execution:completed", final_result
      assert final_result.status in ["success", "partial_success"]
      assert final_result.recovery_applied == true
    end
    
    test "handles WebSocket disconnection and reconnection", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Start long-running pipeline
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{"long_running" => true},
        "estimated_duration" => 10000
      })
      
      assert_reply pipeline_ref, :ok, _response
      
      # Simulate disconnection
      Process.exit(channel_socket.channel_pid, :kill)
      
      # Reconnect
      {:ok, _response, new_channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20", "resume" => true}
      )
      
      # Should receive state recovery
      assert_receive %Phoenix.Socket.Message{
        event: "state:recovered",
        payload: state_data
      }
      
      assert state_data.pipeline_status in ["running", "completed"]
      assert is_map(state_data.current_metrics)
      
      # Should continue receiving updates
      assert_receive %Phoenix.Socket.Message{
        event: "execution:progress",
        payload: _progress_data
      }
    end
    
    test "handles rate limiting gracefully", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Send many requests rapidly to trigger rate limiting
      refs = for i <- 1..250 do
        push(channel_socket, "telemetry:subscribe", %{
          "metrics" => ["cpu"],
          "request_id" => i
        })
      end
      
      # Some should succeed
      successful_replies = Enum.count(refs, fn ref ->
        receive do
          %Phoenix.Socket.Reply{ref: ^ref, status: :ok} -> true
          %Phoenix.Socket.Reply{ref: ^ref, status: :error} -> false
        after
          1000 -> false
        end
      end)
      
      # Some should be rate limited
      rate_limited_replies = Enum.count(refs, fn ref ->
        receive do
          %Phoenix.Socket.Reply{ref: ^ref, status: :error, payload: %{reason: reason}} ->
            String.contains?(reason, "rate limit")
          _ -> false
        after
          1000 -> false
        end
      end)
      
      assert successful_replies > 0
      assert rate_limited_replies > 0
      assert successful_replies + rate_limited_replies == length(refs)
    end
  end
  
  describe "Performance and Scalability" do
    test "handles concurrent pipeline executions", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Execute multiple pipelines concurrently
      pipeline_refs = for i <- 1..10 do
        push(channel_socket, "pipeline:execute", %{
          "strategy" => "80_20",
          "input_data" => %{"batch_id" => i, "concurrent" => true},
          "execution_id" => "concurrent_#{i}"
        })
      end
      
      # All should receive responses
      successful_executions = Enum.count(pipeline_refs, fn ref ->
        receive do
          %Phoenix.Socket.Reply{ref: ^ref, status: :ok, payload: response} ->
            assert response.executed_stages == @critical_stages
            true
          %Phoenix.Socket.Reply{ref: ^ref, status: :error} ->
            false
        after
          5000 -> false
        end
      end)
      
      assert successful_executions >= 8  # Allow for some failures under load
      
      # Should receive completion broadcasts for all
      completion_count = for _i <- 1..successful_executions do
        assert_receive %Phoenix.Socket.Message{
          event: "execution:completed",
          payload: completion_data
        }
        
        assert completion_data.optimization_savings.efficiency_gain > 0
        1
      end |> Enum.sum()
      
      assert completion_count == successful_executions
    end
    
    test "maintains performance under telemetry load", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Subscribe to high-frequency telemetry
      telemetry_ref = push(channel_socket, "telemetry:subscribe", %{
        "metrics" => ["cpu", "memory", "latency"],
        "mode" => "real_time",
        "frequency" => "high"
      })
      
      assert_reply telemetry_ref, :ok, _response
      
      # Execute pipeline while telemetry is streaming
      start_time = :os.system_time(:millisecond)
      
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{"high_telemetry_load" => true}
      })
      
      assert_reply pipeline_ref, :ok, response
      
      end_time = :os.system_time(:millisecond)
      execution_time = end_time - start_time
      
      # Should maintain performance despite telemetry load
      assert execution_time < 3000  # Should complete within 3 seconds
      assert response.optimization_savings.efficiency_gain > 30
      
      # Should receive telemetry data without overwhelming the channel
      telemetry_messages = for _i <- 1..5 do
        assert_receive %Phoenix.Socket.Message{
          event: "telemetry:data",
          payload: _data
        }, 1000
        
        1
      end |> Enum.sum()
      
      assert telemetry_messages == 5
    end
  end
  
  describe "Integration with External Systems" do
    test "integrates with K8s deployment pipeline", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Execute pipeline with K8s integration
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "target" => "k8s",
        "k8s_config" => %{
          "namespace" => "cns-swarm",
          "deployment" => "ultrathink-pipeline",
          "replicas" => 3
        },
        "input_data" => %{"deploy_to_k8s" => true}
      })
      
      assert_reply pipeline_ref, :ok, response
      
      # Should reach K8s stage
      assert "k8s" in response.executed_stages
      
      # Should receive K8s-specific broadcasts
      assert_broadcast "stage:k8s:deployment", k8s_data
      assert k8s_data.namespace == "cns-swarm"
      assert k8s_data.status in ["deploying", "deployed"]
      
      # Should receive optimization metrics specific to K8s
      assert_broadcast "execution:completed", completion_data
      assert Map.has_key?(completion_data.metrics, "k8s_metrics")
      assert completion_data.metrics.k8s_metrics.replicas_optimized == true
    end
    
    test "validates data flow through all critical stages", %{socket: socket, swarm: swarm} do
      {:ok, _response, channel_socket} = subscribe_and_join(
        socket,
        SwarmChannel,
        "swarm:#{swarm.id}",
        %{"optimization" => "80_20"}
      )
      
      # Execute with data validation tracking
      pipeline_ref = push(channel_socket, "pipeline:execute", %{
        "strategy" => "80_20",
        "track_data_flow" => true,
        "input_data" => %{
          "original_format" => "owl",
          "target_format" => "jsonld",
          "validation_required" => true
        }
      })
      
      assert_reply pipeline_ref, :ok, response
      
      # Should track data transformations through each stage
      data_flow_broadcasts = for stage <- @critical_stages do
        assert_receive %Phoenix.Socket.Message{
          event: "data:flow:#{stage}",
          payload: flow_data
        }
        
        assert Map.has_key?(flow_data, "input_format")
        assert Map.has_key?(flow_data, "output_format")
        assert Map.has_key?(flow_data, "validation_status")
        assert flow_data.stage == stage
        
        flow_data
      end
      
      # Should maintain data integrity through pipeline
      assert hd(data_flow_broadcasts).input_format == "owl"
      assert List.last(data_flow_broadcasts).output_format == "jsonld"
      assert Enum.all?(data_flow_broadcasts, &(&1.validation_status == "passed"))
      
      # Final response should include complete data lineage
      assert response.data_lineage.stages == @critical_stages
      assert response.data_lineage.transformations_applied >= 5
      assert response.data_lineage.validation_checkpoints == 5
    end
  end
end