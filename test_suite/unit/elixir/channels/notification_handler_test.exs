defmodule CnsWeb.Channels.NotificationHandlerTest do
  @moduledoc """
  Unit tests for NotificationHandler with 80/20 optimization validation.
  Tests notification filtering, batching, and smart routing.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: true
  
  alias CnsWeb.Channels.NotificationHandler
  alias CnsWeb.UserSocket
  
  setup do
    socket = socket(UserSocket, "user_id", %{
      current_user: %{id: "test_user", role: "operator"},
      optimization_mode: "80_20",
      notification_preferences: %{
        batch_size: 5,
        max_frequency: 100
      }
    })
    
    %{socket: socket}
  end
  
  describe "notification broadcasting" do
    test "broadcasts critical notifications immediately", %{socket: socket} do
      payload = %{
        "level" => "critical",
        "channel" => "error_alerts",
        "message" => "Critical system error",
        "priority" => "high"
      }
      
      result = NotificationHandler.broadcast(payload, %{}, socket)
      
      assert {:reply, {:ok, %{broadcast: true}}, _socket} = result
    end
    
    test "filters non-critical notifications in 80/20 mode", %{socket: socket} do
      payload = %{
        "level" => "info",
        "channel" => "pipeline_events", 
        "message" => "Stage completed",
        "priority" => "low"
      }
      
      result = NotificationHandler.broadcast(payload, %{}, socket)
      
      # Should be filtered or batched
      assert {:reply, {:ok, %{filtered: true}}, _socket} = result
    end
    
    test "applies smart filtering for duplicate messages", %{socket: socket} do
      duplicate_payload = %{
        "level" => "info",
        "channel" => "performance_metrics",
        "message" => "CPU usage: 75%",
        "priority" => "medium"
      }
      
      # Send multiple duplicate notifications
      for _i <- 1..5 do
        NotificationHandler.broadcast(duplicate_payload, %{}, socket)
      end
      
      # Should deduplicate or batch
      result = NotificationHandler.get_notification_stats(%{}, %{}, socket)
      assert {:reply, {:ok, stats}, _socket} = result
      assert stats.duplicates_filtered > 0
    end
  end
  
  describe "notification batching" do
    test "batches low-priority notifications", %{socket: socket} do
      batch_payload = %{
        "channel" => "pipeline_events",
        "batch_size" => 3,
        "timeout_ms" => 500
      }
      
      result = NotificationHandler.configure_batching(batch_payload, %{}, socket)
      
      assert {:reply, {:ok, %{batching_enabled: true}}, _socket} = result
    end
    
    test "processes notification batch when size reached", %{socket: socket} do
      # Configure batching
      NotificationHandler.configure_batching(%{
        "channel" => "performance_metrics",
        "batch_size" => 3
      }, %{}, socket)
      
      # Add notifications to batch
      for i <- 1..3 do
        NotificationHandler.add_to_batch(%{
          "channel" => "performance_metrics",
          "message" => "Metric update #{i}"
        }, %{}, socket)
      end
      
      # Should trigger batch processing
      result = NotificationHandler.process_batch(%{
        "channel" => "performance_metrics"
      }, %{}, socket)
      
      assert {:reply, {:ok, %{batch_processed: true}}, _socket} = result
    end
    
    test "processes batch on timeout", %{socket: socket} do
      # Configure short timeout for testing
      NotificationHandler.configure_batching(%{
        "channel" => "debug_logs",
        "batch_size" => 10,
        "timeout_ms" => 100
      }, %{}, socket)
      
      # Add fewer notifications than batch size
      NotificationHandler.add_to_batch(%{
        "channel" => "debug_logs",
        "message" => "Debug message"
      }, %{}, socket)
      
      # Wait for timeout
      Process.sleep(150)
      
      # Should have processed due to timeout
      result = NotificationHandler.get_batch_status(%{
        "channel" => "debug_logs"
      }, %{}, socket)
      
      assert {:reply, {:ok, %{timeout_processed: true}}, _socket} = result
    end
  end
  
  describe "smart routing" do
    test "routes notifications based on channel priority weights", %{socket: socket} do
      high_priority_payload = %{
        "channel" => "ash_resources",
        "level" => "info",
        "message" => "Resource created"
      }
      
      low_priority_payload = %{
        "channel" => "pipeline_events",
        "level" => "info", 
        "message" => "Stage started"
      }
      
      # Should prioritize ash_resources over pipeline_events
      result1 = NotificationHandler.route_notification(high_priority_payload, %{}, socket)
      result2 = NotificationHandler.route_notification(low_priority_payload, %{}, socket)
      
      assert {:reply, {:ok, %{priority_score: score1}}, _socket} = result1
      assert {:reply, {:ok, %{priority_score: score2}}, _socket} = result2
      
      assert score1 > score2
    end
    
    test "applies channel-specific routing rules", %{socket: socket} do
      payload = %{
        "channel" => "reactor_workflows",
        "routing_rules" => %{
          "min_level" => "info",
          "batch_enabled" => false,
          "immediate_delivery" => true
        }
      }
      
      result = NotificationHandler.apply_routing_rules(payload, %{}, socket)
      
      assert {:reply, {:ok, %{rules_applied: true}}, _socket} = result
    end
    
    test "handles notification overflow gracefully", %{socket: socket} do
      overflow_payload = %{
        "channel" => "performance_metrics",
        "rate_limit" => 50
      }
      
      # Simulate overflow by rapid notifications
      for _i <- 1..100 do
        NotificationHandler.broadcast(%{
          "channel" => "performance_metrics",
          "level" => "info",
          "message" => "Performance update"
        }, %{}, socket)
      end
      
      result = NotificationHandler.handle_overflow(overflow_payload, %{}, socket)
      
      assert {:reply, {:ok, %{overflow_handled: true}}, _socket} = result
    end
  end
  
  describe "80/20 optimization filtering" do
    test "applies 80/20 filter to notification channels", %{socket: socket} do
      # Critical channels (20% that handle 80% of important notifications)
      critical_channels = ["error_alerts", "ash_resources", "reactor_workflows"]
      
      for channel <- critical_channels do
        payload = %{
          "channel" => channel,
          "level" => "info",
          "message" => "Important notification"
        }
        
        result = NotificationHandler.apply_80_20_filter(payload, %{}, socket)
        assert {:reply, {:ok, %{passed_filter: true}}, _socket} = result
      end
    end
    
    test "filters non-critical channels appropriately", %{socket: socket} do
      non_critical_payload = %{
        "channel" => "debug_logs",
        "level" => "debug",
        "message" => "Debug information"
      }
      
      result = NotificationHandler.apply_80_20_filter(non_critical_payload, %{}, socket)
      
      # Should be filtered or marked for batching
      assert {:reply, {:ok, response}, _socket} = result
      assert response.passed_filter == false or response.batched == true
    end
    
    test "calculates optimization metrics", %{socket: socket} do
      result = NotificationHandler.get_optimization_metrics(%{}, %{}, socket)
      
      assert {:reply, {:ok, metrics}, _socket} = result
      assert Map.has_key?(metrics, :total_processed)
      assert Map.has_key?(metrics, :filtered_count)
      assert Map.has_key?(metrics, :optimization_ratio)
      
      # Should show efficiency improvement
      if metrics.total_processed > 0 do
        assert metrics.optimization_ratio >= 0.2
      end
    end
  end
  
  describe "adaptive learning" do
    test "learns from notification patterns", %{socket: socket} do
      pattern_data = %{
        "channel" => "ash_resources",
        "frequency" => 45,
        "user_engagement" => 0.8,
        "error_rate" => 0.02
      }
      
      result = NotificationHandler.learn_pattern(pattern_data, %{}, socket)
      
      assert {:reply, {:ok, %{pattern_learned: true}}, _socket} = result
    end
    
    test "adapts filtering based on learned patterns", %{socket: socket} do
      # Simulate learning that users ignore certain notifications
      learning_payload = %{
        "channel" => "performance_metrics",
        "user_interactions" => [
          %{"action" => "dismiss", "timestamp" => DateTime.utc_now()},
          %{"action" => "dismiss", "timestamp" => DateTime.utc_now()},
          %{"action" => "dismiss", "timestamp" => DateTime.utc_now()}
        ]
      }
      
      NotificationHandler.record_user_interaction(learning_payload, %{}, socket)
      
      # Should adapt filtering to reduce these notifications
      result = NotificationHandler.get_adaptive_config(%{
        "channel" => "performance_metrics"
      }, %{}, socket)
      
      assert {:reply, {:ok, config}, _socket} = result
      assert config.filter_strength > 0.5
    end
  end
  
  describe "real-time streaming" do
    test "subscribes to notification streams", %{socket: socket} do
      payload = %{
        "channels" => ["ash_resources", "reactor_workflows"],
        "stream_type" => "real_time"
      }
      
      result = NotificationHandler.subscribe_stream(payload, %{}, socket)
      
      assert {:reply, {:ok, %{subscribed: true}}, _socket} = result
    end
    
    test "unsubscribes from notification streams", %{socket: socket} do
      # First subscribe
      NotificationHandler.subscribe_stream(%{
        "channels" => ["pipeline_events"]
      }, %{}, socket)
      
      # Then unsubscribe
      result = NotificationHandler.unsubscribe_stream(%{
        "channels" => ["pipeline_events"]
      }, %{}, socket)
      
      assert {:reply, {:ok, %{unsubscribed: true}}, _socket} = result
    end
    
    test "handles stream backpressure", %{socket: socket} do
      backpressure_payload = %{
        "channel" => "high_frequency_metrics",
        "max_rate" => 10
      }
      
      result = NotificationHandler.configure_backpressure(backpressure_payload, %{}, socket)
      
      assert {:reply, {:ok, %{backpressure_enabled: true}}, _socket} = result
    end
  end
  
  describe "error handling and resilience" do
    test "handles malformed notifications gracefully", %{socket: socket} do
      malformed_payload = %{
        "invalid_field" => "test",
        "missing_channel" => true
      }
      
      result = NotificationHandler.broadcast(malformed_payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: _reason}}, _socket} = result
    end
    
    test "recovers from channel failures", %{socket: socket} do
      failure_payload = %{
        "channel" => "failing_channel",
        "simulate_failure" => true
      }
      
      result = NotificationHandler.handle_channel_failure(failure_payload, %{}, socket)
      
      assert {:reply, {:ok, %{recovery_initiated: true}}, _socket} = result
    end
    
    test "maintains service during high load", %{socket: socket} do
      high_load_payload = %{
        "load_level" => "extreme",
        "notification_rate" => 1000
      }
      
      result = NotificationHandler.handle_high_load(high_load_payload, %{}, socket)
      
      assert {:reply, {:ok, %{load_balanced: true}}, _socket} = result
    end
  end
  
  describe "telemetry integration" do
    test "emits telemetry events for notification processing", %{socket: socket} do
      # Set up telemetry handler
      test_pid = self()
      
      :telemetry.attach("test_notification_processing",
        [:cns, :notification, :processed],
        fn _event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, measurements, metadata})
        end,
        nil
      )
      
      NotificationHandler.broadcast(%{
        "channel" => "test_channel",
        "level" => "info",
        "message" => "Test notification"
      }, %{}, socket)
      
      # Should receive telemetry event
      assert_receive {:telemetry, measurements, metadata}, 1000
      
      assert measurements.processing_time_ms > 0
      assert metadata.channel == "test_channel"
      
      :telemetry.detach("test_notification_processing")
    end
  end
end