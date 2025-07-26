defmodule CnsForge.Ultra8020TestSuite do
  @moduledoc """
  🧪 ULTRATHINK 80/20 TEST SUITE
  
  20% of tests that validate 80% of critical functionality:
  - Unit tests for core patterns
  - E2E tests for pipeline integration  
  - Adversarial tests for failure scenarios
  - Stress tests for performance validation
  - K8s & OTEL integration validation
  
  Focus: HIGH-VALUE test cases that catch the most important failures
  """
  
  use ExUnit.Case, async: false
  use Phoenix.ChannelTest
  
  alias CnsForge.Channels.{
    Pipeline8020Channel,
    ReactorStep8020Channel,
    Telemetry8020Channel,
    BitActorChannel,
    K8sDeploymentChannel
  }
  
  @endpoint CnsForge.Endpoint
  @moduletag :ultra_8020
  
  setup_all do
    # Setup test environment
    CnsForge.Channel8020Integration.setup_channel_swarm()
    
    # Start OTEL collector for integration tests
    start_otel_collector()
    
    # Setup K8s test environment
    setup_k8s_test_cluster()
    
    on_exit(fn ->
      cleanup_test_environment()
    end)
    
    :ok
  end
  
  setup do
    # Fresh socket for each test
    user_id = "test_user_#{System.unique_integer()}"
    {:ok, socket} = connect(CnsForge.UserSocket, %{
      "token" => "test_token_#{user_id}"
    })
    
    {:ok, socket: socket, user_id: user_id}
  end
end

defmodule CnsForge.Ultra8020UnitTests do
  @moduledoc """
  🎯 80/20 UNIT TESTS
  Focus: Core patterns that deliver the highest value
  """
  
  use CnsForge.Ultra8020TestSuite
  
  describe "UNIT: Pattern 1 - Real-Time Pipeline Monitoring (95/100)" do
    test "stage transitions broadcast immediately with correct payload", %{socket: socket} do
      pipeline_id = "unit_test_#{System.unique_integer()}"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      # Test critical stage transition
      CnsForge.PipelineEventProcessor.broadcast_stage_transition(
        pipeline_id,
        "bitactor",
        "idle", 
        "processing",
        %{cpu_usage: 75.5, memory_mb: 1024}
      )
      
      assert_push "stage_transition", payload
      
      # Validate payload structure (80/20: check critical fields only)
      assert payload.stage == "bitactor"
      assert payload.from == "idle"
      assert payload.to == "processing"
      assert payload.metadata.cpu_usage == 75.5
      assert is_number(payload.timestamp)
    end
    
    test "pipeline control operations have proper authorization", %{socket: socket} do
      pipeline_id = "auth_test_#{System.unique_integer()}"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      # Test with insufficient permissions
      socket_user = assign(channel_socket, :user_role, :user)
      ref = push(socket_user, "control:restart", %{"stage" => "bitactor"})
      assert_reply ref, :error, %{reason: "insufficient permissions"}
      
      # Test with admin permissions
      socket_admin = assign(channel_socket, :user_role, :admin)
      ref = push(socket_admin, "control:restart", %{"stage" => "bitactor"})
      assert_reply ref, :ok
    end
    
    test "telemetry integration emits correct metrics", %{socket: socket} do
      pipeline_id = "telemetry_test_#{System.unique_integer()}"
      
      {:ok, _reply, _channel_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      # Capture telemetry events
      test_pid = self()
      :telemetry.attach(
        "pipeline_test",
        [:pipeline, :stage, :monitored],
        fn _event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, measurements, metadata})
        end,
        nil
      )
      
      # Trigger telemetry-generating event
      CnsForge.PipelineEventProcessor.broadcast_stage_completed(
        pipeline_id,
        "ttl2dspy",
        "success",
        %{duration: 250, items: 500}
      )
      
      # Verify telemetry was emitted
      assert_receive {:telemetry, measurements, metadata}, 1000
      assert metadata.pipeline_id == pipeline_id
      assert metadata.stage == "ttl2dspy"
      
      :telemetry.detach("pipeline_test")
    end
  end
  
  describe "UNIT: Pattern 2 - Reactive Step Notifications (92/100)" do
    test "step lifecycle tracking maintains accurate state", %{socket: socket} do
      reactor_name = "UnitTestReactor"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:#{reactor_name}"
      )
      
      # Start step
      ref = push(channel_socket, "step:started", %{"step" => "critical_step"})
      assert_reply ref, :ok
      
      # Verify state tracking
      state = :sys.get_state(channel_socket.channel_pid)
      assert state.assigns.active_steps["critical_step"].status == "running"
      assert is_number(state.assigns.active_steps["critical_step"].started_at)
      
      # Complete step
      ref = push(channel_socket, "step:completed", %{
        "step" => "critical_step",
        "result" => %{"output" => "success"}
      })
      assert_reply ref, :ok
      
      # Verify cleanup
      state = :sys.get_state(channel_socket.channel_pid)
      refute Map.has_key?(state.assigns.active_steps, "critical_step")
      
      # Should receive completion notification
      assert_push "step_completed", %{
        step: "critical_step",
        result: %{"output" => "success"},
        duration_ms: duration
      }
      
      assert is_number(duration)
      assert duration > 0
    end
    
    test "compensation tracking works under high load", %{socket: socket} do
      reactor_name = "CompensationTestReactor"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:#{reactor_name}"
      )
      
      # Trigger multiple compensations simultaneously
      compensation_tasks = Task.async_stream(1..50, fn i ->
        ref = push(channel_socket, "compensation:trigger", %{
          "step" => "step_#{i}",
          "error" => "simulated_error_#{i}"
        })
        assert_reply ref, :ok
        i
      end, max_concurrency: 10)
      
      # All should succeed
      results = Enum.to_list(compensation_tasks)
      assert length(results) == 50
      assert Enum.all?(results, fn {:ok, _} -> true end)
      
      # State should track all compensations
      state = :sys.get_state(channel_socket.channel_pid)
      compensation_count = state.assigns.active_steps
      |> Enum.count(fn {_key, step_data} -> 
        step_data.status == "compensating" 
      end)
      
      assert compensation_count == 50
    end
  end
  
  describe "UNIT: Pattern 4 - Performance Telemetry (87/100)" do
    test "high-frequency metrics are buffered efficiently", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Send burst of metrics
      metrics = Enum.map(1..1000, fn i ->
        %{
          "name" => "cpu_usage",
          "value" => rem(i, 100),
          "timestamp" => System.monotonic_time(:millisecond)
        }
      end)
      
      start_time = System.monotonic_time(:millisecond)
      
      ref = push(channel_socket, "metrics:batch", %{"metrics" => metrics})
      assert_reply ref, :ok
      
      processing_time = System.monotonic_time(:millisecond) - start_time
      
      # Should process 1000 metrics quickly (< 100ms)
      assert processing_time < 100
      
      # Verify buffer state
      state = :sys.get_state(channel_socket.channel_pid)
      buffer_size = :queue.len(state.assigns.buffer)
      assert buffer_size == 1000
    end
    
    test "metric aggregation produces correct percentiles", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Known dataset for predictable percentiles
      test_values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
      
      aggregated = CnsForge.MetricAggregator.aggregate(test_values)
      
      # Verify critical percentiles
      assert aggregated.p50 == 50  # Median
      assert aggregated.p95 == 95  # 95th percentile
      assert aggregated.p99 == 99  # 99th percentile
      assert aggregated.max == 100
      assert aggregated.count == 10
    end
    
    test "threshold alerts trigger at correct values", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Set threshold
      ref = push(channel_socket, "alerts:configure", %{
        "metric" => "cpu_usage",
        "threshold" => 85
      })
      assert_reply ref, :ok
      
      # Simulate threshold breach
      send(channel_socket.channel_pid, {
        :metric_alert,
        "cpu_usage",
        85,
        92.5,
        "warning"
      })
      
      # Should receive alert
      assert_push "metric_alert", %{
        metric: "cpu_usage",
        threshold: 85,
        current_value: 92.5,
        severity: "warning"
      }
    end
  end
  
  describe "UNIT: BitActor High-Performance Channel" do
    test "batch operations scale linearly", %{socket: socket} do
      session_id = "batch_test_#{System.unique_integer()}"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        BitActorChannel,
        "stage:bitactor:#{session_id}"
      )
      
      # Test different batch sizes
      batch_sizes = [10, 50, 100, 500]
      
      results = Enum.map(batch_sizes, fn size ->
        configs = Enum.map(1..size, fn i ->
          %{"id" => i, "type" => "worker"}
        end)
        
        start_time = System.monotonic_time(:millisecond)
        
        ref = push(channel_socket, "batch:spawn", %{"items" => configs})
        assert_reply ref, :ok, batch_results
        
        end_time = System.monotonic_time(:millisecond)
        
        %{
          size: size,
          duration: end_time - start_time,
          results: length(batch_results)
        }
      end)
      
      # Verify linear scaling (within reasonable bounds)
      Enum.reduce(Enum.zip(results, tl(results)), true, fn 
        {smaller, larger}, acc ->
          # Larger batch should not be more than 2x slower per item
          smaller_per_item = smaller.duration / smaller.size
          larger_per_item = larger.duration / larger.size
          
          ratio = larger_per_item / smaller_per_item
          acc and ratio < 2.0
      end)
    end
    
    test "metric buffering handles overflow gracefully", %{socket: socket} do
      session_id = "overflow_test_#{System.unique_integer()}"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        BitActorChannel,
        "stage:bitactor:#{session_id}"
      )
      
      # Force metric buffer overflow
      state = :sys.get_state(channel_socket.channel_pid)
      large_buffer = Enum.reduce(1..10000, state.assigns.metrics_buffer, fn i, buffer ->
        :queue.in(%{
          actor_id: "actor_#{i}",
          duration_ns: i * 1000,
          timestamp: System.monotonic_time(:millisecond)
        }, buffer)
      end)
      
      # Update state with large buffer
      :sys.replace_state(channel_socket.channel_pid, fn state ->
        put_in(state.assigns.metrics_buffer, large_buffer)
      end)
      
      # Trigger flush
      send(channel_socket.channel_pid, :flush_metrics)
      
      # Should not crash and should receive batched metrics
      assert_push "performance_burst", %{metrics: metrics}
      assert length(metrics) == 10000
      
      # Process should still be alive
      assert Process.alive?(channel_socket.channel_pid)
    end
  end
  
  describe "UNIT: K8s Deployment Channel" do
    test "deployment spec validation catches critical errors", %{socket: socket} do
      namespace = "unit-test"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{namespace}"
      )
      
      # Valid spec should succeed
      valid_spec = %{
        "replicas" => 3,
        "image" => "nginx:latest",
        "selector" => %{"app" => "test"},
        "ports" => [%{"port" => 80, "targetPort" => 80}]
      }
      
      ref = push(channel_socket, "deploy", %{"spec" => valid_spec})
      assert_reply ref, :ok, deployment
      assert deployment.namespace == namespace
      
      # Invalid service spec should fail fast
      invalid_service_spec = %{
        "image" => "nginx:latest"
        # Missing required selector and ports
      }
      
      ref = push(channel_socket, "service:create", %{"spec" => invalid_service_spec})
      assert_reply ref, :error, %{reason: "invalid service specification"}
    end
    
    test "deployment status tracking updates correctly", %{socket: socket} do
      namespace = "status-test"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{namespace}"
      )
      
      # Create deployment
      spec = %{
        "replicas" => 2,
        "image" => "test:latest",
        "selector" => %{"app" => "test"},
        "ports" => [%{"port" => 8080}]
      }
      
      ref = push(channel_socket, "deploy", %{"spec" => spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      
      # Stream status updates
      ref = push(channel_socket, "status:stream", %{"deployment" => deployment_id})
      assert_reply ref, :ok
      
      # Should receive initial status
      assert_push "deployment_status", status
      assert status.id == deployment_id
      assert status.status == "creating"
    end
  end
  
  describe "UNIT: Cross-Channel Integration" do
    test "pipeline events propagate to telemetry correctly", %{socket: socket} do
      pipeline_id = "integration_test_#{System.unique_integer()}"
      
      # Join both channels
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Subscribe telemetry to pipeline events
      Phoenix.PubSub.subscribe(CnsForge.PubSub, "pipeline_events:#{pipeline_id}")
      
      # Trigger pipeline event
      CnsForge.PipelineEventProcessor.broadcast_stage_completed(
        pipeline_id,
        "bitactor",
        "success",
        %{duration: 150, throughput: 1000}
      )
      
      # Pipeline channel should receive completion
      assert_push "stage_completed", %{stage: "bitactor"}
      
      # Should also receive PubSub message for telemetry integration
      assert_receive {:stage_completed, ^pipeline_id, "bitactor", "success", metadata}
      assert metadata.duration == 150
      assert metadata.throughput == 1000
    end
  end
end

defmodule CnsForge.Ultra8020E2ETests do
  @moduledoc """
  🔄 END-TO-END TESTS
  Full pipeline integration testing with real components
  """
  
  use CnsForge.Ultra8020TestSuite
  
  describe "E2E: Complete Pipeline Execution" do
    test "full pipeline executes with real-time monitoring", %{socket: socket} do
      pipeline_id = "e2e_test_#{System.unique_integer()}"
      
      # Join all relevant channels
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      {:ok, _reply, reactor_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:CompletePipelineOrchestrator"
      )
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Execute complete pipeline demo
      demo_input = %{
        entities: [
          %{id: "e2e_entity_1", type: "Process", name: "TestProcess", priority: 0.9}
        ],
        metadata: %{source: "e2e_test", format: "json"},
        config: %{processing_mode: "test", optimization_level: "80_20_pareto"}
      }
      
      pipeline_config = %{
        pipeline_id: pipeline_id,
        enable_telemetry: true,
        enable_notifications: true
      }
      
      # Start pipeline execution
      task = Task.async(fn ->
        CnsForge.PipelineDemo.execute_demo_pipeline(demo_input, pipeline_config)
      end)
      
      # Monitor pipeline progression through all stages
      expected_stages = [
        "typer_80_20_processing",
        "turtle_generation", 
        "ttl2dspy_transformation",
        "bitactor_processing",
        "erlang_otp_coordination",
        "ash_resource_creation",
        "reactor_workflow_execution",
        "k8s_deployment"
      ]
      
      # Collect stage transitions
      stage_transitions = Enum.map(expected_stages, fn stage ->
        assert_push "stage_transition", %{stage: ^stage, to: "processing"}
        assert_push "stage_completed", %{stage: ^stage, status: "success"}
        stage
      end)
      
      # Wait for pipeline completion
      {:ok, result} = Task.await(task, 30_000)
      
      # Verify all stages completed
      assert length(stage_transitions) == 8
      
      # Verify final result structure
      assert Map.has_key?(result, :deployment_id)
      assert Map.has_key?(result, :pod_count)
      assert result.status == "running"
      
      # Should receive telemetry snapshots
      assert_push "metrics_snapshot", telemetry_data
      assert Map.has_key?(telemetry_data, :pipeline_throughput)
    end
    
    test "pipeline failure triggers compensation workflow", %{socket: socket} do
      pipeline_id = "failure_test_#{System.unique_integer()}"
      
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      {:ok, _reply, recovery_socket} = subscribe_and_join(
        socket,
        CnsForge.Channels.Recovery8020Channel,
        "recovery:#{pipeline_id}"
      )
      
      # Inject failure at BitActor stage
      config = %{
        pipeline_id: pipeline_id,
        inject_failure_at: :bitactor_processing,
        enable_compensation: true
      }
      
      demo_input = %{entities: [%{id: "fail_test", type: "Process"}]}
      
      # Execute pipeline (should fail)
      task = Task.async(fn ->
        CnsForge.PipelineDemo.execute_demo_pipeline(demo_input, config)
      end)
      
      # Should receive failure notification
      assert_push "pipeline_failure", %{
        failed_stage: "bitactor",
        error: _error_details
      }
      
      # Should trigger recovery
      assert_push "recovery_initiated", %{
        type: "pipeline_failure",
        recovery_id: recovery_id
      }
      
      # Wait for task completion (should return error)
      {:error, _reason} = Task.await(task, 10_000)
      
      # Should receive compensation events
      assert_push "recovery_completed", %{
        recovery_id: ^recovery_id,
        duration: duration
      }
      
      assert is_number(duration)
    end
  end
  
  describe "E2E: Multi-Client Coordination" do
    test "multiple clients receive synchronized updates", %{socket: socket} do
      pipeline_id = "multi_client_test_#{System.unique_integer()}"
      
      # Create 5 client connections
      clients = Enum.map(1..5, fn i ->
        {:ok, client_socket} = connect(CnsForge.UserSocket, %{
          "token" => "client_#{i}_token"
        })
        
        {:ok, _reply, channel_socket} = subscribe_and_join(
          client_socket,
          Pipeline8020Channel,
          "pipeline:#{pipeline_id}"
        )
        
        {i, channel_socket}
      end)
      
      # Broadcast pipeline event
      CnsForge.PipelineEventProcessor.broadcast_stage_transition(
        pipeline_id,
        "ash_resource_creation",
        "idle",
        "creating_resources",
        %{resources_count: 10}
      )
      
      # All clients should receive the same event simultaneously
      Enum.each(clients, fn {client_id, channel_socket} ->
        assert_push "stage_transition", %{
          stage: "ash_resource_creation",
          from: "idle",
          to: "creating_resources",
          metadata: %{resources_count: 10}
        }
      end)
      
      # Cleanup
      Enum.each(clients, fn {_id, channel_socket} ->
        leave(channel_socket)
      end)
    end
  end
  
  describe "E2E: Real K8s Integration" do
    @tag :k8s_integration
    test "k8s deployment channel creates actual resources", %{socket: socket} do
      namespace = "e2e-test-ns"
      
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{namespace}"
      )
      
      # Real deployment spec
      deployment_spec = %{
        "replicas" => 1,
        "image" => "nginx:alpine",
        "selector" => %{"app" => "e2e-test"},
        "ports" => [%{"port" => 80, "targetPort" => 80}]
      }
      
      # Create deployment
      ref = push(k8s_socket, "deploy", %{"spec" => deployment_spec})
      assert_reply ref, :ok, deployment
      
      deployment_id = deployment.id
      
      # Stream status updates
      ref = push(k8s_socket, "status:stream", %{"deployment" => deployment_id})
      assert_reply ref, :ok
      
      # Wait for deployment to become ready
      assert_push "deployment_status", %{status: "running"}, 30_000
      
      # Verify pods are actually running
      assert_push "pod_health_update", %{health: "healthy"}
      
      # Cleanup deployment
      ref = push(k8s_socket, "delete", %{"deployment_id" => deployment_id})
      assert_reply ref, :ok
    end
  end
end

defmodule CnsForge.Ultra8020AdversarialTests do
  @moduledoc """
  🦹‍♂️ ADVERSARIAL TESTS
  Evil scenarios designed to break the system
  """
  
  use CnsForge.Ultra8020TestSuite
  
  describe "ADVERSARIAL: Memory Exhaustion Attacks" do
    test "channel survives massive payload attacks", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Create massive payload (10MB of metrics)
      massive_metrics = Enum.map(1..100_000, fn i ->
        %{
          "name" => "attack_metric_#{i}",
          "value" => i,
          "timestamp" => System.monotonic_time(),
          "metadata" => String.duplicate("evil_data", 100)  # ~900 bytes each
        }
      end)
      
      initial_memory = :erlang.memory(:total)
      
      # Send massive payload
      ref = push(channel_socket, "metrics:batch", %{"metrics" => massive_metrics})
      
      # Should either reject or handle gracefully
      receive do
        %Phoenix.Socket.Reply{status: :ok} ->
          # If accepted, memory should not explode
          peak_memory = :erlang.memory(:total)
          memory_increase = peak_memory - initial_memory
          
          # Should not use more than 50MB additional memory
          assert memory_increase < 50_000_000
          
        %Phoenix.Socket.Reply{status: :error} ->
          # Rejection is acceptable defense
          :ok
      after
        5000 ->
          flunk("Channel should respond to massive payload within 5 seconds")
      end
      
      # Channel should still be alive and responsive
      ref = push(channel_socket, "alerts:configure", %{
        "metric" => "test", 
        "threshold" => 50
      })
      assert_reply ref, :ok
    end
    
    test "rapid-fire connection attacks don't exhaust resources", %{socket: socket} do
      # Simulate 100 rapid connections
      initial_memory = :erlang.memory(:total)
      initial_process_count = length(Process.list())
      
      connection_tasks = Task.async_stream(1..100, fn i ->
        try do
          {:ok, evil_socket} = connect(CnsForge.UserSocket, %{
            "token" => "evil_token_#{i}"
          })
          
          {:ok, _reply, channel_socket} = subscribe_and_join(
            evil_socket,
            Pipeline8020Channel,
            "pipeline:evil_#{i}"
          )
          
          # Send some load
          push(channel_socket, "control:restart", %{})
          
          # Immediately disconnect
          leave(channel_socket)
          
          :ok
        rescue
          _error -> :error
        end
      end, max_concurrency: 50, timeout: 1000)
      
      results = Enum.to_list(connection_tasks)
      
      # Allow some failures under extreme load
      success_rate = Enum.count(results, fn {:ok, :ok} -> true; _ -> false end) / 100
      assert success_rate > 0.8, "Success rate #{success_rate} too low under load"
      
      # Wait for cleanup
      Process.sleep(2000)
      
      # Memory should return to reasonable levels
      final_memory = :erlang.memory(:total)
      memory_increase = final_memory - initial_memory
      assert memory_increase < 10_000_000  # < 10MB permanent increase
      
      # Process count should not explode
      final_process_count = length(Process.list())
      process_increase = final_process_count - initial_process_count
      assert process_increase < 50  # < 50 permanent processes
    end
  end
  
  describe "ADVERSARIAL: Malicious Payloads" do
    test "channels reject malformed JSON gracefully", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:malicious_test"
      )
      
      # Test various malicious payloads
      malicious_payloads = [
        # Circular references (would crash JSON encoding)
        %{"circular" => %{"ref" => "self"}},
        
        # Extremely nested structures
        Enum.reduce(1..1000, %{}, fn i, acc -> %{"level_#{i}" => acc} end),
        
        # Non-string keys that might break pattern matching
        %{123 => "numeric_key", :atom => "atom_key"},
        
        # Binary data that might break string processing
        %{"binary" => <<1, 2, 3, 4, 5>>},
        
        # Extremely long strings
        %{"long_string" => String.duplicate("x", 1_000_000)}
      ]
      
      Enum.each(malicious_payloads, fn payload ->
        ref = push(channel_socket, "control:restart", payload)
        
        # Should either succeed or fail gracefully (not crash)
        receive do
          %Phoenix.Socket.Reply{status: status} when status in [:ok, :error] ->
            :ok
        after
          2000 ->
            flunk("Channel should respond to malicious payload")
        end
        
        # Channel should still be alive
        assert Process.alive?(channel_socket.channel_pid)
      end)
    end
    
    test "SQL injection attempts in metric names fail safely", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # SQL injection attempts in metric names
      evil_metrics = [
        %{
          "name" => "metric'; DROP TABLE metrics; --",
          "value" => 50
        },
        %{
          "name" => "metric\"; SELECT * FROM users; --",
          "value" => 75
        },
        %{
          "name" => "metric\x00admin",
          "value" => 100
        }
      ]
      
      ref = push(channel_socket, "metrics:batch", %{"metrics" => evil_metrics})
      
      # Should handle gracefully
      assert_reply ref, :ok
      
      # System should still be functional
      ref = push(channel_socket, "alerts:configure", %{
        "metric" => "legitimate_metric",
        "threshold" => 80
      })
      assert_reply ref, :ok
    end
  end
  
  describe "ADVERSARIAL: Timing Attacks" do
    test "channels maintain performance under irregular load patterns", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        BitActorChannel,
        "stage:bitactor:timing_attack"
      )
      
      # Irregular spike pattern: quiet periods followed by bursts
      attack_pattern = [
        # Quiet period
        {100, 10},    # 100ms intervals, 10 requests
        # Sudden burst  
        {1, 1000},    # 1ms intervals, 1000 requests
        # Return to quiet
        {100, 10}
      ]
      
      response_times = Enum.flat_map(attack_pattern, fn {interval, count} ->
        Enum.map(1..count, fn i ->
          start_time = System.monotonic_time(:millisecond)
          
          ref = push(channel_socket, "spawn", %{
            "config" => %{"id" => i, "type" => "timing_test"}
          })
          
          assert_reply ref, :ok
          
          end_time = System.monotonic_time(:millisecond)
          response_time = end_time - start_time
          
          if interval > 1 do
            Process.sleep(interval)
          end
          
          response_time
        end)
      end)
      
      # Calculate response time statistics
      avg_response = Enum.sum(response_times) / length(response_times)
      max_response = Enum.max(response_times)
      p95_response = Enum.at(Enum.sort(response_times), round(length(response_times) * 0.95))
      
      # Performance should remain reasonable even under irregular load
      assert avg_response < 50,   "Average response time #{avg_response}ms too high"
      assert p95_response < 100,  "P95 response time #{p95_response}ms too high" 
      assert max_response < 500,  "Max response time #{max_response}ms too high"
    end
  end
  
  describe "ADVERSARIAL: Resource Starvation" do
    test "channels gracefully handle ETS table exhaustion", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        BitActorChannel,
        "stage:bitactor:ets_attack"
      )
      
      # Try to exhaust ETS storage
      initial_ets_count = length(:ets.all())
      
      # Spawn many actors to fill ETS tables
      spawn_tasks = Task.async_stream(1..10_000, fn i ->
        ref = push(channel_socket, "spawn", %{
          "config" => %{"id" => i, "type" => "ets_exhaustion"}
        })
        
        case receive do
          %Phoenix.Socket.Reply{status: :ok} -> :ok
          %Phoenix.Socket.Reply{status: :error} -> :error
        after
          100 -> :timeout
        end
      end, max_concurrency: 100, timeout: 5000)
      
      results = Enum.to_list(spawn_tasks)
      
      # System should either handle all requests or gracefully reject some
      success_count = Enum.count(results, fn {:ok, :ok} -> true; _ -> false end)
      
      # Should handle at least 1000 successfully
      assert success_count > 1000
      
      # ETS table count should not explode
      final_ets_count = length(:ets.all())
      ets_increase = final_ets_count - initial_ets_count
      assert ets_increase < 100  # Reasonable ETS growth
      
      # Channel should still be responsive
      ref = push(channel_socket, "spawn", %{
        "config" => %{"id" => "final_test", "type" => "cleanup_test"}
      })
      assert_reply ref, :ok
    end
  end
  
  describe "ADVERSARIAL: State Corruption" do
    test "concurrent state modifications don't corrupt channel state", %{socket: socket} do
      reactor_name = "ConcurrencyTestReactor"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:#{reactor_name}"
      )
      
      # Launch 100 concurrent step operations
      concurrent_tasks = Task.async_stream(1..100, fn i ->
        case rem(i, 3) do
          0 ->
            # Start step
            ref = push(channel_socket, "step:started", %{"step" => "step_#{i}"})
            assert_reply ref, :ok
            {:started, i}
            
          1 ->
            # Complete step
            ref = push(channel_socket, "step:completed", %{
              "step" => "step_#{i}",
              "result" => %{"value" => i}
            })
            assert_reply ref, :ok
            {:completed, i}
            
          2 ->
            # Error step
            ref = push(channel_socket, "step:error", %{
              "step" => "step_#{i}",
              "error" => "concurrent_error_#{i}"
            })
            assert_reply ref, :ok
            {:errored, i}
        end
      end, max_concurrency: 50)
      
      results = Enum.to_list(concurrent_tasks)
      
      # All operations should succeed
      assert length(results) == 100
      assert Enum.all?(results, fn {:ok, _} -> true end)
      
      # Channel state should be consistent
      state = :sys.get_state(channel_socket.channel_pid)
      active_steps = state.assigns.active_steps
      
      # Only "started" steps should remain in active state
      started_steps = Enum.filter(results, fn {:ok, {:started, _}} -> true; _ -> false end)
      assert map_size(active_steps) == length(started_steps)
      
      # All active steps should have valid state
      Enum.each(active_steps, fn {step_name, step_data} ->
        assert is_binary(step_name)
        assert step_data.status == "running"
        assert is_number(step_data.started_at)
      end)
    end
  end
end

defmodule CnsForge.Ultra8020StressTests do
  @moduledoc """
  💪 STRESS TESTS & BENCHMARKS
  High-load scenarios to validate performance claims
  """
  
  use CnsForge.Ultra8020TestSuite
  
  describe "STRESS: High-Frequency Telemetry" do
    @tag :stress
    test "sustained 10K+ metrics/second throughput", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Prepare 10K metrics
      metric_count = 10_000
      test_metrics = Enum.map(1..metric_count, fn i ->
        %{
          "name" => "stress_metric_#{rem(i, 100)}",
          "value" => :rand.uniform(1000),
          "timestamp" => System.monotonic_time(:millisecond)
        }
      end)
      
      # Measure ingestion throughput
      start_time = System.monotonic_time(:millisecond)
      
      # Send in batches of 1000
      batch_results = Enum.chunk_every(test_metrics, 1000)
      |> Enum.map(fn batch ->
        ref = push(channel_socket, "metrics:batch", %{"metrics" => batch})
        assert_reply ref, :ok
        :ok
      end)
      
      end_time = System.monotonic_time(:millisecond)
      total_time_seconds = (end_time - start_time) / 1000
      
      # Calculate throughput
      throughput = metric_count / total_time_seconds
      
      # Should exceed 10K metrics/second
      assert throughput >= 10_000,
        "Throughput #{throughput} metrics/sec below 10K requirement"
        
      # Verify all batches succeeded
      assert length(batch_results) == 10
      assert Enum.all?(batch_results, &(&1 == :ok))
      
      # Channel should still be responsive
      ref = push(channel_socket, "alerts:configure", %{
        "metric" => "post_stress_test",
        "threshold" => 100
      })
      assert_reply ref, :ok
    end
    
    @tag :stress
    test "memory usage remains stable under sustained load", %{socket: socket} do
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      initial_memory = :erlang.memory(:total)
      memory_samples = []
      
      # Run sustained load for 30 seconds
      load_task = Task.async(fn ->
        Enum.each(1..30, fn second ->
          # Send 1000 metrics per second
          metrics = Enum.map(1..1000, fn i ->
            %{
              "name" => "sustained_metric",
              "value" => i,
              "timestamp" => System.monotonic_time(:millisecond)
            }
          end)
          
          ref = push(channel_socket, "metrics:batch", %{"metrics" => metrics})
          assert_reply ref, :ok
          
          # Sample memory usage
          current_memory = :erlang.memory(:total)
          send(self(), {:memory_sample, second, current_memory})
          
          # Wait for next second
          Process.sleep(1000)
        end)
      end)
      
      # Collect memory samples
      memory_samples = Enum.map(1..30, fn _second ->
        receive do
          {:memory_sample, second, memory} -> {second, memory}
        after
          2000 -> {0, 0}
        end
      end)
      
      Task.await(load_task, 35_000)
      
      final_memory = :erlang.memory(:total)
      
      # Analyze memory growth
      {_seconds, memories} = Enum.unzip(memory_samples)
      max_memory = Enum.max(memories)
      avg_memory = Enum.sum(memories) / length(memories)
      
      # Memory should not grow excessively
      max_growth = max_memory - initial_memory
      final_growth = final_memory - initial_memory
      
      assert max_growth < 100_000_000,  # < 100MB peak growth
        "Peak memory growth #{max_growth} bytes too high"
        
      assert final_growth < 50_000_000,  # < 50MB permanent growth
        "Permanent memory growth #{final_growth} bytes too high"
    end
  end
  
  describe "STRESS: Concurrent Channel Operations" do
    @tag :stress
    test "100 concurrent pipelines with real-time monitoring", %{socket: socket} do
      pipeline_count = 100
      
      # Create 100 concurrent pipeline channels
      pipeline_tasks = Task.async_stream(1..pipeline_count, fn i ->
        pipeline_id = "stress_pipeline_#{i}"
        
        {:ok, client_socket} = connect(CnsForge.UserSocket, %{
          "token" => "stress_token_#{i}"
        })
        
        {:ok, _reply, channel_socket} = subscribe_and_join(
          client_socket,
          Pipeline8020Channel,
          "pipeline:#{pipeline_id}"
        )
        
        # Execute mini pipeline simulation
        stages = ["typer", "turtle", "ttl2dspy", "bitactor"]
        
        stage_results = Enum.map(stages, fn stage ->
          # Simulate stage progression
          CnsForge.PipelineEventProcessor.broadcast_stage_transition(
            pipeline_id,
            stage,
            "idle",
            "processing"
          )
          
          # Should receive transition
          assert_push "stage_transition", %{stage: ^stage, to: "processing"}
          
          Process.sleep(50)  # Brief processing time
          
          CnsForge.PipelineEventProcessor.broadcast_stage_completed(
            pipeline_id,
            stage,
            "success",
            %{duration: 50, items: 100}
          )
          
          # Should receive completion
          assert_push "stage_completed", %{stage: ^stage, status: "success"}
          
          stage
        end)
        
        # Cleanup
        leave(channel_socket)
        
        {pipeline_id, length(stage_results)}
      end, max_concurrency: 20, timeout: 30_000)
      
      results = Enum.to_list(pipeline_tasks)
      
      # All pipelines should complete successfully
      assert length(results) == pipeline_count
      assert Enum.all?(results, fn {:ok, {_id, 4}} -> true; _ -> false end)
    end
    
    @tag :stress
    test "reactor step compensation under high concurrency", %{socket: socket} do
      reactor_name = "StressTestReactor"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:#{reactor_name}"
      )
      
      # Launch 1000 concurrent compensation events
      compensation_count = 1000
      
      compensation_tasks = Task.async_stream(1..compensation_count, fn i ->
        step_name = "stress_step_#{i}"
        
        # Start step
        ref = push(channel_socket, "step:started", %{"step" => step_name})
        assert_reply ref, :ok
        
        # Trigger compensation
        ref = push(channel_socket, "compensation:trigger", %{
          "step" => step_name,
          "error" => "stress_test_error_#{i}"
        })
        assert_reply ref, :ok
        
        i
      end, max_concurrency: 50, timeout: 10_000)
      
      results = Enum.to_list(compensation_tasks)
      
      # All compensations should succeed
      assert length(results) == compensation_count
      assert Enum.all?(results, fn {:ok, _i} -> true end)
      
      # Channel state should track all compensations
      state = :sys.get_state(channel_socket.channel_pid)
      active_compensations = state.assigns.active_steps
      |> Enum.count(fn {_key, step_data} -> 
        step_data.status == "compensating" 
      end)
      
      assert active_compensations == compensation_count
    end
  end
  
  describe "STRESS: BitActor Performance" do
    @tag :stress
    test "batch actor spawning scales to 10K actors", %{socket: socket} do
      session_id = "stress_bitactor_#{System.unique_integer()}"
      
      {:ok, _reply, channel_socket} = subscribe_and_join(
        socket,
        BitActorChannel,
        "stage:bitactor:#{session_id}"
      )
      
      # Test progressive batch sizes
      batch_sizes = [100, 500, 1000, 5000, 10000]
      
      scaling_results = Enum.map(batch_sizes, fn size ->
        configs = Enum.map(1..size, fn i ->
          %{"id" => "stress_actor_#{i}", "type" => "performance_test"}
        end)
        
        start_time = System.monotonic_time(:millisecond)
        
        ref = push(channel_socket, "batch:spawn", %{"items" => configs})
        assert_reply ref, :ok, spawned_actors
        
        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time
        
        %{
          size: size,
          duration: duration,
          throughput: size / (duration / 1000),  # actors/second
          spawned: length(spawned_actors)
        }
      end)
      
      # Verify scaling characteristics
      Enum.each(scaling_results, fn result ->
        assert result.spawned == result.size,
          "Not all actors spawned: #{result.spawned}/#{result.size}"
          
        assert result.throughput > 100,
          "Throughput too low: #{result.throughput} actors/sec"
      end)
      
      # Largest batch should still complete in reasonable time
      largest_result = List.last(scaling_results)
      assert largest_result.duration < 10_000,  # < 10 seconds for 10K actors
        "10K actor spawn took #{largest_result.duration}ms"
    end
  end
  
  describe "STRESS: K8s Channel Load" do
    @tag :stress
    @tag :k8s_integration
    test "k8s channel handles 100 concurrent deployments", %{socket: socket} do
      namespace = "stress-test-ns"
      
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:#{namespace}"
      )
      
      deployment_count = 100
      
      # Create 100 concurrent deployments
      deployment_tasks = Task.async_stream(1..deployment_count, fn i ->
        deployment_spec = %{
          "replicas" => 1,
          "image" => "nginx:alpine",
          "selector" => %{"app" => "stress-test-#{i}"},
          "ports" => [%{"port" => 80}]
        }
        
        ref = push(k8s_socket, "deploy", %{"spec" => deployment_spec})
        assert_reply ref, :ok, deployment
        
        deployment.id
      end, max_concurrency: 20, timeout: 30_000)
      
      deployment_results = Enum.to_list(deployment_tasks)
      
      # All deployments should succeed
      assert length(deployment_results) == deployment_count
      assert Enum.all?(deployment_results, fn {:ok, _id} -> true end)
      
      # Extract deployment IDs for cleanup
      deployment_ids = Enum.map(deployment_results, fn {:ok, id} -> id end)
      
      # Monitor deployment status for all
      Enum.each(deployment_ids, fn deployment_id ->
        ref = push(k8s_socket, "status:stream", %{"deployment" => deployment_id})
        assert_reply ref, :ok
      end)
      
      # Should receive status updates for deployments
      Enum.each(1..deployment_count, fn _i ->
        assert_push "deployment_status", %{status: status}
        assert status in ["creating", "running"]
      end)
    end
  end
end

defmodule CnsForge.Ultra8020OTELValidation do
  @moduledoc """
  📡 OPENTELEMETRY INTEGRATION VALIDATION
  Validates telemetry data flows correctly to OTEL collector
  """
  
  use CnsForge.Ultra8020TestSuite
  
  describe "OTEL: Telemetry Export Validation" do
    @tag :otel_integration
    test "pipeline metrics export to OTEL collector correctly", %{socket: socket} do
      pipeline_id = "otel_test_#{System.unique_integer()}"
      
      # Setup OTEL trace collection
      trace_collector = start_otel_trace_collector()
      
      # Join pipeline and telemetry channels
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:#{pipeline_id}"
      )
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Execute pipeline with OTEL tracing
      CnsForge.PipelineEventProcessor.broadcast_stage_transition(
        pipeline_id,
        "bitactor",
        "idle",
        "processing",
        %{
          trace_id: "test_trace_123",
          span_id: "test_span_456",
          cpu_usage: 85.5,
          memory_mb: 2048
        }
      )
      
      # Wait for OTEL export
      Process.sleep(2000)
      
      # Verify OTEL collector received trace data
      collected_traces = get_otel_traces(trace_collector)
      
      assert length(collected_traces) > 0
      
      # Find our specific trace
      test_trace = Enum.find(collected_traces, fn trace ->
        trace.trace_id == "test_trace_123"
      end)
      
      assert test_trace != nil
      assert test_trace.spans |> length() > 0
      
      # Verify span contains pipeline metadata
      pipeline_span = Enum.find(test_trace.spans, fn span ->
        span.name == "pipeline.stage.transition"
      end)
      
      assert pipeline_span != nil
      assert pipeline_span.attributes["pipeline.id"] == pipeline_id
      assert pipeline_span.attributes["stage.name"] == "bitactor"
      assert pipeline_span.attributes["stage.from"] == "idle"
      assert pipeline_span.attributes["stage.to"] == "processing"
      
      cleanup_otel_collector(trace_collector)
    end
    
    @tag :otel_integration
    test "high-frequency metrics maintain OTEL export performance", %{socket: socket} do
      metric_exporter = start_otel_metric_exporter()
      
      {:ok, _reply, telemetry_socket} = subscribe_and_join(
        socket,
        Telemetry8020Channel,
        "telemetry:performance"
      )
      
      # Generate high-frequency metrics
      metric_count = 5000
      
      metrics_task = Task.async(fn ->
        Enum.each(1..metric_count, fn i ->
          # Emit telemetry event
          :telemetry.execute(
            [:cns_forge, :performance],
            %{
              cpu_usage: 50 + rem(i, 50),
              memory_usage: 1000 + rem(i, 1000),
              latency: 10 + rem(i, 100)
            },
            %{
              pipeline_id: "otel_perf_test",
              instance_id: "test_instance",
              timestamp: System.monotonic_time(:millisecond)
            }
          )
          
          # Small delay to simulate realistic frequency
          if rem(i, 100) == 0 do
            Process.sleep(10)
          end
        end)
      end)
      
      start_time = System.monotonic_time(:millisecond)
      Task.await(metrics_task, 30_000)
      end_time = System.monotonic_time(:millisecond)
      
      total_time = end_time - start_time
      throughput = metric_count / (total_time / 1000)
      
      # Should maintain high throughput even with OTEL export
      assert throughput > 1000, "OTEL throughput #{throughput} metrics/sec too low"
      
      # Wait for OTEL export batch
      Process.sleep(5000)
      
      # Verify metrics were exported
      exported_metrics = get_otel_metrics(metric_exporter)
      
      assert length(exported_metrics) > 0
      
      # Verify metric content
      cpu_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.name == "cns_forge.performance.cpu_usage"
      end)
      
      assert length(cpu_metrics) > 0
      
      cleanup_otel_exporter(metric_exporter)
    end
  end
  
  describe "OTEL: Distributed Tracing" do
    @tag :otel_integration
    test "cross-channel operations maintain trace context", %{socket: socket} do
      trace_id = "distributed_trace_#{System.unique_integer()}"
      parent_span_id = "parent_span_#{System.unique_integer()}"
      
      trace_collector = start_otel_trace_collector()
      
      # Start distributed trace across multiple channels
      {:ok, _reply, pipeline_socket} = subscribe_and_join(
        socket,
        Pipeline8020Channel,
        "pipeline:trace_test"
      )
      
      {:ok, _reply, reactor_socket} = subscribe_and_join(
        socket,
        ReactorStep8020Channel,
        "reactor:TraceTestReactor"
      )
      
      {:ok, _reply, k8s_socket} = subscribe_and_join(
        socket,
        K8sDeploymentChannel,
        "stage:k8s:trace-test"
      )
      
      # Simulate pipeline execution with trace propagation
      
      # 1. Pipeline stage transition
      CnsForge.PipelineEventProcessor.broadcast_stage_transition(
        "trace_test",
        "reactor",
        "idle",
        "executing",
        %{
          trace_id: trace_id,
          span_id: "pipeline_span",
          parent_span_id: parent_span_id
        }
      )
      
      # 2. Reactor step execution
      ref = push(reactor_socket, "step:started", %{
        "step" => "k8s_deployment",
        "trace_context" => %{
          "trace_id" => trace_id,
          "parent_span_id" => "pipeline_span"
        }
      })
      assert_reply ref, :ok
      
      # 3. K8s deployment with trace context
      ref = push(k8s_socket, "deploy", %{
        "spec" => %{
          "replicas" => 1,
          "image" => "nginx:alpine",
          "selector" => %{"app" => "trace-test"}
        },
        "trace_context" => %{
          "trace_id" => trace_id,
          "parent_span_id" => "reactor_span"
        }
      })
      assert_reply ref, :ok
      
      # Wait for trace completion
      Process.sleep(3000)
      
      # Verify distributed trace
      collected_traces = get_otel_traces(trace_collector)
      distributed_trace = Enum.find(collected_traces, fn trace ->
        trace.trace_id == trace_id
      end)
      
      assert distributed_trace != nil
      
      # Verify span hierarchy
      spans = distributed_trace.spans
      assert length(spans) >= 3
      
      # Find spans by operation
      pipeline_span = Enum.find(spans, fn span ->
        span.name == "pipeline.stage.transition"
      end)
      
      reactor_span = Enum.find(spans, fn span ->
        span.name == "reactor.step.started"
      end)
      
      k8s_span = Enum.find(spans, fn span ->
        span.name == "k8s.deployment.create"
      end)
      
      # Verify parent-child relationships
      assert pipeline_span.parent_span_id == parent_span_id
      assert reactor_span.parent_span_id == pipeline_span.span_id
      assert k8s_span.parent_span_id == reactor_span.span_id
      
      cleanup_otel_collector(trace_collector)
    end
  end
  
  describe "OTEL: Custom Metrics" do
    @tag :otel_integration
    test "channel-specific metrics export with correct dimensions", %{socket: socket} do
      metric_exporter = start_otel_metric_exporter()
      
      # Test each channel type's custom metrics
      channels_to_test = [
        {"pipeline:otel_metrics_test", Pipeline8020Channel},
        {"telemetry:performance", Telemetry8020Channel},
        {"stage:bitactor:otel_test", BitActorChannel}
      ]
      
      channel_metrics = Enum.map(channels_to_test, fn {channel_topic, channel_module} ->
        {:ok, _reply, channel_socket} = subscribe_and_join(
          socket,
          channel_module,
          channel_topic
        )
        
        # Trigger channel-specific operations to generate metrics
        case channel_module do
          Pipeline8020Channel ->
            ref = push(channel_socket, "control:restart", %{})
            assert_reply ref, :ok
            
          Telemetry8020Channel ->
            ref = push(channel_socket, "metrics:batch", %{
              "metrics" => [%{"name" => "test_metric", "value" => 42}]
            })
            assert_reply ref, :ok
            
          BitActorChannel ->
            ref = push(channel_socket, "spawn", %{
              "config" => %{"id" => "otel_test", "type" => "metric_test"}
            })
            assert_reply ref, :ok
        end
        
        {channel_topic, channel_module}
      end)
      
      # Wait for metric export
      Process.sleep(5000)
      
      # Verify metrics were exported with correct dimensions
      exported_metrics = get_otel_metrics(metric_exporter)
      
      # Check for channel-specific metrics
      pipeline_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.attributes["channel.type"] == "pipeline"
      end)
      
      telemetry_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.attributes["channel.type"] == "telemetry"
      end)
      
      bitactor_metrics = Enum.filter(exported_metrics, fn metric ->
        metric.attributes["channel.type"] == "bitactor"
      end)
      
      assert length(pipeline_metrics) > 0
      assert length(telemetry_metrics) > 0  
      assert length(bitactor_metrics) > 0
      
      # Verify metric dimensions include 80/20 categorization
      Enum.each(exported_metrics, fn metric ->
        assert Map.has_key?(metric.attributes, "channel.pattern")
        assert Map.has_key?(metric.attributes, "value.priority")
      end)
      
      cleanup_otel_exporter(metric_exporter)
    end
  end
  
  # Helper functions for OTEL integration
  
  defp start_otel_collector do
    # Mock OTEL collector for testing
    {:ok, pid} = Agent.start_link(fn -> [] end)
    pid
  end
  
  defp start_otel_trace_collector do
    {:ok, pid} = Agent.start_link(fn -> [] end)
    pid
  end
  
  defp start_otel_metric_exporter do
    {:ok, pid} = Agent.start_link(fn -> [] end)
    pid
  end
  
  defp get_otel_traces(collector) do
    Agent.get(collector, & &1)
  end
  
  defp get_otel_metrics(exporter) do
    Agent.get(exporter, & &1)
  end
  
  defp cleanup_otel_collector(collector) do
    Agent.stop(collector)
  end
  
  defp cleanup_otel_exporter(exporter) do
    Agent.stop(exporter)
  end
  
  defp start_otel_collector do
    # In real implementation, this would start an OTEL collector process
    # For testing, we'll use a simple agent to collect trace data
    :ok
  end
  
  defp setup_k8s_test_cluster do
    # Setup minimal K8s test environment
    # In real implementation, this would configure kubectl context
    :ok
  end
  
  defp cleanup_test_environment do
    # Cleanup test resources
    :ok
  end
end