defmodule CnsWeb.Channels.StressTest do
  @moduledoc """
  Stress tests and load testing for ultrathink 80/20 swarm channels.
  Tests system behavior under extreme load, resource exhaustion, and failure scenarios.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Accounts, Swarm}
  
  @moduletag :stress_test
  @moduletag timeout: 300_000  # 5 minutes for stress tests
  
  # Test configuration
  @max_concurrent_connections 100
  @max_concurrent_pipelines 50
  @stress_test_duration 60_000  # 1 minute
  @load_test_duration 30_000    # 30 seconds
  @memory_limit_mb 500
  @cpu_time_limit_ms 10_000
  
  setup_all do
    # Configure for stress testing
    :logger.set_application_level(:cns, :error)
    
    # Start monitoring processes
    start_supervised!({Phoenix.PubSub, name: CnsWeb.PubSub})
    start_supervised!({Registry, keys: :unique, name: CnsWeb.StressRegistry})
    
    # Pre-create test users for load testing
    users = for i <- 1..@max_concurrent_connections do
      %{
        id: i,
        email: "stress_user_#{i}@test.com",
        role: if(rem(i, 10) == 0, do: "admin", else: "user"),
        active: true
      }
    end
    
    swarms = for i <- 1..10 do
      %{
        id: "stress-swarm-#{i}",
        name: "Stress Test Swarm #{i}",
        optimization_mode: if(rem(i, 2) == 0, do: "80_20", else: "full")
      }
    end
    
    {:ok, %{users: users, swarms: swarms}}
  end
  
  describe "Concurrent Connection Stress" do
    test "handles maximum concurrent connections", %{users: users, swarms: swarms} do
      # Track system resources before test
      initial_memory = :erlang.memory(:total)
      initial_processes = :erlang.system_info(:process_count)
      
      # Create many concurrent connections
      connection_tasks = users
      |> Enum.take(@max_concurrent_connections)
      |> Enum.map(fn user ->
        Task.async(fn ->
          try do
            token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
            
            case connect(UserSocket, %{"token" => token, "optimization" => "80_20"}) do
              {:ok, socket} ->
                # Join random swarm
                swarm = Enum.random(swarms)
                
                case subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}") do
                  {:ok, _response, channel_socket} ->
                    # Perform some operations
                    ref = push(channel_socket, "pipeline:status", %{})
                    
                    case assert_reply(ref, _, response) do
                      {:ok, _} -> {:success, user.id}
                      {:error, _} -> {:failed, user.id}
                    end
                    
                  {:error, _} -> {:join_failed, user.id}
                end
                
              :error -> {:connection_failed, user.id}
            end
          rescue
            error -> {:exception, user.id, error}
          end
        end)
      end)
      
      # Wait for all connections with timeout
      results = Task.await_many(connection_tasks, 30_000)
      
      # Analyze results
      successful_connections = Enum.count(results, fn
        {:success, _} -> true
        _ -> false
      end)
      
      failed_connections = Enum.count(results, fn
        {:connection_failed, _} -> true
        {:join_failed, _} -> true
        {:failed, _} -> true
        _ -> false
      end)
      
      exceptions = Enum.count(results, fn
        {:exception, _, _} -> true
        _ -> false
      end)
      
      # Validate system behavior
      assert successful_connections > @max_concurrent_connections * 0.8  # At least 80% success
      assert exceptions < @max_concurrent_connections * 0.1  # Less than 10% exceptions
      
      # Check resource usage
      final_memory = :erlang.memory(:total)
      final_processes = :erlang.system_info(:process_count)
      
      memory_increase_mb = (final_memory - initial_memory) / (1024 * 1024)
      process_increase = final_processes - initial_processes
      
      # Should not consume excessive resources
      assert memory_increase_mb < @memory_limit_mb
      assert process_increase < @max_concurrent_connections * 5  # Max 5 processes per connection
      
      # Log results
      IO.puts """
      Concurrent Connection Stress Test Results:
      - Successful: #{successful_connections}/#{@max_concurrent_connections} (#{successful_connections/@max_concurrent_connections*100}%)
      - Failed: #{failed_connections}
      - Exceptions: #{exceptions}
      - Memory increase: #{memory_increase_mb} MB
      - Process increase: #{process_increase}
      """
    end
    
    test "handles connection churn (rapid connect/disconnect)", %{users: users, swarms: swarms} do
      test_start = :os.system_time(:millisecond)
      connection_count = 0
      error_count = 0
      
      # Run for specified duration
      {:ok, _} = :timer.apply_after(@load_test_duration, __MODULE__, :stop_churn_test, [self()])
      
      churn_loop(users, swarms, connection_count, error_count, test_start)
    end
    
    defp churn_loop(users, swarms, connection_count, error_count, test_start) do
      receive do
        :stop_churn -> 
          test_duration = :os.system_time(:millisecond) - test_start
          
          IO.puts """
          Connection Churn Test Results:
          - Duration: #{test_duration} ms
          - Connections: #{connection_count}
          - Errors: #{error_count}
          - Rate: #{connection_count * 1000 / test_duration} connections/sec
          - Error rate: #{error_count/connection_count*100}%
          """
          
          assert connection_count > 100  # Should handle at least 100 connections
          assert error_count / connection_count < 0.2  # Less than 20% error rate
          
      after
        0 ->
          # Create and immediately disconnect
          user = Enum.random(users)
          swarm = Enum.random(swarms)
          
          result = try do
            token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
            
            case connect(UserSocket, %{"token" => token}) do
              {:ok, socket} ->
                case subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}") do
                  {:ok, _response, _channel_socket} ->
                    # Immediately disconnect
                    :ok
                    
                  {:error, _} -> :error
                end
                
              :error -> :error
            end
          rescue
            _ -> :error
          end
          
          new_connection_count = connection_count + 1
          new_error_count = if result == :error, do: error_count + 1, else: error_count
          
          churn_loop(users, swarms, new_connection_count, new_error_count, test_start)
      end
    end
    
    def stop_churn_test(pid) do
      send(pid, :stop_churn)
    end
  end
  
  describe "Pipeline Execution Load" do
    test "handles concurrent pipeline executions", %{users: users, swarms: swarms} do
      # Setup multiple connections
      connections = users
      |> Enum.take(@max_concurrent_pipelines)
      |> Enum.map(fn user ->
        token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
        swarm = Enum.random(swarms)
        {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        {user, channel_socket, swarm}
      end)
      
      # Execute pipelines concurrently
      execution_start = :os.system_time(:millisecond)
      
      pipeline_tasks = connections
      |> Enum.map(fn {user, channel_socket, swarm} ->
        Task.async(fn ->
          ref = push(channel_socket, "pipeline:execute", %{
            "strategy" => "80_20",
            "input_data" => %{
              "test_id" => user.id,
              "swarm_id" => swarm.id,
              "concurrent_test" => true
            }
          })
          
          case assert_reply(ref, _, response, 15_000) do  # 15 second timeout
            {:ok, result} -> 
              {:success, user.id, result.optimization_savings.efficiency_gain}
            {:error, error} -> 
              {:error, user.id, error.reason}
          end
        end)
      end)
      
      # Wait for all executions
      pipeline_results = Task.await_many(pipeline_tasks, 30_000)
      
      execution_end = :os.system_time(:millisecond)
      total_duration = execution_end - execution_start
      
      # Analyze results
      successful_pipelines = Enum.count(pipeline_results, fn
        {:success, _, _} -> true
        _ -> false
      end)
      
      failed_pipelines = Enum.count(pipeline_results, fn
        {:error, _, _} -> true
        _ -> false
      end)
      
      avg_efficiency_gain = pipeline_results
      |> Enum.filter(fn {:success, _, _} -> true; _ -> false end)
      |> Enum.map(fn {:success, _, gain} -> gain end)
      |> case do
        [] -> 0
        gains -> Enum.sum(gains) / length(gains)
      end
      
      # Validate performance
      assert successful_pipelines >= @max_concurrent_pipelines * 0.8  # 80% success rate
      assert total_duration < 45_000  # Complete within 45 seconds
      assert avg_efficiency_gain > 30  # Maintain optimization efficiency
      
      IO.puts """
      Concurrent Pipeline Execution Results:
      - Successful: #{successful_pipelines}/#{@max_concurrent_pipelines}
      - Failed: #{failed_pipelines}
      - Total duration: #{total_duration} ms
      - Avg efficiency gain: #{avg_efficiency_gain}%
      - Throughput: #{successful_pipelines * 1000 / total_duration} pipelines/sec
      """
    end
    
    test "maintains performance under sustained load", %{users: users, swarms: swarms} do
      # Setup sustained connections
      connections = users
      |> Enum.take(20)  # Smaller number for sustained test
      |> Enum.map(fn user ->
        token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
        swarm = Enum.random(swarms)
        {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        {user, channel_socket, swarm}
      end)
      
      # Run sustained load test
      test_start = :os.system_time(:millisecond)
      execution_times = []
      error_count = 0
      
      {:ok, _} = :timer.apply_after(@stress_test_duration, __MODULE__, :stop_sustained_test, [self()])
      
      {final_execution_times, final_error_count, total_executions} = 
        sustained_load_loop(connections, execution_times, error_count, 0, test_start)
      
      # Analyze sustained performance
      avg_execution_time = Enum.sum(final_execution_times) / length(final_execution_times)
      max_execution_time = Enum.max(final_execution_times)
      min_execution_time = Enum.min(final_execution_times)
      
      error_rate = final_error_count / total_executions * 100
      
      # Performance should remain stable
      assert avg_execution_time < 2000  # Average under 2 seconds
      assert max_execution_time < 5000  # Max under 5 seconds
      assert error_rate < 10  # Less than 10% errors
      
      # Performance should not degrade significantly over time
      first_half = Enum.take(final_execution_times, div(length(final_execution_times), 2))
      second_half = Enum.drop(final_execution_times, div(length(final_execution_times), 2))
      
      first_half_avg = Enum.sum(first_half) / length(first_half)
      second_half_avg = Enum.sum(second_half) / length(second_half)
      
      performance_degradation = (second_half_avg - first_half_avg) / first_half_avg * 100
      
      assert performance_degradation < 50  # Less than 50% degradation
      
      IO.puts """
      Sustained Load Test Results:
      - Total executions: #{total_executions}
      - Avg execution time: #{avg_execution_time} ms
      - Max execution time: #{max_execution_time} ms
      - Min execution time: #{min_execution_time} ms
      - Error rate: #{error_rate}%
      - Performance degradation: #{performance_degradation}%
      """
    end
    
    defp sustained_load_loop(connections, execution_times, error_count, total_executions, test_start) do
      receive do
        :stop_sustained ->
          {execution_times, error_count, total_executions}
      after
        100 ->  # Execute every 100ms
          {user, channel_socket, swarm} = Enum.random(connections)
          
          execution_start = :os.system_time(:millisecond)
          
          ref = push(channel_socket, "pipeline:execute", %{
            "strategy" => "80_20",
            "input_data" => %{"sustained_test" => true, "execution" => total_executions}
          })
          
          {new_execution_times, new_error_count} = case assert_reply(ref, _, response, 5000) do
            {:ok, _result} ->
              execution_time = :os.system_time(:millisecond) - execution_start
              {[execution_time | execution_times], error_count}
              
            {:error, _error} ->
              {execution_times, error_count + 1}
          end
          
          sustained_load_loop(connections, new_execution_times, new_error_count, total_executions + 1, test_start)
      end
    end
    
    def stop_sustained_test(pid) do
      send(pid, :stop_sustained)
    end
  end
  
  describe "Memory and Resource Stress" do
    test "handles memory pressure gracefully", %{users: users, swarms: swarms} do
      initial_memory = :erlang.memory(:total)
      
      # Create memory pressure through large operations
      memory_tasks = for i <- 1..50 do
        Task.async(fn ->
          user = Enum.at(users, rem(i, length(users)))
          token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
          {:ok, socket} = connect(UserSocket, %{"token" => token})
          swarm = Enum.random(swarms)
          {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
          
          # Create large payload (but within limits)
          large_payload = %{
            "strategy" => "80_20",
            "input_data" => %{
              "large_list" => for(j <- 1..1000, do: "item_#{j}"),
              "large_map" => for(j <- 1..1000, into: %{}, do: {"key_#{j}", "value_#{j}"}),
              "memory_test" => i
            }
          }
          
          ref = push(channel_socket, "pipeline:execute", large_payload)
          
          case assert_reply(ref, _, response, 10_000) do
            {:ok, _} -> :success
            {:error, _} -> :error
          end
        end)
      end
      
      results = Task.await_many(memory_tasks, 30_000)
      
      final_memory = :erlang.memory(:total)
      memory_increase_mb = (final_memory - initial_memory) / (1024 * 1024)
      
      successful_operations = Enum.count(results, &(&1 == :success))
      
      # Should handle memory pressure without excessive growth
      assert memory_increase_mb < @memory_limit_mb
      assert successful_operations >= 40  # At least 80% success
      
      # Force garbage collection and check memory cleanup
      :erlang.garbage_collect()
      Process.sleep(1000)
      
      cleanup_memory = :erlang.memory(:total)
      memory_after_gc = (cleanup_memory - initial_memory) / (1024 * 1024)
      
      # Memory should be mostly cleaned up
      assert memory_after_gc < memory_increase_mb * 0.5
      
      IO.puts """
      Memory Stress Test Results:
      - Successful operations: #{successful_operations}/50
      - Peak memory increase: #{memory_increase_mb} MB
      - Memory after GC: #{memory_after_gc} MB
      """
    end
    
    test "handles process limit stress", %{users: users, swarms: swarms} do
      initial_processes = :erlang.system_info(:process_count)
      
      # Create many concurrent operations that spawn processes
      process_tasks = for i <- 1..100 do
        Task.async(fn ->
          user = Enum.at(users, rem(i, length(users)))
          token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
          {:ok, socket} = connect(UserSocket, %{"token" => token})
          swarm = Enum.random(swarms)
          {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
          
          # Execute workflow that spawns tasks
          workflow_ref = push(channel_socket, "reactor:workflow:create", %{
            "workflow" => %{
              "name" => "process_stress_#{i}",
              "steps" => [
                %{"name" => "step1", "critical" => true},
                %{"name" => "step2", "critical" => true}
              ]
            }
          })
          
          case assert_reply(workflow_ref, _, workflow_response, 5000) do
            {:ok, workflow} ->
              exec_ref = push(channel_socket, "reactor:workflow:execute", %{
                "workflow_id" => workflow.id,
                "input" => %{"process_test" => i}
              })
              
              case assert_reply(exec_ref, _, exec_response, 5000) do
                {:ok, _} -> :success
                {:error, _} -> :error
              end
              
            {:error, _} -> :error
          end
        end)
      end
      
      results = Task.await_many(process_tasks, 45_000)
      
      final_processes = :erlang.system_info(:process_count)
      process_increase = final_processes - initial_processes
      
      successful_workflows = Enum.count(results, &(&1 == :success))
      
      # Should not create excessive processes
      assert process_increase < 1000  # Reasonable limit
      assert successful_workflows >= 80  # Most should succeed
      
      IO.puts """
      Process Stress Test Results:
      - Successful workflows: #{successful_workflows}/100
      - Process increase: #{process_increase}
      - Processes per workflow: #{process_increase / 100}
      """
    end
  end
  
  describe "Network and I/O Stress" do
    test "handles high-frequency message bursts", %{users: users, swarms: swarms} do
      # Setup connection
      user = hd(users)
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
      swarm = hd(swarms)
      {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
      
      # Send burst of messages
      burst_size = 1000
      burst_start = :os.system_time(:millisecond)
      
      refs = for i <- 1..burst_size do
        push(channel_socket, "telemetry:subscribe", %{
          "metrics" => ["cpu"],
          "burst_id" => i
        })
      end
      
      # Count responses
      {successful_responses, rate_limited_responses} = Enum.reduce(refs, {0, 0}, fn ref, {success, limited} ->
        case assert_reply(ref, _, response, 1000) do
          {:ok, _} -> {success + 1, limited}
          {:error, %{reason: reason}} when reason =~ "rate limit" -> {success, limited + 1}
          {:error, _} -> {success, limited}
        end
      end)
      
      burst_end = :os.system_time(:millisecond)
      burst_duration = burst_end - burst_start
      
      # Should handle burst with rate limiting
      assert successful_responses > 0
      assert rate_limited_responses > 0
      assert successful_responses + rate_limited_responses == burst_size
      assert burst_duration < 5000  # Complete within 5 seconds
      
      message_rate = burst_size * 1000 / burst_duration
      
      IO.puts """
      Message Burst Test Results:
      - Burst size: #{burst_size}
      - Successful: #{successful_responses}
      - Rate limited: #{rate_limited_responses}
      - Duration: #{burst_duration} ms
      - Message rate: #{message_rate} msg/sec
      """
    end
    
    test "handles sustained telemetry streaming", %{users: users, swarms: swarms} do
      # Setup multiple telemetry streams
      streams = for i <- 1..10 do
        user = Enum.at(users, i)
        token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
        swarm = Enum.random(swarms)
        {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        
        # Subscribe to telemetry
        ref = push(channel_socket, "telemetry:subscribe", %{
          "metrics" => ["cpu", "memory", "latency"],
          "mode" => "real_time"
        })
        
        assert_reply ref, :ok, _response
        
        {channel_socket, i}
      end
      
      # Start telemetry simulation
      telemetry_start = :os.system_time(:millisecond)
      
      # Send telemetry data for sustained period
      telemetry_task = Task.async(fn ->
        simulate_telemetry_load(streams, @load_test_duration)
      end)
      
      # Collect messages received
      received_messages = collect_telemetry_messages(length(streams), @load_test_duration)
      
      Task.await(telemetry_task, @load_test_duration + 5000)
      
      telemetry_end = :os.system_time(:millisecond)
      total_duration = telemetry_end - telemetry_start
      
      # Analyze telemetry performance
      message_rate = length(received_messages) * 1000 / total_duration
      
      # Should handle sustained telemetry load
      assert length(received_messages) > 100  # At least some messages
      assert message_rate < 1000  # Should not overwhelm with too many messages
      
      IO.puts """
      Sustained Telemetry Test Results:
      - Streams: #{length(streams)}
      - Duration: #{total_duration} ms
      - Messages received: #{length(received_messages)}
      - Message rate: #{message_rate} msg/sec
      """
    end
    
    defp simulate_telemetry_load(streams, duration) do
      end_time = :os.system_time(:millisecond) + duration
      
      simulate_loop(streams, end_time)
    end
    
    defp simulate_loop(streams, end_time) do
      current_time = :os.system_time(:millisecond)
      
      if current_time < end_time do
        # Simulate telemetry data for random stream
        {channel_socket, stream_id} = Enum.random(streams)
        
        push(channel_socket, "telemetry:data:simulated", %{
          "stream_id" => stream_id,
          "cpu" => :rand.uniform(100),
          "memory" => :rand.uniform(100),
          "timestamp" => current_time
        })
        
        Process.sleep(10)  # 10ms delay
        
        simulate_loop(streams, end_time)
      end
    end
    
    defp collect_telemetry_messages(stream_count, duration) do
      end_time = :os.system_time(:millisecond) + duration
      collect_messages([], end_time)
    end
    
    defp collect_messages(messages, end_time) do
      current_time = :os.system_time(:millisecond)
      
      if current_time < end_time do
        receive do
          %Phoenix.Socket.Message{event: "telemetry:" <> _, payload: payload} ->
            collect_messages([payload | messages], end_time)
        after
          100 -> collect_messages(messages, end_time)
        end
      else
        messages
      end
    end
  end
  
  describe "Failure Recovery Stress" do
    test "recovers from cascading failures", %{users: users, swarms: swarms} do
      # Setup multiple connections
      connections = for i <- 1..20 do
        user = Enum.at(users, i)
        token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
        {:ok, socket} = connect(UserSocket, %{"token" => token})
        swarm = Enum.random(swarms)
        {:ok, _response, channel_socket} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        {channel_socket, i}
      end
      
      # Simulate cascading failures
      failure_tasks = connections
      |> Enum.map(fn {channel_socket, id} ->
        Task.async(fn ->
          ref = push(channel_socket, "pipeline:execute", %{
            "strategy" => "80_20",
            "input_data" => %{
              "simulate_failure" => true,
              "failure_cascade" => true,
              "connection_id" => id
            }
          })
          
          case assert_reply(ref, _, response, 20_000) do
            {:ok, result} -> {:recovered, id, result.recovery_applied}
            {:error, error} -> {:failed, id, error.reason}
          end
        end)
      end)
      
      results = Task.await_many(failure_tasks, 30_000)
      
      # Analyze recovery performance
      recovered = Enum.count(results, fn
        {:recovered, _, _} -> true
        _ -> false
      end)
      
      failed = Enum.count(results, fn
        {:failed, _, _} -> true
        _ -> false
      end)
      
      recovery_applied = Enum.count(results, fn
        {:recovered, _, true} -> true
        _ -> false
      end)
      
      # Should recover from most failures
      recovery_rate = recovered / length(connections) * 100
      assert recovery_rate >= 70  # At least 70% recovery
      assert recovery_applied >= recovered * 0.8  # Most recoveries should apply fixes
      
      IO.puts """
      Cascading Failure Recovery Results:
      - Total connections: #{length(connections)}
      - Recovered: #{recovered}
      - Failed: #{failed}
      - Recovery rate: #{recovery_rate}%
      - Recovery mechanisms applied: #{recovery_applied}
      """
    end
  end
end