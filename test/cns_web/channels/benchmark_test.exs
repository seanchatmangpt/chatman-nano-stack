defmodule CnsWeb.Channels.BenchmarkTest do
  @moduledoc """
  Performance benchmarks for ultrathink 80/20 swarm channels.
  Validates optimization claims and measures system performance.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: false
  
  alias CnsWeb.{SwarmChannel, UserSocket}
  alias Cns.{Accounts, Swarm}
  
  @moduletag :benchmark
  @moduletag timeout: 600_000  # 10 minutes for benchmarks
  
  # Benchmark configuration
  @benchmark_iterations 100
  @warmup_iterations 10
  @pipeline_stages ["typer", "turtle", "ttl2dspy", "bitactor", "erlang", "ash", "reactor", "k8s"]
  @critical_stages ["typer", "turtle", "ash", "reactor", "k8s"]
  @non_critical_stages ["ttl2dspy", "bitactor", "erlang"]
  
  # Performance targets (based on 80/20 optimization claims)
  @target_80_20_speedup 1.6        # 60% faster
  @target_resource_reduction 0.5   # 50% fewer resources
  @target_efficiency_gain 40       # 40% efficiency improvement
  @target_latency_ms 200           # Sub-200ms for critical operations
  
  setup_all do
    # Disable logging for cleaner benchmark output
    :logger.set_application_level(:cns, :emergency)
    
    # Start required services
    start_supervised!({Phoenix.PubSub, name: CnsWeb.PubSub})
    start_supervised!({Registry, keys: :unique, name: CnsWeb.BenchmarkRegistry})
    
    # Create benchmark users and swarms
    admin_user = %{
      id: 1,
      email: "benchmark_admin@test.com",
      role: "admin",
      active: true
    }
    
    test_swarms = [
      %{id: "benchmark-80-20", name: "80/20 Benchmark", optimization_mode: "80_20"},
      %{id: "benchmark-full", name: "Full Benchmark", optimization_mode: "full"}
    ]
    
    {:ok, %{admin_user: admin_user, swarms: test_swarms}}
  end
  
  describe "80/20 vs Full Pipeline Performance" do
    test "pipeline execution speed comparison", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Pipeline Execution Speed Benchmark ==="
      
      # Setup connections for both modes
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      
      {:ok, socket_80_20} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
      {:ok, socket_full} = connect(UserSocket, %{"token" => token, "optimization" => "full"})
      
      swarm_80_20 = Enum.find(swarms, &(&1.optimization_mode == "80_20"))
      swarm_full = Enum.find(swarms, &(&1.optimization_mode == "full"))
      
      {:ok, _, channel_80_20} = subscribe_and_join(socket_80_20, SwarmChannel, "swarm:#{swarm_80_20.id}")
      {:ok, _, channel_full} = subscribe_and_join(socket_full, SwarmChannel, "swarm:#{swarm_full.id}")
      
      # Warmup
      IO.puts "Warming up..."
      warmup_pipeline(channel_80_20, "80_20", @warmup_iterations)
      warmup_pipeline(channel_full, "full", @warmup_iterations)
      
      # Benchmark 80/20 mode
      IO.puts "Benchmarking 80/20 optimization mode..."
      times_80_20 = benchmark_pipeline_execution(channel_80_20, "80_20", @benchmark_iterations)
      
      # Benchmark full mode
      IO.puts "Benchmarking full mode..."
      times_full = benchmark_pipeline_execution(channel_full, "full", @benchmark_iterations)
      
      # Calculate statistics
      stats_80_20 = calculate_statistics(times_80_20)
      stats_full = calculate_statistics(times_full)
      
      speedup = stats_full.avg / stats_80_20.avg
      
      # Validate performance claims
      assert speedup >= @target_80_20_speedup, 
        "80/20 speedup (#{speedup}x) below target (#{@target_80_20_speedup}x)"
      
      assert stats_80_20.avg < @target_latency_ms,
        "80/20 average latency (#{stats_80_20.avg}ms) above target (#{@target_latency_ms}ms)"
      
      # Print detailed results
      print_benchmark_results("Pipeline Execution Speed", %{
        "80/20 Mode" => stats_80_20,
        "Full Mode" => stats_full,
        "Speedup" => "#{Float.round(speedup, 2)}x",
        "Target Met" => speedup >= @target_80_20_speedup
      })
    end
    
    test "resource utilization comparison", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Resource Utilization Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      
      # Measure 80/20 mode resources
      IO.puts "Measuring 80/20 mode resource usage..."
      resources_80_20 = measure_resource_usage(token, 
        Enum.find(swarms, &(&1.optimization_mode == "80_20")), 
        "80_20", @benchmark_iterations)
      
      # Measure full mode resources
      IO.puts "Measuring full mode resource usage..."
      resources_full = measure_resource_usage(token, 
        Enum.find(swarms, &(&1.optimization_mode == "full")), 
        "full", @benchmark_iterations)
      
      # Calculate resource reduction
      memory_reduction = 1 - (resources_80_20.avg_memory / resources_full.avg_memory)
      process_reduction = 1 - (resources_80_20.avg_processes / resources_full.avg_processes)
      message_reduction = 1 - (resources_80_20.total_messages / resources_full.total_messages)
      
      # Validate resource reduction claims
      assert memory_reduction >= @target_resource_reduction,
        "Memory reduction (#{memory_reduction * 100}%) below target (#{@target_resource_reduction * 100}%)"
      
      assert process_reduction >= 0.3,  # At least 30% process reduction
        "Process reduction (#{process_reduction * 100}%) below 30%"
      
      # Print results
      print_resource_comparison(%{
        "80/20 Mode" => resources_80_20,
        "Full Mode" => resources_full,
        "Memory Reduction" => "#{Float.round(memory_reduction * 100, 1)}%",
        "Process Reduction" => "#{Float.round(process_reduction * 100, 1)}%",
        "Message Reduction" => "#{Float.round(message_reduction * 100, 1)}%"
      })
    end
    
    test "throughput and scalability benchmark", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Throughput and Scalability Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      
      concurrent_levels = [1, 5, 10, 20, 50]
      
      throughput_results = for concurrency <- concurrent_levels do
        IO.puts "Testing concurrency level: #{concurrency}"
        
        # Test 80/20 mode
        throughput_80_20 = measure_throughput(token, 
          Enum.find(swarms, &(&1.optimization_mode == "80_20")), 
          "80_20", concurrency)
        
        # Test full mode
        throughput_full = measure_throughput(token, 
          Enum.find(swarms, &(&1.optimization_mode == "full")), 
          "full", concurrency)
        
        {concurrency, throughput_80_20, throughput_full}
      end
      
      # Analyze scalability
      analyze_scalability(throughput_results)
      
      # Validate that 80/20 maintains higher throughput
      for {concurrency, t_80_20, t_full} <- throughput_results do
        throughput_advantage = t_80_20 / t_full
        
        assert throughput_advantage >= 1.2,
          "80/20 throughput advantage (#{throughput_advantage}x) at concurrency #{concurrency} below 1.2x"
      end
    end
  end
  
  describe "Optimization Effectiveness" do
    test "stage skipping efficiency", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Stage Skipping Efficiency Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
      swarm = Enum.find(swarms, &(&1.optimization_mode == "80_20"))
      {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
      
      # Test different stage configurations
      configurations = [
        %{name: "All Stages", stages: @pipeline_stages, expected_time: 1000},
        %{name: "Critical Only", stages: @critical_stages, expected_time: 600},
        %{name: "Custom Mix", stages: ["typer", "ash", "k8s"], expected_time: 400}
      ]
      
      efficiency_results = for config <- configurations do
        IO.puts "Testing configuration: #{config.name}"
        
        times = for _i <- 1..20 do
          start_time = :os.system_time(:millisecond)
          
          ref = push(channel, "pipeline:execute", %{
            "strategy" => "custom",
            "stages" => config.stages,
            "input_data" => %{"benchmark" => true}
          })
          
          assert_reply ref, :ok, result
          
          end_time = :os.system_time(:millisecond)
          execution_time = end_time - start_time
          
          %{
            execution_time: execution_time,
            stages_executed: length(result.executed_stages),
            stages_skipped: length(result.skipped_stages),
            efficiency_gain: result.optimization_savings.efficiency_gain
          }
        end
        
        avg_time = Enum.sum(Enum.map(times, & &1.execution_time)) / length(times)
        avg_efficiency = Enum.sum(Enum.map(times, & &1.efficiency_gain)) / length(times)
        
        {config.name, avg_time, avg_efficiency, length(config.stages)}
      end
      
      # Print efficiency analysis
      print_efficiency_analysis(efficiency_results)
      
      # Validate efficiency claims
      {_, critical_time, critical_efficiency, _} = Enum.find(efficiency_results, fn {name, _, _, _} -> 
        name == "Critical Only" 
      end)
      
      assert critical_efficiency >= @target_efficiency_gain,
        "Critical-only efficiency (#{critical_efficiency}%) below target (#{@target_efficiency_gain}%)"
    end
    
    test "notification filtering performance", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Notification Filtering Performance Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      
      # Test notification throughput with different filtering modes
      filtering_modes = [
        %{mode: "80_20", filter: "critical", expected_reduction: 0.8},
        %{mode: "full", filter: "all", expected_reduction: 0.0}
      ]
      
      notification_results = for config <- filtering_modes do
        swarm = Enum.find(swarms, &(&1.optimization_mode == config.mode))
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => config.mode})
        {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        
        # Subscribe to notifications
        ref = push(channel, "notifications:subscribe", %{
          "types" => ["all"],
          "level" => config.filter
        })
        
        assert_reply ref, :ok, subscription
        
        # Generate test notifications
        notification_count = 1000
        start_time = :os.system_time(:millisecond)
        
        for i <- 1..notification_count do
          level = case rem(i, 5) do
            0 -> "critical"
            1 -> "error"
            2 -> "warning"
            _ -> "info"
          end
          
          push(channel, "notifications:test:#{level}", %{
            "title" => "Test notification #{i}",
            "message" => "Benchmark notification",
            "level" => level
          })
        end
        
        # Collect received notifications
        received_notifications = collect_notifications(notification_count, 5000)
        
        end_time = :os.system_time(:millisecond)
        processing_time = end_time - start_time
        
        reduction_rate = 1 - (length(received_notifications) / notification_count)
        
        {config.mode, processing_time, length(received_notifications), reduction_rate}
      end
      
      # Analyze notification filtering effectiveness
      print_notification_analysis(notification_results)
      
      # Validate filtering effectiveness
      {_, _, received_80_20, reduction_80_20} = Enum.find(notification_results, fn {mode, _, _, _} -> 
        mode == "80_20" 
      end)
      
      assert reduction_80_20 >= 0.6,
        "80/20 notification reduction (#{reduction_80_20 * 100}%) below 60%"
    end
    
    test "telemetry compression efficiency", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Telemetry Compression Efficiency Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      
      compression_results = for swarm <- swarms do
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => swarm.optimization_mode})
        {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        
        # Subscribe to telemetry
        ref = push(channel, "telemetry:subscribe", %{
          "metrics" => ["cpu", "memory", "latency", "throughput", "error_rate"],
          "mode" => "real_time"
        })
        
        assert_reply ref, :ok, subscription
        
        # Measure telemetry overhead
        start_memory = :erlang.memory(:total)
        start_time = :os.system_time(:millisecond)
        
        # Simulate telemetry load
        for i <- 1..1000 do
          push(channel, "telemetry:data:simulate", %{
            "cpu" => :rand.uniform(100),
            "memory" => :rand.uniform(100), 
            "latency" => :rand.uniform(500),
            "throughput" => :rand.uniform(1000),
            "error_rate" => :rand.uniform(10),
            "timestamp" => start_time + i,
            "instance_id" => "benchmark_#{i}"
          })
        end
        
        # Collect telemetry messages
        telemetry_messages = collect_telemetry_data(1000, 10000)
        
        end_memory = :erlang.memory(:total)
        end_time = :os.system_time(:millisecond)
        
        memory_usage = end_memory - start_memory
        processing_time = end_time - start_time
        
        {swarm.optimization_mode, memory_usage, processing_time, length(telemetry_messages), subscription.config}
      end
      
      # Analyze compression effectiveness
      print_telemetry_analysis(compression_results)
      
      # Validate compression benefits
      {_, memory_80_20, time_80_20, msgs_80_20, _} = Enum.find(compression_results, fn {mode, _, _, _, _} -> 
        mode == "80_20" 
      end)
      
      {_, memory_full, time_full, msgs_full, _} = Enum.find(compression_results, fn {mode, _, _, _, _} -> 
        mode == "full" 
      end)
      
      memory_reduction = 1 - (memory_80_20 / memory_full)
      time_reduction = 1 - (time_80_20 / time_full)
      
      assert memory_reduction >= 0.3,
        "Telemetry memory reduction (#{memory_reduction * 100}%) below 30%"
    end
  end
  
  describe "Performance Regression Detection" do
    test "performance stability over time", %{admin_user: user, swarms: swarms} do
      IO.puts "\n=== Performance Stability Benchmark ==="
      
      token = Phoenix.Token.sign(CnsWeb.Endpoint, "user socket", user.id)
      swarm = Enum.find(swarms, &(&1.optimization_mode == "80_20"))
      
      # Run extended performance test
      test_duration = 30_000  # 30 seconds
      measurements = []
      
      {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => "80_20"})
      {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
      
      start_time = :os.system_time(:millisecond)
      
      measurements = collect_performance_measurements(channel, start_time, test_duration)
      
      # Analyze performance stability
      stability_analysis = analyze_performance_stability(measurements)
      
      # Validate stability
      assert stability_analysis.performance_degradation < 20,
        "Performance degradation (#{stability_analysis.performance_degradation}%) exceeds 20%"
      
      assert stability_analysis.variability_coefficient < 0.3,
        "Performance variability (#{stability_analysis.variability_coefficient}) exceeds 0.3"
      
      print_stability_analysis(stability_analysis)
    end
  end
  
  # Helper functions
  
  defp warmup_pipeline(channel, strategy, iterations) do
    for _i <- 1..iterations do
      ref = push(channel, "pipeline:execute", %{
        "strategy" => strategy,
        "input_data" => %{"warmup" => true}
      })
      
      assert_reply ref, :ok, _result
    end
    
    # Allow system to stabilize
    Process.sleep(1000)
  end
  
  defp benchmark_pipeline_execution(channel, strategy, iterations) do
    for i <- 1..iterations do
      start_time = :os.system_time(:millisecond)
      
      ref = push(channel, "pipeline:execute", %{
        "strategy" => strategy,
        "input_data" => %{
          "benchmark" => true,
          "iteration" => i,
          "data_size" => "standard"
        }
      })
      
      assert_reply ref, :ok, _result
      
      end_time = :os.system_time(:millisecond)
      end_time - start_time
    end
  end
  
  defp measure_resource_usage(token, swarm, optimization_mode, iterations) do
    {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => optimization_mode})
    {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
    
    initial_memory = :erlang.memory(:total)
    initial_processes = :erlang.system_info(:process_count)
    message_count = 0
    
    for i <- 1..iterations do
      ref = push(channel, "pipeline:execute", %{
        "strategy" => optimization_mode,
        "input_data" => %{"iteration" => i}
      })
      
      assert_reply ref, :ok, _result
      message_count = message_count + 1
    end
    
    final_memory = :erlang.memory(:total)
    final_processes = :erlang.system_info(:process_count)
    
    %{
      avg_memory: (final_memory - initial_memory) / iterations,
      avg_processes: (final_processes - initial_processes) / iterations,
      total_messages: message_count,
      peak_memory: final_memory
    }
  end
  
  defp measure_throughput(token, swarm, optimization_mode, concurrency) do
    tasks = for _i <- 1..concurrency do
      Task.async(fn ->
        {:ok, socket} = connect(UserSocket, %{"token" => token, "optimization" => optimization_mode})
        {:ok, _, channel} = subscribe_and_join(socket, SwarmChannel, "swarm:#{swarm.id}")
        
        start_time = :os.system_time(:millisecond)
        
        for j <- 1..10 do  # 10 operations per task
          ref = push(channel, "pipeline:execute", %{
            "strategy" => optimization_mode,
            "input_data" => %{"concurrency_test" => j}
          })
          
          assert_reply ref, :ok, _result
        end
        
        end_time = :os.system_time(:millisecond)
        (end_time - start_time) / 10  # Average time per operation
      end)
    end
    
    times = Task.await_many(tasks, 30_000)
    avg_time = Enum.sum(times) / length(times)
    
    # Throughput = operations per second
    1000 / avg_time * concurrency
  end
  
  defp collect_notifications(expected_count, timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_notifications_loop([], expected_count, end_time)
  end
  
  defp collect_notifications_loop(notifications, remaining, end_time) do
    if remaining <= 0 or :os.system_time(:millisecond) >= end_time do
      notifications
    else
      receive do
        %Phoenix.Socket.Message{event: "notification:" <> _, payload: payload} ->
          collect_notifications_loop([payload | notifications], remaining - 1, end_time)
      after
        100 -> collect_notifications_loop(notifications, remaining, end_time)
      end
    end
  end
  
  defp collect_telemetry_data(expected_count, timeout) do
    end_time = :os.system_time(:millisecond) + timeout
    collect_telemetry_loop([], expected_count, end_time)
  end
  
  defp collect_telemetry_loop(data, remaining, end_time) do
    if remaining <= 0 or :os.system_time(:millisecond) >= end_time do
      data
    else
      receive do
        %Phoenix.Socket.Message{event: "telemetry:" <> _, payload: payload} ->
          collect_telemetry_loop([payload | data], remaining - 1, end_time)
      after
        50 -> collect_telemetry_loop(data, remaining, end_time)
      end
    end
  end
  
  defp collect_performance_measurements(channel, start_time, duration) do
    end_time = start_time + duration
    measurements = []
    
    collect_measurements_loop(channel, measurements, start_time, end_time)
  end
  
  defp collect_measurements_loop(channel, measurements, current_time, end_time) do
    if current_time >= end_time do
      measurements
    else
      measurement_start = :os.system_time(:millisecond)
      
      ref = push(channel, "pipeline:execute", %{
        "strategy" => "80_20",
        "input_data" => %{"performance_test" => current_time}
      })
      
      assert_reply ref, :ok, result
      
      measurement_end = :os.system_time(:millisecond)
      
      measurement = %{
        timestamp: current_time,
        execution_time: measurement_end - measurement_start,
        efficiency_gain: result.optimization_savings.efficiency_gain
      }
      
      Process.sleep(500)  # 500ms between measurements
      next_time = :os.system_time(:millisecond)
      
      collect_measurements_loop(channel, [measurement | measurements], next_time, end_time)
    end
  end
  
  defp calculate_statistics(times) do
    sorted_times = Enum.sort(times)
    count = length(times)
    
    %{
      avg: Enum.sum(times) / count,
      min: Enum.min(times),
      max: Enum.max(times),
      median: Enum.at(sorted_times, div(count, 2)),
      p95: Enum.at(sorted_times, round(count * 0.95) - 1),
      p99: Enum.at(sorted_times, round(count * 0.99) - 1),
      std_dev: calculate_std_deviation(times)
    }
  end
  
  defp calculate_std_deviation(values) do
    mean = Enum.sum(values) / length(values)
    variance = values
    |> Enum.map(fn x -> :math.pow(x - mean, 2) end)
    |> Enum.sum()
    |> Kernel./(length(values))
    
    :math.sqrt(variance)
  end
  
  defp analyze_scalability(throughput_results) do
    IO.puts "\n--- Scalability Analysis ---"
    
    for {concurrency, t_80_20, t_full} <- throughput_results do
      advantage = Float.round(t_80_20 / t_full, 2)
      
      IO.puts "Concurrency #{concurrency}: 80/20=#{Float.round(t_80_20, 1)} ops/sec, " <>
              "Full=#{Float.round(t_full, 1)} ops/sec, Advantage=#{advantage}x"
    end
  end
  
  defp analyze_performance_stability(measurements) do
    execution_times = Enum.map(measurements, & &1.execution_time)
    
    first_half = Enum.take(execution_times, div(length(execution_times), 2))
    second_half = Enum.drop(execution_times, div(length(execution_times), 2))
    
    first_avg = Enum.sum(first_half) / length(first_half)
    second_avg = Enum.sum(second_half) / length(second_half)
    
    performance_degradation = (second_avg - first_avg) / first_avg * 100
    
    overall_avg = Enum.sum(execution_times) / length(execution_times)
    std_dev = calculate_std_deviation(execution_times)
    variability_coefficient = std_dev / overall_avg
    
    %{
      performance_degradation: performance_degradation,
      variability_coefficient: variability_coefficient,
      first_half_avg: first_avg,
      second_half_avg: second_avg,
      overall_avg: overall_avg,
      std_dev: std_dev
    }
  end
  
  # Print functions
  
  defp print_benchmark_results(title, results) do
    IO.puts "\n--- #{title} Results ---"
    
    for {key, value} <- results do
      case value do
        %{avg: avg, min: min, max: max, p95: p95} ->
          IO.puts "#{key}: avg=#{Float.round(avg, 1)}ms, min=#{min}ms, max=#{max}ms, p95=#{Float.round(p95, 1)}ms"
        _ ->
          IO.puts "#{key}: #{value}"
      end
    end
  end
  
  defp print_resource_comparison(results) do
    IO.puts "\n--- Resource Usage Comparison ---"
    
    for {key, value} <- results do
      case value do
        %{avg_memory: memory, avg_processes: processes, total_messages: messages} ->
          IO.puts "#{key}: memory=#{Float.round(memory/1024/1024, 1)}MB, processes=#{Float.round(processes, 1)}, messages=#{messages}"
        _ ->
          IO.puts "#{key}: #{value}"
      end
    end
  end
  
  defp print_efficiency_analysis(results) do
    IO.puts "\n--- Stage Skipping Efficiency ---"
    
    for {name, avg_time, avg_efficiency, stage_count} <- results do
      IO.puts "#{name}: #{stage_count} stages, #{Float.round(avg_time, 1)}ms avg, #{Float.round(avg_efficiency, 1)}% efficiency"
    end
  end
  
  defp print_notification_analysis(results) do
    IO.puts "\n--- Notification Filtering Analysis ---"
    
    for {mode, processing_time, received_count, reduction_rate} <- results do
      IO.puts "#{mode}: #{processing_time}ms processing, #{received_count} received, #{Float.round(reduction_rate * 100, 1)}% filtered"
    end
  end
  
  defp print_telemetry_analysis(results) do
    IO.puts "\n--- Telemetry Compression Analysis ---"
    
    for {mode, memory_usage, processing_time, message_count, config} <- results do
      compression = if Map.has_key?(config, :compression), do: config.compression, else: false
      IO.puts "#{mode}: #{Float.round(memory_usage/1024/1024, 1)}MB memory, #{processing_time}ms processing, #{message_count} messages, compression=#{compression}"
    end
  end
  
  defp print_stability_analysis(analysis) do
    IO.puts "\n--- Performance Stability Analysis ---"
    IO.puts "Performance degradation: #{Float.round(analysis.performance_degradation, 1)}%"
    IO.puts "Variability coefficient: #{Float.round(analysis.variability_coefficient, 3)}"
    IO.puts "First half avg: #{Float.round(analysis.first_half_avg, 1)}ms"
    IO.puts "Second half avg: #{Float.round(analysis.second_half_avg, 1)}ms"
    IO.puts "Overall avg: #{Float.round(analysis.overall_avg, 1)}ms"
    IO.puts "Standard deviation: #{Float.round(analysis.std_dev, 1)}ms"
  end
end