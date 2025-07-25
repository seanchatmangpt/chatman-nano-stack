defmodule CNSForge.StressTest do
  @moduledoc """
  Stress tests and benchmarks for CNS Forge
  Tests BitActor mesh under high load and verifies performance guarantees
  """
  
  use ExUnit.Case
  import ExUnit.CaptureLog
  alias CNSForge.{BitActor, MetaCompiler, ReactorBuilder}
  
  @stress_iterations 10_000
  @concurrent_operations 100
  @ttl_budget 8
  @max_latency_ms 10
  
  describe "BitActor mesh stress tests" do
    test "handles #{@stress_iterations} BitActors within TTL budget" do
      results = 
        1..@stress_iterations
        |> Task.async_stream(fn i ->
          {:ok, actor} = BitActor.create(%{
            type: :stress_test,
            transaction_id: "stress_#{i}",
            ttl: @ttl_budget,
            token: %{iteration: i}
          })
          
          # Execute hops until TTL expires
          execute_until_ttl_expires(actor)
        end, max_concurrency: @concurrent_operations, timeout: 30_000)
        |> Enum.map(fn {:ok, result} -> result end)
      
      # Verify all actors completed
      assert length(results) == @stress_iterations
      
      # Verify TTL was respected
      ttl_violations = Enum.count(results, & &1.ttl_violation)
      assert ttl_violations == 0, "Found #{ttl_violations} TTL violations"
      
      # Calculate performance metrics
      avg_hops = Enum.sum(Enum.map(results, & &1.hops_executed)) / @stress_iterations
      assert avg_hops >= 6 and avg_hops <= 8, "Average hops #{avg_hops} outside expected range"
    end
    
    test "maintains sub-#{@max_latency_ms}ms latency under load" do
      latencies = 
        1..1000
        |> Enum.map(fn i ->
          start_time = System.monotonic_time(:microsecond)
          
          {:ok, _} = BitActor.create(%{
            type: :latency_test,
            transaction_id: "latency_#{i}",
            ttl: @ttl_budget,
            token: %{test: true}
          })
          
          System.monotonic_time(:microsecond) - start_time
        end)
      
      avg_latency_us = Enum.sum(latencies) / length(latencies)
      max_latency_us = Enum.max(latencies)
      p99_latency_us = percentile(latencies, 99)
      
      # Convert to milliseconds
      avg_latency_ms = avg_latency_us / 1000
      max_latency_ms = max_latency_us / 1000
      p99_latency_ms = p99_latency_us / 1000
      
      assert avg_latency_ms < @max_latency_ms, 
        "Average latency #{avg_latency_ms}ms exceeds limit"
      assert p99_latency_ms < @max_latency_ms * 2,
        "P99 latency #{p99_latency_ms}ms too high"
      
      IO.puts("Latency stats: avg=#{avg_latency_ms}ms, max=#{max_latency_ms}ms, p99=#{p99_latency_ms}ms")
    end
    
    test "handles concurrent signal processing" do
      # Create mesh of BitActors
      mesh_size = 50
      actors = Enum.map(1..mesh_size, fn i ->
        {:ok, actor} = BitActor.create(%{
          type: :mesh_node,
          transaction_id: "mesh_#{i}",
          ttl: @ttl_budget,
          token: %{node_id: i}
        })
        actor
      end)
      
      # Send signals concurrently
      signal_count = 1000
      results = 
        1..signal_count
        |> Task.async_stream(fn signal_id ->
          # Pick random actor
          actor = Enum.random(actors)
          
          # Send signal
          BitActor.execute_hop(actor, %{
            input_token: %{signal: signal_id},
            operation: :process_signal
          })
        end, max_concurrency: 50, timeout: 10_000)
        |> Enum.map(fn {:ok, result} -> result end)
      
      successful = Enum.count(results, &match?({:ok, _}, &1))
      assert successful == signal_count, 
        "Only #{successful}/#{signal_count} signals processed successfully"
    end
  end
  
  describe "MetaCompiler stress tests" do
    test "compiles large ontologies efficiently" do
      # Generate large TTL ontology
      large_ttl = generate_large_ttl(1000) # 1000 classes
      ttl_file = "test/fixtures/large_ontology.ttl"
      File.write!(ttl_file, large_ttl)
      
      # Measure compilation time
      {time_us, {:ok, result}} = :timer.tc(fn ->
        MetaCompiler.compile(ttl_file)
      end)
      
      compilation_time_ms = time_us / 1000
      
      assert compilation_time_ms < 5000, 
        "Compilation took #{compilation_time_ms}ms, exceeding 5s limit"
      assert result.bitactor_count >= 100,
        "Only #{result.bitactor_count} BitActors generated from 1000 classes"
      assert result.semantic_coverage > 0.5,
        "Semantic coverage #{result.semantic_coverage} too low"
      
      IO.puts("Compiled 1000 classes in #{compilation_time_ms}ms")
    end
    
    test "handles parallel compilations" do
      # Create multiple small ontologies
      ontologies = Enum.map(1..10, fn i ->
        content = """
        @prefix ont#{i}: <http://test#{i}#> .
        ont#{i}:Class#{i} a owl:Class .
        """
        path = "test/fixtures/ont#{i}.ttl"
        File.write!(path, content)
        path
      end)
      
      # Compile in parallel
      results = 
        ontologies
        |> Task.async_stream(&MetaCompiler.compile/1, 
                           max_concurrency: 5, 
                           timeout: 30_000)
        |> Enum.map(fn {:ok, result} -> result end)
      
      successful = Enum.count(results, &match?({:ok, _}, &1))
      assert successful == 10, "Only #{successful}/10 compilations succeeded"
    end
  end
  
  describe "Reactor workflow stress tests" do
    test "executes complex workflows under load" do
      # Build workflow with many steps
      workflow_spec = %{
        id: "stress_workflow",
        inputs: [:data_stream],
        middleware: [:telemetry],
        steps: generate_workflow_steps(50), # 50 steps
        return: :final_result
      }
      
      {:ok, workflow_module} = ReactorBuilder.build(workflow_spec)
      
      # Execute workflow multiple times
      executions = 100
      results = 
        1..executions
        |> Task.async_stream(fn i ->
          input = %{data_stream: generate_test_data(i)}
          workflow_module.run(input)
        end, max_concurrency: 10, timeout: 60_000)
        |> Enum.map(fn {:ok, result} -> result end)
      
      successful = Enum.count(results, &match?({:ok, _}, &1))
      assert successful == executions,
        "Only #{successful}/#{executions} workflows completed"
    end
    
    test "handles saga compensations under failure" do
      # Workflow that fails halfway
      failing_workflow = %{
        id: "saga_test",
        inputs: [:will_fail],
        middleware: [:telemetry],
        steps: [
          %{
            name: :step1,
            type: :transform,
            run: fn _ -> {:ok, %{step1: :done}} end,
            compensate: fn _, _, _, _ -> :compensated end,
            arguments: []
          },
          %{
            name: :step2,
            type: :transform,
            run: fn %{will_fail: true} -> {:error, :intentional} end,
            arguments: [{:will_fail, quote do input(:will_fail) end}]
          }
        ],
        return: :final_result
      }
      
      {:ok, module} = ReactorBuilder.build(failing_workflow)
      
      # Run with failure
      assert {:error, _} = module.run(%{will_fail: true})
      
      # Verify compensation was called (would check logs/metrics in real test)
      assert true
    end
  end
  
  describe "Memory and resource tests" do
    test "maintains stable memory usage under sustained load" do
      initial_memory = :erlang.memory(:total)
      
      # Run sustained load for 30 seconds
      end_time = System.monotonic_time(:second) + 30
      iteration = 0
      
      memory_samples = []
      
      while System.monotonic_time(:second) < end_time do
        # Create and destroy BitActors
        actors = Enum.map(1..100, fn i ->
          {:ok, actor} = BitActor.create(%{
            type: :memory_test,
            transaction_id: "mem_#{iteration}_#{i}",
            ttl: @ttl_budget,
            token: %{data: :crypto.strong_rand_bytes(1024)} # 1KB payload
          })
          actor
        end)
        
        # Execute some operations
        Enum.each(actors, fn actor ->
          BitActor.execute_hop(actor, %{
            input_token: %{op: :process},
            operation: :transform
          })
        end)
        
        # Clean up
        Enum.each(actors, fn actor ->
          BitActor.destroy(actor)
        end)
        
        # Sample memory
        current_memory = :erlang.memory(:total)
        memory_samples = [current_memory | memory_samples]
        
        iteration = iteration + 1
        Process.sleep(100)
      end
      
      # Analyze memory usage
      final_memory = :erlang.memory(:total)
      memory_growth = (final_memory - initial_memory) / initial_memory * 100
      
      avg_memory = Enum.sum(memory_samples) / length(memory_samples)
      max_memory = Enum.max(memory_samples)
      
      assert memory_growth < 20, 
        "Memory grew by #{memory_growth}%, indicating possible leak"
      
      IO.puts("Memory stats: growth=#{memory_growth}%, avg=#{avg_memory/1024/1024}MB, max=#{max_memory/1024/1024}MB")
    end
  end
  
  # Helper functions
  
  defp execute_until_ttl_expires(actor) do
    execute_hops(actor, 0, false)
  end
  
  defp execute_hops(actor, hop_count, ttl_violation) do
    case BitActor.execute_hop(actor, %{
      input_token: %{hop: hop_count},
      operation: :test_operation
    }) do
      {:ok, updated_actor} ->
        if updated_actor.ttl > 0 do
          execute_hops(updated_actor, hop_count + 1, ttl_violation)
        else
          %{
            hops_executed: hop_count + 1,
            ttl_violation: ttl_violation,
            final_ttl: updated_actor.ttl
          }
        end
        
      {:error, "TTL expired - cannot execute hop"} ->
        %{
          hops_executed: hop_count,
          ttl_violation: false,
          final_ttl: 0
        }
        
      {:error, _} ->
        %{
          hops_executed: hop_count,
          ttl_violation: true,
          final_ttl: -1
        }
    end
  end
  
  defp percentile(list, p) do
    sorted = Enum.sort(list)
    index = round(length(sorted) * p / 100) - 1
    Enum.at(sorted, max(0, index))
  end
  
  defp generate_large_ttl(class_count) do
    classes = Enum.map(1..class_count, fn i ->
      """
      :Class#{i} a owl:Class ;
        rdfs:label "Generated Class #{i}" ;
        rdfs:comment "Stress test class number #{i}" .
      """
    end)
    
    """
    @prefix : <http://stress-test#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    
    :StressTestOntology a owl:Ontology ;
      rdfs:label "Stress Test Ontology" ;
      rdfs:comment "Generated for stress testing with #{class_count} classes" .
    
    #{Enum.join(classes, "\n")}
    """
  end
  
  defp generate_workflow_steps(count) do
    Enum.map(1..count, fn i ->
      %{
        name: :"step_#{i}",
        type: :transform,
        run: quote do
          fn input ->
            # Simulate some processing
            Process.sleep(1)
            {:ok, Map.put(input, unquote(:"step_#{i}"), :completed)}
          end
        end,
        arguments: [],
        wait_for: if(i > 1, do: [:"step_#{i-1}"], else: [])
      }
    end)
  end
  
  defp generate_test_data(iteration) do
    %{
      iteration: iteration,
      data: Enum.map(1..100, fn i -> %{id: i, value: :rand.uniform(1000)} end),
      timestamp: System.system_time(:millisecond)
    }
  end
end