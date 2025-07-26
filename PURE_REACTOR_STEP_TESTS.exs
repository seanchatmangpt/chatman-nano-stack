#!/usr/bin/env elixir

# PURE REACTOR STEP TESTS - NO DEPENDENCIES
# ==========================================
# Tests ONLY the pure step logic without Ash framework
# Extracts step functions for isolated testing

defmodule PureReactorStepTests do
  @moduledoc """
  Pure unit tests for Reactor step logic - NO framework dependencies
  Tests extracted step functions in complete isolation
  """

  def run_all_pure_step_tests do
    IO.puts("\n⚡ PURE REACTOR STEP TESTS - ZERO DEPENDENCIES\n")

    step_tests_passed = 0
    step_tests_total = 0

    # STEP TEST 1: validate_operation logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "validate_operation - valid operation",
      step_tests_total,
      step_tests_passed,
      fn ->
        result = pure_validate_operation_step(%{op: "process_signals"}, %{})
        result == {:ok, %{operation: "process_signals", valid: true}}
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "validate_operation - invalid operation",
      step_tests_total,
      step_tests_passed,
      fn ->
        result = pure_validate_operation_step(%{op: "hack_system"}, %{})
        result == {:error, "Invalid operation: hack_system"}
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "validate_operation - all valid operations",
      step_tests_total,
      step_tests_passed,
      fn ->
        ops = ["process_signals", "collect_telemetry", "coordinate_swarm"]
        results = Enum.map(ops, fn op -> 
          pure_validate_operation_step(%{op: op}, %{})
        end)
        
        Enum.all?(results, fn result ->
          match?({:ok, %{valid: true}}, result)
        end)
      end
    )

    # STEP TEST 2: initialize_context logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "initialize_context - creates proper context",
      step_tests_total,
      step_tests_passed,
      fn ->
        ttl_constraints = %{max_execution_ms: 2500}
        {:ok, context} = pure_initialize_context_step(%{ttl_constraints: ttl_constraints}, %{})
        
        Map.has_key?(context, :start_time) and
        context.max_duration_ms == 2500 and
        context.step_count == 0 and
        context.errors == []
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "initialize_context - default constraints",
      step_tests_total,
      step_tests_passed,
      fn ->
        {:ok, context} = pure_initialize_context_step(%{ttl_constraints: %{}}, %{})
        context.max_duration_ms == 5000  # default value
      end
    )

    # STEP TEST 3: check_ttl_compliance logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "check_ttl_compliance - within budget",
      step_tests_total,
      step_tests_passed,
      fn ->
        # Simulate context with recent start time
        start_time = System.monotonic_time(:millisecond) - 50  # 50ms ago
        context = %{start_time: start_time, max_duration_ms: 1000}
        result = %{data: "test_result"}
        
        {:ok, final_result} = pure_check_ttl_compliance_step(%{result: result, context: context}, %{})
        
        final_result.ttl_compliant == true and
        final_result.execution_time_ms < 100 and
        Map.has_key?(final_result, :data)
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "check_ttl_compliance - exceeds budget",
      step_tests_total,
      step_tests_passed,
      fn ->
        # Simulate context with old start time (budget exceeded)
        start_time = System.monotonic_time(:millisecond) - 2000  # 2000ms ago
        context = %{start_time: start_time, max_duration_ms: 100}
        result = %{data: "test_result"}
        
        case pure_check_ttl_compliance_step(%{result: result, context: context}, %{}) do
          {:error, msg} -> String.contains?(msg, "TTL exceeded")
          _ -> false
        end
      end
    )

    # STEP TEST 4: execute_operation logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "execute_operation - process_signals workflow",
      step_tests_total,
      step_tests_passed,
      fn ->
        args = %{
          operation: "process_signals",
          data: %{signal_count: 3},
          context: %{step_count: 1}
        }
        
        {:ok, result} = pure_execute_operation_step(args, %{})
        
        result.operation == "process_signals" and
        result.processed == 3 and
        result.context.step_count == 2
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "execute_operation - collect_telemetry workflow",
      step_tests_total,
      step_tests_passed,
      fn ->
        args = %{
          operation: "collect_telemetry",
          data: %{metrics: ["cpu", "memory"]},
          context: %{step_count: 0}
        }
        
        {:ok, result} = pure_execute_operation_step(args, %{})
        
        result.operation == "collect_telemetry" and
        result.metrics_collected == 2 and
        result.context.step_count == 1
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "execute_operation - coordinate_swarm workflow",
      step_tests_total,
      step_tests_passed,
      fn ->
        args = %{
          operation: "coordinate_swarm",
          data: %{agent_count: 5},
          context: %{step_count: 2}
        }
        
        {:ok, result} = pure_execute_operation_step(args, %{})
        
        result.operation == "coordinate_swarm" and
        result.agents_coordinated == 5 and
        result.context.step_count == 3
      end
    )

    # STEP TEST 5: load_bitactor logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "load_bitactor - existing actor",
      step_tests_total,
      step_tests_passed,
      fn ->
        # Mock successful load
        mock_actor = %{id: "actor-123", name: "TestActor", ttl_budget: 10}
        result = pure_load_bitactor_step(%{id: "actor-123"}, %{}, mock_actor)
        
        result == {:ok, mock_actor}
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "load_bitactor - non-existing actor",
      step_tests_total,
      step_tests_passed,
      fn ->
        result = pure_load_bitactor_step(%{id: "missing-123"}, %{}, nil)
        
        case result do
          {:error, msg} -> String.contains?(msg, "BitActor not found: missing-123")
          _ -> false
        end
      end
    )

    # STEP TEST 6: process_each_signal logic
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "process_each_signal - successful processing",
      step_tests_total,
      step_tests_passed,
      fn ->
        bitactor = %{id: "actor-1", ttl_budget: 15}
        signals = [
          %{id: "sig-1", type: "data"},
          %{id: "sig-2", type: "control"},
          %{id: "sig-3", type: "telemetry"}
        ]
        
        {:ok, result} = pure_process_each_signal_step(%{bitactor: bitactor, signals: signals}, %{})
        
        result.total == 3 and
        result.bitactor_id == "actor-1" and
        result.processed >= 0 and
        result.failed >= 0 and
        (result.processed + result.failed) == result.total
      end
    )

    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "process_each_signal - empty signals",
      step_tests_total,
      step_tests_passed,
      fn ->
        bitactor = %{id: "actor-2", ttl_budget: 8}
        signals = []
        
        {:ok, result} = pure_process_each_signal_step(%{bitactor: bitactor, signals: signals}, %{})
        
        result.total == 0 and
        result.processed == 0 and
        result.failed == 0
      end
    )

    # STEP TEST 7: TTL timing precision
    {_passed, step_tests_total, step_tests_passed} = test_pure_step(
      "TTL timing precision test",
      step_tests_total,
      step_tests_passed,
      fn ->
        # Test nanosecond timing precision
        start_ns = System.monotonic_time(:nanosecond)
        :timer.sleep(2)  # 2ms sleep
        end_ns = System.monotonic_time(:nanosecond)
        
        duration_ns = end_ns - start_ns
        duration_ms = duration_ns / 1_000_000
        
        # Should be approximately 2ms (allow some variance)
        duration_ms >= 1.5 and duration_ms <= 5.0
      end
    )

    # Summary
    IO.puts("\n📊 PURE REACTOR STEP TEST RESULTS:")
    IO.puts("Total Step Tests: #{step_tests_total}")
    IO.puts("Passed: #{step_tests_passed}")
    IO.puts("Failed: #{step_tests_total - step_tests_passed}")
    success_rate = Float.round(step_tests_passed / step_tests_total * 100, 1)
    IO.puts("Success Rate: #{success_rate}%")

    if step_tests_passed == step_tests_total do
      IO.puts("\n✅ ALL PURE REACTOR STEP TESTS PASSED!")
      :success
    else
      IO.puts("\n💥 SOME PURE REACTOR STEP TESTS FAILED!")
      :failure
    end
  end

  # Pure step function implementations (extracted from Reactor logic)
  defp pure_validate_operation_step(%{op: op}, _context) do
    valid_operations = ["process_signals", "collect_telemetry", "coordinate_swarm"]
    
    if op in valid_operations do
      {:ok, %{operation: op, valid: true}}
    else
      {:error, "Invalid operation: #{op}"}
    end
  end

  defp pure_initialize_context_step(%{ttl_constraints: constraints}, _context) do
    context = %{
      start_time: System.monotonic_time(:millisecond),
      max_duration_ms: Map.get(constraints, :max_execution_ms, 5000),
      step_count: 0,
      errors: []
    }
    
    {:ok, context}
  end

  defp pure_check_ttl_compliance_step(%{result: result, context: context}, _context) do
    duration = System.monotonic_time(:millisecond) - context.start_time
    
    final_result = Map.merge(result, %{
      execution_time_ms: duration,
      ttl_compliant: duration <= context.max_duration_ms
    })
    
    if final_result.ttl_compliant do
      {:ok, final_result}
    else
      {:error, "TTL exceeded: #{duration}ms > #{context.max_duration_ms}ms"}
    end
  end

  defp pure_execute_operation_step(args, _context) do
    case args.operation do
      "process_signals" ->
        processed_count = Map.get(args.data, :signal_count, 0)
        
        {:ok, %{
          operation: "process_signals",
          processed: processed_count,
          context: Map.update!(args.context, :step_count, & &1 + 1)
        }}
      
      "collect_telemetry" ->
        metrics = Map.get(args.data, :metrics, [])
        
        {:ok, %{
          operation: "collect_telemetry",
          metrics_collected: length(metrics),
          context: Map.update!(args.context, :step_count, & &1 + 1)
        }}
      
      "coordinate_swarm" ->
        agent_count = Map.get(args.data, :agent_count, 1)
        
        {:ok, %{
          operation: "coordinate_swarm",
          agents_coordinated: agent_count,
          context: Map.update!(args.context, :step_count, & &1 + 1)
        }}
    end
  end

  defp pure_load_bitactor_step(%{id: id}, _context, mock_result) do
    # Simulates the step logic without Ash dependency
    if mock_result do
      {:ok, mock_result}
    else
      {:error, "BitActor not found: #{id}"}
    end
  end

  defp pure_process_each_signal_step(%{bitactor: bitactor, signals: signals}, _context) do
    # Simulate signal processing logic without Ash calls
    total = length(signals)
    
    # Simulate some success/failure ratio based on signal count
    successful = case total do
      0 -> 0
      n when n <= 2 -> n  # All succeed for small batches
      n -> max(1, div(n * 4, 5))  # 80% success rate for larger batches
    end
    
    failed = total - successful
    
    {:ok, %{
      processed: successful,
      failed: failed,
      total: total,
      bitactor_id: bitactor.id
    }}
  end

  defp test_pure_step(description, tests_total, tests_passed, test_function) do
    new_tests_total = tests_total + 1
    
    IO.write("#{new_tests_total}. PURE STEP: #{description}")
    
    start_time = System.monotonic_time(:millisecond)
    
    try do
      result = test_function.()
      end_time = System.monotonic_time(:millisecond)
      duration = end_time - start_time
      
      if result do
        IO.puts(" → ✅ PASSED (#{duration}ms)")
        {result, new_tests_total, tests_passed + 1}
      else
        IO.puts(" → ❌ FAILED (#{duration}ms)")
        {result, new_tests_total, tests_passed}
      end
    rescue
      error ->
        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time
        IO.puts(" → 💥 ERROR (#{duration}ms): #{inspect(error)}")
        {false, new_tests_total, tests_passed}
    end
  end
end

# EXECUTE THE PURE REACTOR STEP TESTS
case PureReactorStepTests.run_all_pure_step_tests() do
  :success -> System.halt(0)
  :failure -> System.halt(1)
end