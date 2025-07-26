#!/usr/bin/env elixir

# Test the Working Ash & Reactor Implementation
# ============================================

Code.require_file("working_ash_reactor_implementation.ex")

defmodule TestWorkingImplementation do
  alias WorkingAshReactor.Resource.BitActor
  alias WorkingAshReactor.Resource.Signal
  
  def run_all_tests do
    IO.puts("\n🧪 TESTING WORKING ASH & REACTOR IMPLEMENTATION\n")
    
    test_resource_creation()
    test_reactor_workflows()
    test_ttl_constraints()
    test_signal_processing()
    
    IO.puts("\n✅ ALL TESTS COMPLETED\n")
  end
  
  defp test_resource_creation do
    IO.puts("1️⃣ Testing Resource Creation...")
    
    # Create BitActor
    case WorkingAshReactor.create_bitactor("TestActor", 10) do
      {:ok, bitactor} ->
        IO.puts("   ✓ Created BitActor: #{bitactor.name} (ID: #{bitactor.id})")
        IO.puts("     TTL Budget: #{bitactor.ttl_budget}ms")
        IO.puts("     Status: #{bitactor.status}")
      {:error, error} ->
        IO.puts("   ✗ Failed to create BitActor: #{inspect(error)}")
    end
    
    # Create Signal
    case WorkingAshReactor.create_signal("market_data", %{price: 100.5, volume: 1000}, 3) do
      {:ok, signal} ->
        IO.puts("   ✓ Created Signal: #{signal.type} (Priority: #{signal.priority})")
        IO.puts("     Payload: #{inspect(signal.payload)}")
      {:error, error} ->
        IO.puts("   ✗ Failed to create Signal: #{inspect(error)}")
    end
  end
  
  defp test_reactor_workflows do
    IO.puts("\n2️⃣ Testing Reactor Workflows...")
    
    # Test process_signals operation
    result = WorkingAshReactor.run_main_coordinator(
      "process_signals",
      %{signal_count: 5},
      %{max_execution_ms: 1000}
    )
    
    case result do
      {:ok, data} ->
        IO.puts("   ✓ Process Signals workflow succeeded")
        IO.puts("     Processed: #{data.processed} signals")
        IO.puts("     Execution time: #{data.execution_time_ms}ms")
        IO.puts("     TTL compliant: #{data.ttl_compliant}")
      {:error, error} ->
        IO.puts("   ✗ Process Signals workflow failed: #{error}")
    end
    
    # Test collect_telemetry operation
    result = WorkingAshReactor.run_main_coordinator(
      "collect_telemetry",
      %{metrics: ["cpu", "memory", "latency"]},
      %{max_execution_ms: 500}
    )
    
    case result do
      {:ok, data} ->
        IO.puts("   ✓ Collect Telemetry workflow succeeded")
        IO.puts("     Metrics collected: #{data.metrics_collected}")
      {:error, error} ->
        IO.puts("   ✗ Collect Telemetry workflow failed: #{error}")
    end
    
    # Test invalid operation
    result = WorkingAshReactor.run_main_coordinator(
      "invalid_operation",
      %{},
      %{}
    )
    
    case result do
      {:ok, _} ->
        IO.puts("   ✗ Invalid operation should have failed!")
      {:error, error} ->
        IO.puts("   ✓ Invalid operation correctly rejected: #{error}")
    end
  end
  
  defp test_ttl_constraints do
    IO.puts("\n3️⃣ Testing TTL Constraints...")
    
    # Test with very short TTL (should fail)
    result = WorkingAshReactor.run_main_coordinator(
      "coordinate_swarm",
      %{agent_count: 10},
      %{max_execution_ms: 1}
    )
    
    case result do
      {:ok, _} ->
        IO.puts("   ⚠️ TTL constraint should have been violated")
      {:error, error} ->
        IO.puts("   ✓ TTL constraint correctly enforced: #{error}")
    end
    
    # Test BitActor TTL processing
    {:ok, bitactor} = WorkingAshReactor.create_bitactor("TTLTestActor", 1)
    {:ok, signal} = WorkingAshReactor.create_signal("test", %{}, 1)
    
    case BitActor.process_signal(bitactor, %{signal_id: signal.id}) do
      {:ok, updated} ->
        IO.puts("   ✓ BitActor processed signal within TTL")
        IO.puts("     Processing time: #{updated.processing_time_ns}ns")
      {:error, %Ash.Error.Invalid{errors: [error | _]}} ->
        if String.contains?(error.message || "", "TTL violation") do
          IO.puts("   ✓ BitActor TTL constraint enforced: #{error.message}")
        else
          IO.puts("   ✗ Unexpected error: #{inspect(error)}")
        end
      {:error, error} ->
        IO.puts("   ✗ Unexpected error: #{inspect(error)}")
    end
  end
  
  defp test_signal_processing do
    IO.puts("\n4️⃣ Testing Signal Processing...")
    
    # Create test data
    {:ok, bitactor} = WorkingAshReactor.create_bitactor("SignalProcessor", 50)
    
    signals = Enum.map(1..3, fn i ->
      {:ok, signal} = WorkingAshReactor.create_signal("signal_#{i}", %{value: i * 10}, i)
      signal
    end)
    
    IO.puts("   Created #{length(signals)} test signals")
    
    # Process signals
    result = WorkingAshReactor.run_signal_processor(bitactor.id, signals)
    
    case result do
      {:ok, data} ->
        IO.puts("   ✓ Signal processing completed")
        IO.puts("     Processed: #{data.processed}/#{data.total}")
        IO.puts("     Failed: #{data.failed}")
        IO.puts("     BitActor ID: #{data.bitactor_id}")
      {:error, error} ->
        IO.puts("   ✗ Signal processing failed: #{error}")
    end
  end
end

# Check if running in proper environment
if Code.ensure_loaded?(Ash) and Code.ensure_loaded?(Reactor) do
  TestWorkingImplementation.run_all_tests()
else
  IO.puts("""
  ⚠️ This test requires Ash and Reactor to be loaded.
  
  Run in a Mix project with dependencies:
  - {:ash, "~> 3.0"}
  - {:reactor, "~> 0.8"}
  
  Or use: mix run test_working_implementation.exs
  """)
end