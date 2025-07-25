# Test script for Reactor workflow validation
Mix.install([
  {:jason, "~> 1.4"}
])

defmodule ReactorTest do
  def test_cybersecurity_workflow do
    IO.puts("=== Reactor Workflow Validation Test ===")
    
    # Test 1: Check if the workflow module can be compiled
    IO.puts("Test 1: Compiling workflow module...")
    
    try do
      Code.compile_file("generated/reactor_workflows/cybersecuritymesh/cybersecuritymesh_workflow.ex")
      IO.puts("✅ Workflow module compiled successfully")
    rescue
      error ->
        IO.puts("❌ Compilation failed: #{inspect(error)}")
        {:error, error}
    end
    
    # Test 2: Check if step modules exist and can be loaded
    IO.puts("Test 2: Checking step modules...")
    
    step_files = [
      "generated/reactor_workflows/cybersecuritymesh/cybersecuritymesh_steps.ex"
    ]
    
    Enum.each(step_files, fn file ->
      case File.exists?(file) do
        true -> 
          IO.puts("✅ Step file exists: #{file}")
          try do
            Code.compile_file(file)
            IO.puts("✅ Step module compiled")
          rescue
            error ->
              IO.puts("❌ Step compilation failed: #{inspect(error)}")
          end
        false -> 
          IO.puts("❌ Step file missing: #{file}")
      end
    end)
    
    # Test 3: Try to create test data
    IO.puts("Test 3: Creating test data...")
    
    test_data = %{
      threat_type: "malware",
      severity: "high", 
      timestamp: System.system_time(:second),
      source_ip: "192.168.1.100",
      payload: %{
        file_hash: "deadbeef123456789",
        process_name: "suspicious.exe"
      }
    }
    
    IO.puts("✅ Test data created: #{inspect(test_data, limit: :infinity)}")
    
    IO.puts("\n=== Test Summary ===")
    IO.puts("Note: Full workflow execution requires:")
    IO.puts("- Ash framework installation")
    IO.puts("- Reactor dependency")
    IO.puts("- BitActor CLI integration")
    IO.puts("- TTL semantic data processing")
    
    :ok
  end
end

# Run the test
ReactorTest.test_cybersecurity_workflow()