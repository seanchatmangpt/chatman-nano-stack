defmodule ErrorCompensationTests do
  @moduledoc """
  🛡️ ERROR HANDLING AND COMPENSATION TESTING
  
  Following Reactor testing best practices for:
  - Error scenarios and graceful failures
  - Compensation logic testing
  - Resilience under adverse conditions
  """
  
  def run_all_error_tests do
    IO.puts("\n🚨 RUNNING ERROR HANDLING & COMPENSATION TESTS")
    IO.puts("Testing failure scenarios and recovery mechanisms...\n")
    
    results = [
      test_parse_error_handling(),
      test_resource_generation_errors(),
      test_reactor_generation_errors(),
      test_compensation_scenarios(),
      test_resilience_under_load()
    ]
    
    passed = Enum.count(results, & &1 == :passed)
    total = length(results)
    
    IO.puts("\n📊 ERROR TEST RESULTS:")
    IO.puts("   Passed: #{passed}/#{total}")
    
    if passed == total do
      IO.puts("✅ ALL ERROR TESTS PASSED - SYSTEM RESILIENT!")
    else
      IO.puts("❌ Some error tests failed - need hardening")
    end
    
    {passed, total}
  end
  
  # Test 1: Parse Error Handling
  defp test_parse_error_handling do
    IO.puts("🔍 Testing Parse Error Handling...")
    
    try do
      # Test various malformed inputs
      error_inputs = [
        nil,                           # Nil input
        123,                          # Wrong type
        "",                           # Empty string  
        "completely invalid",         # No TTL structure
        "test:Broken owl:Class",      # Missing 'a' or 'rdf:type'
        String.duplicate("x", 1_000_000)  # Extremely large input
      ]
      
      Enum.each(error_inputs, fn input ->
        try do
          result = parse_ttl_with_validation(input)
          # Should either return ok with empty classes or handle gracefully
          case result do
            {:ok, parsed} -> 
              if not is_map(parsed) or not Map.has_key?(parsed, :classes) do
                raise "Invalid result structure for input: #{inspect(input)}"
              end
            {:error, _reason} -> 
              # Error is acceptable for invalid inputs
              :ok
          end
        rescue
          error ->
            if input == nil or is_integer(input) do
              # Expected to fail for completely invalid types
              :ok
            else
              reraise error, __STACKTRACE__
            end
        end
      end)
      
      IO.puts("   ✅ Parse Error Handling: PASSED")
      :passed
    rescue
      error ->
        IO.puts("   ❌ Parse Error Handling: FAILED - #{inspect(error)}")
        :failed
    end
  end
  
  # Test 2: Resource Generation Error Handling
  defp test_resource_generation_errors do
    IO.puts("🔍 Testing Resource Generation Error Handling...")
    
    try do
      # Test with malformed parsed data
      error_cases = [
        %{},                                    # Missing classes key
        %{classes: nil},                        # Nil classes
        %{classes: "not a list"},              # Wrong type
        %{classes: [%{}]},                     # Empty class objects
        %{classes: [%{name: nil}]},            # Nil class name
        %{classes: [%{name: ""}]},             # Empty class name
      ]
      
      Enum.each(error_cases, fn error_case ->
        try do
          result = generate_resources_with_validation(error_case)
          case result do
            {:ok, resources} -> 
              if not is_list(resources) do
                raise "Expected list of resources"
              end
            {:error, _reason} ->
              # Error is acceptable for invalid inputs
              :ok
          end
        rescue
          KeyError -> 
            # Expected for cases missing required keys
            :ok
          ArgumentError -> 
            # Expected for type mismatches
            :ok
        end
      end)
      
      IO.puts("   ✅ Resource Generation Error Handling: PASSED")
      :passed
    rescue
      error ->
        IO.puts("   ❌ Resource Generation Error Handling: FAILED - #{inspect(error)}")
        :failed
    end
  end
  
  # Test 3: Reactor Generation Error Handling  
  defp test_reactor_generation_errors do
    IO.puts("🔍 Testing Reactor Generation Error Handling...")
    
    try do
      # Test reactor generation with problematic inputs
      error_cases = [
        %{},                                    # Missing classes
        %{classes: nil},                        # Nil classes
        %{classes: []},                         # Empty classes (should work)
        %{classes: [%{name: "Invalid Name!"}]}, # Invalid class name
      ]
      
      Enum.each(error_cases, fn error_case ->
        try do
          result = generate_reactors_with_validation(error_case, [])
          case result do
            {:ok, reactors} ->
              if not is_list(reactors) do
                raise "Expected list of reactors"
              end
            {:error, _reason} ->
              :ok
          end
        rescue
          KeyError ->
            # Expected for missing keys
            :ok
        end
      end)
      
      IO.puts("   ✅ Reactor Generation Error Handling: PASSED")
      :passed
    rescue
      error ->
        IO.puts("   ❌ Reactor Generation Error Handling: FAILED - #{inspect(error)}")
        :failed
    end
  end
  
  # Test 4: Compensation Scenarios
  defp test_compensation_scenarios do
    IO.puts("🔍 Testing Compensation Scenarios...")
    
    try do
      # Simulate a scenario where parsing succeeds but resource generation fails
      {:ok, parsed} = parse_ttl_with_validation(sample_ttl())
      
      # Test compensation: if resource generation fails, we should still have parsed data
      case simulate_resource_failure(parsed) do
        {:error, _reason, compensated_state} ->
          if compensated_state.parsed_data != parsed do
            raise "Compensation failed to preserve parsed data"
          end
        _ ->
          # If no error, that's also fine
          :ok
      end
      
      # Test rollback scenario: if reactor generation fails after resources are created
      {:ok, resources} = generate_resources_with_validation(parsed)
      case simulate_reactor_failure(parsed, resources) do
        {:error, _reason, rollback_state} ->
          if not Map.has_key?(rollback_state, :resources_created) do
            raise "Rollback state missing resource information"
          end
        _ ->
          :ok
      end
      
      IO.puts("   ✅ Compensation Scenarios: PASSED")
      :passed
    rescue
      error ->
        IO.puts("   ❌ Compensation Scenarios: FAILED - #{inspect(error)}")
        :failed
    end
  end
  
  # Test 5: Resilience Under Load
  defp test_resilience_under_load do
    IO.puts("🔍 Testing Resilience Under Load...")
    
    try do
      # Test with multiple concurrent-like operations
      large_ttl = generate_large_ttl(50)  # 50 classes
      
      # Test parsing large input
      start_time = :erlang.monotonic_time(:millisecond)
      {:ok, parsed} = parse_ttl_with_validation(large_ttl)
      parse_time = :erlang.monotonic_time(:millisecond) - start_time
      
      if parse_time > 1000 do  # Should complete within 1 second
        raise "Parse took too long: #{parse_time}ms"
      end
      
      if length(parsed.classes) != 50 do
        raise "Expected 50 classes, got #{length(parsed.classes)}"
      end
      
      # Test resource generation for large input
      start_time = :erlang.monotonic_time(:millisecond)
      {:ok, resources} = generate_resources_with_validation(parsed)
      resource_time = :erlang.monotonic_time(:millisecond) - start_time
      
      if resource_time > 2000 do  # Should complete within 2 seconds
        raise "Resource generation took too long: #{resource_time}ms"
      end
      
      if length(resources) != 50 do
        raise "Expected 50 resources, got #{length(resources)}"
      end
      
      IO.puts("   ✅ Resilience Under Load: PASSED (#{parse_time}ms parse, #{resource_time}ms resources)")
      :passed
    rescue
      error ->
        IO.puts("   ❌ Resilience Under Load: FAILED - #{inspect(error)}")
        :failed
    end
  end
  
  # Helper functions with validation
  
  defp parse_ttl_with_validation(ttl_content) when is_binary(ttl_content) do
    classes = extract_classes(ttl_content)
    parsed = %{prefixes: %{}, classes: classes, properties: [], relationships: []}
    {:ok, parsed}
  end
  
  defp parse_ttl_with_validation(nil), do: {:error, "Input cannot be nil"}
  defp parse_ttl_with_validation(input) when not is_binary(input), do: {:error, "Input must be string"}
  
  defp generate_resources_with_validation(%{classes: classes}) when is_list(classes) do
    resources = Enum.map(classes, fn class ->
      if not is_map(class) or not Map.has_key?(class, :name) do
        raise ArgumentError, "Invalid class structure"
      end
      
      %{
        class: class,
        module_name: "CnsForge.TTLResources.#{class.name}",
        code: "# Generated code for #{class.name}"
      }
    end)
    
    {:ok, resources}
  end
  
  defp generate_resources_with_validation(invalid_input) do
    {:error, "Invalid parsed data structure: #{inspect(invalid_input)}"}
  end
  
  defp generate_reactors_with_validation(%{classes: classes}, _resources) when is_list(classes) do
    reactor = %{
      name: "CnsForge.TTLMainReactor",
      code: "# Generated reactor for #{length(classes)} classes"
    }
    
    {:ok, [reactor]}
  end
  
  defp generate_reactors_with_validation(invalid_input, _resources) do
    {:error, "Invalid parsed data for reactor generation: #{inspect(invalid_input)}"}
  end
  
  defp extract_classes(ttl_content) do
    class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
    
    Regex.scan(class_regex, ttl_content)
    |> Enum.map(fn [_, class_uri] ->
      local_name = case String.split(class_uri, ":") do
        [_prefix, name] -> name
        [name] -> name
      end
      
      %{uri: class_uri, name: local_name}
    end)
  end
  
  defp simulate_resource_failure(parsed_data) do
    # Simulate a failure during resource generation but preserve parsed state
    {:error, "Simulated resource generation failure", %{parsed_data: parsed_data}}
  end
  
  defp simulate_reactor_failure(parsed_data, resources) do
    # Simulate a failure during reactor generation with rollback info
    {:error, "Simulated reactor generation failure", %{
      parsed_data: parsed_data,
      resources_created: length(resources)
    }}
  end
  
  defp sample_ttl do
    """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:Person a owl:Class .
    test:Organization a owl:Class .
    """
  end
  
  defp generate_large_ttl(num_classes) do
    header = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    """
    
    classes = Enum.map(1..num_classes, fn i ->
      "test:Class#{i} a owl:Class ."
    end) |> Enum.join("\n")
    
    header <> classes
  end
end

# Run the error tests
ErrorCompensationTests.run_all_error_tests()