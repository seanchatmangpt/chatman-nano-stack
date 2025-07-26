#!/usr/bin/env elixir

# MINIMAL REAL TEST - NO DEPENDENCIES
# ===================================
# Tests ACTUAL code execution without Mix/ExUnit dependencies

defmodule MinimalRealTest do
  @moduledoc """
  Tests that ACTUALLY EXECUTE code and verify it works
  NO dependencies, NO compilation issues, NO red team fakes
  """
  
  def run_all_tests do
    IO.puts("\n🔥 MINIMAL REAL TESTS - ACTUAL EXECUTION\n")
    
    test_count = 0
    pass_count = 0
    
    # Test 1: Basic TTL timing works
    {result, new_test_count, new_pass_count} = run_test("TTL Timing", test_count, pass_count, fn ->
      start_time = System.monotonic_time(:nanosecond)
      :timer.sleep(1)  # 1ms
      end_time = System.monotonic_time(:nanosecond)
      duration = end_time - start_time
      
      # Should be roughly 1ms (1,000,000 ns)
      duration >= 1_000_000 and duration < 5_000_000
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Test 2: TTL Parser module exists and can be loaded
    {result, new_test_count, new_pass_count} = run_test("TTL Parser Loading", test_count, pass_count, fn ->
      try do
        Code.require_file("lib/cns_forge/ttl_parser.ex", "/Users/sac/cns")
        # Check if module was loaded
        Code.ensure_loaded?(CNSForge.TTLParser)
      rescue
        _ -> false
      end
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Test 3: TTL Transformer module exists and can be loaded
    {result, new_test_count, new_pass_count} = run_test("TTL Transformer Loading", test_count, pass_count, fn ->
      try do
        Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex", "/Users/sac/cns")
        Code.ensure_loaded?(CnsForge.TTLAshReactorTransformer)
      rescue
        _ -> false
      end
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Test 4: Can actually parse simple TTL
    {result, new_test_count, new_pass_count} = run_test("TTL Parsing Execution", test_count, pass_count, fn ->
      try do
        simple_ttl = """
        @prefix test: <http://test.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        test:TestClass a owl:Class .
        """
        
        result = CNSForge.TTLParser.parse(simple_ttl)
        case result do
          {:ok, ontology} -> 
            length(ontology.classes) == 1 and 
            List.first(ontology.classes).name == "TestClass"
          _ -> false
        end
      rescue
        error -> 
          IO.puts("    Parse error: #{inspect(error)}")
          false
      end
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Test 5: Can actually transform TTL to code
    {result, new_test_count, new_pass_count} = run_test("TTL Transformation Execution", test_count, pass_count, fn ->
      try do
        simple_ttl = """
        @prefix test: <http://test.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        test:SimpleEntity a owl:Class .
        """
        
        result = CnsForge.TTLAshReactorTransformer.transform_ttl(simple_ttl)
        case result do
          {:ok, components} -> 
            length(components.resources) >= 1 and
            String.contains?(List.first(components.resources).code, "use Ash.Resource")
          _ -> false
        end
      rescue
        error ->
          IO.puts("    Transform error: #{inspect(error)}")
          false
      end
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Test 6: Working implementation file has no red team artifacts
    {result, new_test_count, new_pass_count} = run_test("No Red Team Artifacts", test_count, pass_count, fn ->
      if File.exists?("working_ash_reactor_implementation.ex") do
        content = File.read!("working_ash_reactor_implementation.ex")
        
        # Check for red team corruption
        no_mocks = not String.contains?(content, "mock")
        no_simulates = not String.contains?(content, "simulate")
        no_fakes = not String.contains?(content, "fake")
        no_stubs = not String.contains?(content, "stub")
        
        # Check for real Ash/Reactor code
        has_ash = String.contains?(content, "use Ash.Resource")
        has_reactor = String.contains?(content, "use Reactor")
        has_ttl = String.contains?(content, "System.monotonic_time")
        
        no_mocks and no_simulates and no_fakes and no_stubs and has_ash and has_reactor and has_ttl
      else
        false
      end
    end)
    
    test_count = new_test_count
    pass_count = new_pass_count
    
    # Summary
    IO.puts("\n📊 REAL TEST RESULTS:")
    IO.puts("Total Tests: #{test_count}")
    IO.puts("Passed: #{pass_count}")
    IO.puts("Failed: #{test_count - pass_count}")
    IO.puts("Success Rate: #{Float.round(pass_count / test_count * 100, 1)}%")
    
    if pass_count == test_count do
      IO.puts("\n✅ ALL REAL TESTS PASSED - CODE ACTUALLY WORKS!")
      :success
    else
      IO.puts("\n💥 SOME TESTS FAILED - INVESTIGATE RED TEAM CORRUPTION!")
      :failure
    end
  end
  
  defp run_test(name, test_count, pass_count, test_function) do
    new_test_count = test_count + 1
    
    IO.write("#{new_test_count}. #{name}: ")
    
    start_time = System.monotonic_time(:millisecond)
    result = test_function.()
    end_time = System.monotonic_time(:millisecond)
    duration = end_time - start_time
    
    if result do
      IO.puts("✅ PASS (#{duration}ms)")
      {result, new_test_count, pass_count + 1}
    else
      IO.puts("❌ FAIL (#{duration}ms)")
      {result, new_test_count, pass_count}
    end
  end
end

# RUN THE TESTS
case MinimalRealTest.run_all_tests() do
  :success -> System.halt(0)
  :failure -> System.halt(1)
end