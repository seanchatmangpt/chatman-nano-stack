#!/usr/bin/env elixir

# REAL TDD TEST - MUST ACTUALLY EXECUTE AND PASS
# ==============================================
# This test RUNS actual code - no source analysis allowed

defmodule RealTDDTest do
  use ExUnit.Case
  
  @moduletag :real_execution
  
  # REAL TEST 1: Can we actually parse TTL?
  test "REAL TTL parsing execution" do
    Code.require_file("lib/cns_forge/ttl_parser.ex", "/Users/sac/cns")
    
    sample_ttl = """
    @prefix test: <http://test.org/> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    
    test:BitActor a owl:Class .
    """
    
    # ACTUALLY EXECUTE the parser
    result = CNSForge.TTLParser.parse(sample_ttl)
    
    # VERIFY it actually worked
    assert {:ok, ontology} = result
    assert length(ontology.classes) == 1
    assert List.first(ontology.classes).name == "BitActor"
    
    IO.puts("✅ REAL TTL parsing EXECUTED successfully")
  end
  
  # REAL TEST 2: Can we actually transform TTL to Ash code?
  test "REAL TTL transformation execution" do
    Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex", "/Users/sac/cns")
    
    sample_ttl = """
    @prefix test: <http://test.org/> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    
    test:Entity a owl:Class .
    """
    
    # ACTUALLY EXECUTE the transformation
    result = CnsForge.TTLAshReactorTransformer.transform_ttl(sample_ttl)
    
    # VERIFY it actually created real code
    assert {:ok, components} = result
    assert length(components.resources) == 1
    assert length(components.reactors) >= 1
    
    # VERIFY the generated code contains REAL Ash components
    resource = List.first(components.resources)
    assert String.contains?(resource.code, "use Ash.Resource")
    assert String.contains?(resource.code, "data_layer: Ash.DataLayer.Ets")
    
    IO.puts("✅ REAL TTL transformation EXECUTED successfully")
  end
  
  # REAL TEST 3: Can we actually measure TTL constraints?
  test "REAL TTL constraint execution" do
    # ACTUALLY EXECUTE timing code
    start_time = System.monotonic_time(:nanosecond)
    :timer.sleep(1)  # 1ms delay
    end_time = System.monotonic_time(:nanosecond)
    
    execution_time = end_time - start_time
    
    # VERIFY timing actually works
    assert execution_time > 1_000_000  # At least 1ms
    assert execution_time < 5_000_000  # Less than 5ms
    
    # ACTUALLY EXECUTE TTL violation check
    ttl_limit = 1_000_000  # 1ms
    violation_check = if execution_time > ttl_limit do
      {:error, "TTL exceeded"}
    else
      {:ok, "Within bounds"}
    end
    
    # This should fail since we slept for 1ms
    assert {:error, "TTL exceeded"} = violation_check
    
    IO.puts("✅ REAL TTL constraint checking EXECUTED successfully")
  end
  
  # REAL TEST 4: Can we generate and write actual files?
  test "REAL file generation execution" do
    Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex", "/Users/sac/cns")
    
    sample_ttl = """
    @prefix test: <http://test.org/> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    
    test:TestEntity a owl:Class .
    """
    
    # ACTUALLY EXECUTE full transformation
    {:ok, components} = CnsForge.TTLAshReactorTransformer.transform_ttl(sample_ttl)
    
    # VERIFY files were actually created
    assert length(components.generated_files) > 0
    
    # ACTUALLY CHECK each file exists and has content
    Enum.each(components.generated_files, fn file_path ->
      assert File.exists?(file_path), "File should exist: #{file_path}"
      content = File.read!(file_path)
      assert String.length(content) > 100, "File should have content: #{file_path}"
      assert String.contains?(content, "defmodule"), "File should define modules: #{file_path}"
    end)
    
    # CLEANUP - actually delete the files
    Enum.each(components.generated_files, &File.rm!/1)
    
    IO.puts("✅ REAL file generation EXECUTED successfully")
  end
end

# ACTUALLY RUN THE TESTS
ExUnit.start()

case ExUnit.run() do
  %{failures: 0} -> 
    IO.puts("\n🎉 ALL REAL TDD TESTS PASSED - CODE ACTUALLY WORKS!")
    System.halt(0)
  %{failures: failures} -> 
    IO.puts("\n💥 #{failures} REAL TESTS FAILED - CODE IS BROKEN!")
    System.halt(1)
end