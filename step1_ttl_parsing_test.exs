#!/usr/bin/env elixir

# 🧪 STEP 1 INCREMENTAL TEST: TTL Parsing Validation
# Direct functional testing without Mix dependency issues

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule Step1TTLParsingTest do
  @moduledoc """
  🛡️ CLEAN Step 1 Testing - TTL Parsing Validation
  Testing parse_ttl/1 function directly
  """
  
  def run_all_tests do
    IO.puts("🧪 STEP 1: TTL PARSING VALIDATION")
    IO.puts("=" <> String.duplicate("=", 40))
    
    test_valid_ttl_parsing()
    test_empty_ttl_content()
    test_ttl_with_no_classes()
    test_malformed_ttl_handling()
    
    IO.puts("\n✅ STEP 1 TESTING COMPLETE")
  end
  
  defp test_valid_ttl_parsing do
    IO.puts("\n🔍 Test 1: Valid TTL Content Parsing")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:Person a owl:Class .
    test:Organization a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        classes = result.classes
        IO.puts("   ✅ Parse successful")
        IO.puts("   ✅ Classes found: #{length(classes)}")
        
        if length(classes) == 2 do
          IO.puts("   ✅ Expected class count: 2")
        else
          IO.puts("   ❌ Expected 2 classes, got #{length(classes)}")
        end
        
        class_names = Enum.map(classes, & &1.name)
        if "Person" in class_names and "Organization" in class_names do
          IO.puts("   ✅ Class names correct: #{inspect(class_names)}")
        else
          IO.puts("   ❌ Expected Person and Organization, got: #{inspect(class_names)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Parse failed: #{reason}")
    end
  end
  
  defp test_empty_ttl_content do
    IO.puts("\n🔍 Test 2: Empty TTL Content")
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl("") do
      {:ok, result} ->
        if result.classes == [] do
          IO.puts("   ✅ Empty TTL handled correctly")
        else
          IO.puts("   ❌ Expected no classes, got: #{length(result.classes)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Unexpected error: #{reason}")
    end
  end
  
  defp test_ttl_with_no_classes do
    IO.puts("\n🔍 Test 3: TTL with No Classes")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    # Just prefixes, no classes
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        if result.classes == [] do
          IO.puts("   ✅ Prefix-only TTL handled correctly")
        else
          IO.puts("   ❌ Expected no classes, got: #{length(result.classes)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Unexpected error: #{reason}")
    end
  end
  
  defp test_malformed_ttl_handling do
    IO.puts("\n🔍 Test 4: Malformed TTL Handling")
    
    malformed_ttl = "this is not valid TTL at all"
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(malformed_ttl) do
      {:ok, result} ->
        if result.classes == [] do
          IO.puts("   ✅ Malformed TTL handled gracefully")
        else
          IO.puts("   ❌ Expected no classes from malformed TTL, got: #{length(result.classes)}")
        end
        
      {:error, _reason} ->
        IO.puts("   ✅ Malformed TTL returned error (acceptable)")
    end
  end
end

# Execute Step 1 Tests
Step1TTLParsingTest.run_all_tests()