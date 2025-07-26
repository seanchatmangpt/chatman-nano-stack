#!/usr/bin/env elixir

# 🏗️ STEP 5 INCREMENTAL TEST: Domain Generation Verification
# Testing generate_simple_domain/0 function

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule Step5DomainGenerationTest do
  @moduledoc """
  🛡️ CLEAN Step 5 Testing - Domain Generation Verification
  Testing Ash Domain generation functionality
  """
  
  def run_all_tests do
    IO.puts("🏗️ STEP 5: DOMAIN GENERATION VERIFICATION")
    IO.puts("=" <> String.duplicate("=", 40))
    
    test_domain_code_generation()
    test_required_components()
    test_domain_structure()
    test_authorization_configuration()
    
    IO.puts("\n✅ STEP 5 TESTING COMPLETE")
  end
  
  defp test_domain_code_generation do
    IO.puts("\n🏗️ Test 1: Domain Code Generation")
    
    # Access domain through public transform_ttl function
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:TestClass a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        domain_code = result.domain
        
        if is_binary(domain_code) and String.length(domain_code) > 0 do
          IO.puts("   ✅ Domain code generated successfully")
          IO.puts("   ✅ Code length: #{String.length(domain_code)} characters")
        else
          IO.puts("   ❌ Domain code generation failed or empty")
        end
        
        # Check if it's valid Elixir-like structure
        if String.contains?(domain_code, "defmodule") and String.contains?(domain_code, "end") do
          IO.puts("   ✅ Contains valid module structure")
        else
          IO.puts("   ❌ Missing valid module structure")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Transform failed: #{reason}")
    end
  end
  
  defp test_required_components do
    IO.puts("\n🏗️ Test 2: Required Domain Components")
    
    ttl = "@prefix owl: <http://www.w3.org/2002/07/owl#> .\ntest:Test a owl:Class ."
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        domain_code = result.domain
        
        # Check for required components
        required_components = [
          "defmodule CnsForge.TTLDomain",
          "use Ash.Domain",
          "authorize :when_requested"
        ]
        
        IO.puts("   ✅ Domain code components:")
        
        Enum.each(required_components, fn component ->
          if String.contains?(domain_code, component) do
            IO.puts("   ✅ Contains: #{component}")
          else
            IO.puts("   ❌ Missing: #{component}")
          end
        end)
        
      {:error, reason} ->
        IO.puts("   ❌ Transform failed: #{reason}")
    end
  end
  
  defp test_domain_structure do
    IO.puts("\n🏗️ Test 3: Domain Structure Validation")
    
    ttl = "@prefix owl: <http://www.w3.org/2002/07/owl#> .\ntest:Test a owl:Class ."
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        domain_code = result.domain
    
    # Check for proper structure elements
    structure_elements = [
      "@moduledoc",
      "authorization do",
      "end"
    ]
    
    IO.puts("   ✅ Domain structure elements:")
    
    Enum.each(structure_elements, fn element ->
      if String.contains?(domain_code, element) do
        IO.puts("   ✅ Contains: #{element}")
      else
        IO.puts("   ❌ Missing: #{element}")
      end
    end)
    
    # Check that module opens and closes properly
    defmodule_count = length(String.split(domain_code, "defmodule")) - 1 
    end_count = length(String.split(domain_code, ~r/\bend\b/)) - 1
    
        if defmodule_count == end_count and defmodule_count == 1 do
          IO.puts("   ✅ Balanced module structure (1 defmodule, 1+ end)")
        else
          IO.puts("   ❌ Unbalanced module structure")
          IO.puts("       defmodule count: #{defmodule_count}, end count: #{end_count}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Transform failed: #{reason}")
    end
  end
  
  defp test_authorization_configuration do
    IO.puts("\n🏗️ Test 4: Authorization Configuration")
    
    ttl = "@prefix owl: <http://www.w3.org/2002/07/owl#> .\ntest:Test a owl:Class ."
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        domain_code = result.domain
    
    # Check authorization block
    if String.contains?(domain_code, "authorization do") do
      IO.puts("   ✅ Authorization block present")
      
      if String.contains?(domain_code, "authorize :when_requested") do
        IO.puts("   ✅ Correct authorization setting: :when_requested")
      else
        IO.puts("   ❌ Missing or incorrect authorization setting")
      end
      
    else
      IO.puts("   ❌ Authorization block missing")
    end
    
    # Check for clean TTL Domain naming
    if String.contains?(domain_code, "Clean TTL Domain") do
      IO.puts("   ✅ Contains descriptive moduledoc")
    else
      IO.puts("   ❌ Missing or incorrect moduledoc")
    end
    
    # Verify no malicious code patterns
    malicious_patterns = ["System.cmd", "File.write", "Process.spawn", ":os.cmd"]
    
    malicious_found = Enum.any?(malicious_patterns, &String.contains?(domain_code, &1))
    
        if not malicious_found do
          IO.puts("   ✅ No malicious code patterns detected")
        else
          IO.puts("   ❌ Malicious code patterns found!")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Transform failed: #{reason}")
    end
  end
end

# Execute Step 5 Tests
Step5DomainGenerationTest.run_all_tests()