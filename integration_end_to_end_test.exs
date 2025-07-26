#!/usr/bin/env elixir

# 🚀 INTEGRATION TEST: End-to-End Pipeline Validation
# Testing complete transform_ttl/1 workflow

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule IntegrationEndToEndTest do
  @moduledoc """
  🛡️ CLEAN Integration Testing - End-to-End Pipeline Validation
  Testing complete TTL → Ash.Reactor transformation pipeline
  """
  
  def run_all_tests do
    IO.puts("🚀 INTEGRATION: END-TO-END PIPELINE VALIDATION")
    IO.puts("=" <> String.duplicate("=", 50))
    
    test_complete_transformation_pipeline()
    test_result_structure_validation()
    test_error_handling()
    test_multi_class_scenarios()
    
    IO.puts("\n✅ INTEGRATION TESTING COMPLETE")
  end
  
  defp test_complete_transformation_pipeline do
    IO.puts("\n🚀 Test 1: Complete Transformation Pipeline")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:User a owl:Class .
    test:Role a owl:Class .
    """
    
    start_time = System.monotonic_time(:millisecond)
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time
        
        IO.puts("   ✅ Pipeline execution successful")
        IO.puts("   ✅ Execution time: #{duration}ms")
        
        # Verify all pipeline components are present
        required_keys = [:parsed_ontology, :resources, :reactors, :domain]
        missing_keys = required_keys -- Map.keys(result)
        
        if missing_keys == [] do
          IO.puts("   ✅ All pipeline components present")
        else
          IO.puts("   ❌ Missing components: #{inspect(missing_keys)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Pipeline failed: #{reason}")
    end
  end
  
  defp test_result_structure_validation do
    IO.puts("\n🚀 Test 2: Complete Result Structure Validation")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix example: <http://example.org/> .
    
    example:Person a owl:Class .
    example:Organization a owl:Class .
    example:Document a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        # Test parsed_ontology structure
        parsed = result.parsed_ontology
        IO.puts("   ✅ Parsed ontology classes: #{length(parsed.classes)}")
        
        if length(parsed.classes) == 3 do
          IO.puts("   ✅ Correct class count in parsed ontology")
        else
          IO.puts("   ❌ Expected 3 classes, got #{length(parsed.classes)}")
        end
        
        # Test resources structure
        resources = result.resources
        IO.puts("   ✅ Generated resources: #{length(resources)}")
        
        if length(resources) == 3 do
          IO.puts("   ✅ Correct resource count")
        else
          IO.puts("   ❌ Expected 3 resources, got #{length(resources)}")
        end
        
        # Verify each resource has proper structure
        all_resources_valid = Enum.all?(resources, fn resource ->
          Map.has_key?(resource, :class) and 
          Map.has_key?(resource, :module_name) and 
          Map.has_key?(resource, :code) and
          String.contains?(resource.code, "use Ash.Resource")
        end)
        
        if all_resources_valid do
          IO.puts("   ✅ All resources have valid structure")
        else
          IO.puts("   ❌ Some resources have invalid structure")
        end
        
        # Test reactors structure
        reactors = result.reactors
        IO.puts("   ✅ Generated reactors: #{length(reactors)}")
        
        if length(reactors) == 1 do
          IO.puts("   ✅ Correct reactor count")
          
          reactor = hd(reactors)
          if String.contains?(reactor.code, "use Reactor") do
            IO.puts("   ✅ Reactor has valid workflow structure")
          else
            IO.puts("   ❌ Reactor missing workflow structure")
          end
        else
          IO.puts("   ❌ Expected 1 reactor, got #{length(reactors)}")
        end
        
        # Test domain structure
        domain = result.domain
        if is_binary(domain) and String.contains?(domain, "use Ash.Domain") do
          IO.puts("   ✅ Domain has valid structure")
        else
          IO.puts("   ❌ Domain has invalid structure")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Transform failed: #{reason}")
    end
  end
  
  defp test_error_handling do
    IO.puts("\n🚀 Test 3: Error Handling and Resilience")
    
    # Test invalid input type
    IO.puts("   Testing invalid input type...")
    try do
      CnsForge.TTLAshReactorTransformer.transform_ttl(123)
      IO.puts("   ❌ Should have failed for invalid input type")
    catch
      :error, %FunctionClauseError{} ->
        IO.puts("   ✅ Invalid input type handled correctly")
      :error, reason ->
        IO.puts("   ❌ Unexpected error for invalid type: #{inspect(reason)}")
    end
    
    # Test nil input
    IO.puts("   Testing nil input...")
    try do
      CnsForge.TTLAshReactorTransformer.transform_ttl(nil)
      IO.puts("   ❌ Should have failed for nil input")
    catch
      :error, %FunctionClauseError{} ->
        IO.puts("   ✅ Nil input handled correctly")
      :error, reason ->
        IO.puts("   ❌ Unexpected error for nil: #{inspect(reason)}")
    end
    
    # Test empty string (should succeed)
    IO.puts("   Testing empty string...")
    case CnsForge.TTLAshReactorTransformer.transform_ttl("") do
      {:ok, result} ->
        if length(result.parsed_ontology.classes) == 0 do
          IO.puts("   ✅ Empty string handled correctly")
        else
          IO.puts("   ❌ Empty string should produce no classes")
        end
      {:error, reason} ->
        IO.puts("   ❌ Empty string should not fail: #{reason}")
    end
  end
  
  defp test_multi_class_scenarios do
    IO.puts("\n🚀 Test 4: Multi-Class Complex Scenarios")
    
    # Test complex TTL with various class patterns
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix cyber: <http://cybersecurity.org/> .
    @prefix infra: <http://infrastructure.org/> .
    
    cyber:Threat a owl:Class .
    cyber:Vulnerability rdf:type owl:Class .
    infra:Asset a owl:Class .
    infra:Network_Device a owl:Class .
    cyber:Security_Control a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        resources = result.resources
        reactors = result.reactors
        
        expected_class_names = ["Threat", "Vulnerability", "Asset", "Network_Device", "Security_Control"]
        actual_class_names = Enum.map(classes, & &1.name)
        
        IO.puts("   ✅ Complex TTL processed")
        IO.puts("   ✅ Classes found: #{inspect(actual_class_names)}")
        
        if length(classes) == 5 do
          IO.puts("   ✅ Correct number of classes (5)")
        else
          IO.puts("   ❌ Expected 5 classes, got #{length(classes)}")
        end
        
        missing_classes = expected_class_names -- actual_class_names
        if missing_classes == [] do
          IO.puts("   ✅ All expected classes extracted")
        else
          IO.puts("   ❌ Missing classes: #{inspect(missing_classes)}")
        end
        
        # Verify resources match classes
        if length(resources) == length(classes) do
          IO.puts("   ✅ Resource count matches class count")
        else
          IO.puts("   ❌ Resource count mismatch")
        end
        
        # Verify reactor references correct class count
        reactor_code = hd(reactors).code
        if String.contains?(reactor_code, "transformed_classes: #{length(classes)}") do
          IO.puts("   ✅ Reactor references correct class count")
        else
          IO.puts("   ❌ Reactor class count mismatch")
        end
        
        # Performance check
        IO.puts("   ✅ Multi-class scenario completed successfully")
        
      {:error, reason} ->
        IO.puts("   ❌ Multi-class transform failed: #{reason}")
    end
    
    # Test edge case: Single class
    IO.puts("   Testing single class scenario...")
    single_ttl = "@prefix owl: <http://www.w3.org/2002/07/owl#> .\ntest:Single a owl:Class ."
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(single_ttl) do
      {:ok, result} ->
        if length(result.parsed_ontology.classes) == 1 and 
           length(result.resources) == 1 and
           length(result.reactors) == 1 do
          IO.puts("   ✅ Single class scenario works correctly")
        else
          IO.puts("   ❌ Single class scenario failed")
        end
      {:error, reason} ->
        IO.puts("   ❌ Single class transform failed: #{reason}")
    end
  end
end

# Execute Integration Tests
IntegrationEndToEndTest.run_all_tests()