#!/usr/bin/env elixir

# 🧠 ULTRATHINK RECURSIVE DEEP SCENARIO TESTING
# Advanced TTL transformation stress testing and edge case exploration

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule RecursiveUltrathinkDeepTest do
  @moduledoc """
  🛡️ RECURSIVE ULTRATHINK Deep Scenario Testing
  Exploring advanced TTL transformation edge cases and stress scenarios
  """
  
  def run_all_recursive_tests do
    IO.puts("🧠 ULTRATHINK RECURSIVE DEEP SCENARIO TESTING")
    IO.puts("=" <> String.duplicate("=", 55))
    
    test_recursive_ontology_patterns()
    test_adversarial_ttl_patterns()
    test_performance_stress_scenarios()
    test_recursive_transformation_validation()
    
    IO.puts("\n✅ RECURSIVE ULTRATHINK TESTING COMPLETE")
  end
  
  defp test_recursive_ontology_patterns do
    IO.puts("\n🔄 Recursive Test 1: Advanced Ontology Patterns")
    
    # Test nested class hierarchies
    IO.puts("   Testing nested class hierarchies...")
    nested_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix cyber: <http://cybersecurity.org/> .
    
    cyber:Asset a owl:Class .
    cyber:NetworkAsset a owl:Class ;
        rdfs:subClassOf cyber:Asset .
    cyber:Server a owl:Class ;
        rdfs:subClassOf cyber:NetworkAsset .
    cyber:WebServer a owl:Class ;
        rdfs:subClassOf cyber:Server .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(nested_ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        class_names = Enum.map(classes, & &1.name)
        
        expected_classes = ["Asset", "NetworkAsset", "Server", "WebServer"]
        if Enum.all?(expected_classes, &(&1 in class_names)) do
          IO.puts("   ✅ Nested hierarchy classes extracted: #{inspect(class_names)}")
        else
          IO.puts("   ❌ Missing hierarchy classes: #{inspect(expected_classes -- class_names)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Nested hierarchy failed: #{reason}")
    end
  end
  
  defp test_adversarial_ttl_patterns do
    IO.puts("\n🔄 Recursive Test 2: Adversarial TTL Patterns")
    
    # Test extremely long class URIs
    IO.puts("   Testing extremely long class URIs...")
    long_uri_class = String.duplicate("VeryLongClassName", 10)
    long_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:#{long_uri_class} a owl:Class .
    test:NormalClass a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(long_ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        long_class = Enum.find(classes, &(String.length(&1.name) > 100))
        
        if long_class do
          IO.puts("   ✅ Long URI handled: #{String.length(long_class.name)} characters")
        else
          IO.puts("   ❌ Long URI not processed correctly")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Long URI test failed: #{reason}")
    end
  end
  
  defp test_performance_stress_scenarios do
    IO.puts("\n🔄 Recursive Test 3: Performance Stress Scenarios")
    
    # Generate large TTL with many classes
    IO.puts("   Testing large-scale TTL processing...")
    
    large_ttl_classes = for i <- 1..50 do
      "test:Class#{i} a owl:Class ."
    end
    
    large_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    """ <> Enum.join(large_ttl_classes, "\n")
    
    start_time = System.monotonic_time(:millisecond)
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(large_ttl) do
      {:ok, result} ->
        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time
        
        classes = result.parsed_ontology.classes
        resources = result.resources
        
        IO.puts("   ✅ Large-scale processing: #{length(classes)} classes in #{duration}ms")
        IO.puts("   ✅ Generated #{length(resources)} resources")
        
        if duration < 100 do
          IO.puts("   ✅ Performance excellent: <100ms")
        elsif duration < 500 do
          IO.puts("   ✅ Performance good: <500ms")
        else
          IO.puts("   ⚠️  Performance concern: #{duration}ms")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Large-scale test failed: #{reason}")
    end
  end
  
  defp test_recursive_transformation_validation do
    IO.puts("\n🔄 Recursive Test 4: Deep Transformation Validation")
    
    # Test comprehensive integration with validation
    integration_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix cyber: <http://cybersecurity.org/> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    
    cyber:ThreatActor a owl:Class .
    cyber:Malware rdf:type owl:Class .
    cyber:Vulnerability a owl:Class .
    cyber:SecurityControl a owl:Class .
    cyber:Asset a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(integration_ttl) do
      {:ok, result} ->
        # Deep validation of all components
        parsed = result.parsed_ontology
        resources = result.resources
        reactors = result.reactors
        domain = result.domain
        
        IO.puts("   ✅ Deep integration test successful")
        
        # Validate parsed ontology completeness
        if length(parsed.classes) == 5 do
          IO.puts("   ✅ Parsed ontology: 5/5 classes")
        else
          IO.puts("   ❌ Parsed ontology incomplete: #{length(parsed.classes)}/5")
        end
        
        # Validate resource generation completeness
        if length(resources) == 5 do
          IO.puts("   ✅ Resource generation: 5/5 resources")
        else
          IO.puts("   ❌ Resource generation incomplete: #{length(resources)}/5")
        end
        
        # Validate all resources have proper Ash.Resource structure
        valid_resources = Enum.count(resources, fn resource ->
          String.contains?(resource.code, "use Ash.Resource") and
          String.contains?(resource.code, "uuid_primary_key :id") and
          String.contains?(resource.code, "attribute :ttl_uri, :string")
        end)
        
        if valid_resources == 5 do
          IO.puts("   ✅ Resource structure validation: 5/5 valid")
        else
          IO.puts("   ❌ Resource structure issues: #{valid_resources}/5 valid")
        end
        
        # Validate reactor workflow accuracy
        reactor = hd(reactors)
        if String.contains?(reactor.code, "transformed_classes: 5") do
          IO.puts("   ✅ Reactor workflow accuracy: correct class count")
        else
          IO.puts("   ❌ Reactor workflow inaccuracy: incorrect class count")
        end
        
        # Validate domain structure
        domain_valid = String.contains?(domain, "defmodule CnsForge.TTLDomain") and
                      String.contains?(domain, "use Ash.Domain") and
                      String.contains?(domain, "authorize :when_requested")
        
        if domain_valid do
          IO.puts("   ✅ Domain structure: fully valid")
        else
          IO.puts("   ❌ Domain structure: validation failed")
        end
        
        IO.puts("   ✅ Recursive transformation validation: COMPLETE")
        
      {:error, reason} ->
        IO.puts("   ❌ Deep transformation validation failed: #{reason}")
    end
  end
end

# Execute Recursive Ultrathink Deep Tests
RecursiveUltrathinkDeepTest.run_all_recursive_tests()