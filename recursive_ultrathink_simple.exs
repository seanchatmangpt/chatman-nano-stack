#!/usr/bin/env elixir

# 🧠 ULTRATHINK RECURSIVE DEEP SCENARIO TESTING - Simplified
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule RecursiveUltrathinkSimple do
  def run_tests do
    IO.puts("🧠 ULTRATHINK RECURSIVE DEEP SCENARIO TESTING")
    IO.puts("=" <> String.duplicate("=", 55))
    
    test_large_scale_processing()
    test_complex_hierarchies()
    test_adversarial_patterns()
    
    IO.puts("\n✅ RECURSIVE ULTRATHINK TESTING COMPLETE")
  end
  
  defp test_large_scale_processing do
    IO.puts("\n🔄 Test 1: Large-Scale Processing")
    
    # Generate 25 classes for performance testing
    classes = for i <- 1..25, do: "test:Class#{i} a owl:Class ."
    
    large_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    #{Enum.join(classes, "\n")}
    """
    
    start_time = System.monotonic_time(:millisecond)
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(large_ttl) do
      {:ok, result} ->
        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time
        
        class_count = length(result.parsed_ontology.classes)
        resource_count = length(result.resources)
        
        IO.puts("   ✅ Processed #{class_count} classes in #{duration}ms")
        IO.puts("   ✅ Generated #{resource_count} resources")
        
        if duration < 50 do
          IO.puts("   ✅ Performance: Excellent (<50ms)")
        elsif duration < 200 do
          IO.puts("   ✅ Performance: Good (<200ms)")
        else
          IO.puts("   ⚠️  Performance: Needs optimization (#{duration}ms)")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Large-scale test failed: #{reason}")
    end
  end
  
  defp test_complex_hierarchies do
    IO.puts("\n🔄 Test 2: Complex Hierarchies")
    
    hierarchy_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix cyber: <http://cybersecurity.org/> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    
    cyber:Asset a owl:Class .
    cyber:NetworkAsset a owl:Class .
    cyber:ComputeAsset rdf:type owl:Class .
    cyber:ServerAsset a owl:Class .
    cyber:DatabaseServer a owl:Class .
    cyber:WebServer a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(hierarchy_ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        class_names = Enum.map(classes, & &1.name)
        
        expected = ["Asset", "NetworkAsset", "ComputeAsset", "ServerAsset", "DatabaseServer", "WebServer"]
        
        if length(classes) == 6 do
          IO.puts("   ✅ Hierarchy classes: 6/6 extracted")
          IO.puts("   ✅ Classes: #{inspect(class_names)}")
        else
          IO.puts("   ❌ Hierarchy incomplete: #{length(classes)}/6")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Hierarchy test failed: #{reason}")
    end
  end
  
  defp test_adversarial_patterns do
    IO.puts("\n🔄 Test 3: Adversarial Patterns")
    
    # Test long class names
    long_name = String.duplicate("VeryLongClassName", 5)
    
    adversarial_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:#{long_name} a owl:Class .
    test:NormalClass a owl:Class .
    test:Class_With_Underscores a owl:Class .
    test:Class123WithNumbers a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(adversarial_ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        
        # Find the long-named class
        long_class = Enum.find(classes, &(String.length(&1.name) > 50))
        
        if long_class do
          IO.puts("   ✅ Long class name handled: #{String.length(long_class.name)} chars")
        else
          IO.puts("   ❌ Long class name not processed")
        end
        
        if length(classes) == 4 do
          IO.puts("   ✅ All adversarial patterns processed: 4/4")
        else
          IO.puts("   ❌ Adversarial processing incomplete: #{length(classes)}/4")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Adversarial test failed: #{reason}")
    end
    
    # Test recursive comment filtering validation
    IO.puts("   Testing advanced comment filtering...")
    
    comment_ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:ValidClass1 a owl:Class .
    # test:CommentedClass1 a owl:Class .
    ## test:CommentedClass2 a owl:Class .
    test:ValidClass2 a owl:Class .
    ### test:CommentedClass3 a owl:Class .
    test:ValidClass3 a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.transform_ttl(comment_ttl) do
      {:ok, result} ->
        classes = result.parsed_ontology.classes
        class_names = Enum.map(classes, & &1.name)
        
        valid_classes = ["ValidClass1", "ValidClass2", "ValidClass3"]
        commented_classes = ["CommentedClass1", "CommentedClass2", "CommentedClass3"]
        
        valid_found = Enum.all?(valid_classes, &(&1 in class_names))
        commented_excluded = Enum.all?(commented_classes, &(&1 not in class_names))
        
        if valid_found and commented_excluded do
          IO.puts("   ✅ Comment filtering: 100% accurate")
        else
          IO.puts("   ❌ Comment filtering issues detected")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Comment filtering test failed: #{reason}")
    end
  end
end

# Execute Tests
RecursiveUltrathinkSimple.run_tests()