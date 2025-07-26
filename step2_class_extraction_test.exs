#!/usr/bin/env elixir

# 🔍 STEP 2 INCREMENTAL TEST: Class Extraction Verification
# Testing extract_classes/1 private function via parse_ttl/1

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule Step2ClassExtractionTest do
  @moduledoc """
  🛡️ CLEAN Step 2 Testing - Class Extraction Verification
  Testing class extraction logic through parse_ttl/1
  """
  
  def run_all_tests do
    IO.puts("🔍 STEP 2: CLASS EXTRACTION VERIFICATION")
    IO.puts("=" <> String.duplicate("=", 40))
    
    test_multiple_prefixes()
    test_underscores_and_numbers()
    test_commented_classes_ignored()
    test_different_syntax_patterns()
    
    IO.puts("\n✅ STEP 2 TESTING COMPLETE")
  end
  
  defp test_multiple_prefixes do
    IO.puts("\n🔍 Test 1: Multiple Classes with Different Prefixes")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix ex: <http://example.org/> .
    @prefix test: <http://test.org/> .
    
    ex:Person a owl:Class .
    test:Vehicle a owl:Class .
    ex:Document rdf:type owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        class_names = Enum.map(result.classes, & &1.name)
        expected_names = ["Person", "Vehicle", "Document"]
        
        IO.puts("   ✅ Classes extracted: #{inspect(class_names)}")
        
        missing = expected_names -- class_names
        extra = class_names -- expected_names
        
        if missing == [] and extra == [] do
          IO.puts("   ✅ All expected classes found")
        else
          if missing != [], do: IO.puts("   ❌ Missing: #{inspect(missing)}")
          if extra != [], do: IO.puts("   ❌ Extra: #{inspect(extra)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Extraction failed: #{reason}")
    end
  end
  
  defp test_underscores_and_numbers do
    IO.puts("\n🔍 Test 2: Classes with Underscores and Numbers")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:User_Profile a owl:Class .
    test:Type123 a owl:Class .
    test:Data_Model_V2 a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        class_names = Enum.map(result.classes, & &1.name)
        expected_names = ["User_Profile", "Type123", "Data_Model_V2"]
        
        IO.puts("   ✅ Classes with special chars: #{inspect(class_names)}")
        
        if Enum.all?(expected_names, &(&1 in class_names)) do
          IO.puts("   ✅ All special character classes extracted")
        else
          missing = expected_names -- class_names
          IO.puts("   ❌ Missing: #{inspect(missing)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Extraction failed: #{reason}")
    end
  end
  
  defp test_commented_classes_ignored do
    IO.puts("\n🔍 Test 3: Commented Class Definitions Ignored")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix test: <http://test.org/> .
    
    test:ValidClass a owl:Class .
    # test:CommentedClass a owl:Class .
    ## test:DoublyCommented a owl:Class .
    test:AnotherValid a owl:Class .
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        class_names = Enum.map(result.classes, & &1.name)
        valid_classes = ["ValidClass", "AnotherValid"] 
        commented_classes = ["CommentedClass", "DoublyCommented"]
        
        IO.puts("   ✅ Extracted classes: #{inspect(class_names)}")
        
        valid_found = Enum.all?(valid_classes, &(&1 in class_names))
        commented_ignored = Enum.all?(commented_classes, &(&1 not in class_names))
        
        if valid_found do
          IO.puts("   ✅ Valid classes extracted")
        else
          IO.puts("   ❌ Some valid classes missing")
        end
        
        if commented_ignored do
          IO.puts("   ✅ Commented classes properly ignored")
        else
          IO.puts("   ❌ Some commented classes incorrectly included")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Extraction failed: #{reason}")
    end
  end
  
  defp test_different_syntax_patterns do
    IO.puts("\n🔍 Test 4: Different TTL Syntax Patterns")
    
    ttl = """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix test: <http://test.org/> .
    
    test:ShorthandClass a owl:Class .
    test:FullRdfType rdf:type owl:Class .
    test:SpacedClass   a   owl:Class   .
    """
    
    case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl) do
      {:ok, result} ->
        class_names = Enum.map(result.classes, & &1.name)
        expected_names = ["ShorthandClass", "FullRdfType", "SpacedClass"]
        
        IO.puts("   ✅ Classes from different syntax: #{inspect(class_names)}")
        
        if length(class_names) == 3 do
          IO.puts("   ✅ Correct number of classes extracted")
        else
          IO.puts("   ❌ Expected 3 classes, got #{length(class_names)}")
        end
        
        missing = expected_names -- class_names
        if missing == [] do
          IO.puts("   ✅ All syntax patterns handled correctly")
        else
          IO.puts("   ❌ Missing classes: #{inspect(missing)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Extraction failed: #{reason}")
    end
  end
end

# Execute Step 2 Tests
Step2ClassExtractionTest.run_all_tests()