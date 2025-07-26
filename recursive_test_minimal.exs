#!/usr/bin/env elixir

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

IO.puts("🧠 ULTRATHINK RECURSIVE TESTING")
IO.puts("=" <> String.duplicate("=", 40))

# Test 1: Large Scale Processing
IO.puts("\n🔄 Test 1: Large-Scale Processing")

large_ttl = """
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix test: <http://test.org/> .

test:Class1 a owl:Class .
test:Class2 a owl:Class .
test:Class3 a owl:Class .
test:Class4 a owl:Class .
test:Class5 a owl:Class .
test:Class6 a owl:Class .
test:Class7 a owl:Class .
test:Class8 a owl:Class .
test:Class9 a owl:Class .
test:Class10 a owl:Class .
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
    
  {:error, reason} ->
    IO.puts("   ❌ Large-scale test failed: #{reason}")
end

# Test 2: Comment Filtering Validation
IO.puts("\n🔄 Test 2: Advanced Comment Filtering")

comment_ttl = """
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix test: <http://test.org/> .

test:ValidClass1 a owl:Class .
# test:CommentedClass1 a owl:Class .
## test:CommentedClass2 a owl:Class .
test:ValidClass2 a owl:Class .
"""

case CnsForge.TTLAshReactorTransformer.transform_ttl(comment_ttl) do
  {:ok, result} ->
    classes = result.parsed_ontology.classes
    class_names = Enum.map(classes, & &1.name)
    
    valid_classes = ["ValidClass1", "ValidClass2"]
    commented_classes = ["CommentedClass1", "CommentedClass2"]
    
    valid_found = Enum.all?(valid_classes, &(&1 in class_names))
    commented_excluded = Enum.all?(commented_classes, &(&1 not in class_names))
    
    if valid_found and commented_excluded do
      IO.puts("   ✅ Comment filtering: 100% accurate")
    else
      IO.puts("   ❌ Comment filtering issues")
    end
    
  {:error, reason} ->
    IO.puts("   ❌ Comment test failed: #{reason}")
end

# Test 3: Complex Names
IO.puts("\n🔄 Test 3: Complex Class Names")

complex_ttl = """
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix test: <http://test.org/> .

test:LongClassNameWithManyWords a owl:Class .
test:Class_With_Underscores a owl:Class .
test:Class123WithNumbers a owl:Class .
"""

case CnsForge.TTLAshReactorTransformer.transform_ttl(complex_ttl) do
  {:ok, result} ->
    classes = result.parsed_ontology.classes
    
    if length(classes) == 3 do
      IO.puts("   ✅ Complex names processed: 3/3")
      Enum.each(classes, fn class ->
        IO.puts("     - #{class.name}")
      end)
    else
      IO.puts("   ❌ Complex names incomplete: #{length(classes)}/3")
    end
    
  {:error, reason} ->
    IO.puts("   ❌ Complex names test failed: #{reason}")
end

IO.puts("\n✅ RECURSIVE TESTING COMPLETE")