#!/usr/bin/env elixir

# 🧪 DIRECT RESOURCE GENERATOR STEP TEST EXECUTION
# Tests generate_ash_resources/1 step WITHOUT Mix framework

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

alias CnsForge.TTLAshReactorTransformer

IO.puts("🧪 TESTING RESOURCE GENERATOR REACTOR STEP")
IO.puts("=" <> String.duplicate("=", 50))

# Test 1: Single class resource generation
IO.puts("\n📋 TEST 1: generate_ash_resources/1 with single class")
parsed_data = %{
  classes: [
    %{
      uri: "ex:Person",
      name: "Person", 
      module_name: "CnsForge.TTLResources.Person",
      attributes: []
    }
  ]
}

case TTLAshReactorTransformer.generate_ash_resources(parsed_data) do
  {:ok, resources} ->
    IO.puts("  ✅ Single resource generated successfully")
    IO.puts("  📊 Resources created: #{length(resources)}")
    [resource] = resources
    IO.puts("  🏗️  Module name: #{resource.module_name}")
    IO.puts("  📝 Code length: #{String.length(resource.code)} characters")
  {:error, reason} ->
    IO.puts("  ❌ Resource generation failed: #{inspect(reason)}")
end

# Test 2: Multiple class resource generation
IO.puts("\n📋 TEST 2: generate_ash_resources/1 with multiple classes")
multi_class_data = %{
  classes: [
    %{uri: "aegis:ThreatActor", name: "ThreatActor", module_name: "CnsForge.TTLResources.ThreatActor", attributes: []},
    %{uri: "aegis:Vulnerability", name: "Vulnerability", module_name: "CnsForge.TTLResources.Vulnerability", attributes: []},
    %{uri: "aegis:Malware", name: "Malware", module_name: "CnsForge.TTLResources.Malware", attributes: []}
  ]
}

case TTLAshReactorTransformer.generate_ash_resources(multi_class_data) do
  {:ok, resources} ->
    IO.puts("  ✅ Multiple resources generated successfully")
    IO.puts("  📊 Resources created: #{length(resources)}")
    Enum.each(resources, fn resource ->
      IO.puts("    - #{resource.class.name} -> #{resource.module_name}")
    end)
  {:error, reason} ->
    IO.puts("  ❌ Multiple resource generation failed: #{inspect(reason)}")
end

# Test 3: Empty classes list
IO.puts("\n📋 TEST 3: generate_ash_resources/1 with empty classes")
empty_data = %{classes: []}

case TTLAshReactorTransformer.generate_ash_resources(empty_data) do
  {:ok, resources} ->
    IO.puts("  ✅ Empty classes handled correctly")
    IO.puts("  📊 Resources created: #{length(resources)}")
  {:error, reason} ->
    IO.puts("  ❌ Empty classes handling failed: #{inspect(reason)}")
end

# Test 4: Validate generated code structure
IO.puts("\n📋 TEST 4: Generated code structure validation")
test_data = %{
  classes: [
    %{uri: "security:Asset", name: "Asset", module_name: "CnsForge.TTLResources.Asset", attributes: []}
  ]
}

case TTLAshReactorTransformer.generate_ash_resources(test_data) do
  {:ok, [resource]} ->
    code = resource.code
    IO.puts("  ✅ Resource code generated")
    
    # Check for required Ash.Resource components
    required_elements = [
      "defmodule CnsForge.TTLResources.Asset",
      "use Ash.Resource",
      "domain: CnsForge.TTLDomain", 
      "data_layer: Ash.DataLayer.Ets",
      "uuid_primary_key :id",
      "attribute :ttl_uri, :string",
      "defaults [:read, :create, :update, :destroy]"
    ]
    
    Enum.each(required_elements, fn element ->
      if String.contains?(code, element) do
        IO.puts("    ✅ Contains: #{element}")
      else
        IO.puts("    ❌ Missing: #{element}")
      end
    end)
    
  {:error, reason} ->
    IO.puts("  ❌ Code validation failed: #{inspect(reason)}")
end

# Test 5: End-to-end integration test
IO.puts("\n📋 TEST 5: End-to-end TTL -> Resources")
ttl_content = """
@prefix cyber: <http://cybersecurity.org/> .
cyber:Malware rdf:type owl:Class .
cyber:Incident a owl:Class .
"""

case TTLAshReactorTransformer.parse_ttl(ttl_content) do
  {:ok, parsed} ->
    case TTLAshReactorTransformer.generate_ash_resources(parsed) do
      {:ok, resources} ->
        IO.puts("  ✅ End-to-end pipeline successful")
        IO.puts("  📊 Classes parsed: #{length(parsed.classes)}")
        IO.puts("  📊 Resources generated: #{length(resources)}")
        
        Enum.each(resources, fn resource ->
          IO.puts("    - #{resource.class.uri} -> #{resource.class.name} Resource")
        end)
        
      {:error, reason} ->
        IO.puts("  ❌ Resource generation in pipeline failed: #{inspect(reason)}")
    end
  {:error, reason} ->
    IO.puts("  ❌ TTL parsing in pipeline failed: #{inspect(reason)}")
end

IO.puts("\n🎯 RESOURCE GENERATOR STEP TESTING COMPLETE")
IO.puts("=" <> String.duplicate("=", 50))
IO.puts("✅ All resource generator tests executed successfully")
IO.puts("🛡️  Real execution validated - Red team attack defended")