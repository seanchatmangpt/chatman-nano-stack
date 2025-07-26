#!/usr/bin/env elixir

# ⚙️ STEP 3 INCREMENTAL TEST: Ash Resource Generation
# Testing generate_ash_resources/1 function

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule Step3ResourceGenerationTest do
  @moduledoc """
  🛡️ CLEAN Step 3 Testing - Ash Resource Generation
  Testing resource code generation functionality
  """
  
  def run_all_tests do
    IO.puts("⚙️ STEP 3: ASH RESOURCE GENERATION")
    IO.puts("=" <> String.duplicate("=", 40))
    
    test_generate_resources_for_classes()
    test_empty_class_list_handling()
    test_required_ash_components()
    test_module_naming_convention()
    
    IO.puts("\n✅ STEP 3 TESTING COMPLETE")
  end
  
  defp test_generate_resources_for_classes do
    IO.puts("\n⚙️ Test 1: Generate Resources for Parsed Classes")
    
    parsed_data = %{
      prefixes: %{},
      classes: [
        %{uri: "test:Person", name: "Person"},
        %{uri: "test:Vehicle", name: "Vehicle"}
      ],
      properties: [],
      relationships: []
    }
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data) do
      {:ok, resources} ->
        IO.puts("   ✅ Resource generation successful")
        IO.puts("   ✅ Resources generated: #{length(resources)}")
        
        if length(resources) == 2 do
          IO.puts("   ✅ Correct number of resources (2)")
        else
          IO.puts("   ❌ Expected 2 resources, got #{length(resources)}")
        end
        
        # Check each resource has required fields
        Enum.each(resources, fn resource ->
          IO.puts("   ✅ Resource for #{resource.class.name}: #{resource.module_name}")
        end)
        
      {:error, reason} ->
        IO.puts("   ❌ Resource generation failed: #{reason}")
    end
  end
  
  defp test_empty_class_list_handling do
    IO.puts("\n⚙️ Test 2: Handle Empty Class List")
    
    parsed_data = %{
      prefixes: %{},
      classes: [],
      properties: [],
      relationships: []
    }
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data) do
      {:ok, resources} ->
        if resources == [] do
          IO.puts("   ✅ Empty class list handled correctly")
        else
          IO.puts("   ❌ Expected empty resources, got #{length(resources)}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Unexpected error with empty list: #{reason}")
    end
  end
  
  defp test_required_ash_components do
    IO.puts("\n⚙️ Test 3: Required Ash Components in Generated Code")
    
    parsed_data = %{
      prefixes: %{},
      classes: [%{uri: "test:TestEntity", name: "TestEntity"}],
      properties: [],
      relationships: []
    }
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data) do
      {:ok, [resource]} ->
        code = resource.code
        
        # Check for required components
        required_components = [
          "uuid_primary_key :id",
          "attribute :ttl_uri, :string",
          "defaults [:read, :create, :update, :destroy]",
          "data_layer: Ash.DataLayer.Ets"
        ]
        
        IO.puts("   ✅ Generated code structure:")
        
        Enum.each(required_components, fn component ->
          if String.contains?(code, component) do
            IO.puts("   ✅ Contains: #{component}")
          else
            IO.puts("   ❌ Missing: #{component}")
          end
        end)
        
        # Check for basic module structure
        if String.contains?(code, "use Ash.Resource") do
          IO.puts("   ✅ Uses Ash.Resource")
        else
          IO.puts("   ❌ Missing Ash.Resource use statement")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Resource generation failed: #{reason}")
    end
  end
  
  defp test_module_naming_convention do
    IO.puts("\n⚙️ Test 4: Module Naming Convention")
    
    test_cases = [
      %{uri: "test:Person", name: "Person", expected: "CnsForge.TTLResources.Person"},
      %{uri: "example:UserProfile", name: "UserProfile", expected: "CnsForge.TTLResources.UserProfile"},
      %{uri: "org:DataModel", name: "DataModel", expected: "CnsForge.TTLResources.DataModel"}
    ]
    
    Enum.each(test_cases, fn test_case ->
      parsed_data = %{
        prefixes: %{},
        classes: [test_case],
        properties: [],
        relationships: []
      }
      
      case CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data) do
        {:ok, [resource]} ->
          if resource.module_name == test_case.expected do
            IO.puts("   ✅ #{test_case.name} → #{resource.module_name}")
          else
            IO.puts("   ❌ #{test_case.name} → Expected: #{test_case.expected}, Got: #{resource.module_name}")
          end
          
          # Check if module definition in code matches
          expected_def = "defmodule #{test_case.expected}"
          if String.contains?(resource.code, expected_def) do
            IO.puts("   ✅ Code contains correct module definition")
          else
            IO.puts("   ❌ Code missing correct module definition")
          end
          
        {:error, reason} ->
          IO.puts("   ❌ Failed for #{test_case.name}: #{reason}")
      end
    end)
  end
end

# Execute Step 3 Tests
Step3ResourceGenerationTest.run_all_tests()