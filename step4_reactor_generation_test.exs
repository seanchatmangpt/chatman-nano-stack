#!/usr/bin/env elixir

# 🔄 STEP 4 INCREMENTAL TEST: Reactor Generation Validation
# Testing generate_ash_reactors/2 function

Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule Step4ReactorGenerationTest do
  @moduledoc """
  🛡️ CLEAN Step 4 Testing - Reactor Generation Validation
  Testing Ash.Reactor workflow generation functionality
  """
  
  def run_all_tests do
    IO.puts("🔄 STEP 4: REACTOR GENERATION VALIDATION")
    IO.puts("=" <> String.duplicate("=", 40))
    
    test_main_reactor_generation()
    test_reactor_structure_components()
    test_empty_class_list_handling()
    test_class_count_accuracy()
    
    IO.puts("\n✅ STEP 4 TESTING COMPLETE")
  end
  
  defp test_main_reactor_generation do
    IO.puts("\n🔄 Test 1: Main Reactor Generation")
    
    parsed_data = %{
      classes: [
        %{name: "Person"}, 
        %{name: "Vehicle"}
      ]
    }
    resources = []  # Not used in current implementation
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, resources) do
      {:ok, reactors} ->
        IO.puts("   ✅ Reactor generation successful")
        IO.puts("   ✅ Reactors generated: #{length(reactors)}")
        
        if length(reactors) == 1 do
          IO.puts("   ✅ Single main reactor generated")
        else
          IO.puts("   ❌ Expected 1 reactor, got #{length(reactors)}")
        end
        
        main_reactor = hd(reactors)
        if main_reactor.name == "CnsForge.TTLMainReactor" do
          IO.puts("   ✅ Correct reactor name: #{main_reactor.name}")
        else
          IO.puts("   ❌ Expected CnsForge.TTLMainReactor, got #{main_reactor.name}")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Reactor generation failed: #{reason}")
    end
  end
  
  defp test_reactor_structure_components do
    IO.puts("\n🔄 Test 2: Reactor Structure Components")
    
    parsed_data = %{classes: [%{name: "TestClass"}]}
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, []) do
      {:ok, [reactor]} ->
        code = reactor.code
        
        # Check for required reactor components
        required_components = [
          "defmodule CnsForge.TTLMainReactor",
          "use Reactor",
          "input :ontology_data",
          "step :transform_classes",
          "return :transform_classes"
        ]
        
        IO.puts("   ✅ Reactor code structure:")
        
        Enum.each(required_components, fn component ->
          if String.contains?(code, component) do
            IO.puts("   ✅ Contains: #{component}")
          else
            IO.puts("   ❌ Missing: #{component}")
          end
        end)
        
        # Check for workflow step structure
        if String.contains?(code, "argument :data, input(:ontology_data)") do
          IO.puts("   ✅ Contains proper argument binding")
        else
          IO.puts("   ❌ Missing argument binding")
        end
        
        if String.contains?(code, "run fn %{data: data}, _context ->") do
          IO.puts("   ✅ Contains run function structure")
        else
          IO.puts("   ❌ Missing run function structure")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Reactor generation failed: #{reason}")
    end
  end
  
  defp test_empty_class_list_handling do
    IO.puts("\n🔄 Test 3: Empty Class List Handling")
    
    parsed_data = %{classes: []}
    
    case CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, []) do
      {:ok, [reactor]} ->
        code = reactor.code
        
        if String.contains?(code, "transformed_classes: 0") do
          IO.puts("   ✅ Empty class list handled correctly (0 classes)")
        else
          IO.puts("   ❌ Expected transformed_classes: 0 in code")
        end
        
        # Should still generate a valid reactor
        if String.contains?(code, "use Reactor") do
          IO.puts("   ✅ Valid reactor structure maintained")
        else
          IO.puts("   ❌ Invalid reactor structure")
        end
        
      {:error, reason} ->
        IO.puts("   ❌ Reactor generation failed: #{reason}")
    end
  end
  
  defp test_class_count_accuracy do
    IO.puts("\n🔄 Test 4: Class Count Accuracy in Generated Reactor")
    
    test_cases = [
      {1, [%{name: "SingleClass"}]},
      {3, [%{name: "First"}, %{name: "Second"}, %{name: "Third"}]},
      {5, [%{name: "A"}, %{name: "B"}, %{name: "C"}, %{name: "D"}, %{name: "E"}]}
    ]
    
    Enum.each(test_cases, fn {expected_count, classes} ->
      parsed_data = %{classes: classes}
      
      case CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, []) do
        {:ok, [reactor]} ->
          code = reactor.code
          
          if String.contains?(code, "transformed_classes: #{expected_count}") do
            IO.puts("   ✅ #{expected_count} classes → correct count in reactor")
          else
            IO.puts("   ❌ #{expected_count} classes → incorrect count in reactor")
            IO.puts("       Expected: transformed_classes: #{expected_count}")
          end
          
        {:error, reason} ->
          IO.puts("   ❌ Failed for #{expected_count} classes: #{reason}")
      end
    end)
  end
end

# Execute Step 4 Tests
Step4ReactorGenerationTest.run_all_tests()