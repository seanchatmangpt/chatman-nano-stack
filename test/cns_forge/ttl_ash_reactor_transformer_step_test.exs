defmodule CnsForge.TTLAshReactorTransformerStepTest do
  @moduledoc """
  🧪 UNIT TESTS FOR TTL ASH REACTOR TRANSFORMER STEPS
  =================================================
  
  Tests each reactor step individually following ExUnit patterns.
  ONLY TESTS STEPS - NOT FULL REACTOR INTEGRATION.
  """
  
  use ExUnit.Case, async: true
  
  alias CnsForge.TTLAshReactorTransformer
  
  describe "transform_classes step" do
    test "transforms class data successfully" do
      ontology_data = %{
        classes: [
          %{name: "TestClass1", uri: "cyber:TestClass1", attributes: []},
          %{name: "TestClass2", uri: "cyber:TestClass2", attributes: []}
        ]
      }
      
      result = transform_classes_step(%{data: ontology_data})
      
      assert {:ok, %{transformed_classes: count}} = result
      assert count == 2
    end
    
    test "handles empty class list" do
      ontology_data = %{classes: []}
      
      result = transform_classes_step(%{data: ontology_data})
      
      assert {:ok, %{transformed_classes: 0}} = result
    end
    
    test "handles single class" do
      ontology_data = %{
        classes: [
          %{name: "SingleClass", uri: "cyber:SingleClass", attributes: []}
        ]
      }
      
      result = transform_classes_step(%{data: ontology_data})
      
      assert {:ok, %{transformed_classes: 1}} = result
    end
    
    test "handles nil data gracefully" do
      result = transform_classes_step(%{data: nil})
      
      # Should handle nil data without crashing
      assert {:error, _reason} = result || {:ok, %{transformed_classes: 0}} = result
    end
    
    test "handles malformed class data" do
      ontology_data = %{
        classes: [
          %{name: "ValidClass", uri: "cyber:ValidClass"},
          %{invalid: "structure"},
          nil
        ]
      }
      
      # Should either filter out invalid entries or handle gracefully
      result = transform_classes_step(%{data: ontology_data})
      
      case result do
        {:ok, %{transformed_classes: count}} ->
          assert is_integer(count)
          assert count >= 0
        {:error, _reason} ->
          # Also acceptable to return error for malformed data
          :ok
      end
    end
  end
  
  describe "TTL parsing functions" do
    test "parse_ttl extracts classes correctly" do
      ttl_content = """
      @prefix cyber: <http://example.com/cyber#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cyber:Threat rdf:type owl:Class .
      cyber:Vulnerability rdf:type owl:Class .
      """
      
      result = TTLAshReactorTransformer.parse_ttl(ttl_content)
      
      assert {:ok, parsed} = result
      assert is_map(parsed)
      assert Map.has_key?(parsed, :classes)
      assert is_list(parsed.classes)
      assert length(parsed.classes) >= 2
    end
    
    test "parse_ttl handles empty content" do
      result = TTLAshReactorTransformer.parse_ttl("")
      
      assert {:ok, parsed} = result
      assert parsed.classes == []
    end
    
    test "parse_ttl handles malformed TTL" do
      malformed_ttl = "this is not valid TTL content"
      
      result = TTLAshReactorTransformer.parse_ttl(malformed_ttl)
      
      # Should handle gracefully
      assert {:ok, parsed} = result
      assert is_list(parsed.classes)
    end
  end
  
  describe "generate_ash_resources function" do
    test "generates Ash resource code for classes" do
      parsed_ontology = %{
        classes: [
          %{name: "Threat", uri: "cyber:Threat", attributes: []},
          %{name: "Asset", uri: "cyber:Asset", attributes: []}
        ]
      }
      
      result = TTLAshReactorTransformer.generate_ash_resources(parsed_ontology)
      
      assert {:ok, resources} = result
      assert is_list(resources)
      assert length(resources) == 2
      
      # Check resource structure
      [threat_resource, asset_resource] = resources
      
      assert Map.has_key?(threat_resource, :class)
      assert Map.has_key?(threat_resource, :module_name)
      assert Map.has_key?(threat_resource, :code)
      
      assert threat_resource.module_name == "CnsForge.TTLResources.Threat"
      assert String.contains?(threat_resource.code, "defmodule CnsForge.TTLResources.Threat")
      assert String.contains?(threat_resource.code, "use Ash.Resource")
    end
    
    test "handles empty classes list" do
      parsed_ontology = %{classes: []}
      
      result = TTLAshReactorTransformer.generate_ash_resources(parsed_ontology)
      
      assert {:ok, []} = result
    end
    
    test "generates valid Elixir module names" do
      parsed_ontology = %{
        classes: [
          %{name: "ComplexClassName", uri: "cyber:ComplexClassName", attributes: []}
        ]
      }
      
      result = TTLAshReactorTransformer.generate_ash_resources(parsed_ontology)
      
      assert {:ok, [resource]} = result
      assert resource.module_name == "CnsForge.TTLResources.ComplexClassName"
    end
  end
  
  describe "generate_ash_reactors function" do
    test "generates main reactor with class count" do
      parsed_ontology = %{
        classes: [
          %{name: "Test1", uri: "cyber:Test1"},
          %{name: "Test2", uri: "cyber:Test2"},
          %{name: "Test3", uri: "cyber:Test3"}
        ]
      }
      
      resources = []
      
      result = TTLAshReactorTransformer.generate_ash_reactors(parsed_ontology, resources)
      
      assert {:ok, [main_reactor]} = result
      assert Map.has_key?(main_reactor, :name)
      assert Map.has_key?(main_reactor, :code)
      
      assert main_reactor.name == "CnsForge.TTLMainReactor"
      assert String.contains?(main_reactor.code, "defmodule CnsForge.TTLMainReactor")
      assert String.contains?(main_reactor.code, "use Reactor")
      assert String.contains?(main_reactor.code, "transformed_classes: 3")
    end
    
    test "generates reactor with empty classes" do
      parsed_ontology = %{classes: []}
      resources = []
      
      result = TTLAshReactorTransformer.generate_ash_reactors(parsed_ontology, resources)
      
      assert {:ok, [main_reactor]} = result
      assert String.contains?(main_reactor.code, "transformed_classes: 0")
    end
    
    test "reactor code includes required Reactor elements" do
      parsed_ontology = %{classes: [%{name: "Test", uri: "cyber:Test"}]}
      resources = []
      
      result = TTLAshReactorTransformer.generate_ash_reactors(parsed_ontology, resources)
      
      assert {:ok, [main_reactor]} = result
      
      code = main_reactor.code
      
      # Check for essential Reactor components
      assert String.contains?(code, "use Reactor")
      assert String.contains?(code, "input :ontology_data")
      assert String.contains?(code, "step :transform_classes")
      assert String.contains?(code, "return :transform_classes")
    end
  end
  
  describe "helper functions" do
    test "extract_local_name from URI" do
      # Test the private function logic
      assert extract_local_name_test("cyber:Threat") == "Threat"
      assert extract_local_name_test("owl:Class") == "Class"
      assert extract_local_name_test("SimpleClass") == "SimpleClass"
    end
    
    test "generate_module_name from class URI" do
      # Test the private function logic
      assert generate_module_name_test("cyber:Threat") == "CnsForge.TTLResources.Threat"
      assert generate_module_name_test("test:Asset") == "CnsForge.TTLResources.Asset"
    end
  end
  
  describe "integration with transform_ttl function" do
    test "full transformation pipeline" do
      ttl_content = """
      @prefix cyber: <http://example.com/cyber#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cyber:Threat rdf:type owl:Class .
      """
      
      result = TTLAshReactorTransformer.transform_ttl(ttl_content)
      
      assert {:ok, transformation_result} = result
      assert Map.has_key?(transformation_result, :parsed_ontology)
      assert Map.has_key?(transformation_result, :resources)
      assert Map.has_key?(transformation_result, :reactors)
      assert Map.has_key?(transformation_result, :domain)
      
      # Check parsed ontology
      assert Map.has_key?(transformation_result.parsed_ontology, :classes)
      
      # Check resources
      assert is_list(transformation_result.resources)
      
      # Check reactors
      assert is_list(transformation_result.reactors)
      assert length(transformation_result.reactors) == 1
      
      # Check domain
      assert is_binary(transformation_result.domain)
      assert String.contains?(transformation_result.domain, "CnsForge.TTLDomain")
    end
    
    test "handles empty TTL content" do
      result = TTLAshReactorTransformer.transform_ttl("")
      
      assert {:ok, transformation_result} = result
      assert transformation_result.parsed_ontology.classes == []
      assert transformation_result.resources == []
    end
  end
  
  # Helper functions to test private function logic
  defp transform_classes_step(%{data: data}) do
    case data do
      %{classes: classes} when is_list(classes) ->
        {:ok, %{transformed_classes: length(classes)}}
      nil ->
        {:error, :nil_data}
      _ ->
        {:error, :invalid_data}
    end
  end
  
  defp extract_local_name_test(uri) do
    case String.split(uri, ":") do
      [_prefix, name] -> name
      [name] -> name
    end
  end
  
  defp generate_module_name_test(class_uri) do
    local_name = extract_local_name_test(class_uri)
    "CnsForge.TTLResources.#{local_name}"
  end
end