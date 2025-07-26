defmodule CnsForge.TTLAshReactorTransformerTest do
  @moduledoc """
  🧪 INCREMENTAL STEP TESTING for TTL Ash Reactor Transformer
  
  Testing individual steps in isolation following ultrathink approach:
  - 80% focus on core transformation steps
  - 20% focus on edge cases and error scenarios
  """
  
  use ExUnit.Case, async: true
  
  alias CnsForge.TTLAshReactorTransformer
  
  describe "Step 1: TTL Parsing - parse_ttl/1" do
    test "parses valid TTL content successfully" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:Person a owl:Class .
      test:Organization a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl)
      assert %{prefixes: _, classes: classes, properties: _, relationships: _} = result
      assert length(classes) == 2
      assert Enum.any?(classes, &(&1.name == "Person"))
      assert Enum.any?(classes, &(&1.name == "Organization"))
    end
    
    test "handles empty TTL content" do
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl("")
      assert result.classes == []
    end
    
    test "handles TTL with no classes" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      # Just prefixes, no classes
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl)
      assert result.classes == []
    end
    
    test "handles malformed TTL gracefully" do
      malformed_ttl = "this is not valid TTL at all"
      
      # Should still return ok but with empty classes
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl(malformed_ttl)
      assert result.classes == []
    end
  end
  
  describe "Step 2: Class Extraction - extract_classes/1 (private - testing via parse_ttl)" do
    test "extracts multiple classes with different prefixes" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      @prefix test: <http://test.org/> .
      
      ex:Person a owl:Class .
      test:Vehicle a owl:Class .
      ex:Document rdf:type owl:Class .
      """
      
      {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl)
      
      class_names = Enum.map(result.classes, & &1.name)
      assert "Person" in class_names
      assert "Vehicle" in class_names
      assert "Document" in class_names
    end
    
    test "handles classes with underscores and numbers" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:User_Profile a owl:Class .
      test:Type123 a owl:Class .
      """
      
      {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl)
      
      class_names = Enum.map(result.classes, & &1.name)
      assert "User_Profile" in class_names
      assert "Type123" in class_names
    end
    
    test "ignores commented out class definitions" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:ValidClass a owl:Class .
      # test:CommentedClass a owl:Class .
      """
      
      {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl)
      
      class_names = Enum.map(result.classes, & &1.name)
      assert "ValidClass" in class_names
      refute "CommentedClass" in class_names
    end
  end
  
  describe "Step 3: Ash Resource Generation - generate_ash_resources/1" do
    test "generates resources for parsed classes" do
      parsed_data = %{
        prefixes: %{},
        classes: [
          %{uri: "test:Person", name: "Person"},
          %{uri: "test:Vehicle", name: "Vehicle"}
        ],
        properties: [],
        relationships: []
      }
      
      assert {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert length(resources) == 2
      
      # Check resource structure
      person_resource = Enum.find(resources, &(&1.class.name == "Person"))
      assert person_resource.module_name == "CnsForge.TTLResources.Person"
      assert String.contains?(person_resource.code, "defmodule CnsForge.TTLResources.Person")
      assert String.contains?(person_resource.code, "use Ash.Resource")
    end
    
    test "handles empty class list" do
      parsed_data = %{
        prefixes: %{},
        classes: [],
        properties: [],
        relationships: []
      }
      
      assert {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert resources == []
    end
    
    test "generated resource code includes required Ash components" do
      parsed_data = %{
        prefixes: %{},
        classes: [%{uri: "test:TestEntity", name: "TestEntity"}],
        properties: [],
        relationships: []
      }
      
      {:ok, [resource]} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Verify generated code contains required elements
      assert String.contains?(resource.code, "uuid_primary_key :id")
      assert String.contains?(resource.code, "attribute :ttl_uri, :string")
      assert String.contains?(resource.code, "defaults [:read, :create, :update, :destroy]")
      assert String.contains?(resource.code, "data_layer: Ash.DataLayer.Ets")
    end
  end
  
  describe "Step 4: Reactor Generation - generate_ash_reactors/2" do
    test "generates main reactor for class processing" do
      parsed_data = %{
        classes: [
          %{name: "Person"}, 
          %{name: "Vehicle"}
        ]
      }
      resources = []  # Not used in current implementation
      
      assert {:ok, reactors} = TTLAshReactorTransformer.generate_ash_reactors(parsed_data, resources)
      assert length(reactors) == 1
      
      main_reactor = hd(reactors)
      assert main_reactor.name == "CnsForge.TTLMainReactor"
      assert String.contains?(main_reactor.code, "defmodule CnsForge.TTLMainReactor")
      assert String.contains?(main_reactor.code, "use Reactor")
    end
    
    test "reactor code includes proper structure" do
      parsed_data = %{classes: [%{name: "TestClass"}]}
      
      {:ok, [reactor]} = TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      # Verify reactor structure
      assert String.contains?(reactor.code, "input :ontology_data")
      assert String.contains?(reactor.code, "step :transform_classes")
      assert String.contains?(reactor.code, "return :transform_classes")
      assert String.contains?(reactor.code, "transformed_classes: 1")
    end
    
    test "handles empty class list in reactor generation" do
      parsed_data = %{classes: []}
      
      {:ok, [reactor]} = TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      # Should still generate reactor but with 0 classes
      assert String.contains?(reactor.code, "transformed_classes: 0")
    end
  end
  
  describe "Step 5: Domain Generation - generate_simple_domain/0" do
    test "generates valid Ash domain code" do
      domain_code = TTLAshReactorTransformer.generate_simple_domain()
      
      assert String.contains?(domain_code, "defmodule CnsForge.TTLDomain")
      assert String.contains?(domain_code, "use Ash.Domain")
      assert String.contains?(domain_code, "authorize :when_requested")
    end
  end
  
  describe "Integration: Complete Transform Pipeline" do
    test "end-to-end transformation with valid TTL" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:User a owl:Class .
      test:Role a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Verify complete result structure
      assert %{parsed_ontology: parsed, resources: resources, reactors: reactors, domain: domain} = result
      assert length(parsed.classes) == 2
      assert length(resources) == 2
      assert length(reactors) == 1
      assert is_binary(domain)
    end
    
    test "handles transformation errors gracefully" do
      # Test with invalid input type
      assert {:error, reason} = TTLAshReactorTransformer.transform_ttl(123)
      assert reason =~ "no function clause matching"
    end
  end
end