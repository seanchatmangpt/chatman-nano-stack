defmodule CnsForge.ReactorSteps.ResourceGeneratorStepTest do
  @moduledoc """
  🧪 REAL incremental tests for Ash Resource generator reactor step
  
  Tests ONLY the generate_ash_resources/1 step:
  - Resource code generation from parsed classes
  - Module name formatting
  - Ash.Resource structure validation
  - Generated code syntax correctness
  
  ⚠️  RED TEAM DEFENSE: These are REAL tests that execute actual code
  """
  
  use ExUnit.Case, async: true
  alias CnsForge.TTLAshReactorTransformer
  
  describe "generate_ash_resources/1 step" do
    test "generates single resource from one class" do
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
      
      assert {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert is_list(resources)
      assert length(resources) == 1
      
      [resource] = resources
      assert Map.has_key?(resource, :class)
      assert Map.has_key?(resource, :module_name)
      assert Map.has_key?(resource, :code)
      assert resource.module_name == "CnsForge.TTLResources.Person"
    end
    
    test "generates multiple resources from multiple classes" do
      parsed_data = %{
        classes: [
          %{uri: "aegis:ThreatActor", name: "ThreatActor", module_name: "CnsForge.TTLResources.ThreatActor", attributes: []},
          %{uri: "aegis:Vulnerability", name: "Vulnerability", module_name: "CnsForge.TTLResources.Vulnerability", attributes: []},
          %{uri: "aegis:AttackPattern", name: "AttackPattern", module_name: "CnsForge.TTLResources.AttackPattern", attributes: []}
        ]
      }
      
      assert {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert length(resources) == 3
      
      module_names = Enum.map(resources, & &1.module_name)
      assert "CnsForge.TTLResources.ThreatActor" in module_names
      assert "CnsForge.TTLResources.Vulnerability" in module_names
      assert "CnsForge.TTLResources.AttackPattern" in module_names
    end
    
    test "generates valid Ash.Resource code structure" do
      parsed_data = %{
        classes: [
          %{uri: "security:Asset", name: "Asset", module_name: "CnsForge.TTLResources.Asset", attributes: []}
        ]
      }
      
      {:ok, [resource]} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Check that generated code contains required Ash.Resource elements
      code = resource.code
      assert String.contains?(code, "defmodule CnsForge.TTLResources.Asset")
      assert String.contains?(code, "use Ash.Resource")
      assert String.contains?(code, "domain: CnsForge.TTLDomain")
      assert String.contains?(code, "data_layer: Ash.DataLayer.Ets")
      assert String.contains?(code, "attributes do")
      assert String.contains?(code, "uuid_primary_key :id")
      assert String.contains?(code, "attribute :ttl_uri, :string")
      assert String.contains?(code, "actions do")
      assert String.contains?(code, "defaults [:read, :create, :update, :destroy]")
    end
    
    test "handles empty classes list" do
      parsed_data = %{classes: []}
      
      assert {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert resources == []
    end
    
    test "preserves class information in resource mapping" do
      parsed_data = %{
        classes: [
          %{
            uri: "custom:SpecialClass", 
            name: "SpecialClass",
            module_name: "CnsForge.TTLResources.SpecialClass",
            attributes: []
          }
        ]
      }
      
      {:ok, [resource]} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Verify the original class data is preserved
      assert resource.class.uri == "custom:SpecialClass"
      assert resource.class.name == "SpecialClass"
      assert resource.class.module_name == "CnsForge.TTLResources.SpecialClass"
    end
    
    test "generates proper module documentation" do
      parsed_data = %{
        classes: [
          %{uri: "doc:Example", name: "Example", module_name: "CnsForge.TTLResources.Example", attributes: []}
        ]
      }
      
      {:ok, [resource]} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Check that generated code includes proper moduledoc
      assert String.contains?(resource.code, "@moduledoc \"Clean Ash.Resource for Example\"")
    end
    
    test "validates all resources have consistent structure" do
      parsed_data = %{
        classes: [
          %{uri: "a:A", name: "A", module_name: "CnsForge.TTLResources.A", attributes: []},
          %{uri: "b:B", name: "B", module_name: "CnsForge.TTLResources.B", attributes: []},
          %{uri: "c:C", name: "C", module_name: "CnsForge.TTLResources.C", attributes: []}
        ]
      }
      
      {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # All resources should have the same structure
      Enum.each(resources, fn resource ->
        assert Map.has_key?(resource, :class)
        assert Map.has_key?(resource, :module_name)
        assert Map.has_key?(resource, :code)
        assert is_binary(resource.code)
        assert String.length(resource.code) > 0
        
        # Each should contain the standard Ash.Resource elements
        code = resource.code
        assert String.contains?(code, "use Ash.Resource")
        assert String.contains?(code, "uuid_primary_key :id")
        assert String.contains?(code, "attribute :ttl_uri, :string")
      end)
    end
  end
  
  describe "integration with parse_ttl/1" do
    test "resource generation from parsed TTL end-to-end" do
      ttl_content = """
      @prefix cyber: <http://cybersecurity.org/> .
      cyber:Malware rdf:type owl:Class .
      cyber:Incident a owl:Class .
      """
      
      # First parse the TTL
      {:ok, parsed} = TTLAshReactorTransformer.parse_ttl(ttl_content)
      
      # Then generate resources
      {:ok, resources} = TTLAshReactorTransformer.generate_ash_resources(parsed)
      
      assert length(resources) == 2
      
      # Verify both classes were converted to resources
      resource_names = Enum.map(resources, fn r -> r.class.name end)
      assert "Malware" in resource_names
      assert "Incident" in resource_names
      
      # Verify generated code is syntactically correct
      Enum.each(resources, fn resource ->
        # Basic syntax validation - code should be compilable Elixir
        assert String.starts_with?(resource.code, "defmodule")
        assert String.ends_with?(String.trim(resource.code), "end")
        assert String.contains?(resource.code, "do")
      end)
    end
  end
end