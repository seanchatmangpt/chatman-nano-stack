defmodule CnsForge.Steps.ResourceGenerationStepTest do
  @moduledoc """
  🧪 UNIT TESTING for Ash Resource Generation Step
  
  Tests the resource generation step in isolation to ensure:
  - Proper Ash.Resource code generation
  - Correct module naming
  - Required attributes and actions
  """
  
  use ExUnit.Case, async: true
  
  describe "Resource Generation Step - Direct Testing" do
    test "generates single resource correctly" do
      parsed_data = %{
        classes: [
          %{
            uri: "test:Person", 
            name: "Person",
            module_name: "CnsForge.TTLResources.Person",
            attributes: []
          }
        ]
      }
      
      assert {:ok, resources} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert length(resources) == 1
      
      resource = hd(resources)
      assert resource.class.name == "Person"
      assert resource.module_name == "CnsForge.TTLResources.Person"
      assert is_binary(resource.code)
    end
    
    test "generates multiple resources from multiple classes" do
      parsed_data = %{
        classes: [
          %{uri: "test:User", name: "User", module_name: "CnsForge.TTLResources.User", attributes: []},
          %{uri: "test:Role", name: "Role", module_name: "CnsForge.TTLResources.Role", attributes: []},
          %{uri: "test:Permission", name: "Permission", module_name: "CnsForge.TTLResources.Permission", attributes: []}
        ]
      }
      
      {:ok, resources} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert length(resources) == 3
      
      names = Enum.map(resources, & &1.class.name)
      assert "User" in names
      assert "Role" in names
      assert "Permission" in names
    end
    
    test "handles empty class list" do
      parsed_data = %{classes: []}
      
      {:ok, resources} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert resources == []
    end
  end
  
  describe "Generated Resource Code Quality" do
    test "generated code contains required Ash components" do
      parsed_data = %{
        classes: [%{
          uri: "test:TestEntity", 
          name: "TestEntity",
          module_name: "CnsForge.TTLResources.TestEntity", 
          attributes: []
        }]
      }
      
      {:ok, [resource]} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      code = resource.code
      
      # Check module definition
      assert String.contains?(code, "defmodule CnsForge.TTLResources.TestEntity do")
      
      # Check Ash.Resource usage
      assert String.contains?(code, "use Ash.Resource")
      assert String.contains?(code, "domain: CnsForge.TTLDomain")
      assert String.contains?(code, "data_layer: Ash.DataLayer.Ets")
      
      # Check required attributes
      assert String.contains?(code, "attributes do")
      assert String.contains?(code, "uuid_primary_key :id")
      assert String.contains?(code, "attribute :ttl_uri, :string, public?: true")
      
      # Check default actions
      assert String.contains?(code, "actions do")
      assert String.contains?(code, "defaults [:read, :create, :update, :destroy]")
    end
    
    test "generated code is syntactically valid Elixir" do
      parsed_data = %{
        classes: [%{
          uri: "test:ValidClass", 
          name: "ValidClass",
          module_name: "CnsForge.TTLResources.ValidClass",
          attributes: []
        }]
      }
      
      {:ok, [resource]} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Test that the generated code can be parsed as valid Elixir
      assert {:ok, _ast} = Code.string_to_quoted(resource.code)
    end
    
    test "handles special characters in class names" do
      parsed_data = %{
        classes: [
          %{uri: "test:User_Profile", name: "User_Profile", module_name: "CnsForge.TTLResources.User_Profile", attributes: []},
          %{uri: "test:Class123", name: "Class123", module_name: "CnsForge.TTLResources.Class123", attributes: []}
        ]
      }
      
      {:ok, resources} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      
      # Check that module names are handled correctly
      user_profile = Enum.find(resources, &(&1.class.name == "User_Profile"))
      assert String.contains?(user_profile.code, "defmodule CnsForge.TTLResources.User_Profile")
      
      class123 = Enum.find(resources, &(&1.class.name == "Class123"))
      assert String.contains?(class123.code, "defmodule CnsForge.TTLResources.Class123")
    end
  end
  
  describe "Resource Generation Error Scenarios" do
    test "handles nil or malformed input gracefully" do
      # Test with missing classes key
      assert_raise(KeyError, fn ->
        CnsForge.TTLAshReactorTransformer.generate_ash_resources(%{})
      end)
    end
    
    test "handles classes with missing required fields" do
      # This tests the robustness of the string interpolation
      parsed_data = %{
        classes: [
          %{uri: "test:Minimal", name: "Minimal", attributes: []}
          # Missing module_name - should not crash but may generate invalid code
        ]
      }
      
      # Should not crash during generation
      assert {:ok, resources} = CnsForge.TTLAshReactorTransformer.generate_ash_resources(parsed_data)
      assert length(resources) == 1
    end
  end
end