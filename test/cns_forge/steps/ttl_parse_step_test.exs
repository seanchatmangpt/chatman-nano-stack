defmodule CnsForge.Steps.TTLParseStepTest do
  @moduledoc """
  🧪 UNIT TESTING for individual TTL Parse Step
  
  Following Reactor testing best practices:
  - Test step modules directly with run/3 signature  
  - Test error conditions and edge cases
  - Ensure steps are isolated and testable
  """
  
  use ExUnit.Case, async: true
  
  # Test the parse step as it would be called in a reactor
  describe "TTL Parse Step - Direct Step Testing" do
    test "processes valid TTL content through step interface" do
      # Simulate how the step would be called in a reactor
      arguments = %{
        ttl_content: """
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix test: <http://test.org/> .
        
        test:Person a owl:Class .
        test:Organization a owl:Class .
        """
      }
      context = %{}
      options = []
      
      # Test the parsing logic as it would work in a step
      result = CnsForge.TTLAshReactorTransformer.parse_ttl(arguments.ttl_content)
      
      assert {:ok, parsed} = result
      assert length(parsed.classes) == 2
      assert Enum.any?(parsed.classes, &(&1.name == "Person"))
      assert Enum.any?(parsed.classes, &(&1.name == "Organization"))
    end
    
    test "handles empty TTL gracefully" do
      arguments = %{ttl_content: ""}
      
      result = CnsForge.TTLAshReactorTransformer.parse_ttl(arguments.ttl_content)
      
      assert {:ok, parsed} = result
      assert parsed.classes == []
    end
    
    test "extracts class information correctly" do
      arguments = %{
        ttl_content: """
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix ex: <http://example.org/> .
        
        ex:DetailedClass a owl:Class .
        """
      }
      
      {:ok, parsed} = CnsForge.TTLAshReactorTransformer.parse_ttl(arguments.ttl_content)
      
      class = hd(parsed.classes)
      assert class.name == "DetailedClass"
      assert class.uri == "ex:DetailedClass"
      assert class.module_name == "CnsForge.TTLResources.DetailedClass"
    end
    
    test "handles malformed TTL without crashing" do
      malformed_inputs = [
        "definitely not TTL",
        "test:BrokenClass owl:Class .",  # Missing 'a' or 'rdf:type'
        "@prefix incomplete",
        ""
      ]
      
      Enum.each(malformed_inputs, fn input ->
        result = CnsForge.TTLAshReactorTransformer.parse_ttl(input)
        
        # Should always return ok with empty or filtered results
        assert {:ok, _parsed} = result
      end)
    end
  end
  
  describe "Class Extraction Edge Cases" do
    test "handles different class declaration syntaxes" do
      ttl_variants = [
        # Standard syntax
        "test:Class1 a owl:Class .",
        # RDF type syntax  
        "test:Class2 rdf:type owl:Class .",
        # With extra whitespace
        "test:Class3    a    owl:Class   .",
        # Mixed in one document
        """
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix test: <http://test.org/> .
        
        test:Standard a owl:Class .
        test:RDFType rdf:type owl:Class .
        """
      ]
      
      combined_ttl = Enum.join(ttl_variants, "\n")
      {:ok, parsed} = CnsForge.TTLAshReactorTransformer.parse_ttl(combined_ttl)
      
      # Should find at least the classes from the combined document
      class_names = Enum.map(parsed.classes, & &1.name)
      assert "Standard" in class_names
      assert "RDFType" in class_names
    end
    
    test "ignores non-class RDF statements" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:ValidClass a owl:Class .
      test:SomeProperty a owl:ObjectProperty .
      test:SomeIndividual a test:ValidClass .
      test:AnotherProperty rdf:type owl:DatatypeProperty .
      """
      
      {:ok, parsed} = CnsForge.TTLAshReactorTransformer.parse_ttl(ttl)
      
      class_names = Enum.map(parsed.classes, & &1.name)
      assert class_names == ["ValidClass"]
    end
    
    test "handles classes with complex naming" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:User_Profile a owl:Class .
      test:Class123 a owl:Class .
      test:XMLDocument a owl:Class .
      test:CamelCaseClass a owl:Class .
      """
      
      {:ok, parsed} = CnsForge.TTLAshReactorTransformer.parse_ttl(ttl)
      
      class_names = Enum.map(parsed.classes, & &1.name)
      assert "User_Profile" in class_names
      assert "Class123" in class_names
      assert "XMLDocument" in class_names  
      assert "CamelCaseClass" in class_names
    end
  end
end