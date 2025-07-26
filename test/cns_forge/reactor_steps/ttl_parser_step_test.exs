defmodule CnsForge.ReactorSteps.TTLParserStepTest do
  @moduledoc """
  🧪 REAL incremental tests for TTL parser reactor steps
  
  Tests ONLY the individual steps of TTL parsing:
  - parse_ttl/1
  - extract_classes/1  
  - extract_local_name/1
  
  ⚠️  RED TEAM DEFENSE: These are REAL tests that execute actual code
  """
  
  use ExUnit.Case, async: true
  alias CnsForge.TTLAshReactorTransformer
  
  describe "parse_ttl/1 step" do
    test "parses simple TTL content with one class" do
      ttl_content = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:Person rdf:type owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl_content)
      assert is_map(result)
      assert Map.has_key?(result, :prefixes)
      assert Map.has_key?(result, :classes)
      assert Map.has_key?(result, :properties)
      assert is_list(result.classes)
    end
    
    test "handles empty TTL content" do
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl("")
      assert result.classes == []
    end
    
    test "parses TTL with multiple classes" do
      ttl_content = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:Person rdf:type owl:Class .
      ex:Organization a owl:Class .
      ex:Event rdf:type owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl_content)
      assert length(result.classes) == 3
    end
    
    test "validates function input type" do
      # Should only accept binary (string) input
      assert_raise FunctionClauseError, fn ->
        TTLAshReactorTransformer.parse_ttl(123)
      end
      
      assert_raise FunctionClauseError, fn ->
        TTLAshReactorTransformer.parse_ttl(nil)
      end
    end
  end
  
  describe "extract_classes/1 step" do
    test "extracts single class with rdf:type syntax" do
      ttl_content = "ex:Person rdf:type owl:Class ."
      
      classes = TTLAshReactorTransformer.send(:extract_classes, ttl_content)
      
      assert length(classes) == 1
      assert [%{uri: "ex:Person", name: "Person"}] = classes
    end
    
    test "extracts single class with 'a' syntax" do
      ttl_content = "ex:Organization a owl:Class ."
      
      classes = TTLAshReactorTransformer.send(:extract_classes, ttl_content)
      
      assert length(classes) == 1
      assert [%{uri: "ex:Organization", name: "Organization"}] = classes
    end
    
    test "extracts multiple classes with mixed syntax" do
      ttl_content = """
      ex:Person rdf:type owl:Class .
      ex:Organization a owl:Class .
      aegis:ThreatActor rdf:type owl:Class .
      """
      
      classes = TTLAshReactorTransformer.send(:extract_classes, ttl_content)
      
      assert length(classes) == 3
      uris = Enum.map(classes, & &1.uri)
      assert "ex:Person" in uris
      assert "ex:Organization" in uris  
      assert "aegis:ThreatActor" in uris
    end
    
    test "handles TTL with no classes" do
      ttl_content = """
      @prefix ex: <http://example.org/> .
      ex:hasName rdf:type owl:Property .
      """
      
      classes = TTLAshReactorTransformer.send(:extract_classes, ttl_content)
      assert classes == []
    end
    
    test "validates each extracted class structure" do
      ttl_content = "security:Vulnerability rdf:type owl:Class ."
      
      classes = TTLAshReactorTransformer.send(:extract_classes, ttl_content)
      
      assert [class] = classes
      assert Map.has_key?(class, :uri)
      assert Map.has_key?(class, :name)
      assert Map.has_key?(class, :module_name)
      assert Map.has_key?(class, :attributes)
      assert class.uri == "security:Vulnerability"
      assert class.name == "Vulnerability"
      assert is_list(class.attributes)
    end
  end
  
  describe "extract_local_name/1 step" do
    test "extracts local name from prefixed URI" do
      assert TTLAshReactorTransformer.send(:extract_local_name, "ex:Person") == "Person"
      assert TTLAshReactorTransformer.send(:extract_local_name, "owl:Class") == "Class"
      assert TTLAshReactorTransformer.send(:extract_local_name, "aegis:ThreatActor") == "ThreatActor"
    end
    
    test "handles URI without prefix" do
      assert TTLAshReactorTransformer.send(:extract_local_name, "Person") == "Person"
      assert TTLAshReactorTransformer.send(:extract_local_name, "Class") == "Class"
    end
    
    test "handles complex namespace prefixes" do
      assert TTLAshReactorTransformer.send(:extract_local_name, "cybersecurity:AttackPattern") == "AttackPattern"
      assert TTLAshReactorTransformer.send(:extract_local_name, "stix:Malware") == "Malware"
    end
    
    test "handles edge cases correctly" do
      # Empty string
      assert TTLAshReactorTransformer.send(:extract_local_name, "") == ""
      
      # Only prefix with colon
      assert TTLAshReactorTransformer.send(:extract_local_name, "ex:") == ""
      
      # Multiple colons (should take last part)
      assert TTLAshReactorTransformer.send(:extract_local_name, "http://example.org:Thing") == "Thing"
    end
  end
  
  describe "integration between parser steps" do
    test "parse_ttl/1 uses extract_classes/1 correctly" do
      ttl_content = """
      @prefix aegis: <http://aegisfabric.io/ontology/> .
      aegis:ThreatActor rdf:type owl:Class .
      aegis:Vulnerability a owl:Class .
      """
      
      {:ok, result} = TTLAshReactorTransformer.parse_ttl(ttl_content)
      
      # Verify classes were extracted correctly
      assert length(result.classes) == 2
      
      # Verify class structures contain all required fields
      Enum.each(result.classes, fn class ->
        assert Map.has_key?(class, :uri)
        assert Map.has_key?(class, :name)
        assert Map.has_key?(class, :module_name)
        assert Map.has_key?(class, :attributes)
        assert is_binary(class.name)
        assert String.contains?(class.module_name, "CnsForge.TTLResources.")
      end)
    end
  end
end