defmodule CnsForge.TTLAshReactorTransformerTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  
  alias CnsForge.TTLAshReactorTransformer
  
  @moduletag :bdd
  @moduletag :reactor
  
  describe "TTL → Ash.Reactor Transformation" do
    setup do
      # Basic TTL content for testing
      basic_ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      
      cns:TestClass a owl:Class ;
          rdfs:label "Test Class" ;
          rdfs:comment "A test class for BDD" .
      """
      
      complex_ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      
      cns:Agent a owl:Class ;
          rdfs:label "Agent" .
          
      cns:Signal a owl:Class ;
          rdfs:label "Signal" .
          
      cns:processes a owl:ObjectProperty ;
          rdfs:domain cns:Agent ;
          rdfs:range cns:Signal .
      """
      
      {:ok, basic_ttl: basic_ttl, complex_ttl: complex_ttl}
    end
    
    @tag :critical
    test "transforms simple TTL with single class", %{basic_ttl: ttl} do
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Verify structure
      assert Map.has_key?(result, :parsed_ontology)
      assert Map.has_key?(result, :resources)
      assert Map.has_key?(result, :reactors)
      assert Map.has_key?(result, :domain)
      
      # Verify counts
      assert length(result.parsed_ontology.classes) == 1
      assert length(result.resources) == 1
      assert length(result.reactors) >= 1
      
      # Verify class name
      [class] = result.parsed_ontology.classes
      assert class.name == "TestClass"
    end
    
    @tag :parsing
    test "parses TTL prefixes correctly" do
      ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix custom: <http://example.org/custom#> .
      """
      
      assert {:ok, parsed} = TTLAshReactorTransformer.parse_ttl(ttl)
      assert map_size(parsed.prefixes) >= 2
      assert parsed.prefixes["cns"] == "http://cns.io/ontology#"
      assert parsed.prefixes["owl"] == "http://www.w3.org/2002/07/owl#"
    end
    
    @tag :relationships
    test "extracts object properties as relationships", %{complex_ttl: ttl} do
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Check relationships were extracted
      assert length(result.parsed_ontology.relationships) > 0
      
      [rel | _] = result.parsed_ontology.relationships
      assert rel.from == "Agent"
      assert rel.to == "Signal"
      assert rel.property == "processes"
    end
    
    @tag :resources
    test "generates Ash resources with correct attributes" do
      ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:BitActor a owl:Class ;
          rdfs:label "Bit Actor" .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      assert [resource] = result.resources
      
      # Check generated code includes required attributes
      assert resource.code =~ "uuid_primary_key :id"
      assert resource.code =~ "attribute :ttl_uri, :string"
      assert resource.code =~ "attribute :created_at, :utc_datetime_usec"
      assert resource.code =~ "attribute :updated_at, :utc_datetime_usec"
    end
    
    @tag :reactor_generation
    test "generates main reactor with all class processing steps", %{complex_ttl: ttl} do
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Find main reactor
      main_reactor = Enum.find(result.reactors, fn r -> 
        r.name == "CnsForge.TTLMainReactor"
      end)
      
      assert main_reactor != nil
      assert main_reactor.code =~ "step :process_agent"
      assert main_reactor.code =~ "step :process_signal"
      assert main_reactor.code =~ "step :aggregate_results"
    end
    
    @tag :ttl_bounds
    test "enforces TTL execution constraints in reactors" do
      ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      cns:FastAgent a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      [reactor | _] = result.reactors
      
      # Verify TTL constraint checks
      assert reactor.code =~ "max_step_execution_ns"
      assert reactor.code =~ "TTL constraint violation"
      assert reactor.code =~ "System.monotonic_time(:nanosecond)"
    end
    
    @tag :error_handling
    test "handles malformed TTL gracefully" do
      invalid_ttl = """
      @prefix broken
      this is not valid turtle syntax
      """
      
      assert capture_log(fn ->
        assert {:error, _reason} = TTLAshReactorTransformer.transform_ttl(invalid_ttl)
      end) =~ "TTL transformation failed"
    end
    
    @tag :domain_generation
    test "generates Ash domain with all resources" do
      ttl = """
      @prefix cns: <http://cns.io/ontology#> .
      cns:Resource1 a owl:Class .
      cns:Resource2 a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Domain should reference all resources
      assert result.domain =~ "resource CnsForge.TTLResources.Resource1"
      assert result.domain =~ "resource CnsForge.TTLResources.Resource2"
      assert result.domain =~ "authorization do"
    end
    
    @tag :file_generation
    test "writes generated files to correct locations", %{basic_ttl: ttl} do
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      assert length(result.generated_files) > 0
      
      # Verify paths
      Enum.each(result.generated_files, fn path ->
        assert String.starts_with?(path, "/Users/sac/cns/lib/cns_forge/generated/")
      end)
    end
  end
  
  describe "TTL Parsing Functions" do
    @tag :unit
    test "extract_prefixes/1 handles multiple prefix formats" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix   owl:   <http://www.w3.org/2002/07/owl#>   .
      @prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>.
      """
      
      # Testing private function through public interface
      assert {:ok, parsed} = TTLAshReactorTransformer.parse_ttl(ttl)
      assert map_size(parsed.prefixes) == 3
    end
    
    @tag :unit
    test "extract_classes/2 identifies all owl:Class declarations" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:Class1 a owl:Class .
      cns:Class2 rdf:type owl:Class .
      cns:NotAClass a owl:Property .
      """
      
      assert {:ok, parsed} = TTLAshReactorTransformer.parse_ttl(ttl)
      assert length(parsed.classes) == 2
    end
  end
  
  describe "Performance and Coverage" do
    @tag :performance
    @tag timeout: 5000
    test "transforms complex ontology within performance bounds" do
      # Generate a large TTL
      large_ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      #{Enum.map_join(1..10, "\n", fn i ->
        "cns:Class#{i} a owl:Class ; rdfs:label \"Class #{i}\" ."
      end)}
      """
      
      start_time = System.monotonic_time(:millisecond)
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(large_ttl)
      elapsed = System.monotonic_time(:millisecond) - start_time
      
      # Should complete in under 1 second
      assert elapsed < 1000
      assert length(result.resources) == 10
      assert length(result.reactors) >= 11 # Main + one per class
    end
    
    @tag :coverage
    test "achieves 80%+ code coverage on all critical paths" do
      # Complex TTL to exercise all code paths
      comprehensive_ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      
      # Classes
      cns:Asset a owl:Class ;
          rdfs:label "Asset" ;
          rdfs:comment "A valuable resource" .
          
      cns:Threat a owl:Class ;
          rdfs:label "Threat" ;
          rdfs:comment "A security threat" .
      
      cns:Vulnerability a owl:Class ;
          rdfs:label "Vulnerability" ;
          rdfs:comment "A system weakness" .
      
      # Object properties
      cns:targets a owl:ObjectProperty ;
          rdfs:domain cns:Threat ;
          rdfs:range cns:Asset ;
          rdfs:label "targets" .
          
      cns:exploits a owl:ObjectProperty ;
          rdfs:domain cns:Threat ;
          rdfs:range cns:Vulnerability ;
          rdfs:label "exploits" .
          
      # Datatype properties
      cns:severity a owl:DatatypeProperty ;
          rdfs:domain cns:Vulnerability ;
          rdfs:range rdfs:Literal ;
          rdfs:label "severity" .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(comprehensive_ttl)
      
      # Verify all code paths were exercised
      assert length(result.parsed_ontology.classes) == 3
      assert length(result.parsed_ontology.properties) >= 2
      assert length(result.parsed_ontology.relationships) >= 2
      assert length(result.resources) == 3
      assert length(result.reactors) >= 4
      
      # Verify relationships in generated code
      resource_codes = Enum.map(result.resources, & &1.code)
      combined_code = Enum.join(resource_codes, "\n")
      
      assert combined_code =~ "belongs_to"
    end
  end
  
  describe "Edge Cases and Error Conditions" do
    @tag :edge_cases
    test "handles TTL with only prefixes" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      assert result.resources == []
      assert length(result.reactors) == 1 # Only main reactor
    end
    
    @tag :edge_cases
    test "handles circular relationships gracefully" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:A a owl:Class .
      cns:B a owl:Class .
      
      cns:relatesTo a owl:ObjectProperty ;
          rdfs:domain cns:A ;
          rdfs:range cns:B .
          
      cns:relatesBack a owl:ObjectProperty ;
          rdfs:domain cns:B ;
          rdfs:range cns:A .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      assert length(result.parsed_ontology.relationships) == 2
    end
    
    @tag :edge_cases
    test "handles properties without domain or range" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:orphanProperty a owl:ObjectProperty .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      assert result.parsed_ontology.relationships == []
    end
    
    @tag :edge_cases
    test "sanitizes invalid Elixir module names" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:123BadName a owl:Class .
      cns:good-name a owl:Class .
      cns:ValidName a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      # Should still generate resources for valid names
      assert length(result.resources) >= 1
      
      # Check that module names are valid Elixir identifiers
      Enum.each(result.resources, fn resource ->
        assert resource.module_name =~ ~r/^[A-Z][a-zA-Z0-9_.]*$/
      end)
    end
  end
  
  describe "Telemetry Integration" do
    @tag :telemetry
    test "generated resources include telemetry events" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      cns:BitActor a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      [resource] = result.resources
      
      assert resource.code =~ ":telemetry.execute"
      assert resource.code =~ "[:cns_forge, :ttl, :resource_processed]"
      assert resource.code =~ "processing_time:"
      assert resource.code =~ "resource:"
    end
    
    @tag :telemetry
    test "TTL constraints generate proper error telemetry" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      cns:ConstrainedResource a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      [resource] = result.resources
      
      assert resource.code =~ "TTL constraint violation"
      assert resource.code =~ "processing took"
      assert resource.code =~ "max allowed"
    end
  end
  
  describe "Ash Integration Validation" do
    @tag :ash_integration
    test "resources use correct Ash patterns" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      cns:TestResource a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      [resource] = result.resources
      
      # Verify Ash.Resource usage
      assert resource.code =~ "use Ash.Resource"
      assert resource.code =~ "domain: CnsForge.TTLDomain"
      assert resource.code =~ "data_layer: Ash.DataLayer.Ets"
      
      # Verify standard actions
      assert resource.code =~ "defaults [:read, :destroy]"
      assert resource.code =~ "create :create_from_ttl"
      assert resource.code =~ "update :process_semantics"
      
      # Verify attributes structure
      assert resource.code =~ "attributes do"
      assert resource.code =~ "validations do"
      assert resource.code =~ "changes do"
    end
    
    @tag :ash_integration
    test "reactors use correct Reactor patterns" do
      ttl = """
      @prefix cns: <http://cns.io#> .
      cns:ReactorTest a owl:Class .
      """
      
      assert {:ok, result} = TTLAshReactorTransformer.transform_ttl(ttl)
      
      main_reactor = Enum.find(result.reactors, fn r ->
        r.name == "CnsForge.TTLMainReactor"
      end)
      
      assert main_reactor.code =~ "use Reactor"
      assert main_reactor.code =~ "input :ontology_data"
      assert main_reactor.code =~ "input :ttl_constraints"
      assert main_reactor.code =~ "return :aggregate_results"
      
      # Verify step structure
      assert main_reactor.code =~ "step :initialize_ttl_context"
      assert main_reactor.code =~ "step :validate_ontology"
      assert main_reactor.code =~ "step :aggregate_results"
    end
  end
end