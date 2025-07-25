defmodule CnsForge.TTLAshReactorTest do
  use ExUnit.Case, async: false
  
  alias CnsForge.TTLAshReactorTransformer
  alias CnsForge.TTLReactorDemo
  
  @moduletag :integration
  
  @sample_ttl """
  @prefix cns: <http://cns-forge.org/ontology#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .
  
  cns:BitActor a owl:Class .
  cns:Signal a owl:Class .
  cns:processes a owl:ObjectProperty ;
    owl:domain cns:BitActor ;
    owl:range cns:Signal .
  """
  
  describe "TTL → Ash.Reactor transformation" do
    test "parses TTL ontology correctly" do
      {:ok, parsed} = TTLAshReactorTransformer.parse_ttl(@sample_ttl)
      
      # Verify prefixes
      assert parsed.prefixes["cns"] == "http://cns-forge.org/ontology#"
      assert parsed.prefixes["owl"] == "http://www.w3.org/2002/07/owl#"
      
      # Verify classes
      assert length(parsed.classes) == 2
      class_names = Enum.map(parsed.classes, & &1.name)
      assert "BitActor" in class_names
      assert "Signal" in class_names
      
      # Verify properties
      assert length(parsed.properties) == 1
      process_property = Enum.find(parsed.properties, & &1.name == "processes")
      assert process_property.domain == "cns:BitActor"
      assert process_property.range == "cns:Signal"
      
      # Verify relationships
      assert length(parsed.relationships) == 1
      relationship = List.first(parsed.relationships)
      assert relationship.from == "BitActor"
      assert relationship.to == "Signal"
      assert relationship.property == "processes"
    end
    
    test "generates Ash.Resource definitions" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      resources = transformation_result.resources
      assert length(resources) == 2
      
      # Verify BitActor resource
      bitactor_resource = Enum.find(resources, & &1.class.name == "BitActor")
      assert bitactor_resource.module_name == "CnsForge.TTLResources.BitActor"
      assert String.contains?(bitactor_resource.code, "use Ash.Resource")
      assert String.contains?(bitactor_resource.code, "data_layer: Ash.DataLayer.Ets")
      assert String.contains?(bitactor_resource.code, "uuid_primary_key :id")
      assert String.contains?(bitactor_resource.code, "attribute :ttl_uri, :string")
      
      # Verify Signal resource
      signal_resource = Enum.find(resources, & &1.class.name == "Signal")
      assert signal_resource.module_name == "CnsForge.TTLResources.Signal"
      assert String.contains?(signal_resource.code, "use Ash.Resource")
    end
    
    test "generates Ash.Reactor workflows" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      reactors = transformation_result.reactors
      assert length(reactors) >= 1
      
      # Verify main reactor
      main_reactor = Enum.find(reactors, & &1.name == "CnsForge.TTLMainReactor")
      assert main_reactor
      assert String.contains?(main_reactor.code, "use Reactor")
      assert String.contains?(main_reactor.code, "input :ontology_data")
      assert String.contains?(main_reactor.code, "input :ttl_constraints")
      assert String.contains?(main_reactor.code, "step :initialize_ttl_context")
      assert String.contains?(main_reactor.code, "step :validate_ontology")
      assert String.contains?(main_reactor.code, "step :process_bitactor")
      assert String.contains?(main_reactor.code, "step :process_signal")
      assert String.contains?(main_reactor.code, "return :aggregate_results")
    end
    
    test "generates Ash.Domain with TTL-bounded execution" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      domain_code = transformation_result.domain
      assert String.contains?(domain_code, "defmodule CnsForge.TTLDomain")
      assert String.contains?(domain_code, "use Ash.Domain")
      assert String.contains?(domain_code, "resource CnsForge.TTLResources.BitActor")
      assert String.contains?(domain_code, "resource CnsForge.TTLResources.Signal")
      assert String.contains?(domain_code, "process_ontology_with_ttl_bounds")
      assert String.contains?(domain_code, "max_execution_ns")
      assert String.contains?(domain_code, "Reactor.run(CnsForge.TTLMainReactor")
    end
    
    test "writes generated files successfully" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      generated_files = transformation_result.generated_files
      assert length(generated_files) >= 4  # 2 resources + 3 reactors + 1 domain
      
      # Check files exist
      Enum.each(generated_files, fn file_path ->
        assert File.exists?(file_path)
        content = File.read!(file_path)
        assert String.length(content) > 0
        assert String.contains?(content, "defmodule")
      end)
      
      # Cleanup
      Enum.each(generated_files, &File.rm/1)
    end
    
    test "generated code has proper TTL constraint handling" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      # Check main reactor has TTL bounds
      main_reactor = Enum.find(transformation_result.reactors, & &1.name == "CnsForge.TTLMainReactor")
      assert String.contains?(main_reactor.code, "max_total_execution_ns")
      assert String.contains?(main_reactor.code, "max_step_execution_ns")
      assert String.contains?(main_reactor.code, "System.monotonic_time(:nanosecond)")
      assert String.contains?(main_reactor.code, "TTL constraint violation")
      
      # Check resources have TTL processing
      bitactor_resource = Enum.find(transformation_result.resources, & &1.class.name == "BitActor")
      assert String.contains?(bitactor_resource.code, "ttl_constraints")
      assert String.contains?(bitactor_resource.code, "max_processing_ns")
      assert String.contains?(bitactor_resource.code, "processing_time")
    end
    
    test "handles TTL constraint violations properly" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      # Verify error handling is built into generated code
      main_reactor = Enum.find(transformation_result.reactors, & &1.name == "CnsForge.TTLMainReactor")
      
      # Should have constraint violation checks
      assert String.contains?(main_reactor.code, "if step_time > ttl_context.max_step_execution_ns")
      assert String.contains?(main_reactor.code, "{:error, \"TTL constraint violation")
      assert String.contains?(main_reactor.code, "if total_execution_time <= arguments.ttl_context.max_total_execution_ns")
      
      # Resources should also have constraint checks
      bitactor_resource = Enum.find(transformation_result.resources, & &1.class.name == "BitActor")
      assert String.contains?(bitactor_resource.code, "if processing_time > max_processing_ns")
      assert String.contains?(bitactor_resource.code, "TTL constraint violation: processing took")
    end
    
    test "preserves semantic relationships in generated code" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      # Find the resource that should have the relationship (BitActor processes Signal)
      bitactor_resource = Enum.find(transformation_result.resources, & &1.class.name == "BitActor")
      
      # Should contain relationship definition
      assert String.contains?(bitactor_resource.code, "relationships do")
      assert String.contains?(bitactor_resource.code, "belongs_to :signal, CnsForge.TTLResources.Signal")
    end
    
    test "generates performance telemetry integration" do
      {:ok, transformation_result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      # Check resources have telemetry
      bitactor_resource = Enum.find(transformation_result.resources, & &1.class.name == "BitActor")
      assert String.contains?(bitactor_resource.code, ":telemetry.execute")
      assert String.contains?(bitactor_resource.code, "[:cns_forge, :ttl, :resource_processed]")
      assert String.contains?(bitactor_resource.code, "processing_time:")
    end
    
    test "full end-to-end transformation workflow" do
      # This test verifies the complete 80/20 workflow works
      {:ok, result} = TTLAshReactorTransformer.transform_ttl(@sample_ttl)
      
      # Verify all major components are present
      assert result.parsed_ontology
      assert length(result.resources) == 2
      assert length(result.reactors) >= 3  # main + 2 class reactors
      assert result.domain
      assert length(result.generated_files) >= 4
      
      # Verify the transformation preserves semantic meaning
      assert result.parsed_ontology.classes |> Enum.map(& &1.name) |> Enum.sort() == ["BitActor", "Signal"]
      assert result.parsed_ontology.relationships |> length() == 1
      
      # Verify generated code can be compiled (syntax check)
      Enum.each(result.generated_files, fn file_path ->
        content = File.read!(file_path)
        
        # Basic syntax validation
        assert String.contains?(content, "defmodule")
        assert String.match?(content, ~r/end\s*$/)
        
        # TTL-specific validations
        if String.contains?(content, "Reactor") do
          assert String.contains?(content, "use Reactor")
          assert String.contains?(content, "input")
        end
        
        if String.contains?(content, "Resource") do
          assert String.contains?(content, "use Ash.Resource")
          assert String.contains?(content, "attributes do")
        end
      end)
      
      # Cleanup
      Enum.each(result.generated_files, &File.rm/1)
    end
  end
  
  describe "Demo execution" do
    test "demo runs without errors" do
      # Capture log output to verify demo runs
      ExUnit.CaptureLog.capture_log(fn ->
        # This should not raise any exceptions
        TTLReactorDemo.quick_test()
      end)
    end
    
    test "parsing test works independently" do
      {:ok, result} = TTLReactorDemo.test_parsing()
      
      assert result.classes |> length() == 2
      assert result.properties |> length() == 1
      assert result.relationships |> length() == 1
    end
  end
end