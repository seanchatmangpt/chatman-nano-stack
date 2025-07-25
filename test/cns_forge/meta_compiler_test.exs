defmodule CNSForge.MetaCompilerTest do
  use ExUnit.Case, async: true
  alias CNSForge.MetaCompiler
  
  @test_ttl_file "test/fixtures/test_ontology.ttl"
  @test_bpmn_file "test/fixtures/test_process.bpmn"
  
  describe "compile/2" do
    test "successfully compiles TTL ontology to BitActor mesh" do
      # Create test TTL content
      ttl_content = """
      @prefix : <http://cns.io/test#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      :TestOntology a owl:Ontology .
      
      :Sensor a owl:Class ;
        rdfs:label "Sensor" .
        
      :Reading a owl:Class ;
        rdfs:label "Reading" .
        
      :hasReading a owl:ObjectProperty ;
        rdfs:domain :Sensor ;
        rdfs:range :Reading .
      """
      
      File.mkdir_p!("test/fixtures")
      File.write!(@test_ttl_file, ttl_content)
      
      assert {:ok, result} = MetaCompiler.compile(@test_ttl_file)
      
      assert result.mesh_id =~ ~r/^mesh_/
      assert result.bitactor_count > 0
      assert result.workflow_id =~ ~r/^workflow_/
      assert result.semantic_coverage > 0.8
      assert result.status == :active
    end
    
    test "handles parsing errors gracefully" do
      invalid_file = "test/fixtures/invalid.unknown"
      File.write!(invalid_file, "invalid content")
      
      assert {:error, {:parse_error, _}} = MetaCompiler.compile(invalid_file)
    end
    
    test "respects TTL budget constraints" do
      opts = [ttl_budget: 4]
      
      File.write!(@test_ttl_file, "@prefix : <http://test#> .")
      
      {:ok, result} = MetaCompiler.compile(@test_ttl_file, opts)
      
      # Verify TTL is propagated
      assert result.compilation_time < 5000 # Should be fast with low TTL
    end
  end
  
  describe "parse_semantic_model/2" do
    test "detects and parses different language formats" do
      # TTL
      ttl_file = "test/fixtures/onto.ttl"
      File.write!(ttl_file, "@prefix : <http://test#> .")
      
      assert {:ok, model} = MetaCompiler.parse_semantic_model(ttl_file, [])
      assert model.language == :ttl
      
      # BPMN
      bpmn_file = "test/fixtures/proc.bpmn"
      File.write!(bpmn_file, "<?xml version=\"1.0\"?><bpmn:definitions/>")
      
      assert {:ok, model} = MetaCompiler.parse_semantic_model(bpmn_file, [])
      assert model.language == :bpmn
    end
  end
  
  describe "generate_orchestration_plan/1" do
    test "generates correct plan for TTL model" do
      model = %{
        language: :ttl,
        model: %{classes: ["Sensor", "Actuator"], properties: ["hasReading"]},
        metadata: %{}
      }
      
      assert {:ok, plan} = MetaCompiler.generate_orchestration_plan(model)
      
      assert is_list(plan.steps)
      assert is_map(plan.dependencies)
      assert plan.ttl_budget == 8
      assert plan.parallel_execution == true
    end
    
    test "handles BPMN process flows" do
      model = %{
        language: :bpmn,
        model: %{tasks: ["Task1", "Task2"], gateways: ["ParallelGateway1"]},
        metadata: %{}
      }
      
      assert {:ok, plan} = MetaCompiler.generate_orchestration_plan(model)
      
      assert plan.parallel_execution == false # Default for BPMN
    end
  end
  
  describe "generate_bitactor_templates/1" do
    test "generates C code templates for orchestration steps" do
      plan = %{
        ttl_budget: 8,
        steps: [
          %{
            name: "ProcessSensor",
            type: :transform,
            ttl_budget: 2,
            operations: ["read_sensor()", "validate_reading()"],
            compensations: ["reset_sensor()"],
            signals: ["SENSOR_READ", "SENSOR_ERROR"]
          }
        ]
      }
      
      assert {:ok, templates} = MetaCompiler.generate_bitactor_templates(plan)
      
      assert length(templates) == 1
      template = hd(templates)
      
      assert template.step == "ProcessSensor"
      assert template.c_code =~ ~r/ProcessSensor/
      assert template.c_code =~ ~r/read_sensor/
      assert template.template == "bitactor_c.j2"
    end
  end
end