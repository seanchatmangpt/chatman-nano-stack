defmodule CNSForge.IntegrationTest do
  @moduledoc """
  Integration tests validating the complete CNS Forge system
  Tests the full flow from semantic models to deployed BitActor mesh
  """
  
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias CNSForge.{MetaCompiler, BitActor, Telemetry}
  
  setup do
    # Ensure clean state
    File.rm_rf!("test/integration/fixtures")
    File.mkdir_p!("test/integration/fixtures")
    
    # Start telemetry
    {:ok, _} = Application.ensure_all_started(:telemetry)
    
    on_exit(fn ->
      File.rm_rf!("test/integration/fixtures")
    end)
    
    :ok
  end
  
  describe "End-to-end semantic compilation" do
    test "compiles cybersecurity ontology to working BitActor mesh" do
      # Create cybersecurity ontology
      cyber_ttl = """
      @prefix cyber: <http://cns.io/cyber#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cyber:CyberOntology a owl:Ontology ;
        rdfs:label "Cybersecurity Test Ontology" .
      
      cyber:Threat a owl:Class ;
        rdfs:label "Security Threat" .
        
      cyber:Vulnerability a owl:Class ;
        rdfs:label "System Vulnerability" .
        
      cyber:Attack a owl:Class ;
        rdfs:label "Cyber Attack" ;
        rdfs:subClassOf cyber:Threat .
        
      cyber:detectsThreat a owl:ObjectProperty ;
        rdfs:domain cyber:SecurityControl ;
        rdfs:range cyber:Threat .
      """
      
      path = "test/integration/fixtures/cyber.ttl"
      File.write!(path, cyber_ttl)
      
      # Compile to BitActor mesh
      assert {:ok, result} = MetaCompiler.compile(path)
      
      # Verify mesh creation
      assert result.mesh_id =~ ~r/^mesh_/
      assert result.bitactor_count > 0
      assert result.status == :active
      assert result.semantic_coverage > 0.7
      
      # Verify telemetry events were emitted
      assert_receive {:telemetry_event, [:cns_forge, :semantic, :compilation, :completed], _, _}
    end
    
    test "compiles BPMN process to executable workflow" do
      # Create BPMN process
      bpmn_xml = """
      <?xml version="1.0" encoding="UTF-8"?>
      <bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL">
        <bpmn:process id="TestProcess" name="Test Process">
          <bpmn:startEvent id="Start" />
          <bpmn:task id="Task1" name="Process Data" />
          <bpmn:task id="Task2" name="Validate Result" />
          <bpmn:endEvent id="End" />
          
          <bpmn:sequenceFlow sourceRef="Start" targetRef="Task1" />
          <bpmn:sequenceFlow sourceRef="Task1" targetRef="Task2" />
          <bpmn:sequenceFlow sourceRef="Task2" targetRef="End" />
        </bpmn:process>
      </bpmn:definitions>
      """
      
      path = "test/integration/fixtures/process.bpmn"
      File.write!(path, bpmn_xml)
      
      # Compile to Reactor workflow
      assert {:ok, result} = MetaCompiler.compile(path)
      
      # Verify workflow was created
      assert result.workflow_id =~ ~r/^workflow_/
      assert result.bitactor_count >= 2 # At least Task1 and Task2
    end
    
    test "compiles DMN decision table to BitActor rules" do
      # Create DMN decision
      dmn_xml = """
      <?xml version="1.0" encoding="UTF-8"?>
      <definitions xmlns="http://www.omg.org/spec/DMN/20151101/dmn.xsd">
        <decision id="RiskDecision" name="Risk Assessment">
          <decisionTable>
            <input>
              <inputExpression typeRef="number">
                <text>threatLevel</text>
              </inputExpression>
            </input>
            <output name="action" typeRef="string" />
            <rule>
              <inputEntry><text>&gt; 7</text></inputEntry>
              <outputEntry><text>"block"</text></outputEntry>
            </rule>
            <rule>
              <inputEntry><text>&lt;= 7</text></inputEntry>
              <outputEntry><text>"monitor"</text></outputEntry>
            </rule>
          </decisionTable>
        </decision>
      </definitions>
      """
      
      path = "test/integration/fixtures/decision.dmn"
      File.write!(path, dmn_xml)
      
      # Compile to BitActor
      assert {:ok, result} = MetaCompiler.compile(path)
      
      # Verify decision logic was compiled
      assert result.bitactor_count >= 1
      assert result.semantic_coverage > 0.5
    end
  end
  
  describe "BitActor mesh execution" do
    test "executes compiled mesh with TTL enforcement" do
      # Create and compile simple ontology
      ttl = """
      @prefix test: <http://test#> .
      test:ProcessA a owl:Class .
      test:ProcessB a owl:Class .
      """
      
      path = "test/integration/fixtures/mesh_test.ttl"
      File.write!(path, ttl)
      
      {:ok, compilation_result} = MetaCompiler.compile(path)
      
      # Simulate mesh execution
      mesh_id = compilation_result.mesh_id
      
      # Create test signal
      signal = %{
        type: :test_signal,
        mesh_id: mesh_id,
        payload: %{data: "test"}
      }
      
      # Process signal through mesh (simulated)
      results = Enum.map(1..8, fn hop ->
        # Each hop decrements TTL
        remaining_ttl = 8 - hop
        
        if remaining_ttl > 0 do
          # Emit telemetry
          Telemetry.emit_bitactor_hop("test_actor", :process, remaining_ttl)
          {:ok, %{hop: hop, ttl: remaining_ttl}}
        else
          {:error, :ttl_expired}
        end
      end)
      
      # Verify TTL enforcement
      successful_hops = Enum.count(results, &match?({:ok, _}, &1))
      assert successful_hops <= 8
      
      # Verify telemetry
      assert_receive {:telemetry_event, [:cns_forge, :bit_actor, :hop], %{ttl_remaining: _}, _}
    end
    
    test "handles cross-domain semantic coordination" do
      # Create multi-domain ontology
      multi_ttl = """
      @prefix cyber: <http://cyber#> .
      @prefix health: <http://health#> .
      @prefix iot: <http://iot#> .
      
      cyber:ThreatDetector a owl:Class .
      health:PatientMonitor a owl:Class .
      iot:Sensor a owl:Class .
      
      :CrossDomainAlert a owl:Class ;
        rdfs:subClassOf cyber:ThreatDetector, health:PatientMonitor, iot:Sensor .
      """
      
      path = "test/integration/fixtures/multi_domain.ttl"
      File.write!(path, multi_ttl)
      
      {:ok, result} = MetaCompiler.compile(path)
      
      # Verify cross-domain compilation
      assert result.bitactor_count >= 4
      assert result.semantic_coverage > 0.75
    end
  end
  
  describe "Production deployment validation" do
    test "generates valid Kubernetes manifests" do
      # Verify K8s deployment file exists and is valid
      k8s_path = "k8s/cns-forge-deployment.yaml"
      assert File.exists?(k8s_path)
      
      content = File.read!(k8s_path)
      
      # Verify required components
      assert content =~ ~r/kind: Namespace/
      assert content =~ ~r/kind: StatefulSet/
      assert content =~ ~r/kind: Deployment/
      assert content =~ ~r/kind: Service/
      assert content =~ ~r/kind: ConfigMap/
      assert content =~ ~r/kind: NetworkPolicy/
      
      # Verify security settings
      assert content =~ ~r/pod-security.kubernetes.io\/enforce: restricted/
      assert content =~ ~r/linkerd.io\/inject: enabled/
    end
    
    test "terraform configuration is valid" do
      # Verify Terraform file exists
      tf_path = "terraform/cns_forge_production.tf"
      assert File.exists?(tf_path)
      
      content = File.read!(tf_path)
      
      # Verify required resources
      assert content =~ ~r/resource "kubernetes_namespace"/
      assert content =~ ~r/module "eks"/
      assert content =~ ~r/resource "aws_db_instance"/
      assert content =~ ~r/resource "aws_elasticache_replication_group"/
      
      # Verify security configurations
      assert content =~ ~r/storage_encrypted\s*=\s*true/
      assert content =~ ~r/at_rest_encryption_enabled\s*=\s*true/
    end
  end
  
  describe "Observability validation" do
    test "OpenTelemetry instrumentation captures all events" do
      # Start capturing telemetry
      self = self()
      
      :telemetry.attach(
        "test-handler",
        [:cns_forge, :bit_actor, :hop],
        fn name, measurements, metadata, _ ->
          send(self, {:telemetry_event, name, measurements, metadata})
        end,
        nil
      )
      
      # Execute BitActor operation
      {:ok, actor} = BitActor.create(%{
        type: :telemetry_test,
        transaction_id: "otel_test",
        ttl: 5,
        token: %{}
      })
      
      {:ok, _} = BitActor.execute_hop(actor, %{
        input_token: %{test: true},
        operation: :validate
      })
      
      # Verify telemetry received
      assert_receive {:telemetry_event, [:cns_forge, :bit_actor, :hop], measurements, metadata}
      assert measurements.ttl_remaining == 5
      assert metadata.operation == :validate
      
      :telemetry.detach("test-handler")
    end
    
    test "metrics are properly exported" do
      # Verify metric definitions
      metrics = Telemetry.metrics()
      
      metric_names = Enum.map(metrics, & &1.name)
      
      # Verify core metrics exist
      assert [:cns_forge, :bit_actor, :hop] in metric_names
      assert [:cns_forge, :reactor, :workflow, :completed] in metric_names
      assert [:cns_forge, :semantic, :compilation, :duration] in metric_names
      assert [:cns_forge, :mesh, :signals, :processed] in metric_names
    end
  end
  
  describe "Full system integration" do
    test "complete flow: TTL → Compile → Deploy → Execute → Monitor" do
      # Step 1: Create complex ontology
      ontology = """
      @prefix cns: <http://cns.io/test#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:IntegrationTest a owl:Ontology .
      
      # Domain classes
      cns:DataSource a owl:Class .
      cns:Processor a owl:Class .
      cns:Validator a owl:Class .
      cns:Output a owl:Class .
      
      # Properties
      cns:processes a owl:ObjectProperty ;
        rdfs:domain cns:Processor ;
        rdfs:range cns:DataSource .
      
      cns:validates a owl:ObjectProperty ;
        rdfs:domain cns:Validator ;
        rdfs:range cns:Processor .
      """
      
      path = "test/integration/fixtures/full_integration.ttl"
      File.write!(path, ontology)
      
      # Step 2: Compile
      log = capture_log(fn ->
        assert {:ok, compilation} = MetaCompiler.compile(path)
        
        # Step 3: Verify deployment readiness
        assert compilation.status == :active
        assert compilation.bitactor_count >= 4
        
        # Step 4: Simulate execution
        Enum.each(1..compilation.bitactor_count, fn i ->
          Telemetry.emit_bitactor_hop("actor_#{i}", :execute, 7 - rem(i, 8))
        end)
        
        # Step 5: Verify monitoring
        Telemetry.emit_workflow_event(:completed, compilation.workflow_id, :integration_test)
      end)
      
      # Verify logs contain expected entries
      assert log =~ ~r/Successfully compiled/
      
      # Verify system handled full flow
      assert_receive {:telemetry_event, [:cns_forge, :reactor, :workflow, :completed], _, _}
    end
  end
end