defmodule CNSForge.MetacompilerTest do
  use ExUnit.Case, async: true
  
  alias CNSForge.Metacompiler
  
  describe "semantic parsing" do
    test "parses TTL ontology successfully" do
      ttl_spec = %{
        language: :ttl,
        content: """
        @prefix cns: <http://cns.io/ontology#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        
        cns:BitActor a rdfs:Class ;
          rdfs:label "BitActor" ;
          rdfs:comment "Temporal computation unit" .
        
        cns:hasToken a rdfs:Property ;
          rdfs:domain cns:BitActor ;
          rdfs:range cns:Token .
        """
      }
      
      assert {:ok, parsed} = Metacompiler.parse_semantic_spec(ttl_spec)
      assert parsed.language == :ttl
      assert is_list(parsed.classes)
      assert is_list(parsed.properties)
    end
    
    test "parses BPMN process successfully" do
      bpmn_spec = %{
        language: :bpmn,
        content: """
        <process id="trading_process">
          <startEvent id="start" />
          <task id="validate_order" name="Validate Order" />
          <task id="execute_trade" name="Execute Trade" />
          <endEvent id="end" />
        </process>
        """
      }
      
      assert {:ok, parsed} = Metacompiler.parse_semantic_spec(bpmn_spec)
      assert parsed.language == :bpmn
      assert is_list(parsed.tasks)
    end
  end
  
  describe "IR generation" do
    test "generates IR from parsed TTL" do
      parsed = %{
        language: :ttl,
        classes: [
          %{uri: "http://cns.io/BitActor", label: "BitActor", properties: []}
        ],
        properties: [
          %{uri: "http://cns.io/hasToken", label: "hasToken", type: :object_property,
           domain: "http://cns.io/BitActor", range: "http://cns.io/Token"}
        ],
        rules: []
      }
      
      assert {:ok, ir} = Metacompiler.generate_intermediate_representation(parsed)
      assert length(ir.nodes) > 0
      assert length(ir.edges) > 0
    end
    
    test "applies TTL constraints to IR" do
      parsed = %{
        language: :ttl,
        classes: [],
        properties: [],
        rules: [
          %{subject: "BitActor", ttl_constraint: 5, priority: :high}
        ]
      }
      
      assert {:ok, ir} = Metacompiler.generate_intermediate_representation(parsed)
      assert length(ir.ttl_constraints) == 1
      assert hd(ir.ttl_constraints).max_hops == 5
    end
  end
  
  describe "IR optimization" do
    test "optimizes IR for parallel execution" do
      ir = %{
        nodes: [
          %{id: "a", ttl_budget: 8},
          %{id: "b", ttl_budget: 4},
          %{id: "c", ttl_budget: 8}
        ],
        edges: [],
        metadata: %{},
        ttl_constraints: [],
        saga_boundaries: []
      }
      
      assert {:ok, optimized} = Metacompiler.optimize_ir(ir, [])
      assert Map.has_key?(optimized.metadata, :parallel_groups)
    end
    
    test "applies target-specific optimizations" do
      ir = %{
        nodes: [],
        edges: [],
        metadata: %{},
        ttl_constraints: [],
        saga_boundaries: []
      }
      
      assert {:ok, optimized} = Metacompiler.optimize_ir(ir, targets: [:c_bitactor])
      assert optimized != nil
    end
  end
  
  describe "target generation" do
    test "generates Elixir Reactor code" do
      ir = %{
        nodes: [
          %{id: "process", label: "process_signal", type: :transform, ttl_budget: 8}
        ],
        edges: [],
        metadata: %{},
        ttl_constraints: [],
        saga_boundaries: []
      }
      
      assert {:ok, targets} = Metacompiler.generate_targets(ir, targets: [:elixir_reactor])
      assert Map.has_key?(targets, :elixir_reactor)
    end
    
    test "generates C BitActor code" do
      ir = %{
        nodes: [
          %{id: "actor", label: "bit_actor", type: :class, ttl_budget: 8}
        ],
        edges: [],
        metadata: %{name: "test"},
        ttl_constraints: [],
        saga_boundaries: []
      }
      
      assert {:ok, targets} = Metacompiler.generate_targets(ir, targets: [:c_bitactor])
      assert Map.has_key?(targets, :c_bitactor)
      
      # Verify C code structure
      c_code = targets[:c_bitactor]
      assert String.contains?(c_code, "#ifndef")
      assert String.contains?(c_code, "typedef struct")
    end
    
    test "generates Kubernetes manifests" do
      ir = %{
        nodes: [],
        edges: [],
        metadata: %{name: "test-app"},
        ttl_constraints: [],
        saga_boundaries: []
      }
      
      assert {:ok, targets} = Metacompiler.generate_targets(ir, targets: [:kubernetes])
      assert Map.has_key?(targets, :kubernetes)
      
      k8s_manifests = targets[:kubernetes]
      assert String.contains?(k8s_manifests.combined, "apiVersion:")
      assert String.contains?(k8s_manifests.combined, "kind: Deployment")
    end
  end
  
  describe "end-to-end compilation" do
    test "compiles TTL to multiple targets" do
      semantic_spec = %{
        language: :ttl,
        content: """
        @prefix cns: <http://cns.io/ontology#> .
        cns:TradingSystem a rdfs:Class .
        """
      }
      
      opts = [
        targets: [:elixir_reactor, :c_bitactor],
        name: "trading_system"
      ]
      
      assert {:ok, result} = Metacompiler.compile(semantic_spec, opts)
      assert result.language == :ttl
      assert map_size(result.targets) == 2
      assert result.metadata.node_count >= 0
      assert result.observability.metrics != nil
    end
    
    test "handles compilation errors gracefully" do
      invalid_spec = %{
        language: :unknown,
        content: "invalid content"
      }
      
      assert {:error, _} = Metacompiler.compile(invalid_spec)
    end
  end
  
  describe "telemetry integration" do
    setup do
      # Attach telemetry handler for testing
      handler_id = "test_#{System.unique_integer()}"
      
      :telemetry.attach(
        handler_id,
        [:cns_forge, :semantic, :compilation, :completed],
        fn _event_name, measurements, metadata, _config ->
          send(self(), {:compilation_completed, measurements, metadata})
        end,
        nil
      )
      
      on_exit(fn -> :telemetry.detach(handler_id) end)
      
      :ok
    end
    
    test "emits telemetry events during compilation" do
      semantic_spec = %{
        language: :ttl,
        content: "@prefix cns: <http://cns.io#> ."
      }
      
      {:ok, _} = Metacompiler.compile(semantic_spec)
      
      assert_receive {:compilation_completed, measurements, metadata}, 5000
      assert measurements.duration > 0
      assert metadata.language == :ttl
      assert is_list(metadata.targets)
    end
  end
end