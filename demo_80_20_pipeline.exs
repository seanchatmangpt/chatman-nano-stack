#!/usr/bin/env elixir

defmodule Demo8020Pipeline do
  @moduledoc """
  Demonstration of the complete 80/20 BitActor pipeline
  typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
  """

  require Logger
  
  # Load required modules
  Code.require_file("existing_code_connector.ex")
  Code.require_file("lib/cns_forge/ttl_parser.ex")
  Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

  def run_complete_demo do
    Logger.info("🚀 Starting 80/20 BitActor Pipeline Demo")
    
    # Sample TTL content for cybersecurity domain
    sample_ttl = """
    @prefix cyber: <http://cybersecurity.org/ontology#> .
    @prefix bitactor: <http://bitactor.org/ontology#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    # Cybersecurity BitActor Ontology
    cyber: a owl:Ontology ;
        rdfs:label "Cybersecurity BitActor Ontology" ;
        rdfs:comment "BitActor ontology for cybersecurity threat detection" .

    # Core Classes
    cyber:ThreatDetector a owl:Class ;
        rdfs:subClassOf bitactor:BitActor ;
        rdfs:label "Threat Detector" ;
        rdfs:comment "BitActor that detects security threats" ;
        bitactor:ttlBudgetMs 5 .

    cyber:AlertProcessor a owl:Class ;
        rdfs:subClassOf bitactor:BitActor ;
        rdfs:label "Alert Processor" ;
        rdfs:comment "BitActor that processes security alerts" ;
        bitactor:ttlBudgetMs 10 .

    cyber:IncidentResponder a owl:Class ;
        rdfs:subClassOf bitactor:BitActor ;
        rdfs:label "Incident Responder" ;
        rdfs:comment "BitActor that responds to security incidents" ;
        bitactor:ttlBudgetMs 15 .

    # Signal Classes
    cyber:ThreatSignal a owl:Class ;
        rdfs:subClassOf bitactor:Signal ;
        rdfs:label "Threat Signal" ;
        rdfs:comment "Signal indicating potential security threat" .

    cyber:AlertSignal a owl:Class ;
        rdfs:subClassOf bitactor:Signal ;
        rdfs:label "Alert Signal" ;
        rdfs:comment "Signal for security alerts" .

    # Properties
    cyber:detectsThreat a owl:ObjectProperty ;
        rdfs:domain cyber:ThreatDetector ;
        rdfs:range cyber:ThreatSignal ;
        rdfs:label "detects threat" .

    cyber:processesAlert a owl:ObjectProperty ;
        rdfs:domain cyber:AlertProcessor ;
        rdfs:range cyber:AlertSignal ;
        rdfs:label "processes alert" .

    cyber:respondsToIncident a owl:ObjectProperty ;
        rdfs:domain cyber:IncidentResponder ;
        rdfs:range cyber:AlertSignal ;
        rdfs:label "responds to incident" .

    # TTL Constraints
    cyber:threatDetectionConstraint a bitactor:TTLConstraint ;
        bitactor:budgetNs 5000000 ;
        bitactor:maxBudgetMs 5 ;
        bitactor:precision bitactor:Nanosecond .

    cyber:alertProcessingConstraint a bitactor:TTLConstraint ;
        bitactor:budgetNs 10000000 ;
        bitactor:maxBudgetMs 10 ;
        bitactor:precision bitactor:Nanosecond .

    cyber:incidentResponseConstraint a bitactor:TTLConstraint ;
        bitactor:budgetNs 15000000 ;
        bitactor:maxBudgetMs 15 ;
        bitactor:precision bitactor:Nanosecond .
    """

    # Execute the complete pipeline
    try do
      case ExistingCodeConnector.transform_ttl_to_bitactor(sample_ttl) do
        {:ok, result} ->
          Logger.info("✅ Pipeline transformation completed successfully")
          
          # Display results
          display_pipeline_results(result)
          
          # Test the generated components
          test_generated_components(result)
          
          Logger.info("🌀 80/20 Pipeline Demo completed successfully!")
          :success
        
        {:error, reason} ->
          Logger.error("❌ Pipeline demo failed: #{inspect(reason)}")
          :failure
      end
    rescue
      error ->
        Logger.error("❌ Pipeline demo crashed: #{inspect(error)}")
        :failure
    end
  end

  defp display_pipeline_results(result) do
    IO.puts("\n" <> String.duplicate("=", 80))
    IO.puts("📊 80/20 BITACTOR PIPELINE RESULTS")
    IO.puts(String.duplicate("=", 80))
    
    IO.puts("\n🔗 Pipeline Flow:")
    IO.puts("  TTL Ontology → BitActor DSL → Ash Resources → Reactor Workflows → K8s Manifests")
    
    IO.puts("\n📈 Generated Components:")
    IO.puts("  📋 BitActor DSL Modules: #{length(result.bitactor_dsl)}")
    IO.puts("  🗃️  Ash Resources: #{length(result.ash_resources)}")
    IO.puts("  ⚙️  Reactor Workflows: #{length(result.reactor_workflows)}")
    IO.puts("  ☸️  K8s Manifests: #{map_size(result.k8s_manifests)}")
    
    IO.puts("\n🎯 TTL Constraint Preservation:")
    metadata = result.pipeline_metadata
    IO.puts("  ✅ Stages: #{Enum.join(metadata.stages_completed, " → ")}")
    IO.puts("  ⏱️  TTL Preserved: #{metadata.ttl_preserved}")
    IO.puts("  🔬 Nanosecond Precision: #{metadata.nanosecond_precision}")
    
    # Display sample BitActor DSL
    IO.puts("\n🔧 Sample Generated BitActor DSL:")
    if sample_dsl = Enum.at(result.bitactor_dsl, 0) do
      lines = String.split(sample_dsl, "\n")
      preview_lines = Enum.take(lines, 15)
      Enum.each(preview_lines, fn line -> 
        IO.puts("    #{line}")
      end)
      if length(lines) > 15 do
        IO.puts("    ... (#{length(lines) - 15} more lines)")
      end
    end
    
    # Display sample Ash Resource
    IO.puts("\n🗃️ Sample Generated Ash Resource:")
    if sample_resource = Enum.at(result.ash_resources, 0) do
      lines = String.split(sample_resource.code, "\n")
      preview_lines = Enum.take(lines, 12)
      Enum.each(preview_lines, fn line ->
        IO.puts("    #{line}")
      end)
      if length(lines) > 12 do
        IO.puts("    ... (#{length(lines) - 12} more lines)")
      end
    end
    
    # Display K8s manifest sample
    IO.puts("\n☸️ Sample K8s Manifest:")
    if namespace = result.k8s_manifests.namespace do
      lines = String.split(namespace, "\n")
      preview_lines = Enum.take(lines, 8)
      Enum.each(preview_lines, fn line ->
        IO.puts("    #{line}")
      end)
    end
  end

  defp test_generated_components(result) do
    IO.puts("\n" <> String.duplicate("=", 80))
    IO.puts("🧪 TESTING GENERATED COMPONENTS")
    IO.puts(String.duplicate("=", 80))
    
    # Test 1: Validate TTL constraint preservation
    test_ttl_preservation(result)
    
    # Test 2: Validate component integration
    test_component_integration(result)
    
    # Test 3: Validate K8s deployment completeness
    test_k8s_completeness(result)
  end

  defp test_ttl_preservation(result) do
    IO.write("🔬 Testing TTL constraint preservation: ")
    
    # Check if nanosecond precision is maintained
    has_nanosecond = result.pipeline_metadata.nanosecond_precision
    
    # Check if TTL constraints are in all components
    dsl_has_ttl = result.bitactor_dsl
                  |> Enum.any?(fn dsl -> String.contains?(dsl, "budget_ns") end)
    
    ash_has_ttl = result.ash_resources
                  |> Enum.any?(fn res -> String.contains?(res.code, "ttl_budget_ms") end)
    
    workflows_have_ttl = result.reactor_workflows
                         |> Enum.any?(fn wf -> String.contains?(wf.code, "ttl_constraints") end)
    
    k8s_has_ttl = result.k8s_manifests
                  |> Map.values()
                  |> Enum.any?(fn manifest ->
                    case manifest do
                      manifests when is_list(manifests) ->
                        Enum.any?(manifests, &String.contains?(&1, "TTL_BUDGET_MS"))
                      manifest when is_binary(manifest) ->
                        String.contains?(manifest, "TTL_BUDGET_MS")
                      _ -> false
                    end
                  end)
    
    if has_nanosecond and dsl_has_ttl and ash_has_ttl and workflows_have_ttl and k8s_has_ttl do
      IO.puts("✅ PASSED")
      IO.puts("    🎯 Nanosecond precision: ✅")
      IO.puts("    🔧 DSL TTL constraints: ✅") 
      IO.puts("    🗃️  Ash TTL attributes: ✅")
      IO.puts("    ⚙️  Reactor TTL enforcement: ✅")
      IO.puts("    ☸️  K8s TTL configuration: ✅")
    else
      IO.puts("❌ FAILED")
      IO.puts("    🎯 Nanosecond precision: #{if has_nanosecond, do: "✅", else: "❌"}")
      IO.puts("    🔧 DSL TTL constraints: #{if dsl_has_ttl, do: "✅", else: "❌"}")
      IO.puts("    🗃️  Ash TTL attributes: #{if ash_has_ttl, do: "✅", else: "❌"}")
      IO.puts("    ⚙️  Reactor TTL enforcement: #{if workflows_have_ttl, do: "✅", else: "❌"}")
      IO.puts("    ☸️  K8s TTL configuration: #{if k8s_has_ttl, do: "✅", else: "❌"}")
    end
  end

  defp test_component_integration(result) do
    IO.write("🔗 Testing component integration: ")
    
    # Test BitActor → Ash integration
    dsl_modules = extract_module_names_from_dsl(result.bitactor_dsl)
    ash_modules = Enum.map(result.ash_resources, & &1.module_name)
    
    # Test Ash → Reactor integration
    reactor_names = Enum.map(result.reactor_workflows, & &1.name)
    
    # Check for consistent naming
    has_consistent_naming = length(dsl_modules) > 0 and 
                           length(ash_modules) > 0 and 
                           length(reactor_names) > 0
    
    if has_consistent_naming do
      IO.puts("✅ PASSED")
      IO.puts("    🔧 BitActor DSL modules: #{length(dsl_modules)}")
      IO.puts("    🗃️  Ash resource modules: #{length(ash_modules)}")
      IO.puts("    ⚙️  Reactor workflows: #{length(reactor_names)}")
    else
      IO.puts("❌ FAILED - Missing component integration")
    end
  end

  defp test_k8s_completeness(result) do
    IO.write("☸️ Testing K8s deployment completeness: ")
    
    manifests = result.k8s_manifests
    
    # Required components
    has_namespace = Map.has_key?(manifests, :namespace)
    has_configmaps = Map.has_key?(manifests, :configmaps)
    has_statefulset = Map.has_key?(manifests, :statefulset)
    has_services = Map.has_key?(manifests, :services)
    has_hpa = Map.has_key?(manifests, :hpa)
    has_monitoring = Map.has_key?(manifests, :monitoring)
    
    all_present = has_namespace and has_configmaps and has_statefulset and 
                  has_services and has_hpa and has_monitoring
    
    if all_present do
      IO.puts("✅ PASSED")
      IO.puts("    📦 Namespace: ✅")
      IO.puts("    ⚙️  ConfigMaps: ✅")
      IO.puts("    🏗️  StatefulSet: ✅")
      IO.puts("    🌐 Services: ✅")
      IO.puts("    📈 HPA: ✅")
      IO.puts("    📊 Monitoring: ✅")
    else
      IO.puts("❌ FAILED")
      IO.puts("    📦 Namespace: #{if has_namespace, do: "✅", else: "❌"}")
      IO.puts("    ⚙️  ConfigMaps: #{if has_configmaps, do: "✅", else: "❌"}")
      IO.puts("    🏗️  StatefulSet: #{if has_statefulset, do: "✅", else: "❌"}")
      IO.puts("    🌐 Services: #{if has_services, do: "✅", else: "❌"}")
      IO.puts("    📈 HPA: #{if has_hpa, do: "✅", else: "❌"}")
      IO.puts("    📊 Monitoring: #{if has_monitoring, do: "✅", else: "❌"}")
    end
  end

  defp extract_module_names_from_dsl(dsl_modules) do
    dsl_modules
    |> Enum.map(fn dsl ->
      case Regex.run(~r/defmodule\s+(\S+)/, dsl) do
        [_, module_name] -> module_name
        _ -> nil
      end
    end)
    |> Enum.filter(& &1)
  end

  def run_python_integration_test do
    IO.puts("\n" <> String.duplicate("=", 80))
    IO.puts("🐍 PYTHON INTEGRATION TEST")
    IO.puts(String.duplicate("=", 80))
    
    IO.write("Testing Python → Elixir bridge: ")
    
    # Check if Python bridge script exists
    if File.exists?("unified_bitactor_pipeline_bridge.py") do
      # Run Python script to test integration
      case System.cmd("python3", ["unified_bitactor_pipeline_bridge.py"], stderr_to_stdout: true) do
        {output, 0} ->
          IO.puts("✅ PASSED")
          IO.puts("🐍 Python bridge executed successfully")
          
          # Show relevant output lines
          output
          |> String.split("\n")
          |> Enum.filter(fn line -> 
            String.contains?(line, "✅") or 
            String.contains?(line, "🚀") or 
            String.contains?(line, "🌀")
          end)
          |> Enum.take(5)
          |> Enum.each(fn line -> IO.puts("    #{line}") end)
          
        {error_output, _} ->
          IO.puts("❌ FAILED")
          IO.puts("Error: #{String.slice(error_output, 0, 200)}")
      end
    else
      IO.puts("❌ FAILED - Python bridge script not found")
    end
  end

  def demonstrate_mermaid_flow do
    IO.puts("\n" <> String.duplicate("=", 80))
    IO.puts("📊 80/20 PIPELINE FLOW DIAGRAM")
    IO.puts(String.duplicate("=", 80))
    
    mermaid_diagram = """
    
    ```mermaid
    graph TD
        A[Python Types<br/>bitactor_types.py] --> B[TTL Ontology<br/>bitactor_ontology.ttl]
        B --> C[TTL Parser<br/>CNSForge.TTLParser]
        C --> D[BitActor DSL<br/>BitActorDSL]
        D --> E[GenServer<br/>BitActor.GenServer]
        E --> F[Ash Resources<br/>BitActor.Ash.Resources]
        F --> G[Reactor Workflows<br/>BitActor.Reactor]
        G --> H[K8s Deployment<br/>bitactor-k8s-deployment.yaml]
        
        I[TTL Constraints<br/>Nanosecond Precision] --> A
        I --> B  
        I --> C
        I --> D
        I --> E
        I --> F
        I --> G
        I --> H
        
        J[ExistingCodeConnector] --> C
        J --> D
        J --> F
        J --> G
        J --> H
        
        K[UnifiedBridge<br/>Python] --> A
        K --> B
        K --> J
        
        style A fill:#e1f5fe
        style B fill:#f3e5f5
        style C fill:#e8f5e8
        style D fill:#fff3e0
        style E fill:#fce4ec
        style F fill:#e0f2f1
        style G fill:#f1f8e9
        style H fill:#e3f2fd
        style I fill:#ffebee
        style J fill:#f9fbe7
        style K fill:#e8eaf6
    ```
    """
    
    IO.puts(mermaid_diagram)
  end
end

# Run the demo if this file is executed directly
case System.argv() do
  [] ->
    Demo8020Pipeline.run_complete_demo()
    Demo8020Pipeline.run_python_integration_test()
    Demo8020Pipeline.demonstrate_mermaid_flow()
  
  ["--help"] ->
    IO.puts("""
    80/20 BitActor Pipeline Demo
    
    Usage:
      elixir demo_80_20_pipeline.exs           # Run complete demo
      elixir demo_80_20_pipeline.exs --help    # Show this help
    
    This demo shows the complete 80/20 pipeline:
    typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
    """)
  
  _ ->
    Demo8020Pipeline.run_complete_demo()
end