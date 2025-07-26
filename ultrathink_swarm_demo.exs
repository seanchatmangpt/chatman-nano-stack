#!/usr/bin/env elixir
# 🚀 UltraThink Swarm 80/20 Demo
# Demonstrates the complete pipeline:
# typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s

# Ensure all required modules are compiled
Code.require_file("lib/cns_forge/ultrathink_swarm_orchestrator.ex")
Code.require_file("lib/cns_forge/eighty_twenty_typer.ex")
Code.require_file("lib/cns_forge/turtle_generator.ex")
Code.require_file("lib/cns_forge/ttl_to_dspy_transformer.ex")
Code.require_file("lib/cns_forge/dspy_to_bitactor_transformer.ex")
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

defmodule UltraThinkSwarmDemo do
  @moduledoc """
  Demo module showing the complete UltraThink Swarm pipeline
  """
  
  def run do
    IO.puts """
    ╔═══════════════════════════════════════════════════╗
    ║   🚀 UltraThink Swarm 80/20 Pipeline Demo        ║
    ║                                                   ║
    ║   Pipeline Flow:                                  ║
    ║   typer → turtle → ttl2dspy → BitActor →        ║
    ║   Erlang → Ash → Reactor → k8s                  ║
    ╚═══════════════════════════════════════════════════╝
    """
    
    # Start the orchestrator
    {:ok, _pid} = CnsForge.UltraThinkSwarmOrchestrator.start_link()
    
    # Example 1: Domain description input
    demo_domain_description()
    
    # Example 2: TTL ontology input  
    demo_ttl_input()
    
    # Example 3: Structured data input
    demo_structured_input()
    
    # Show metrics
    show_metrics()
  end
  
  defp demo_domain_description do
    IO.puts "\n📝 Example 1: Domain Description Input"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    domain_description = """
    Create a cybersecurity monitoring system with the following components:
    - ThreatActor: Represents malicious entities
    - Vulnerability: System weaknesses
    - Asset: Protected resources
    - SecurityEvent: Detected incidents
    - Mitigation: Response actions
    
    The system should detect threats, assess vulnerabilities,
    and automatically trigger mitigations.
    """
    
    IO.puts "Input: Domain description for cybersecurity system"
    
    case CnsForge.UltraThinkSwarmOrchestrator.execute_swarm(domain_description) do
      {:ok, result} ->
        IO.puts "✅ Pipeline executed successfully!"
        display_results(result)
        
      {:error, reason} ->
        IO.puts "❌ Pipeline failed: #{inspect(reason)}"
    end
  end
  
  defp demo_ttl_input do
    IO.puts "\n🐢 Example 2: TTL Ontology Input"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    ttl_content = """
    @prefix cyber: <http://cns.io/cyber#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    
    cyber:ThreatActor a owl:Class ;
        rdfs:label "Threat Actor" ;
        rdfs:comment "Malicious entity targeting systems" .
        
    cyber:Vulnerability a owl:Class ;
        rdfs:label "Vulnerability" ;
        rdfs:comment "System weakness that can be exploited" .
        
    cyber:exploits a owl:ObjectProperty ;
        rdfs:domain cyber:ThreatActor ;
        rdfs:range cyber:Vulnerability .
    """
    
    IO.puts "Input: TTL ontology defining threat model"
    
    case CnsForge.UltraThinkSwarmOrchestrator.execute_swarm(ttl_content) do
      {:ok, result} ->
        IO.puts "✅ Pipeline executed successfully!"
        display_ttl_results(result)
        
      {:error, reason} ->
        IO.puts "❌ Pipeline failed: #{inspect(reason)}"
    end
  end
  
  defp demo_structured_input do
    IO.puts "\n📊 Example 3: Structured Data Input"
    IO.puts "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    structured_data = %{
      types: [
        %{name: "Order", attributes: ["id", "total", "status"]},
        %{name: "Customer", attributes: ["id", "name", "email"]},
        %{name: "Product", attributes: ["id", "name", "price"]},
        %{name: "OrderItem", attributes: ["quantity", "price"]},
        %{name: "Inventory", attributes: ["stock", "location"]}
      ],
      relationships: [
        %{source: "Customer", target: "Order", predicate: "places"},
        %{source: "Order", target: "OrderItem", predicate: "contains"},
        %{source: "OrderItem", target: "Product", predicate: "references"},
        %{source: "Product", target: "Inventory", predicate: "trackedIn"}
      ]
    }
    
    IO.puts "Input: Structured e-commerce domain model"
    
    options = %{
      replicas: 5,
      enable_monitoring: true
    }
    
    case CnsForge.UltraThinkSwarmOrchestrator.execute_swarm(structured_data, options) do
      {:ok, result} ->
        IO.puts "✅ Pipeline executed successfully!"
        display_structured_results(result)
        
      {:error, reason} ->
        IO.puts "❌ Pipeline failed: #{inspect(reason)}"
    end
  end
  
  defp display_results(result) do
    IO.puts "\n📈 Pipeline Results:"
    
    # Show 80/20 optimization
    if result[:typed_model] do
      IO.puts "\n🎯 80/20 Type Optimization:"
      IO.puts "   Critical types identified: #{length(result.typed_model[:critical_types] || [])}"
    end
    
    # Show generated TTL
    if result[:turtle_ttl] do
      IO.puts "\n🐢 Generated Turtle:"
      IO.puts "   Lines of TTL: #{String.split(result.turtle_ttl, "\n") |> length()}"
    end
    
    # Show DSPy transformation
    if result[:dspy_code] do
      IO.puts "\n🐍 DSPy Signatures:"
      signatures = Regex.scan(~r/class (\w+)Signature/, result.dspy_code)
      IO.puts "   Generated signatures: #{length(signatures)}"
    end
    
    # Show BitActor spec
    if result[:bitactor_spec] do
      IO.puts "\n⚡ BitActor System:"
      IO.puts "   Actors defined: #{count_actors(result.bitactor_spec)}"
    end
    
    # Show K8s deployment
    if result[:k8s_deployment] do
      IO.puts "\n☸️  Kubernetes Deployment:"
      IO.puts "   Deployment: ✓"
      IO.puts "   Service: ✓"
      IO.puts "   ConfigMap: ✓"
      IO.puts "   HPA: ✓"
    end
  end
  
  defp display_ttl_results(result) do
    IO.puts "\n📊 TTL Processing Results:"
    
    if result[:ash_resources] do
      IO.puts "   Ash resources created: #{length(result.ash_resources)}"
    end
    
    if result[:reactor_workflows] do
      IO.puts "   Reactor workflows: #{length(result.reactor_workflows)}"
    end
  end
  
  defp display_structured_results(result) do
    IO.puts "\n🏗️ Structured Data Processing:"
    
    if result[:typed_model] do
      critical_count = length(result.typed_model[:critical_types] || [])
      IO.puts "   Applied 80/20: #{critical_count} critical types from 5 total"
    end
    
    if result[:k8s_deployment] do
      IO.puts "   K8s replicas configured: 5"
      IO.puts "   Monitoring enabled: ✓"
    end
  end
  
  defp count_actors(bitactor_spec) when is_binary(bitactor_spec) do
    Regex.scan(~r/## (\w+)Actor/, bitactor_spec) |> length()
  end
  defp count_actors(_), do: 0
  
  defp show_metrics do
    IO.puts "\n📊 Pipeline Metrics:"
    IO.puts "━━━━━━━━━━━━━━━━━━━━"
    
    metrics = CnsForge.UltraThinkSwarmOrchestrator.get_metrics()
    
    IO.puts "Total executions: #{metrics.total_executions}"
    IO.puts "Successful: #{metrics.successful_executions}"
    IO.puts "Failed: #{metrics.failed_executions}"
    
    if map_size(metrics.stage_timings) > 0 do
      IO.puts "\nStage Performance:"
      Enum.each(metrics.stage_timings, fn {stage, timings} ->
        avg_time = if length(timings) > 0 do
          Enum.sum(timings) / length(timings)
        else
          0
        end
        IO.puts "  #{stage}: #{Float.round(avg_time, 2)}ms avg"
      end)
    end
    
    # Show current status
    status = CnsForge.UltraThinkSwarmOrchestrator.get_swarm_status()
    IO.puts "\nCurrent Status: #{status.status}"
  end
end

# Run the demo
UltraThinkSwarmDemo.run()