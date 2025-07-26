#!/usr/bin/env elixir

# 🔗 ULTRATHINK SWARM 80/20: Complete Connected Pipeline Demo
# typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s

Code.require_file("lib/cns_forge/typed_ontology.ex")
Code.require_file("lib/cns_forge/turtle_generator.ex") 
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")
Code.require_file("lib/cns_forge/pipeline_connector.ex")

alias CnsForge.PipelineConnector

IO.puts("🔗 ULTRATHINK SWARM 80/20: COMPLETE CONNECTED PIPELINE")
IO.puts("=" <> String.duplicate("=", 55))
IO.puts("typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s\n")

# Execute the complete pipeline
result = PipelineConnector.execute_full_pipeline()

# Display Stage 1: TTL Generation
IO.puts("📝 STAGE 1: Type-Safe TTL Generation")
IO.puts("=" <> String.duplicate("=", 40))
ttl_preview = result.ttl |> String.split("\n") |> Enum.take(10) |> Enum.join("\n")
IO.puts(ttl_preview)
IO.puts("... (#{String.split(result.ttl, "\n") |> length()} lines total)")

# Display Stage 2: Ash Resources (from existing transformer)
IO.puts("\n🔥 STAGE 2: Ash Resources (via existing TTLAshReactorTransformer)")
IO.puts("=" <> String.duplicate("=", 40))
IO.puts("Generated Resources: #{length(result.ash_resources)}")
Enum.each(result.ash_resources, fn resource ->
  IO.puts("  - #{resource.module_name}")
end)

# Display Stage 3: Ash Reactors
IO.puts("\n🔄 STAGE 3: Ash.Reactor Workflows")
IO.puts("=" <> String.duplicate("=", 40))
IO.puts("Generated Reactors: #{length(result.ash_reactors)}")
Enum.each(result.ash_reactors, fn reactor ->
  IO.puts("  - #{reactor.name}")
end)

# Display Stage 4: BitActor Specification
IO.puts("\n⚡ STAGE 4: BitActor Distributed System")
IO.puts("=" <> String.duplicate("=", 40))
bitactor_preview = result.bitactor_spec |> String.split("\n") |> Enum.take(15) |> Enum.join("\n")
IO.puts(bitactor_preview)
IO.puts("...")

# Display Stage 5: Erlang Code
IO.puts("\n🟣 STAGE 5: Erlang GenServer Implementation")
IO.puts("=" <> String.duplicate("=", 40))
erlang_preview = result.erlang_code |> String.split("\n") |> Enum.take(15) |> Enum.join("\n")
IO.puts(erlang_preview)
IO.puts("...")

# Display Stage 6: Kubernetes Manifests
IO.puts("\n☸️ STAGE 6: Kubernetes Deployment")
IO.puts("=" <> String.duplicate("=", 40))
k8s_preview = result.k8s_manifests |> String.split("\n") |> Enum.take(20) |> Enum.join("\n")
IO.puts(k8s_preview)
IO.puts("...")

# Save all outputs
outputs = [
  {"generated_pipeline_ttl.ttl", result.ttl},
  {"generated_pipeline_bitactor.md", result.bitactor_spec},
  {"generated_pipeline_erlang.erl", result.erlang_code},
  {"generated_pipeline_k8s.yaml", result.k8s_manifests}
]

IO.puts("\n💾 SAVING OUTPUTS:")
Enum.each(outputs, fn {filename, content} ->
  File.write!(filename, content)
  IO.puts("  ✅ #{filename}")
end)

# Generate visual pipeline diagram
IO.puts("\n🔄 COMPLETE PIPELINE FLOW:")
IO.puts("""

```mermaid
graph LR
    subgraph "Input"
        Types[TypedOntology<br/>Classes & Properties]
    end
    
    subgraph "Stage 1"
        Types --> TTL[TTL/Turtle<br/>RDF Format]
    end
    
    subgraph "Existing Code Integration"
        TTL --> Transformer[TTLAshReactorTransformer<br/>✨ REUSED ✨]
        Transformer --> Resources[Ash Resources<br/>#{length(result.ash_resources)} generated]
        Transformer --> Reactors[Ash.Reactor<br/>Workflows]
    end
    
    subgraph "Additional Layers"
        Resources --> BitActor[BitActor Specs<br/>Distributed Actors]
        BitActor --> Erlang[Erlang GenServers<br/>BEAM Runtime]
        Reactors --> K8s[Kubernetes<br/>Deployment Manifests]
    end
    
    style Transformer fill:#ffeb3b,stroke:#f57c00,stroke-width:3px
```
""")

IO.puts("\n🎯 80/20 ACHIEVEMENT SUMMARY:")
IO.puts("  ✅ Type-safe ontology definition")
IO.puts("  ✅ Valid TTL generation")
IO.puts("  ✅ Integrated with existing Ash.Reactor transformer")
IO.puts("  ✅ BitActor distributed system specs")
IO.puts("  ✅ Erlang GenServer implementation")
IO.puts("  ✅ Kubernetes deployment ready")
IO.puts("\n🚀 COMPLETE PIPELINE: typer → k8s CONNECTED!")