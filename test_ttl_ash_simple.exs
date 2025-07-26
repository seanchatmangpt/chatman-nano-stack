#!/usr/bin/env elixir

# Simple test of TTL transformation without Mix dependencies
IO.puts("🚀 Testing TTL → Ash.Reactor transformation (no dependencies)...")

# Load the transformer
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

# Test TTL content
ttl_content = """
@prefix cns: <http://cns-forge.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

cns:BitActor a owl:Class ;
    rdfs:label "BitActor" ;
    rdfs:comment "AI trading agent" .

cns:Signal a owl:Class ;
    rdfs:label "Signal" ;
    rdfs:comment "Trading signal" .

cns:processes a owl:ObjectProperty ;
    rdfs:domain cns:BitActor ;
    rdfs:range cns:Signal .
"""

# Test the transformation
case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl_content) do
  {:ok, result} ->
    IO.puts("✅ Transformation successful!")
    IO.puts("\nParsed ontology:")
    IO.inspect(result.parsed_ontology, pretty: true, limit: :infinity)
    
    IO.puts("\nGenerated #{length(result.resources)} resources")
    IO.puts("Generated #{length(result.reactors)} reactors")
    
    # Show sample resource code
    if resource = List.first(result.resources) do
      IO.puts("\n📝 Sample Ash.Resource code:")
      IO.puts(String.slice(resource.code, 0, 500) <> "...")
    end
    
    # Show sample reactor code  
    if reactor = List.first(result.reactors) do
      IO.puts("\n📝 Sample Ash.Reactor code:")
      IO.puts(String.slice(reactor.code, 0, 500) <> "...")
    end
    
  {:error, reason} ->
    IO.puts("❌ Transformation failed: #{inspect(reason)}")
end