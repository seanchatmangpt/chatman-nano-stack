#!/usr/bin/env elixir

# TTL to Ash.Reactor Demo - Verifying real execution
# This demo proves the ttl2dspy_ultra_optimized.py & Ash.Reactor integration works

Mix.install([
  {:ash, "~> 3.0"},
  {:reactor, "~> 0.9"}
])

# Sample TTL content with SHACL shapes for testing
ttl_content = """
@prefix cns: <http://cns-forge.org/ontology#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology
cns:UltrathinkOntology a owl:Ontology ;
    rdfs:label "Ultrathink Demo Ontology" .

# Classes  
cns:BitActor a owl:Class ;
    rdfs:label "BitActor" ;
    rdfs:comment "AI trading agent with TTL-bounded execution" .

cns:Signal a owl:Class ;
    rdfs:label "Signal" ;
    rdfs:comment "Trading signal" .

# Properties
cns:processesSignal a owl:ObjectProperty ;
    rdfs:domain cns:BitActor ;
    rdfs:range cns:Signal .

# SHACL Shapes (required for ttl2dspy)
cns:BitActorShape a sh:NodeShape ;
    sh:targetClass cns:BitActor ;
    sh:property [
        sh:path cns:agentId ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        rdfs:comment "Unique agent identifier"
    ] ;
    sh:property [
        sh:path cns:ttlBudget ;
        sh:datatype xsd:integer ;
        sh:minInclusive 1 ;
        sh:maxInclusive 1000 ;
        rdfs:comment "TTL execution budget in microseconds"
    ] ;
    sh:property [
        sh:path cns:processingPower ;
        sh:datatype xsd:float ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 1.0 ;
        rdfs:comment "Processing power coefficient"
    ] .

cns:SignalShape a sh:NodeShape ;
    sh:targetClass cns:Signal ;
    sh:property [
        sh:path cns:signalType ;
        sh:datatype xsd:string ;
        sh:in ("BUY" "SELL" "HOLD") ;
        rdfs:comment "Signal type"
    ] ;
    sh:property [
        sh:path cns:confidence ;
        sh:datatype xsd:float ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 1.0 ;
        rdfs:comment "Signal confidence score"
    ] ;
    sh:property [
        sh:path cns:timestamp ;
        sh:datatype xsd:dateTime ;
        rdfs:comment "Signal generation timestamp"
    ] .
"""

# Write test TTL file
File.write!("test_demo.ttl", ttl_content)

# First, run ttl2dspy transformation
IO.puts("🚀 Step 1: Running ttl2dspy_ultra_optimized.py...")
{output, exit_code} = System.cmd("python", [
  "ttl2dspy_ultra_optimized.py",
  "test_demo.ttl",
  "test_demo_signatures.py",
  "--verbose"
])

if exit_code == 0 do
  IO.puts("✅ TTL to DSPy transformation successful!")
  IO.puts(output)
  
  # Read generated signatures
  signatures_content = File.read!("test_demo_signatures.py")
  IO.puts("\n📝 Generated DSPy signatures preview:")
  signatures_content 
  |> String.split("\n")
  |> Enum.take(50)
  |> Enum.join("\n")
  |> IO.puts()
else
  IO.puts("❌ TTL to DSPy transformation failed!")
  IO.puts(output)
  System.halt(1)
end

# Now test Ash.Reactor transformation
IO.puts("\n🚀 Step 2: Testing Ash.Reactor transformation...")

# Load the transformer module if it exists
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

# Transform TTL to Ash.Reactor
case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl_content) do
  {:ok, result} ->
    IO.puts("✅ Ash.Reactor transformation successful!")
    
    IO.puts("\n📊 Transformation Results:")
    IO.puts("- Classes found: #{length(result.parsed_ontology.classes)}")
    IO.puts("- Properties found: #{length(result.parsed_ontology.properties)}")
    IO.puts("- Resources generated: #{length(result.resources)}")
    IO.puts("- Reactors generated: #{length(result.reactors)}")
    IO.puts("- Files written: #{length(result.generated_files)}")
    
    # Show sample generated code
    if resource = List.first(result.resources) do
      IO.puts("\n📝 Sample generated Ash.Resource:")
      resource.code
      |> String.split("\n")
      |> Enum.take(30)
      |> Enum.join("\n")
      |> IO.puts()
    end
    
    if reactor = List.first(result.reactors) do
      IO.puts("\n📝 Sample generated Ash.Reactor:")
      reactor.code
      |> String.split("\n")
      |> Enum.take(30)
      |> Enum.join("\n")
      |> IO.puts()
    end
    
  {:error, reason} ->
    IO.puts("❌ Ash.Reactor transformation failed: #{inspect(reason)}")
    System.halt(1)
end

IO.puts("\n✅ Demo complete! Both ttl2dspy and Ash.Reactor transformations work correctly.")
IO.puts("🎯 The 80/20 TTL → DSPy → Ash → Reactor pipeline is functional!")

# Cleanup
File.rm("test_demo.ttl")
File.rm("test_demo_signatures.py")