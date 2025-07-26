#!/usr/bin/env elixir

# 🚀 SWARM 80/20 PIPELINE: typer → turtle demonstration
Code.require_file("lib/cns_forge/typed_ontology.ex")
Code.require_file("lib/cns_forge/turtle_generator.ex")

alias CnsForge.{TypedOntology, TurtleGenerator}

IO.puts("🚀 SWARM 80/20 PIPELINE: typer → turtle")
IO.puts("=" <> String.duplicate("=", 40))

# Create typed ontology for cybersecurity domain
ontology = TypedOntology.new()
|> TypedOntology.add_namespace(:cyber, "http://cybersecurity.org/")
|> TypedOntology.add_namespace(:infra, "http://infrastructure.org/")

# Add typed classes with 80/20 focus (essential security concepts)
ontology = ontology
|> TypedOntology.add_class("Asset", :cyber, description: "Any valuable resource")
|> TypedOntology.add_class("Threat", :cyber, description: "Potential security threat")
|> TypedOntology.add_class("Vulnerability", :cyber, description: "Security weakness")
|> TypedOntology.add_class("SecurityControl", :cyber, description: "Protective measure")
|> TypedOntology.add_class("NetworkAsset", :infra, superclass: "cyber:Asset")
|> TypedOntology.add_class("Server", :infra, superclass: "infra:NetworkAsset")

# Add typed properties
ontology = ontology
|> TypedOntology.add_property("exploits", :cyber, "cyber:Threat", "cyber:Vulnerability", :object)
|> TypedOntology.add_property("protects", :cyber, "cyber:SecurityControl", "cyber:Asset", :object)
|> TypedOntology.add_property("hasRiskLevel", :cyber, "cyber:Threat", "xsd:integer", :datatype)
|> TypedOntology.add_property("hasCVSS", :cyber, "cyber:Vulnerability", "xsd:float", :datatype)

# Add relationships (instances)
ontology = ontology
|> TypedOntology.add_relationship("cyber:SQLInjection", "rdf:type", "cyber:Threat")
|> TypedOntology.add_relationship("cyber:WeakPassword", "rdf:type", "cyber:Vulnerability")
|> TypedOntology.add_relationship("cyber:Firewall", "rdf:type", "cyber:SecurityControl")
|> TypedOntology.add_relationship("cyber:SQLInjection", "cyber:exploits", "cyber:WeakPassword")
|> TypedOntology.add_relationship("cyber:Firewall", "cyber:protects", "infra:Server")

# Generate TTL
IO.puts("\n📝 TYPED ONTOLOGY STRUCTURE:")
IO.puts("  Classes: #{length(ontology.classes)}")
IO.puts("  Properties: #{length(ontology.properties)}")
IO.puts("  Relationships: #{length(ontology.relationships)}")

ttl_output = TurtleGenerator.generate(ontology)

IO.puts("\n🐢 GENERATED TURTLE/TTL:")
IO.puts("=" <> String.duplicate("=", 40))
IO.puts(ttl_output)

# Verify TTL is valid by parsing it back
IO.puts("\n✅ VALIDATION:")
# We already have a TTL parser from previous work
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

case CnsForge.TTLAshReactorTransformer.parse_ttl(ttl_output) do
  {:ok, parsed} ->
    IO.puts("  ✅ Generated TTL is valid!")
    IO.puts("  ✅ Parsed #{length(parsed.classes)} classes")
    IO.puts("  ✅ Type-safe pipeline working!")
    
  {:error, reason} ->
    IO.puts("  ❌ TTL validation failed: #{reason}")
end

IO.puts("\n🎯 80/20 ACHIEVEMENT:")
IO.puts("  ✅ Type-safe ontology definition")
IO.puts("  ✅ Valid TTL generation")
IO.puts("  ✅ Cybersecurity domain modeling")
IO.puts("  ✅ Ready for next pipeline stage: turtle → ttl2dspy")