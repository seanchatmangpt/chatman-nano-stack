# Test script for TTL to Ash generation
# Runs without mix task infrastructure

# Load the transformer
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

ttl_content = """
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix cyber: <http://cybersecurity.org/> .

cyber:ThreatActor a owl:Class .
cyber:Vulnerability a owl:Class .
cyber:SecurityControl a owl:Class .
"""

IO.puts("🐢 Testing TTL to Ash transformation...")
IO.puts("=====================================\n")

case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl_content) do
  {:ok, result} ->
    IO.puts("✅ Successfully transformed TTL!")
    IO.puts("   Classes found: #{length(result.parsed_ontology.classes)}")
    IO.puts("   Resources generated: #{length(result.resources)}")
    IO.puts("   Reactors generated: #{length(result.reactors)}")
    
    IO.puts("\n📋 Generated Classes:")
    Enum.each(result.parsed_ontology.classes, fn class ->
      IO.puts("   • #{class.name} (#{class.uri})")
    end)
    
    IO.puts("\n📦 Generated Resource Code Sample:")
    IO.puts("=====================================")
    first_resource = hd(result.resources)
    IO.puts(first_resource.code)
    
    IO.puts("\n⚡ Generated Reactor Code:")
    IO.puts("=====================================")
    first_reactor = hd(result.reactors)
    IO.puts(first_reactor.code)
    
    IO.puts("\n🏗️  Generated Domain Code:")
    IO.puts("=====================================")
    IO.puts(result.domain)
    
  {:error, reason} ->
    IO.puts("❌ Transformation failed: #{inspect(reason)}")
end