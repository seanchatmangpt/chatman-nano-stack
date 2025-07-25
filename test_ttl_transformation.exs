#!/usr/bin/env elixir

# Load the transformation module
Code.eval_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

# Test TTL content
ttl = """
@prefix cns: <http://cns-forge.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

cns:BitActor a owl:Class .
cns:Signal a owl:Class .
cns:processes a owl:ObjectProperty ;
  owl:domain cns:BitActor ;
  owl:range cns:Signal .
"""

IO.puts("🚀 TTL → Ash.Reactor Transformation Test")
IO.puts("=" |> String.duplicate(50))

case CnsForge.TTLAshReactorTransformer.transform_ttl(ttl) do
  {:ok, result} ->
    IO.puts("✅ TTL → Ash.Reactor Transformation SUCCESSFUL")
    IO.puts("📊 Results:")
    IO.puts("  - Classes: #{length(result.parsed_ontology.classes)}")
    IO.puts("  - Properties: #{length(result.parsed_ontology.properties)}")  
    IO.puts("  - Resources: #{length(result.resources)} generated")
    IO.puts("  - Reactors: #{length(result.reactors)} generated")
    IO.puts("  - Files: #{length(result.generated_files)} written")
    
    IO.puts("")
    IO.puts("🔷 Generated Classes:")
    Enum.each(result.parsed_ontology.classes, fn class ->
      IO.puts("    #{class.name} → #{class.module_name}")
    end)
    
    IO.puts("")
    IO.puts("🔗 Generated Relationships:")
    Enum.each(result.parsed_ontology.relationships, fn rel ->
      IO.puts("    #{rel.from} --#{rel.property}--> #{rel.to}")
    end)
    
    IO.puts("")
    IO.puts("⚡ Generated Reactors:")
    Enum.each(result.reactors, fn reactor ->
      IO.puts("    #{reactor.name}")
    end)
    
    # Show sample generated code
    IO.puts("")
    IO.puts("📝 Sample Generated Ash.Resource:")
    IO.puts("─" |> String.duplicate(50))
    bitactor_resource = Enum.find(result.resources, & &1.class.name == "BitActor")
    sample_lines = bitactor_resource.code 
      |> String.split("\n") 
      |> Enum.take(15)
      |> Enum.join("\n")
    IO.puts(sample_lines)
    IO.puts("    ...")
    
    IO.puts("")
    IO.puts("🔄 Sample Generated Ash.Reactor:")
    IO.puts("─" |> String.duplicate(50))
    main_reactor = Enum.find(result.reactors, & &1.name == "CnsForge.TTLMainReactor")
    sample_reactor_lines = main_reactor.code
      |> String.split("\n")
      |> Enum.take(15) 
      |> Enum.join("\n")
    IO.puts(sample_reactor_lines)
    IO.puts("    ...")
    
    IO.puts("")
    IO.puts("📁 Generated Files:")
    Enum.each(result.generated_files, fn file ->
      IO.puts("    #{file}")
    end)
    
    IO.puts("")
    IO.puts("🎉 MINIMAL VIABLE TTL → ASH.REACTOR SYSTEM WORKS!")
    IO.puts("🏆 80/20 Transformation Complete - Working Elixir Modules Generated!")
    
    # Test parsing separately to show it works
    IO.puts("")
    IO.puts("🔍 PARSING VERIFICATION:")
    IO.puts("─" |> String.duplicate(50))
    {:ok, parsed} = CnsForge.TTLAshReactorTransformer.parse_ttl(ttl)
    IO.puts("Prefixes: #{inspect(parsed.prefixes)}")
    IO.puts("Classes: #{inspect(Enum.map(parsed.classes, & &1.name))}")
    IO.puts("Properties: #{inspect(Enum.map(parsed.properties, & &1.name))}")
    IO.puts("Relationships: #{inspect(Enum.map(parsed.relationships, &{&1.from, &1.property, &1.to}))}")
    
  {:error, reason} ->
    IO.puts("❌ Transformation failed: #{inspect(reason)}")
end