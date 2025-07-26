#!/usr/bin/env elixir

# Test compilation of TTL transformer
try do
  Code.eval_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")
  IO.puts("✅ COMPILATION SUCCESS - TTL Transformer compiles correctly")
  
  # Test basic parsing
  sample_ttl = """
  @prefix cns: <http://cns-forge.org/ontology#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .
  
  cns:BitActor a owl:Class .
  cns:Signal a owl:Class .
  cns:processes a owl:ObjectProperty ;
    owl:domain cns:BitActor ;
    owl:range cns:Signal .
  """
  
  case CnsForge.TTLAshReactorTransformer.parse_ttl(sample_ttl) do
    {:ok, parsed} ->
      IO.puts("✅ PARSING SUCCESS - TTL parsing works")
      IO.puts("  Classes: #{inspect(Enum.map(parsed.classes, & &1.name))}")
      IO.puts("  Properties: #{inspect(Enum.map(parsed.properties, & &1.name))}")
      
      # Test full transformation
      case CnsForge.TTLAshReactorTransformer.transform_ttl(sample_ttl) do
        {:ok, result} ->
          IO.puts("✅ TRANSFORMATION SUCCESS - Full TTL transformation works")
          IO.puts("  Resources generated: #{length(result.resources)}")
          IO.puts("  Reactors generated: #{length(result.reactors)}")
          IO.puts("  Files written: #{length(result.generated_files)}")
          
        {:error, reason} ->
          IO.puts("❌ TRANSFORMATION ERROR: #{inspect(reason)}")
      end
      
    {:error, reason} ->
      IO.puts("❌ PARSING ERROR: #{inspect(reason)}")
  end
  
rescue
  e -> 
    IO.puts("❌ COMPILATION ERROR: #{inspect(e)}")
    System.halt(1)
end