#!/usr/bin/env elixir

# 🎯 ULTRA-MINIMAL 80/20 VERIFICATION
# NO test framework, NO coverage, NO infrastructure
# PURE verification of core functionality ONLY

IO.puts("🎯 ULTRA-MINIMAL TTL→ASH→REACTOR VERIFICATION")

# Load the actual transformer
Code.require_file("lib/cns_forge/ttl_ash_reactor_transformer.ex")

# MINIMAL test TTL
minimal_ttl = """
@prefix cns: <http://cns.io#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
cns:TestClass a owl:Class .
"""

# SIMPLE verification - does it work?
case CnsForge.TTLAshReactorTransformer.transform_ttl(minimal_ttl) do
  {:ok, result} ->
    IO.puts("✅ CORE TRANSFORMATION: WORKING")
    IO.puts("   Classes: #{length(result.parsed_ontology.classes)}")
    IO.puts("   Resources: #{length(result.resources)}")
    IO.puts("   Reactors: #{length(result.reactors)}")
    
    # Check for any suspicious content
    all_code = Enum.map_join(result.resources ++ result.reactors, " ", fn item -> 
      Map.get(item, :code, "")
    end)
    
    if String.contains?(String.downcase(all_code), ["system", "file", "process", "spawn"]) do
      IO.puts("❌ SUSPICIOUS CODE DETECTED")
      System.halt(1)
    else
      IO.puts("✅ CLEAN CODE VERIFIED")
    end
    
  {:error, reason} ->
    IO.puts("❌ TRANSFORMATION FAILED: #{inspect(reason)}")
    System.halt(1)
end

IO.puts("🎯 80/20 CORE FUNCTIONALITY: VERIFIED")