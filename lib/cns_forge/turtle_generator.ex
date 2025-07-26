defmodule CnsForge.TurtleGenerator do
  @moduledoc """
  🐢 SWARM 80/20: Generate Turtle/TTL from typed ontology
  Converts TypedOntology → valid TTL format
  """
  
  alias CnsForge.TypedOntology
  
  @doc """
  Generate TTL from a typed ontology
  """
  def generate(%TypedOntology{} = ontology) do
    ttl = []
    
    # Generate namespace prefixes
    ttl = ttl ++ generate_prefixes(ontology.namespaces)
    ttl = ttl ++ [""]  # blank line
    
    # Generate classes
    ttl = ttl ++ generate_classes(ontology.classes)
    ttl = ttl ++ [""] 
    
    # Generate properties
    ttl = ttl ++ generate_properties(ontology.properties)
    ttl = ttl ++ [""]
    
    # Generate relationships
    ttl = ttl ++ generate_relationships(ontology.relationships)
    
    # Join with newlines
    Enum.join(ttl, "\n")
  end
  
  # Generate namespace prefix declarations
  defp generate_prefixes(namespaces) do
    namespaces
    |> Enum.reverse()  # Maintain order
    |> Enum.map(fn {prefix, uri} ->
      "@prefix #{prefix}: <#{uri}> ."
    end)
  end
  
  # Generate class declarations
  defp generate_classes(classes) do
    classes
    |> Enum.reverse()
    |> Enum.flat_map(fn class ->
      base = ["#{class.namespace}:#{class.name} a owl:Class"]
      
      base = if class.description do
        base ++ ["; rdfs:comment \"#{class.description}\""]
      else
        base
      end
      
      base = if class.superclass do
        base ++ ["; rdfs:subClassOf #{class.superclass}"]
      else
        base
      end
      
      [Enum.join(base, " ") <> " ."]
    end)
  end
  
  # Generate property declarations
  defp generate_properties(properties) do
    properties
    |> Enum.reverse()
    |> Enum.flat_map(fn prop ->
      property_type = case prop.type do
        :object -> "owl:ObjectProperty"
        :datatype -> "owl:DatatypeProperty"
      end
      
      [
        "#{prop.namespace}:#{prop.name} a #{property_type} ;",
        "    rdfs:domain #{prop.domain} ;",
        "    rdfs:range #{prop.range} ."
      ]
    end)
  end
  
  # Generate relationship triples
  defp generate_relationships(relationships) do
    relationships
    |> Enum.reverse()
    |> Enum.map(fn rel ->
      "#{rel.subject} #{rel.predicate} #{rel.object} ."
    end)
  end
end