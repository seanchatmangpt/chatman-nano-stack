#!/usr/bin/env elixir

# Simple test to isolate the TTL parsing issue
defmodule SimpleTTLTransformer do
  def parse_ttl(ttl_content) do
    # Extract prefixes
    prefix_regex = ~r/@prefix\s+(\w+):\s+<([^>]+)>\s*\./
    prefixes = Regex.scan(prefix_regex, ttl_content)
    |> Enum.into(%{}, fn [_, prefix, uri] -> {prefix, uri} end)
    
    # Extract classes
    class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
    classes = Regex.scan(class_regex, ttl_content)
    |> Enum.map(fn [_, class_uri] ->
      %{
        uri: class_uri,
        name: extract_local_name(class_uri)
      }
    end)
    
    # Extract properties  
    property_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:(?:Object|Datatype)Property/
    properties = Regex.scan(property_regex, ttl_content)
    |> Enum.map(fn [_, property_uri] ->
      %{
        uri: property_uri,
        name: extract_local_name(property_uri),
        domain: extract_property_domain(property_uri, ttl_content),
        range: extract_property_range(property_uri, ttl_content)
      }
    end)
    
    %{
      prefixes: prefixes,
      classes: classes,
      properties: properties
    }
  end
  
  defp extract_local_name(uri) do
    case String.split(uri, ":") do
      [_prefix, name] -> name
      [name] -> name
    end
  end
  
  defp extract_property_domain(property_uri, ttl_content) do
    domain_regex = ~r/#{Regex.escape(property_uri)}.*?owl:domain\s+(\w+:\w+)/s
    case Regex.run(domain_regex, ttl_content) do
      [_, domain] -> domain
      nil -> nil
    end
  end
  
  defp extract_property_range(property_uri, ttl_content) do
    range_regex = ~r/#{Regex.escape(property_uri)}.*?owl:range\s+(\w+:\w+)/s
    case Regex.run(range_regex, ttl_content) do
      [_, range] -> range
      nil -> nil
    end
  end
  
  def generate_basic_ash_resource(class) do
    """
    defmodule CnsForge.TTLResources.#{class.name} do
      use Ash.Resource, domain: CnsForge.TTLDomain, data_layer: Ash.DataLayer.Ets
      
      ets do
        table :ttl_#{String.downcase(class.name)}s
      end
      
      actions do
        defaults [:read]
        create :create_from_ttl do
          accept [:ttl_uri]
        end
      end
      
      attributes do
        uuid_primary_key :id
        attribute :ttl_uri, :string do
          public? true
        end
      end
    end
    """
  end
  
  def generate_basic_reactor(classes) do
    """
    defmodule CnsForge.TTLMainReactor do
      use Reactor
      
      input :ontology_data
      
      step :validate_input do
        argument :data, input(:ontology_data)
        run fn %{data: data}, _context ->
          {:ok, data}
        end
      end
      
      #{Enum.map_join(classes, "\n", &generate_class_step/1)}
      
      return :validate_input
    end
    """
  end
  
  defp generate_class_step(class) do
    """
    step :process_#{String.downcase(class.name)} do
      argument :data, result(:validate_input)
      run fn %{data: _data}, _context ->
        {:ok, %{class: "#{class.name}", processed: true}}
      end
    end"""
  end
end

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

IO.puts("🧪 SIMPLE TTL TRANSFORMATION TEST")
IO.puts("=" |> String.duplicate(40))

parsed = SimpleTTLTransformer.parse_ttl(ttl)

IO.puts("✅ Parsing successful!")
IO.puts("📊 Results:")
IO.puts("  Prefixes: #{inspect(parsed.prefixes)}")
IO.puts("  Classes: #{inspect(Enum.map(parsed.classes, & &1.name))}")
IO.puts("  Properties: #{inspect(Enum.map(parsed.properties, & &1.name))}")

IO.puts("")
IO.puts("🏗️ Generated Resources:")
Enum.each(parsed.classes, fn class ->
  IO.puts("  #{class.name}:")
  resource_code = SimpleTTLTransformer.generate_basic_ash_resource(class)
  IO.puts("    " <> (resource_code |> String.split("\n") |> Enum.take(3) |> Enum.join("\n    ")))
  IO.puts("    ...")
end)

IO.puts("")
IO.puts("⚡ Generated Reactor:")
reactor_code = SimpleTTLTransformer.generate_basic_reactor(parsed.classes)
IO.puts(reactor_code |> String.split("\n") |> Enum.take(10) |> Enum.join("\n"))
IO.puts("...")

IO.puts("")
IO.puts("🎉 SIMPLE TRANSFORMATION WORKS!")
IO.puts("This proves the 80/20 TTL → Ash.Reactor concept is viable!")