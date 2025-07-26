defmodule CNSForge.TTLParser do
  @moduledoc """
  TTL (Time-To-Live) bounded parser for RDF/Turtle ontologies
  
  Parses TTL content into structured data for Ash.Reactor generation
  Focus: 80/20 parsing that extracts essential ontology components
  """
  
  require Logger

  @doc """
  Parse TTL content into structured ontology data
  """
  def parse(ttl_content) when is_binary(ttl_content) do
    Logger.info("Starting TTL parsing")
    
    with {:ok, prefixes} <- extract_prefixes(ttl_content),
         {:ok, classes} <- extract_classes(ttl_content, prefixes),
         {:ok, properties} <- extract_properties(ttl_content, prefixes) do
      
      ontology = %{
        name: determine_ontology_name(prefixes),
        prefixes: prefixes,
        classes: classes,
        properties: properties,
        ttl_constraints: extract_ttl_constraints(ttl_content),
        shacl_constraints: extract_shacl_constraints(ttl_content)
      }
      
      Logger.info("TTL parsing completed: #{length(classes)} classes, #{length(properties)} properties")
      {:ok, ontology}
    else
      {:error, reason} -> 
        Logger.error("TTL parsing failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp extract_prefixes(ttl_content) do
    prefix_regex = ~r/@prefix\s+(\w+):\s+<([^>]+)>\s*\./
    
    prefixes = Regex.scan(prefix_regex, ttl_content)
    |> Enum.into(%{}, fn [_, prefix, uri] -> {prefix, uri} end)
    
    {:ok, prefixes}
  end

  defp extract_classes(ttl_content, prefixes) do
    class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
    
    classes = Regex.scan(class_regex, ttl_content)
    |> Enum.map(fn [_, class_uri] ->
      build_class_data(class_uri, ttl_content, prefixes)
    end)
    
    {:ok, classes}
  end

  defp extract_properties(ttl_content, prefixes) do
    property_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:(?:Object|Datatype)Property/
    
    properties = Regex.scan(property_regex, ttl_content)
    |> Enum.map(fn [_, property_uri] ->
      build_property_data(property_uri, ttl_content, prefixes)
    end)
    
    {:ok, properties}
  end

  defp build_class_data(class_uri, ttl_content, _prefixes) do
    local_name = extract_local_name(class_uri)
    
    %{
      uri: class_uri,
      name: local_name,
      type: determine_class_type(local_name),
      label: extract_label(class_uri, ttl_content),
      comment: extract_comment(class_uri, ttl_content),
      properties: extract_class_properties(class_uri, ttl_content),
      ttl_budget: extract_ttl_budget(class_uri, ttl_content)
    }
  end

  defp build_property_data(property_uri, ttl_content, _prefixes) do
    local_name = extract_local_name(property_uri)
    
    %{
      uri: property_uri,
      name: local_name,
      type: determine_property_type(local_name),
      domain: extract_property_domain(property_uri, ttl_content),
      range: extract_property_range(property_uri, ttl_content),
      direction: determine_property_direction(local_name)
    }
  end

  defp extract_local_name(uri) do
    case String.split(uri, ":") do
      [_prefix, name] -> name
      [name] -> name
    end
  end

  defp determine_class_type(name) do
    cond do
      String.contains?(name, ["Process", "Workflow", "Reactor"]) -> "Process"
      String.contains?(name, ["Resource", "Entity"]) -> "Resource"
      String.contains?(name, ["Step", "Action"]) -> "Step"
      true -> "Resource"
    end
  end

  defp determine_property_type(name) do
    cond do
      String.contains?(name, ["step", "action", "operation"]) -> "Step"
      String.contains?(name, ["input", "parameter"]) -> "Input"
      String.contains?(name, ["output", "result"]) -> "Output"
      true -> "Attribute"
    end
  end

  defp determine_property_direction(name) do
    cond do
      String.contains?(name, ["input", "parameter", "arg"]) -> "input"
      String.contains?(name, ["output", "result", "return"]) -> "output"
      true -> "attribute"
    end
  end

  defp extract_label(uri, ttl_content) do
    label_regex = ~r/#{Regex.escape(uri)}.*?rdfs:label\s+"([^"]+)"/s
    
    case Regex.run(label_regex, ttl_content) do
      [_, label] -> label
      nil -> nil
    end
  end

  defp extract_comment(uri, ttl_content) do
    comment_regex = ~r/#{Regex.escape(uri)}.*?rdfs:comment\s+"([^"]+)"/s
    
    case Regex.run(comment_regex, ttl_content) do
      [_, comment] -> comment
      nil -> nil
    end
  end

  defp extract_property_domain(property_uri, ttl_content) do
    domain_regex = ~r/#{Regex.escape(property_uri)}.*?rdfs:domain\s+(\w+:\w+)/s
    
    case Regex.run(domain_regex, ttl_content) do
      [_, domain] -> domain
      nil -> nil
    end
  end

  defp extract_property_range(property_uri, ttl_content) do
    range_regex = ~r/#{Regex.escape(property_uri)}.*?rdfs:range\s+(\w+:\w+)/s
    
    case Regex.run(range_regex, ttl_content) do
      [_, range] -> range
      nil -> nil
    end
  end

  defp extract_class_properties(_class_uri, _ttl_content), do: []
  defp extract_ttl_budget(_class_uri, _ttl_content), do: 8

  defp extract_ttl_constraints(_ttl_content) do
    %{
      max_execution_hops: 8,
      max_processing_time_ms: 5000,
      enable_bounds_checking: true
    }
  end

  defp extract_shacl_constraints(_ttl_content) do
    %{
      enable_validation: false,
      validation_mode: :lenient
    }
  end

  defp determine_ontology_name(prefixes) do
    main_prefixes = Map.keys(prefixes) -- ["owl", "rdf", "rdfs", "xsd"]
    
    case main_prefixes do
      [main | _] -> main
      [] -> "generated"
    end
  end
end