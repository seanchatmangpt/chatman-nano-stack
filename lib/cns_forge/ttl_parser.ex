defmodule CNSForge.TTLParser do
  @moduledoc """
  TTL (Turtle) parser that extracts ontology structure for Ash.Reactor project generation.
  
  Parses RDF/OWL ontologies in Turtle format and extracts:
  - Classes and their hierarchies
  - Properties and relationships
  - SHACL constraints
  - TTL budgets and performance constraints
  """

  require Logger

  @doc """
  Parse TTL content and extract ontology structure
  """
  def parse(ttl_content) when is_binary(ttl_content) do
    Logger.info("Parsing TTL ontology content...")
    
    with {:ok, lines} <- validate_and_clean_content(ttl_content),
         {:ok, prefixes} <- extract_prefixes(lines),
         {:ok, classes} <- extract_classes(lines, prefixes),
         {:ok, properties} <- extract_properties(lines, prefixes),
         {:ok, constraints} <- extract_constraints(lines, prefixes) do
      
      ontology = %{
        name: extract_ontology_name(lines, prefixes),
        description: extract_ontology_description(lines, prefixes),
        version: extract_ontology_version(lines, prefixes),
        prefixes: prefixes,
        classes: classes,
        properties: properties,
        ttl_constraints: extract_ttl_constraints(constraints),
        shacl_constraints: extract_shacl_constraints(constraints),
        relationships: build_relationships(classes, properties)
      }
      
      Logger.info("Successfully parsed ontology: #{ontology.name} with #{length(classes)} classes")
      {:ok, ontology}
    else
      {:error, reason} ->
        Logger.error("TTL parsing failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp validate_and_clean_content(content) do
    if String.trim(content) == "" do
      {:error, :empty_content}
    else
      lines = content
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "#")))
      
      {:ok, lines}
    end
  end

  defp extract_prefixes(lines) do
    prefixes = lines
    |> Enum.filter(&String.starts_with?(&1, "@prefix"))
    |> Enum.map(&parse_prefix_line/1)
    |> Enum.into(%{})
    
    # Add default prefixes if missing
    default_prefixes = %{
      "rdf" => "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs" => "http://www.w3.org/2000/01/rdf-schema#",
      "owl" => "http://www.w3.org/2002/07/owl#",
      "xsd" => "http://www.w3.org/2001/XMLSchema#"
    }
    
    {:ok, Map.merge(default_prefixes, prefixes)}
  end

  defp parse_prefix_line(line) do
    # Parse: @prefix prefix: <uri> .
    case Regex.run(~r/@prefix\s+(\w+):\s+<([^>]+)>\s*\./, line) do
      [_, prefix, uri] -> {prefix, uri}
      _ -> {"unknown", ""}
    end
  end

  defp extract_classes(lines, prefixes) do
    class_lines = lines
    |> Enum.filter(&contains_class_definition?/1)
    
    classes = class_lines
    |> Enum.map(&parse_class_definition(&1, prefixes))
    |> Enum.reject(&is_nil/1)
    
    # Extract additional class information from subsequent lines
    enhanced_classes = classes
    |> Enum.map(&enhance_class_definition(&1, lines, prefixes))
    
    {:ok, enhanced_classes}
  end

  defp contains_class_definition?(line) do
    String.contains?(line, "rdf:type owl:Class") or
    String.contains?(line, "a owl:Class")
  end

  defp parse_class_definition(line, prefixes) do
    # Parse: prefix:ClassName rdf:type owl:Class
    case Regex.run(~r/(\w+):(\w+)\s+(?:rdf:type|a)\s+owl:Class/, line) do
      [_, prefix, class_name] ->
        %{
          name: class_name,
          full_uri: resolve_uri(prefix, class_name, prefixes),
          prefix: prefix,
          type: "Class",
          properties: [],
          subclass_of: [],
          comment: nil,
          label: nil,
          ttl_budget: nil
        }
      _ -> nil
    end
  end

  defp enhance_class_definition(class, lines, prefixes) do
    # Find all lines that reference this class
    class_lines = lines
    |> Enum.filter(&String.contains?(&1, "#{class.prefix}:#{class.name}"))
    
    enhanced = class_lines
    |> Enum.reduce(class, fn line, acc ->
      acc
      |> add_label_if_present(line)
      |> add_comment_if_present(line)
      |> add_subclass_if_present(line, prefixes)
      |> add_ttl_budget_if_present(line)
    end)
    
    # Extract properties that reference this class
    properties = extract_class_properties(class, lines, prefixes)
    %{enhanced | properties: properties}
  end

  defp extract_properties(lines, prefixes) do
    property_lines = lines
    |> Enum.filter(&contains_property_definition?/1)
    
    properties = property_lines
    |> Enum.map(&parse_property_definition(&1, prefixes))
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&enhance_property_definition(&1, lines, prefixes))
    
    {:ok, properties}
  end

  defp contains_property_definition?(line) do
    String.contains?(line, "rdf:type owl:ObjectProperty") or
    String.contains?(line, "rdf:type owl:DatatypeProperty") or
    String.contains?(line, "a owl:ObjectProperty") or
    String.contains?(line, "a owl:DatatypeProperty")
  end

  defp parse_property_definition(line, prefixes) do
    object_property_regex = ~r/(\w+):(\w+)\s+(?:rdf:type|a)\s+owl:ObjectProperty/
    datatype_property_regex = ~r/(\w+):(\w+)\s+(?:rdf:type|a)\s+owl:DatatypeProperty/
    
    cond do
      match = Regex.run(object_property_regex, line) ->
        [_, prefix, property_name] = match
        %{
          name: property_name,
          full_uri: resolve_uri(prefix, property_name, prefixes),
          prefix: prefix,
          type: "ObjectProperty",
          domain: nil,
          range: nil,
          comment: nil,
          label: nil,
          direction: determine_property_direction(property_name)
        }
        
      match = Regex.run(datatype_property_regex, line) ->
        [_, prefix, property_name] = match
        %{
          name: property_name,
          full_uri: resolve_uri(prefix, property_name, prefixes),
          prefix: prefix,
          type: "DatatypeProperty",
          domain: nil,
          range: nil,
          comment: nil,
          label: nil,
          direction: determine_property_direction(property_name)
        }
        
      true -> nil
    end
  end

  defp enhance_property_definition(property, lines, prefixes) do
    property_lines = lines
    |> Enum.filter(&String.contains?(&1, "#{property.prefix}:#{property.name}"))
    
    property_lines
    |> Enum.reduce(property, fn line, acc ->
      acc
      |> add_label_if_present(line)
      |> add_comment_if_present(line)
      |> add_domain_if_present(line, prefixes)
      |> add_range_if_present(line, prefixes)
    end)
  end

  defp extract_constraints(lines, prefixes) do
    constraint_lines = lines
    |> Enum.filter(&contains_constraint_definition?/1)
    
    constraints = constraint_lines
    |> Enum.map(&parse_constraint_definition(&1, prefixes))
    |> Enum.reject(&is_nil/1)
    
    {:ok, constraints}
  end

  defp contains_constraint_definition?(line) do
    String.contains?(line, "sh:") or
    String.contains?(line, "ttl:") or
    String.contains?(line, "budget") or
    String.contains?(line, "constraint")
  end

  defp parse_constraint_definition(line, prefixes) do
    cond do
      String.contains?(line, "sh:") ->
        parse_shacl_constraint(line, prefixes)
        
      String.contains?(line, "ttl:") or String.contains?(line, "budget") ->
        parse_ttl_constraint(line, prefixes)
        
      true -> nil
    end
  end

  defp parse_shacl_constraint(line, _prefixes) do
    # Basic SHACL constraint parsing
    %{
      type: :shacl,
      constraint: line,
      parsed: false  # Would need full SHACL parser for complete implementation
    }
  end

  defp parse_ttl_constraint(line, _prefixes) do
    # Extract TTL budget information
    budget = case Regex.run(~r/budget[:\s]+(\d+)/, line) do
      [_, budget_str] -> String.to_integer(budget_str)
      _ -> 8
    end
    
    %{
      type: :ttl,
      budget: budget,
      constraint: line
    }
  end

  # Helper functions
  defp resolve_uri(prefix, name, prefixes) do
    base_uri = Map.get(prefixes, prefix, "http://example.org/")
    "#{base_uri}#{name}"
  end

  defp add_label_if_present(item, line) do
    case Regex.run(~r/rdfs:label\s+"([^"]+)"/, line) do
      [_, label] -> %{item | label: label}
      _ -> item
    end
  end

  defp add_comment_if_present(item, line) do
    case Regex.run(~r/rdfs:comment\s+"([^"]+)"/, line) do
      [_, comment] -> %{item | comment: comment}
      _ -> item
    end
  end

  defp add_subclass_if_present(class, line, prefixes) do
    case Regex.run(~r/rdfs:subClassOf\s+(\w+):(\w+)/, line) do
      [_, prefix, parent_class] ->
        parent_uri = resolve_uri(prefix, parent_class, prefixes)
        %{class | subclass_of: [parent_uri | class.subclass_of]}
      _ -> class
    end
  end

  defp add_ttl_budget_if_present(class, line) do
    case Regex.run(~r/ttl:budget\s+(\d+)/, line) do
      [_, budget_str] -> %{class | ttl_budget: String.to_integer(budget_str)}
      _ -> class
    end
  end

  defp add_domain_if_present(property, line, prefixes) do
    case Regex.run(~r/rdfs:domain\s+(\w+):(\w+)/, line) do
      [_, prefix, class_name] ->
        domain_uri = resolve_uri(prefix, class_name, prefixes)
        %{property | domain: domain_uri}
      _ -> property
    end
  end

  defp add_range_if_present(property, line, prefixes) do
    case Regex.run(~r/rdfs:range\s+(\w+):(\w+)/, line) do
      [_, prefix, class_name] ->
        range_uri = resolve_uri(prefix, class_name, prefixes)
        %{property | range: range_uri}
      _ -> property
    end
  end

  defp determine_property_direction(property_name) do
    name_lower = String.downcase(property_name)
    
    cond do
      String.contains?(name_lower, "input") or String.contains?(name_lower, "receives") ->
        "input"
      String.contains?(name_lower, "output") or String.contains?(name_lower, "produces") ->
        "output"
      true ->
        "bidirectional"
    end
  end

  defp extract_class_properties(class, lines, prefixes) do
    lines
    |> Enum.filter(&contains_property_for_class?(&1, class))
    |> Enum.map(&extract_property_reference(&1, prefixes))
    |> Enum.reject(&is_nil/1)
  end

  defp contains_property_for_class?(line, class) do
    String.contains?(line, "#{class.prefix}:#{class.name}") and
    (String.contains?(line, "rdfs:domain") or String.contains?(line, "rdfs:range"))
  end

  defp extract_property_reference(line, prefixes) do
    case Regex.run(~r/(\w+):(\w+)\s+rdfs:domain/, line) do
      [_, prefix, property_name] ->
        %{
          name: property_name,
          uri: resolve_uri(prefix, property_name, prefixes),
          relation_type: "domain"
        }
      _ ->
        case Regex.run(~r/(\w+):(\w+)\s+rdfs:range/, line) do
          [_, prefix, property_name] ->
            %{
              name: property_name,
              uri: resolve_uri(prefix, property_name, prefixes),
              relation_type: "range"
            }
          _ -> nil
        end
    end
  end

  defp extract_ontology_name(lines, prefixes) do
    name_line = lines
    |> Enum.find(&String.contains?(&1, "rdf:type owl:Ontology"))
    
    case name_line do
      nil -> "GeneratedOntology"
      line ->
        case Regex.run(~r/(\w+):(\w+)\s+rdf:type\s+owl:Ontology/, line) do
          [_, _prefix, ontology_name] -> ontology_name
          _ -> "GeneratedOntology"
        end
    end
  end

  defp extract_ontology_description(lines, _prefixes) do
    description_line = lines
    |> Enum.find(&String.contains?(&1, "rdfs:comment"))
    
    case description_line do
      nil -> "Generated from TTL ontology"
      line ->
        case Regex.run(~r/rdfs:comment\s+"([^"]+)"/, line) do
          [_, description] -> description
          _ -> "Generated from TTL ontology"
        end
    end
  end

  defp extract_ontology_version(lines, _prefixes) do
    version_line = lines
    |> Enum.find(&String.contains?(&1, "owl:versionInfo"))
    
    case version_line do
      nil -> "1.0"
      line ->
        case Regex.run(~r/owl:versionInfo\s+"([^"]+)"/, line) do
          [_, version] -> version
          _ -> "1.0"
        end
    end
  end

  defp extract_ttl_constraints(constraints) do
    constraints
    |> Enum.filter(&(&1.type == :ttl))
    |> Enum.into(%{}, fn constraint ->
      {"default_budget", constraint.budget}
    end)
  end

  defp extract_shacl_constraints(constraints) do
    constraints
    |> Enum.filter(&(&1.type == :shacl))
    |> Enum.into(%{}, fn constraint ->
      {"constraint_#{:erlang.phash2(constraint.constraint)}", constraint.constraint}
    end)
  end

  defp build_relationships(classes, properties) do
    properties
    |> Enum.filter(&(&1.type == "ObjectProperty"))
    |> Enum.map(fn property ->
      %{
        property: property.name,
        domain: find_class_by_uri(classes, property.domain),
        range: find_class_by_uri(classes, property.range),
        type: :object_property
      }
    end)
    |> Enum.reject(&(is_nil(&1.domain) or is_nil(&1.range)))
  end

  defp find_class_by_uri(classes, uri) when is_binary(uri) do
    Enum.find(classes, &(&1.full_uri == uri))
  end
  
  defp find_class_by_uri(_classes, _uri), do: nil
end