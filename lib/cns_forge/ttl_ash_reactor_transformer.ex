defmodule CnsForge.TTLAshReactorTransformer do
  @moduledoc """
  🛡️ ULTRA-CLEAN TTL → Ash.Reactor transformation
  RED TEAM ELIMINATED - PURE FUNCTIONAL APPROACH
  """
  
  require Logger
  
  def transform_ttl(ttl_content) when is_binary(ttl_content) do
    Logger.info("Starting CLEAN TTL transformation")
    
    with {:ok, parsed} <- parse_ttl(ttl_content),
         {:ok, resources} <- generate_ash_resources(parsed),
         {:ok, reactors} <- generate_ash_reactors(parsed, resources) do
      
      result = %{
        parsed_ontology: parsed,
        resources: resources,
        reactors: reactors,
        domain: generate_simple_domain(),
        generated_files: generate_file_list(resources, reactors)
      }
      
      Logger.info("CLEAN TTL transformation completed")
      {:ok, result}
    else
      {:error, reason} -> 
        Logger.error("TTL transformation failed: #{inspect(reason)}")
        {:error, reason}
    end
  end
  
  def parse_ttl(ttl_content) do
    classes = extract_classes(ttl_content)
    
    parsed = %{
      prefixes: %{},
      classes: classes,
      properties: [],
      relationships: []
    }
    
    {:ok, parsed}
  end
  
  def generate_ash_resources(%{classes: classes}) do
    resources = Enum.map(classes, fn class ->
      %{
        class: class,
        module_name: "CnsForge.TTLResources.#{class.name}",
        code: """
defmodule CnsForge.TTLResources.#{class.name} do
  @moduledoc "Clean Ash.Resource for #{class.name}"
  
  use Ash.Resource,
    domain: CnsForge.TTLDomain,
    data_layer: Ash.DataLayer.Ets
    
  attributes do
    uuid_primary_key :id
    attribute :ttl_uri, :string, public?: true
  end
  
  actions do
    defaults [:read, :create, :update, :destroy]
  end
end
"""
      }
    end)
    
    {:ok, resources}
  end
  
  def generate_ash_reactors(%{classes: classes}, _resources) do
    main_reactor = %{
      name: "CnsForge.TTLMainReactor",
      code: """
defmodule CnsForge.TTLMainReactor do
  @moduledoc "Clean Ash.Reactor workflow"
  
  use Reactor
  
  input :ontology_data
  
  step :transform_classes do
    argument :data, input(:ontology_data)
    
    run fn %{data: data}, _context ->
      {:ok, %{transformed_classes: #{length(classes)}}}
    end
  end
  
  return :transform_classes
end
"""
    }
    
    {:ok, [main_reactor]}
  end
  
  defp generate_simple_domain do
    """
defmodule CnsForge.TTLDomain do
  @moduledoc "Clean TTL Domain"
  use Ash.Domain
  
  authorization do
    authorize :when_requested
  end
end
"""
  end
  
  defp extract_classes(ttl_content) do
    class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
    
    # 🔄 RECURSIVE FIX: Filter out commented lines before regex scanning
    filtered_content = ttl_content
    |> String.split("\n")
    |> Enum.reject(fn line ->
      trimmed = String.trim(line)
      String.starts_with?(trimmed, "#") or String.starts_with?(trimmed, "##")
    end)
    |> Enum.join("\n")
    
    Regex.scan(class_regex, filtered_content)
    |> Enum.map(fn [_, class_uri] ->
      %{
        uri: class_uri,
        name: extract_local_name(class_uri),
        module_name: generate_module_name(class_uri),
        attributes: []
      }
    end)
  end
  
  defp extract_local_name(uri) do
    case String.split(uri, ":") do
      [_prefix, name] -> name
      [name] -> name
    end
  end
  
  defp generate_module_name(class_uri) do
    local_name = extract_local_name(class_uri)
    "CnsForge.TTLResources.#{local_name}"
  end
  
  defp generate_file_list(resources, reactors) do
    resource_files = Enum.map(resources, fn resource ->
      %{
        name: "lib/cns_forge/ttl_resources/#{String.downcase(resource.class.name)}.ex",
        content: resource.code
      }
    end)
    
    reactor_files = Enum.map(reactors, fn reactor ->
      %{
        name: "lib/cns_forge/reactors/#{String.downcase(String.replace(reactor.name, "CnsForge.", ""))}.ex",
        content: reactor.code
      }
    end)
    
    domain_file = %{
      name: "lib/cns_forge/ttl_domain.ex",
      content: generate_simple_domain()
    }
    
    [domain_file | resource_files ++ reactor_files]
  end
end