defmodule CnsForge.TTLAshReactorTransformer do
  @moduledoc """
  Minimal viable TTL → Ash.Reactor transformation system
  
  Converts TTL ontology classes and properties into:
  1. Ash.Resource definitions
  2. Ash.Reactor workflows 
  3. Working Elixir modules that compile and execute
  
  Focus: 80/20 transformation that actually works
  """
  
  require Logger
  
  @doc """
  Transform TTL ontology into complete Ash.Reactor system
  
  ## Example
      iex> ttl = '''
      ...> @prefix cns: <http://cns-forge.org/ontology#> .
      ...> @prefix owl: <http://www.w3.org/2002/07/owl#> .
      ...> 
      ...> cns:BitActor a owl:Class .
      ...> cns:Signal a owl:Class .
      ...> cns:processes a owl:ObjectProperty ;
      ...>   owl:domain cns:BitActor ;
      ...>   owl:range cns:Signal .
      ...> '''
      iex> CnsForge.TTLAshReactorTransformer.transform_ttl(ttl)
      {:ok, %{resources: [...], reactors: [...], domain: "..."}}
  """
  def transform_ttl(ttl_content) when is_binary(ttl_content) do
    Logger.info("Starting TTL → Ash.Reactor transformation")
    
    with {:ok, parsed} <- parse_ttl(ttl_content),
         {:ok, resources} <- generate_ash_resources(parsed),
         {:ok, reactors} <- generate_ash_reactors(parsed, resources),
         {:ok, domain} <- generate_ash_domain(resources, reactors) do
      
      result = %{
        parsed_ontology: parsed,
        resources: resources,
        reactors: reactors,
        domain: domain,
        generated_files: write_generated_files(resources, reactors, domain)
      }
      
      Logger.info("TTL transformation completed successfully")
      {:ok, result}
    else
      {:error, reason} -> 
        Logger.error("TTL transformation failed: #{inspect(reason)}")
        {:error, reason}
    end
  end
  
  @doc "Parse TTL content into structured data"
  def parse_ttl(ttl_content) do
    # Extract prefixes
    prefixes = extract_prefixes(ttl_content)
    
    # Extract classes
    classes = extract_classes(ttl_content, prefixes)
    
    # Extract properties  
    properties = extract_properties(ttl_content, prefixes)
    
    # Extract relationships
    relationships = extract_relationships(properties)
    
    parsed = %{
      prefixes: prefixes,
      classes: classes,
      properties: properties,
      relationships: relationships
    }
    
    Logger.debug("Parsed TTL: #{inspect(parsed)}")
    {:ok, parsed}
  end
  
  @doc "Generate Ash.Resource definitions from TTL classes"
  def generate_ash_resources(%{classes: classes, relationships: relationships}) do
    resources = Enum.map(classes, fn class ->
      generate_single_resource(class, relationships)
    end)
    
    {:ok, resources}
  end
  
  @doc "Generate Ash.Reactor workflows from TTL semantics"
  def generate_ash_reactors(%{classes: classes, properties: properties}, resources) do
    # Generate main ontology processing reactor
    main_reactor = generate_main_reactor(classes, properties, resources)
    
    # Generate per-class processing reactors
    class_reactors = Enum.map(classes, fn class ->
      generate_class_reactor(class, properties, resources)
    end)
    
    all_reactors = [main_reactor | class_reactors]
    {:ok, all_reactors}
  end
  
  @doc "Generate Ash.Domain that orchestrates resources and reactors"
  def generate_ash_domain(resources, _reactors) do
    domain_code = """
    defmodule CnsForge.TTLDomain do
      @moduledoc \"\"\"
      Auto-generated Ash.Domain from TTL ontology transformation
      Orchestrates all resources and reactors for ontology processing
      \"\"\"
      
      use Ash.Domain
      
      resources do
    #{Enum.map_join(resources, "\n", fn resource -> "    resource #{resource.module_name}" end)}
      end
      
      authorization do
        authorize :when_requested
      end
      
      # TTL-bounded execution context
      def process_ontology_with_ttl_bounds(input_data, ttl_constraints \\\\ %{}) do
        context = %{
          ttl_constraints: ttl_constraints,
          execution_start: System.monotonic_time(:nanosecond),
          max_execution_ns: Map.get(ttl_constraints, :max_execution_ns, 1_000_000_000)
        }
        
        case Reactor.run(CnsForge.TTLMainReactor, input_data, context) do
          {:ok, result} -> 
            execution_time = System.monotonic_time(:nanosecond) - context.execution_start
            Logger.info("TTL ontology processed in \#{execution_time}ns")
            {:ok, Map.put(result, :execution_time_ns, execution_time)}
          
          {:error, reason} -> 
            {:error, reason}
        end
      end
    end
    """
    
    {:ok, domain_code}
  end
  
  # Private implementation functions
  
  defp extract_prefixes(ttl_content) do
    # Extract @prefix declarations
    prefix_regex = ~r/@prefix\s+(\w+):\s+<([^>]+)>\s*\./
    
    Regex.scan(prefix_regex, ttl_content)
    |> Enum.into(%{}, fn [_, prefix, uri] -> {prefix, uri} end)
  end
  
  defp extract_classes(ttl_content, _prefixes) do
    # Extract owl:Class declarations
    class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
    
    Regex.scan(class_regex, ttl_content)
    |> Enum.map(fn [_, class_uri] ->
      %{
        uri: class_uri,
        name: extract_local_name(class_uri),
        module_name: generate_module_name(class_uri),
        attributes: extract_class_attributes(class_uri, ttl_content)
      }
    end)
  end
  
  defp extract_properties(ttl_content, _prefixes) do
    # Extract owl:ObjectProperty and owl:DatatypeProperty
    property_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:(?:Object|Datatype)Property/
    
    Regex.scan(property_regex, ttl_content)
    |> Enum.map(fn [_, property_uri] ->
      %{
        uri: property_uri,
        name: extract_local_name(property_uri),
        domain: extract_property_domain(property_uri, ttl_content),
        range: extract_property_range(property_uri, ttl_content)
      }
    end)
  end
  
  defp extract_relationships(properties) do
    Enum.filter(properties, fn property ->
      property.domain && property.range
    end)
    |> Enum.map(fn property ->
      %{
        property: property.name,
        from: extract_local_name(property.domain),
        to: extract_local_name(property.range),
        relationship_type: :belongs_to
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
  
  defp extract_class_attributes(_class_uri, _ttl_content) do
    # For minimal implementation, provide basic attributes
    [
      %{name: :id, type: :uuid_primary_key},
      %{name: :ttl_uri, type: :string},
      %{name: :created_at, type: :utc_datetime_usec},
      %{name: :updated_at, type: :utc_datetime_usec}
    ]
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
  
  defp generate_single_resource(class, relationships) do
    # Find relationships for this class
    class_relationships = Enum.filter(relationships, fn rel ->
      rel.from == class.name || rel.to == class.name
    end)
    
    resource_code = """
    defmodule #{class.module_name} do
      @moduledoc \"\"\"
      Auto-generated Ash.Resource for TTL class: #{class.uri}
      Represents semantic concept with TTL-bounded execution
      \"\"\"
      
      use Ash.Resource,
        domain: CnsForge.TTLDomain,
        data_layer: Ash.DataLayer.Ets
        
      ets do
        table :ttl_#{String.downcase(class.name)}s
      end
      
      actions do
        defaults [:read, :destroy]
        
        create :create_from_ttl do
          accept [:ttl_uri]
          
          change fn changeset, _context ->
            changeset
            |> Ash.Changeset.force_change_attribute(:created_at, DateTime.utc_now())
            |> Ash.Changeset.force_change_attribute(:updated_at, DateTime.utc_now())
          end
        end
        
        update :process_semantics do
          accept []
          
          change fn changeset, context ->
            # TTL-bounded semantic processing
            ttl_constraints = get_in(context, [:private, :ttl_constraints]) || %{}
            max_processing_ns = Map.get(ttl_constraints, :max_processing_ns, 1_000_000)
            
            start_time = System.monotonic_time(:nanosecond)
            
            # Simulate semantic processing (replace with actual logic)
            :timer.sleep(1) # Minimal processing time
            
            processing_time = System.monotonic_time(:nanosecond) - start_time
            
            if processing_time > max_processing_ns do
              Ash.Changeset.add_error(changeset, 
                field: :base, 
                message: "TTL constraint violation: processing took \#{processing_time}ns, max allowed \#{max_processing_ns}ns"
              )
            else
              changeset
              |> Ash.Changeset.force_change_attribute(:updated_at, DateTime.utc_now())
            end
          end
        end
      end
      
      attributes do
    #{Enum.map_join(class.attributes, "\n", &generate_attribute/1)}
      end
      
    #{generate_relationships(class_relationships, class.name)}
      
      # TTL semantic validation
      validations do
        validate present(:ttl_uri), message: "TTL URI is required for semantic binding"
      end
      
      # Telemetry for TTL processing
      changes do
        change after_action(fn changeset, result, _context ->
          :telemetry.execute(
            [:cns_forge, :ttl, :resource_processed],
            %{processing_time: 1},
            %{resource: "#{class.name}", action: changeset.action.name}
          )
          
          {:ok, result}
        end)
      end
    end
    """
    
    %{
      class: class,
      module_name: class.module_name,
      code: resource_code
    }
  end
  
  defp generate_attribute(%{name: :id, type: :uuid_primary_key}), do: "    uuid_primary_key :id"
  defp generate_attribute(%{name: name, type: type}) do
    "    attribute :#{name}, :#{type} do\n      public? true\n    end"
  end
  
  defp generate_relationships([], _class_name), do: ""
  defp generate_relationships(relationships, class_name) do
    relationship_code = relationships
    |> Enum.filter(fn rel -> rel.from == class_name end)
    |> Enum.map(fn rel ->
      "    belongs_to :#{String.downcase(rel.to)}, CnsForge.TTLResources.#{rel.to}"
    end)
    |> Enum.join("\n")
    
    if relationship_code != "" do
      "\n  relationships do\n#{relationship_code}\n  end\n"
    else
      ""
    end
  end
  
  defp generate_main_reactor(classes, _properties, _resources) do
    main_reactor_code = """
    defmodule CnsForge.TTLMainReactor do
      @moduledoc \"\"\"
      Main Ash.Reactor workflow for TTL ontology processing
      Orchestrates semantic processing with TTL-bounded execution
      \"\"\"
      
      use Reactor
      
      input :ontology_data
      input :ttl_constraints, default: %{}
      
      # Initialize TTL execution context
      step :initialize_ttl_context do
        argument :constraints, input(:ttl_constraints)
        
        run fn %{constraints: constraints}, context ->
          ttl_context = %{
            max_total_execution_ns: Map.get(constraints, :max_total_execution_ns, 10_000_000_000),
            max_step_execution_ns: Map.get(constraints, :max_step_execution_ns, 1_000_000_000),
            execution_start: System.monotonic_time(:nanosecond),
            processed_classes: []
          }
          
          {:ok, ttl_context}
        end
      end
      
      # Validate ontology structure
      step :validate_ontology do
        argument :data, input(:ontology_data)
        argument :context, result(:initialize_ttl_context)
        
        run fn %{data: data, context: ttl_context}, _context ->
          step_start = System.monotonic_time(:nanosecond)
          
          # Basic ontology validation
          valid = is_map(data) and Map.has_key?(data, :classes)
          
          step_time = System.monotonic_time(:nanosecond) - step_start
          
          if step_time > ttl_context.max_step_execution_ns do
            {:error, "TTL constraint violation in ontology validation: \#{step_time}ns > \#{ttl_context.max_step_execution_ns}ns"}
          else
            {:ok, %{valid: valid, validation_time_ns: step_time}}
          end
        end
      end
      
    #{Enum.map_join(classes, "\n", &generate_class_processing_step/1)}
      
      # Aggregate results with TTL bounds check
      step :aggregate_results do
        argument :validation, result(:validate_ontology)
    #{Enum.map_join(classes, "\n", fn class -> "    argument :#{String.downcase(class.name)}_result, result(:process_#{String.downcase(class.name)})" end)}
        argument :ttl_context, result(:initialize_ttl_context)
        
        run fn arguments, _context ->
          total_execution_time = System.monotonic_time(:nanosecond) - arguments.ttl_context.execution_start
          
          results = %{
            validation: arguments.validation,
    #{Enum.map_join(classes, ",\n", fn class -> "        #{String.downcase(class.name)}: arguments.#{String.downcase(class.name)}_result" end)},
            total_execution_time_ns: total_execution_time,
            ttl_compliance: total_execution_time <= arguments.ttl_context.max_total_execution_ns
          }
          
          if results.ttl_compliance do
            {:ok, results}
          else
            {:error, "TTL constraint violation: total execution \#{total_execution_time}ns > \#{arguments.ttl_context.max_total_execution_ns}ns"}
          end
        end
      end
      
      return :aggregate_results
    end
    """
    
    %{
      name: "CnsForge.TTLMainReactor",
      code: main_reactor_code
    }
  end
  
  defp generate_class_processing_step(class) do
    """
      # Process #{class.name} with TTL bounds
      step :process_#{String.downcase(class.name)} do
        argument :ontology_data, input(:ontology_data)
        argument :ttl_context, result(:initialize_ttl_context)
        depends_on :validate_ontology
        
        run fn %{ontology_data: data, ttl_context: context}, _reactor_context ->
          step_start = System.monotonic_time(:nanosecond)
          
          # Create resource instance for #{class.name}
          case Ash.create(#{class.module_name}, %{ttl_uri: "#{class.uri}"}, 
                         domain: CnsForge.TTLDomain,
                         private: %{ttl_constraints: %{max_processing_ns: context.max_step_execution_ns}}) do
            {:ok, instance} ->
              # Process semantics with TTL bounds
              case Ash.update(instance, :process_semantics, %{}, domain: CnsForge.TTLDomain) do
                {:ok, processed_instance} ->
                  step_time = System.monotonic_time(:nanosecond) - step_start
                  
                  result = %{
                    instance: processed_instance,
                    processing_time_ns: step_time,
                    ttl_compliant: step_time <= context.max_step_execution_ns
                  }
                  
                  {:ok, result}
                
                {:error, reason} ->
                  {:error, "Failed to process #{class.name} semantics: \#{inspect(reason)}"}
              end
            
            {:error, reason} ->
              {:error, "Failed to create #{class.name} instance: \#{inspect(reason)}"}
          end
        end
      end"""
  end
  
  defp generate_class_reactor(class, _properties, _resources) do
    code_content = """
defmodule CnsForge.TTL#{class.name}Reactor do
  @moduledoc \"\"\"
  Specialized Ash.Reactor for #{class.name} semantic processing
  Focuses on class-specific TTL-bounded operations
  \"\"\"
  
  use Reactor
  
  input :#{String.downcase(class.name)}_data
  input :ttl_constraints, default: %{}
  
  step :process_#{String.downcase(class.name)}_semantics do
    argument :data, input(:#{String.downcase(class.name)}_data)
    argument :constraints, input(:ttl_constraints)
    
    run fn %{data: data, constraints: constraints}, _context ->
      max_processing_ns = Map.get(constraints, :max_processing_ns, 1_000_000)
      start_time = System.monotonic_time(:nanosecond)
      
      # Specific semantic processing for #{class.name}
      # This would be customized based on the class semantics
      result = %{
        processed_data: data,
        semantic_type: "#{class.name}",
        ttl_uri: "#{class.uri}"
      }
      
      processing_time = System.monotonic_time(:nanosecond) - start_time
      
      if processing_time > max_processing_ns do
        {:error, "TTL constraint violation for #{class.name}: \#{processing_time}ns > \#{max_processing_ns}ns"}
      else
        {:ok, Map.put(result, :processing_time_ns, processing_time)}
      end
    end
  end
  
  return :process_#{String.downcase(class.name)}_semantics
end
"""
    
    %{
      name: "CnsForge.TTL#{class.name}Reactor",
      code: code_content
    }
  end
  
  defp write_generated_files(resources, reactors, domain) do
    base_path = "/Users/sac/cns/lib/cns_forge/generated"
    File.mkdir_p!(base_path)
    
    # Write resource files
    resource_files = Enum.map(resources, fn resource ->
      file_path = Path.join(base_path, "#{String.downcase(resource.class.name)}_resource.ex")
      File.write!(file_path, resource.code)
      file_path
    end)
    
    # Write reactor files
    reactor_files = Enum.map(reactors, fn reactor ->
      module_name = reactor.name |> String.split(".") |> List.last() |> String.downcase()
      file_path = Path.join(base_path, "#{module_name}.ex")
      File.write!(file_path, reactor.code)
      file_path
    end)
    
    # Write domain file
    domain_path = Path.join(base_path, "ttl_domain.ex")
    File.write!(domain_path, domain)
    
    # Return all file paths
    resource_files ++ reactor_files ++ [domain_path]
  end
end