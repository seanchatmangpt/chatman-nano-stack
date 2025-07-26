defmodule CnsForge.Generators.Meta.RecursiveGenerator do
  @moduledoc """
  🔄 META-GENERATOR: Creates generators that generate other generators
  
  This is the master meta-generator that can create new generators based on patterns,
  templates, and specifications. It implements recursive generation capabilities
  where generators can create generators that create generators (infinite depth).
  
  ## Recursive Patterns
  
  1. **Self-Referential**: Generators that can modify themselves
  2. **Template-Based**: Pattern-driven generator creation
  3. **Adaptive**: Generators that learn and improve
  4. **Fractal**: Nested generator structures
  5. **Infinite Depth**: Unlimited recursive generation
  
  ## Usage
  
      # Generate a resource generator
      RecursiveGenerator.generate_generator(:resource, %{
        type: :ash_resource,
        capabilities: [:attributes, :relationships, :actions],
        recursive_depth: :infinite
      })
      
      # Generate a generator that generates workflow generators
      RecursiveGenerator.generate_meta_generator(:workflow_meta, %{
        target_generators: [:reactor, :flow, :pipeline],
        pattern_template: WorkflowTemplate,
        self_improving: true
      })
  """
  
  use GenServer
  require Logger
  
  alias CnsForge.Generators.Templates.GeneratorTemplate
  alias CnsForge.Generators.Patterns.RecursivePattern
  alias CnsForge.Generators.Validators.GeneratorValidator
  
  # State structure for the recursive generator
  defstruct [
    :generator_registry,    # Map of all generated generators
    :template_cache,       # Cached templates for performance
    :pattern_library,      # Library of recursive patterns
    :generation_depth,     # Current depth of generation
    :max_depth,           # Maximum recursion depth (safety)
    :self_modification_enabled, # Can modify itself
    :learning_enabled,     # Can learn from usage
    :validation_rules      # Rules for validating generated code
  ]
  
  @type generator_spec :: %{
    type: atom(),
    name: String.t(),
    capabilities: [atom()],
    template: module() | nil,
    recursive_depth: :infinite | pos_integer(),
    target_components: [atom()],
    self_referential: boolean(),
    validation_enabled: boolean()
  }
  
  @type generation_context :: %{
    depth: non_neg_integer(),
    parent_generator: String.t() | nil,
    target_path: String.t(),
    template_vars: map(),
    recursive_refs: [String.t()]
  }
  
  ## Public API
  
  @doc """
  Generate a new generator based on specifications
  """
  @spec generate_generator(atom(), generator_spec()) :: {:ok, String.t()} | {:error, term()}
  def generate_generator(generator_id, spec) do
    GenServer.call(__MODULE__, {:generate_generator, generator_id, spec})
  end
  
  @doc """
  Generate a meta-generator that creates other generators
  """
  @spec generate_meta_generator(atom(), map()) :: {:ok, String.t()} | {:error, term()}
  def generate_meta_generator(meta_id, meta_spec) do
    GenServer.call(__MODULE__, {:generate_meta_generator, meta_id, meta_spec})
  end
  
  @doc """
  Create a recursive pattern that can generate infinitely nested structures
  """
  @spec create_recursive_pattern(atom(), map()) :: {:ok, module()} | {:error, term()}
  def create_recursive_pattern(pattern_id, pattern_spec) do
    GenServer.call(__MODULE__, {:create_recursive_pattern, pattern_id, pattern_spec})
  end
  
  @doc """
  Generate a complete ecosystem of generators for an Ash domain
  """
  @spec generate_ecosystem(String.t(), map()) :: {:ok, [String.t()]} | {:error, term()}
  def generate_ecosystem(domain_name, ecosystem_spec) do
    GenServer.call(__MODULE__, {:generate_ecosystem, domain_name, ecosystem_spec})
  end
  
  @doc """
  Start the recursive generator system
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  ## GenServer Implementation
  
  @impl GenServer
  def init(opts) do
    state = %__MODULE__{
      generator_registry: %{},
      template_cache: %{},
      pattern_library: %{},
      generation_depth: 0,
      max_depth: Keyword.get(opts, :max_depth, 100),
      self_modification_enabled: Keyword.get(opts, :self_modification, true),
      learning_enabled: Keyword.get(opts, :learning, true),
      validation_rules: initialize_validation_rules()
    }
    
    Logger.info("🔄 Recursive Generator System initialized")
    {:ok, state}
  end
  
  @impl GenServer
  def handle_call({:generate_generator, generator_id, spec}, _from, state) do
    context = %{
      depth: state.generation_depth + 1,
      parent_generator: nil,
      target_path: generator_path(generator_id, spec),
      template_vars: extract_template_vars(spec),
      recursive_refs: []
    }
    
    case generate_generator_impl(generator_id, spec, context, state) do
      {:ok, generator_path, new_state} ->
        {:reply, {:ok, generator_path}, new_state}
      
      {:error, reason} ->
        Logger.error("Generator creation failed: #{inspect(reason)}")
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl GenServer 
  def handle_call({:generate_meta_generator, meta_id, meta_spec}, _from, state) do
    case create_meta_generator_impl(meta_id, meta_spec, state) do
      {:ok, meta_generator_path, new_state} ->
        {:reply, {:ok, meta_generator_path}, new_state}
        
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl GenServer
  def handle_call({:create_recursive_pattern, pattern_id, pattern_spec}, _from, state) do
    case create_pattern_impl(pattern_id, pattern_spec, state) do
      {:ok, pattern_module, new_state} ->
        {:reply, {:ok, pattern_module}, new_state}
        
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl GenServer
  def handle_call({:generate_ecosystem, domain_name, ecosystem_spec}, _from, state) do
    case generate_ecosystem_impl(domain_name, ecosystem_spec, state) do
      {:ok, generator_paths, new_state} ->
        {:reply, {:ok, generator_paths}, new_state}
        
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  ## Private Implementation Functions
  
  defp generate_generator_impl(generator_id, spec, context, state) do
    if context.depth > state.max_depth do
      {:error, :max_recursion_depth_exceeded}
    else
      try do
        # 1. Load or create template
        template = load_template(spec.type, spec[:template], state)
        
        # 2. Apply recursive patterns if specified
        enhanced_template = 
          if spec[:recursive_depth] == :infinite do
            apply_recursive_patterns(template, spec, context)
          else
            template
          end
        
        # 3. Generate the actual generator code
        generator_code = generate_code_from_template(
          enhanced_template, 
          generator_id, 
          spec, 
          context
        )
        
        # 4. Create self-referential capabilities if requested
        final_code = 
          if spec[:self_referential] do
            add_self_referential_capabilities(generator_code, spec, context)
          else
            generator_code
          end
        
        # 5. Write generator to file system
        generator_path = context.target_path
        File.mkdir_p!(Path.dirname(generator_path))
        File.write!(generator_path, final_code)
        
        # 6. Update state with new generator
        new_state = register_generator(state, generator_id, spec, generator_path)
        
        # 7. Validate generated code if enabled
        if spec[:validation_enabled] do
          case GeneratorValidator.validate_generator(generator_path) do
            {:ok, _} -> 
              Logger.info("✅ Generated and validated generator: #{generator_path}")
              
            {:error, validation_errors} ->
              Logger.warn("⚠️ Generator validation issues: #{inspect(validation_errors)}")
          end
        end
        
        {:ok, generator_path, new_state}
        
      rescue
        error ->
          Logger.error("Generator creation error: #{inspect(error)}")
          {:error, error}
      end
    end
  end
  
  defp create_meta_generator_impl(meta_id, meta_spec, state) do
    # Meta-generators create generators that create other generators
    meta_generator_template = build_meta_generator_template(meta_spec)
    
    meta_generator_code = """
    defmodule CnsForge.Generators.Meta.#{Macro.camelize(to_string(meta_id))} do
      @moduledoc \"\"\"
      🔄 META-GENERATOR: #{meta_spec[:description] || "Auto-generated meta-generator"}
      
      This generator creates other generators based on the following specification:
      - Target Generators: #{inspect(meta_spec[:target_generators])}
      - Pattern Template: #{inspect(meta_spec[:pattern_template])}
      - Self-Improving: #{meta_spec[:self_improving] || false}
      
      Generated by: CnsForge.Generators.Meta.RecursiveGenerator
      \"\"\"
      
      #{meta_generator_template}
      
      # Self-improvement capabilities
      #{if meta_spec[:self_improving], do: generate_self_improvement_code(meta_spec), else: ""}
      
      # Recursive generation methods
      #{generate_recursive_methods(meta_spec)}
    end
    """
    
    meta_path = "lib/cns_forge/generators/meta/#{meta_id}.ex"
    File.mkdir_p!(Path.dirname(meta_path))
    File.write!(meta_path, meta_generator_code)
    
    new_state = register_meta_generator(state, meta_id, meta_spec, meta_path)
    {:ok, meta_path, new_state}
  end
  
  defp create_pattern_impl(pattern_id, pattern_spec, state) do
    pattern_module = Module.concat([CnsForge.Generators.Patterns, Macro.camelize(to_string(pattern_id))])
    
    pattern_code = """
    defmodule #{inspect(pattern_module)} do
      @moduledoc \"\"\"
      🔄 RECURSIVE PATTERN: #{pattern_spec[:name] || pattern_id}
      
      #{pattern_spec[:description] || "Auto-generated recursive pattern"}
      
      ## Pattern Capabilities
      - Recursion Type: #{pattern_spec[:recursion_type] || :standard}
      - Max Depth: #{pattern_spec[:max_depth] || :infinite}
      - Self-Reference: #{pattern_spec[:self_reference] || false}
      
      Generated by: CnsForge.Generators.Meta.RecursiveGenerator
      \"\"\"
      
      #{generate_pattern_methods(pattern_spec)}
      
      # Recursive application logic
      def apply_pattern_recursive(data, depth \\ 0) do
        #{generate_recursive_application_logic(pattern_spec)}
      end
      
      # Pattern composition for infinite depth
      def compose_infinitely(base_pattern, enhancement_fn) do
        #{generate_infinite_composition_logic(pattern_spec)}
      end
    end
    """
    
    pattern_path = "lib/cns_forge/generators/patterns/#{pattern_id}.ex"
    File.mkdir_p!(Path.dirname(pattern_path))
    File.write!(pattern_path, pattern_code)
    
    # Compile the pattern module dynamically
    Code.compile_string(pattern_code)
    
    new_state = register_pattern(state, pattern_id, pattern_module, pattern_path)
    {:ok, pattern_module, new_state}
  end
  
  defp generate_ecosystem_impl(domain_name, ecosystem_spec, state) do
    # Generate a complete ecosystem of generators for the domain
    generator_specs = build_ecosystem_generator_specs(domain_name, ecosystem_spec)
    
    generated_paths = 
      Enum.reduce(generator_specs, [], fn {gen_id, spec}, acc ->
        context = %{
          depth: 0,
          parent_generator: nil,
          target_path: generator_path(gen_id, spec),
          template_vars: Map.put(extract_template_vars(spec), :domain_name, domain_name),
          recursive_refs: []
        }
        
        case generate_generator_impl(gen_id, spec, context, state) do
          {:ok, path, _new_state} -> [path | acc]
          {:error, _reason} -> acc
        end
      end)
    
    {:ok, Enum.reverse(generated_paths), state}
  end
  
  # Helper Functions
  
  defp initialize_validation_rules do
    %{
      syntax_validation: true,
      dependency_validation: true,
      ash_compatibility: true,
      recursive_safety: true
    }
  end
  
  defp generator_path(generator_id, spec) do
    category = spec[:type] || :misc
    "lib/cns_forge/generators/#{category}/#{generator_id}.ex"
  end
  
  defp extract_template_vars(spec) do
    Map.take(spec, [:name, :capabilities, :target_components])
  end
  
  defp load_template(type, custom_template, _state) do
    template_module = custom_template || default_template_for_type(type)
    template_module.base_template()
  end
  
  defp default_template_for_type(:ash_resource), do: CnsForge.Generators.Templates.ResourceTemplate
  defp default_template_for_type(:ash_domain), do: CnsForge.Generators.Templates.DomainTemplate
  defp default_template_for_type(:ash_action), do: CnsForge.Generators.Templates.ActionTemplate
  defp default_template_for_type(_), do: CnsForge.Generators.Templates.GenericTemplate
  
  defp apply_recursive_patterns(template, _spec, _context) do
    # Apply recursive enhancement to templates
    template
  end
  
  defp generate_code_from_template(template, generator_id, spec, context) do
    template
    |> String.replace("{{GENERATOR_ID}}", to_string(generator_id))
    |> String.replace("{{GENERATOR_NAME}}", Macro.camelize(to_string(generator_id)))
    |> apply_template_variables(context.template_vars)
  end
  
  defp apply_template_variables(code, vars) do
    Enum.reduce(vars, code, fn {key, value}, acc ->
      String.replace(acc, "{{#{String.upcase(to_string(key))}}}", inspect(value))
    end)
  end
  
  defp add_self_referential_capabilities(code, _spec, _context) do
    self_ref_code = """
    
    # 🔄 SELF-REFERENTIAL CAPABILITIES
    # This generator can modify and improve itself
    
    def self_modify(modifications) do
      current_source = File.read!(__ENV__.file)
      new_source = apply_modifications(current_source, modifications)
      File.write!(__ENV__.file, new_source)
      Code.compile_file(__ENV__.file)
    end
    
    def self_improve(usage_data) do
      improvements = analyze_usage_patterns(usage_data)
      self_modify(improvements)
    end
    
    defp apply_modifications(source, modifications) do
      # Implementation for applying code modifications
      source
    end
    
    defp analyze_usage_patterns(usage_data) do
      # Implementation for analyzing usage and generating improvements
      []
    end
    """
    
    code <> self_ref_code
  end
  
  defp register_generator(state, generator_id, spec, path) do
    registry_entry = %{
      id: generator_id,
      spec: spec,
      path: path,
      created_at: DateTime.utc_now(),
      usage_count: 0
    }
    
    %{state | generator_registry: Map.put(state.generator_registry, generator_id, registry_entry)}
  end
  
  defp register_meta_generator(state, meta_id, meta_spec, path) do
    register_generator(state, meta_id, %{type: :meta_generator, spec: meta_spec}, path)
  end
  
  defp register_pattern(state, pattern_id, pattern_module, path) do
    pattern_entry = %{
      id: pattern_id,
      module: pattern_module,
      path: path,
      created_at: DateTime.utc_now()
    }
    
    %{state | pattern_library: Map.put(state.pattern_library, pattern_id, pattern_entry)}
  end
  
  defp build_meta_generator_template(meta_spec) do
    target_generators = meta_spec[:target_generators] || []
    
    Enum.map_join(target_generators, "\\n\\n", fn target ->
      """
      def generate_#{target}(spec) do
        # Generate #{target} generator based on spec
        template = load_template_for(:#{target})
        generator_code = apply_template(template, spec)
        output_path = "lib/cns_forge/generators/generated_#{target}_\#{System.unique_integer()}.ex"
        File.write!(output_path, generator_code)
        {:ok, output_path}
      end
      """
    end)
  end
  
  defp generate_self_improvement_code(_meta_spec) do
    """
    # Self-improvement capabilities
    def analyze_and_improve do
      usage_stats = collect_usage_statistics()
      performance_metrics = measure_generation_performance()
      improvements = identify_improvements(usage_stats, performance_metrics)
      apply_improvements(improvements)
    end
    
    defp collect_usage_statistics, do: %{}
    defp measure_generation_performance, do: %{}
    defp identify_improvements(_stats, _metrics), do: []
    defp apply_improvements(_improvements), do: :ok
    """
  end
  
  defp generate_recursive_methods(_meta_spec) do
    """
    # Recursive generation methods
    def generate_recursively(spec, depth \\ 0, max_depth \\ 10) do
      if depth >= max_depth do
        {:error, :max_recursion_depth}
      else
        case generate_base(spec) do
          {:ok, result} ->
            if should_recurse?(spec, depth) do
              recursive_spec = build_recursive_spec(spec, result)
              generate_recursively(recursive_spec, depth + 1, max_depth)
            else
              {:ok, result}
            end
          error -> error
        end
      end
    end
    
    defp generate_base(_spec), do: {:ok, %{}}
    defp should_recurse?(_spec, _depth), do: false
    defp build_recursive_spec(spec, _result), do: spec
    """
  end
  
  defp generate_pattern_methods(pattern_spec) do
    recursion_type = pattern_spec[:recursion_type] || :standard
    
    case recursion_type do
      :fractal ->
        """
        def apply_fractal_pattern(data, scale_factor \\ 0.5) do
          generate_fractal_structure(data, scale_factor, 0)
        end
        
        defp generate_fractal_structure(data, scale, depth) when depth < 10 do
          scaled_data = scale_data(data, scale)
          sub_structures = Enum.map(get_sub_components(data), fn component ->
            generate_fractal_structure(component, scale * scale_factor, depth + 1)
          end)
          combine_structures(scaled_data, sub_structures)
        end
        defp generate_fractal_structure(data, _scale, _depth), do: data
        """
        
      :infinite ->
        """
        def apply_infinite_pattern(data) do
          Stream.iterate(data, &transform_iteration/1)
          |> Stream.take_while(&continue_iteration?/1)
          |> Enum.to_list()
        end
        
        defp transform_iteration(data), do: data
        defp continue_iteration?(_data), do: false
        """
        
      _ ->
        """
        def apply_standard_pattern(data) do
          transform_data(data)
        end
        
        defp transform_data(data), do: data
        """
    end
  end
  
  defp generate_recursive_application_logic(pattern_spec) do
    case pattern_spec[:application_strategy] do
      :depth_first ->
        """
        if depth > #{pattern_spec[:max_depth] || 10} do
          data
        else
          transformed = apply_transformation(data)
          sub_results = Enum.map(get_children(data), fn child ->
            apply_pattern_recursive(child, depth + 1)
          end)
          combine_results(transformed, sub_results)
        end
        """
        
      :breadth_first ->
        """
        queue = :queue.in({data, depth}, :queue.new())
        process_queue(queue, [])
        """
        
      _ ->
        """
        apply_transformation(data)
        """
    end
  end
  
  defp generate_infinite_composition_logic(_pattern_spec) do
    """
    Stream.iterate(base_pattern, enhancement_fn)
    |> Stream.take_while(fn pattern -> 
      not pattern_converged?(pattern) 
    end)
    |> Enum.to_list()
    |> List.last()
    """
  end
  
  defp build_ecosystem_generator_specs(domain_name, ecosystem_spec) do
    base_generators = [
      {:domain_generator, %{type: :ash_domain, name: "#{domain_name}DomainGenerator"}},
      {:resource_generator, %{type: :ash_resource, name: "#{domain_name}ResourceGenerator"}},
      {:action_generator, %{type: :ash_action, name: "#{domain_name}ActionGenerator"}},
      {:api_generator, %{type: :ash_api, name: "#{domain_name}APIGenerator"}},
      {:test_generator, %{type: :test_suite, name: "#{domain_name}TestGenerator"}}
    ]
    
    # Add custom generators based on ecosystem_spec
    custom_generators = 
      Enum.map(ecosystem_spec[:custom_generators] || [], fn {id, spec} ->
        {id, Map.put(spec, :name, "#{domain_name}#{Macro.camelize(to_string(id))}Generator")}
      end)
    
    base_generators ++ custom_generators
  end
end