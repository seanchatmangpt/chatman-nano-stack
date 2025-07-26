defmodule Mix.Tasks.Cns.Gen.FromTtlSimple do
  @moduledoc """
  🐢 Simple version of TTL to Ash generator (no Igniter dependency)
  
  ## Example
      mix cns.gen.from_ttl_simple examples/cybersecurity.ttl MyApp.Cybersecurity
  """
  
  @shortdoc "Generates Ash Resources/Domains from TTL (simple version)"
  
  use Mix.Task
  
  alias CnsForge.TTLAshReactorTransformer
  
  def run(argv) do
    case argv do
      [ttl_file, domain_name] ->
        generate_from_ttl(ttl_file, domain_name)
      _ ->
        Mix.shell().error("""
        Usage: mix cns.gen.from_ttl_simple <ttl_file> <domain_module>
        
        Example:
          mix cns.gen.from_ttl_simple examples/cybersecurity.ttl MyApp.Cybersecurity
        """)
    end
  end
  
  defp generate_from_ttl(ttl_file, domain_name) do
    Mix.shell().info("🐢 Reading TTL file: #{ttl_file}")
    
    with {:ok, ttl_content} <- File.read(ttl_file),
         {:ok, transformation} <- TTLAshReactorTransformer.transform_ttl(ttl_content) do
      
      Mix.shell().info("✅ Successfully parsed TTL with #{length(transformation.resources)} classes")
      
      # Parse domain module parts
      domain_parts = String.split(domain_name, ".")
      app_name = domain_parts |> hd() |> Macro.underscore() |> String.to_atom()
      
      # Create output directory structure
      base_path = Path.join(["lib", Macro.underscore(hd(domain_parts))])
      domain_path = Path.join([base_path | Enum.map(tl(domain_parts), &Macro.underscore/1)])
      resources_path = Path.join(domain_path, "resources")
      reactors_path = Path.join(domain_path, "reactors")
      
      File.mkdir_p!(resources_path)
      File.mkdir_p!(reactors_path)
      
      # Generate resources
      Mix.shell().info("\n📦 Generating Ash Resources:")
      Enum.each(transformation.resources, fn resource ->
        generate_resource_file(resource, domain_name, resources_path)
      end)
      
      # Generate domain
      Mix.shell().info("\n🏗️  Generating Ash Domain:")
      generate_domain_file(transformation, domain_name, domain_path, app_name)
      
      # Generate reactor
      Mix.shell().info("\n⚡ Generating Ash.Reactor:")
      generate_reactor_file(transformation, domain_name, reactors_path)
      
      Mix.shell().info("""
      
      🎉 Successfully generated from TTL:
         • #{length(transformation.resources)} Ash Resources
         • 1 Ash Domain (with resources configured)
         • 1 Ash.Reactor workflow
      
      Generated structure:
      #{base_path}/
      └── #{Path.relative_to(domain_path, base_path)}/
          ├── #{Path.basename(domain_path)}.ex         # Domain
          ├── resources/
          │   #{Enum.map_join(transformation.resources, "\n          │   ", fn r -> "├── #{Macro.underscore(r.class.name)}.ex" end)}
          └── reactors/
              └── main_workflow.ex     # Reactor
              
      Next steps:
      1. Add `{:ash, "~> 3.0"}` to your mix.exs dependencies
      2. Run `mix deps.get`
      3. Review and customize the generated modules
      4. Add the domain to your config:
         
         config :#{app_name}, ash_domains: [#{domain_name}]
      """)
      
    else
      {:error, reason} ->
        Mix.shell().error("Failed to process TTL: #{inspect(reason)}")
    end
  end
  
  defp generate_resource_file(resource, domain_name, resources_path) do
    filename = "#{Macro.underscore(resource.class.name)}.ex"
    filepath = Path.join(resources_path, filename)
    
    module_name = "#{domain_name}.Resources.#{resource.class.name}"
    
    content = """
    defmodule #{module_name} do
      @moduledoc \"\"\"
      Ash Resource for #{resource.class.name}
      
      Generated from TTL class: #{resource.class.uri}
      \"\"\"
      
      use Ash.Resource,
        domain: #{domain_name},
        data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        
        attribute :ttl_uri, :string do
          description "Original TTL URI for this entity"
          default "#{resource.class.uri}"
          public? true
        end
        
        attribute :name, :string do
          description "Name of the #{resource.class.name}"
          allow_nil? false
          public? true
        end
        
        attribute :description, :string do
          description "Description of the #{resource.class.name}"
          public? true
        end
        
        create_timestamp :created_at
        update_timestamp :updated_at
      end

      actions do
        defaults [:read, :destroy]
        
        create :create do
          primary? true
          accept [:name, :description]
          change set_attribute(:ttl_uri, "#{resource.class.uri}")
        end
        
        update :update do
          primary? true
          accept [:name, :description]
        end
      end

      code_interface do
        define :create, args: [:name]
        define :read
        define :update, args: [:name]
        define :destroy
      end
    end
    """
    
    File.write!(filepath, content)
    Mix.shell().info("   • #{module_name}")
  end
  
  defp generate_domain_file(transformation, domain_name, domain_path, app_name) do
    filename = "#{Path.basename(domain_path)}.ex"
    filepath = Path.join(domain_path, filename)
    
    resources = Enum.map_join(transformation.resources, "\n    ", fn resource ->
      "resource #{domain_name}.Resources.#{resource.class.name}"
    end)
    
    content = """
    defmodule #{domain_name} do
      @moduledoc \"\"\"
      Ash Domain generated from TTL ontology
      
      This domain contains #{length(transformation.resources)} resources transformed from OWL classes.
      \"\"\"
      
      use Ash.Domain, otp_app: :#{app_name}

      resources do
        #{resources}
      end
    end
    """
    
    File.write!(filepath, content)
    Mix.shell().info("   • #{domain_name}")
  end
  
  defp generate_reactor_file(_transformation, domain_name, reactors_path) do
    filename = "main_workflow.ex"
    filepath = Path.join(reactors_path, filename)
    
    module_name = "#{domain_name}.Reactors.MainWorkflow"
    
    content = """
    defmodule #{module_name} do
      @moduledoc \"\"\"
      Main Ash.Reactor workflow for #{domain_name}
      
      Orchestrates operations across TTL-generated resources.
      \"\"\"
      
      use Reactor

      input :operation, :atom do
        description "The operation to perform (:create, :read, :update, :destroy)"
        constraints one_of: [:create, :read, :update, :destroy]
      end

      input :resource_type, :atom do
        description "The type of resource to operate on"
      end

      input :params, :map do
        description "Parameters for the operation"
        default %{}
      end

      step :validate_inputs do
        argument :operation, input(:operation)
        argument :resource_type, input(:resource_type)
        
        run fn args, _context ->
          if args.operation && args.resource_type do
            {:ok, %{valid: true}}
          else
            {:error, "Missing required inputs"}
          end
        end
      end

      step :execute_operation do
        wait_for :validate_inputs
        
        argument :operation, input(:operation)
        argument :resource_type, input(:resource_type)
        argument :params, input(:params)
        
        run fn args, _context ->
          # In a real implementation, would dynamically dispatch to resources
          {:ok, %{
            operation: args.operation,
            resource_type: args.resource_type,
            status: :completed,
            timestamp: DateTime.utc_now()
          }}
        end
      end

      return :execute_operation
    end
    """
    
    File.write!(filepath, content)
    Mix.shell().info("   • #{module_name}")
  end
end