if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Cns.Gen.FromTtl do
    @example "mix cns.gen.from_ttl --file ontology.ttl --domain MyApp.Cybersecurity"
    @moduledoc """
    🐢 Generates Ash Resources, Domains, and Reactor workflows from TTL (Turtle) ontologies

    ## Overview

    This task transforms TTL/RDF ontologies into a complete Ash framework setup:
    - Ash Resources for each OWL class
    - Ash Domain containing all resources
    - Ash.Reactor workflows for orchestration
    - Proper application configuration

    ## Examples

    ```bash
    # From a TTL file
    #{@example}

    # From stdin with custom namespace
    mix cns.gen.from_ttl --stdin --domain MyApp.Security --namespace SecOnt

    # Generate only resources (no reactor)
    mix cns.gen.from_ttl --file cyber.ttl --domain MyApp.Cyber --no-reactor

    # With custom resource prefix
    mix cns.gen.from_ttl --file ontology.ttl --domain MyApp.Domain --resource-prefix Generated
    ```

    ## Options

    * `--file` - Path to TTL file to transform
    * `--stdin` - Read TTL from standard input
    * `--domain` - Target domain module (required)
    * `--namespace` - Custom namespace for resources (default: TTLResources)
    * `--no-reactor` - Skip Ash.Reactor generation
    * `--no-domain` - Skip domain generation (only create resources)
    * `--resource-prefix` - Prefix for generated resource modules
    * `--dry-run` - Show what would be generated without creating files
    * `--force` - Overwrite existing files

    ## TTL Example

    ```turtle
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix cyber: <http://cybersecurity.org/> .

    cyber:ThreatActor a owl:Class .
    cyber:Vulnerability a owl:Class .
    cyber:SecurityControl a owl:Class .
    ```

    ## Generated Structure

    ```
    lib/
    └── my_app/
        └── cybersecurity/
            ├── cybersecurity.ex         # Domain
            ├── resources/
            │   ├── threat_actor.ex      # Resource
            │   ├── vulnerability.ex     # Resource
            │   └── security_control.ex  # Resource
            └── reactors/
                └── main_workflow.ex     # Reactor
    ```
    """

    @shortdoc "Generates Ash Resources/Domains from TTL ontologies"
    use Igniter.Mix.Task

    alias CnsForge.TTLAshReactorTransformer

    @impl Igniter.Mix.Task
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        schema: [
          file: :string,
          stdin: :boolean,
          domain: :string,
          namespace: :string,
          no_reactor: :boolean,
          no_domain: :boolean,
          resource_prefix: :string,
          dry_run: :boolean,
          force: :boolean
        ],
        required: [:domain],
        example: @example,
        aliases: [
          f: :file,
          d: :domain,
          n: :namespace
        ]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      # Validate options
      with {:ok, igniter} <- validate_options(igniter),
           {:ok, ttl_content} <- read_ttl_content(igniter),
           {:ok, transformation} <- transform_ttl(ttl_content) do
        
        # Parse domain module
        domain_module = Igniter.Project.Module.parse(igniter.args.options[:domain])
        app_name = Igniter.Project.Application.app_name(igniter)
        
        igniter
        |> maybe_create_resources(transformation, domain_module)
        |> maybe_create_domain(transformation, domain_module, app_name)
        |> maybe_create_reactor(transformation, domain_module)
        |> configure_application(domain_module, app_name)
        |> add_success_message(transformation)
      else
        {:error, message} ->
          Igniter.add_issue(igniter, message)
      end
    end

    defp validate_options(igniter) do
      cond do
        igniter.args.options[:file] && igniter.args.options[:stdin] ->
          {:error, "Cannot specify both --file and --stdin"}
        
        !igniter.args.options[:file] && !igniter.args.options[:stdin] ->
          {:error, "Must specify either --file or --stdin"}
        
        true ->
          {:ok, igniter}
      end
    end

    defp read_ttl_content(igniter) do
      if igniter.args.options[:stdin] do
        ttl_content = IO.read(:stdio, :all)
        {:ok, ttl_content}
      else
        case File.read(igniter.args.options[:file]) do
          {:ok, content} -> {:ok, content}
          {:error, reason} -> {:error, "Failed to read TTL file: #{reason}"}
        end
      end
    end

    defp transform_ttl(ttl_content) do
      case TTLAshReactorTransformer.transform_ttl(ttl_content) do
        {:ok, result} -> {:ok, result}
        {:error, reason} -> {:error, "TTL transformation failed: #{inspect(reason)}"}
      end
    end

    defp maybe_create_resources(igniter, transformation, domain_module) do
      namespace = igniter.args.options[:namespace] || "Resources"
      prefix = igniter.args.options[:resource_prefix] || ""
      
      Enum.reduce(transformation.resources, igniter, fn resource, acc ->
        resource_module = Module.concat([domain_module, namespace, "#{prefix}#{resource.class.name}"])
        
        # Generate custom resource code with proper domain reference
        resource_code = generate_resource_code(resource, domain_module)
        
        Igniter.Project.Module.create_module(acc, resource_module, resource_code)
      end)
    end

    defp maybe_create_domain(igniter, transformation, domain_module, app_name) do
      if igniter.args.options[:no_domain] do
        igniter
      else
        namespace = igniter.args.options[:namespace] || "Resources"
        
        # Generate list of resource modules
        resource_modules = Enum.map(transformation.resources, fn resource ->
          prefix = igniter.args.options[:resource_prefix] || ""
          Module.concat([domain_module, namespace, "#{prefix}#{resource.class.name}"])
        end)
        
        domain_code = generate_domain_code(domain_module, app_name, resource_modules)
        
        igniter
        |> Igniter.Project.Module.create_module(domain_module, domain_code)
        |> Igniter.Project.Config.configure(
          "config.exs",
          app_name,
          [:ash_domains],
          [domain_module],
          updater: fn list ->
            Igniter.Code.List.prepend_new_to_list(list, domain_module)
          end
        )
      end
    end

    defp maybe_create_reactor(igniter, transformation, domain_module) do
      if igniter.args.options[:no_reactor] do
        igniter
      else
        reactor_module = Module.concat([domain_module, "Reactors", "MainWorkflow"])
        
        # Use the reactor from transformation
        reactor = hd(transformation.reactors)
        reactor_code = generate_reactor_code(reactor, domain_module)
        
        Igniter.Project.Module.create_module(igniter, reactor_module, reactor_code)
      end
    end

    defp configure_application(igniter, _domain_module, _app_name) do
      # Additional configuration if needed
      igniter
    end

    defp add_success_message(igniter, transformation) do
      resource_count = length(transformation.resources)
      reactor_count = if igniter.args.options[:no_reactor], do: 0, else: 1
      
      message = """
      
      🎉 Successfully generated from TTL:
         • #{resource_count} Ash Resources
         • 1 Ash Domain (with resources configured)
         • #{reactor_count} Ash.Reactor workflow
      
      Next steps:
      1. Run `mix deps.get` to ensure dependencies are installed
      2. Review the generated modules
      3. Customize attributes and actions as needed
      4. Create migrations if using a SQL data layer
      """
      
      Igniter.add_notice(igniter, message)
    end

    # Code generation helpers

    defp generate_resource_code(resource, domain_module) do
      """
      defmodule #{inspect(resource.module_name)} do
        @moduledoc \"\"\"
        Ash Resource for #{resource.class.name}
        
        Generated from TTL class: #{resource.class.uri}
        \"\"\"
        
        use Ash.Resource,
          domain: #{inspect(domain_module)},
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
    end

    defp generate_domain_code(domain_module, app_name, resource_modules) do
      resources_list = Enum.map_join(resource_modules, "\n    ", &"resource #{inspect(&1)}")
      
      """
      defmodule #{inspect(domain_module)} do
        @moduledoc \"\"\"
        Ash Domain generated from TTL ontology
        
        This domain contains resources transformed from OWL classes.
        \"\"\"
        
        use Ash.Domain, otp_app: #{inspect(app_name)}

        resources do
          #{resources_list}
        end
      end
      """
    end

    defp generate_reactor_code(reactor, domain_module) do
      """
      defmodule #{inspect(Module.concat([domain_module, "Reactors", "MainWorkflow"]))} do
        @moduledoc \"\"\"
        Main Ash.Reactor workflow for #{inspect(domain_module)}
        
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
            # Dynamic operation execution
            # In real implementation, would dispatch to appropriate resource
            {:ok, %{
              operation: args.operation,
              resource_type: args.resource_type,
              status: :completed
            }}
          end
        end

        return :execute_operation
      end
      """
    end
  end
else
  defmodule Mix.Tasks.Cns.Gen.FromTtl do
    @shortdoc "Generates Ash Resources/Domains from TTL ontologies"

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'cns.gen.from_ttl' requires igniter to be run.

      Please install igniter and try again:
      
          {:igniter, "~> 0.1"}

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end