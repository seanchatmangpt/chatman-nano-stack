defmodule Mix.Tasks.CnsForge.Scaffold do
  @moduledoc """
  Generate complete Elixir/Ash.Reactor project from TTL ontology

  ## Usage

      mix cns_forge.scaffold path/to/ontology.ttl output_project [options]

  ## Options

      --app-name NAME        Set the application name (default: derived from output directory)
      --version VERSION      Set the initial version (default: "1.0.0")
      --author AUTHOR        Set the project author (default: "CNS Forge Scaffolder")
      --description DESC     Set the project description
      --elixir-version VER   Set the Elixir version requirement (default: "~> 1.15")
      --port PORT            Set the Phoenix server port (default: 4000)
      --no-deps              Skip dependency installation
      --no-compile           Skip initial compilation
      --no-test              Skip initial test run

  ## Examples

      # Basic scaffolding
      mix cns_forge.scaffold ontologies/cybersecurity_core.ttl cyber_project

      # With custom options
      mix cns_forge.scaffold ontologies/trading.ttl forex_system \\
        --app-name "forex_trading" \\
        --author "ACME Corp" \\
        --description "Forex trading system with Ash.Reactor" \\
        --port 4001

  After scaffolding, you can:

      cd output_project
      mix deps.get
      mix compile
      mix test
      mix phx.server

  """

  use Mix.Task

  alias CNSForge.ProjectScaffolder

  @switches [
    app_name: :string,
    version: :string,
    author: :string,
    description: :string,
    elixir_version: :string,
    port: :integer,
    no_deps: :boolean,
    no_compile: :boolean,
    no_test: :boolean
  ]

  @aliases [
    n: :app_name,
    v: :version,
    a: :author,
    d: :description,
    e: :elixir_version,
    p: :port
  ]

  @impl Mix.Task
  def run(args) do
    {opts, args, _} = OptionParser.parse(args, switches: @switches, aliases: @aliases)

    case args do
      [ttl_file, output_directory] ->
        scaffold_project(ttl_file, output_directory, opts)

      [ttl_file] ->
        output_directory = derive_output_directory(ttl_file)
        scaffold_project(ttl_file, output_directory, opts)

      [] ->
        Mix.shell().error("Missing required arguments")
        show_help()

      _ ->
        Mix.shell().error("Too many arguments")
        show_help()
    end
  end

  defp scaffold_project(ttl_file, output_directory, opts) do
    Mix.shell().info("CNS Forge Project Scaffolding")
    Mix.shell().info("=============================")
    Mix.shell().info("")
    Mix.shell().info("TTL file: #{ttl_file}")
    Mix.shell().info("Output directory: #{output_directory}")
    Mix.shell().info("")

    # Validate inputs
    with :ok <- validate_ttl_file(ttl_file),
         :ok <- validate_output_directory(output_directory) do
      
      # Run scaffolding
      case ProjectScaffolder.scaffold(ttl_file, output_directory, opts) do
        {:ok, result} ->
          report_success(result, opts)
          
        {:error, reason} ->
          Mix.shell().error("Scaffolding failed: #{inspect(reason)}")
          exit({:shutdown, 1})
      end
    else
      {:error, reason} ->
        Mix.shell().error("Validation failed: #{inspect(reason)}")
        exit({:shutdown, 1})
    end
  end

  defp validate_ttl_file(ttl_file) do
    cond do
      not File.exists?(ttl_file) ->
        {:error, "TTL file does not exist: #{ttl_file}"}
        
      not String.ends_with?(ttl_file, ".ttl") ->
        {:error, "File must have .ttl extension: #{ttl_file}"}
        
      true ->
        case File.read(ttl_file) do
          {:ok, content} when byte_size(content) > 0 ->
            :ok
            
          {:ok, _} ->
            {:error, "TTL file is empty: #{ttl_file}"}
            
          {:error, reason} ->
            {:error, "Cannot read TTL file: #{reason}"}
        end
    end
  end

  defp validate_output_directory(output_directory) do
    if File.exists?(output_directory) do
      if File.dir?(output_directory) do
        case File.ls(output_directory) do
          {:ok, []} ->
            :ok  # Directory exists but is empty
            
          {:ok, _files} ->
            answer = Mix.shell().yes?("Directory #{output_directory} is not empty. Continue?")
            if answer, do: :ok, else: {:error, :user_cancelled}
            
          {:error, reason} ->
            {:error, "Cannot list directory contents: #{reason}"}
        end
      else
        {:error, "Output path exists but is not a directory: #{output_directory}"}
      end
    else
      :ok  # Directory doesn't exist, will be created
    end
  end

  defp derive_output_directory(ttl_file) do
    ttl_file
    |> Path.basename(".ttl")
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9_]/, "_")
    |> (&"#{&1}_project").()
  end

  defp report_success(result, opts) do
    Mix.shell().info("")
    Mix.shell().info("✅ Project scaffolding completed successfully!")
    Mix.shell().info("")
    Mix.shell().info("Project details:")
    Mix.shell().info("  App name: #{result.app_name}")
    Mix.shell().info("  Directory: #{result.project_directory}")
    Mix.shell().info("  Ontology classes: #{result.ontology_classes}")
    Mix.shell().info("  Reactors generated: #{result.reactors_generated}")
    Mix.shell().info("  Workflows generated: #{result.workflows_generated}")
    Mix.shell().info("")

    unless opts[:no_deps] do
      Mix.shell().info("Installing dependencies...")
      run_in_project(result.project_directory, "mix deps.get")
    end

    unless opts[:no_compile] do
      Mix.shell().info("Compiling project...")
      run_in_project(result.project_directory, "mix compile")
    end

    unless opts[:no_test] do
      Mix.shell().info("Running tests...")
      run_in_project(result.project_directory, "mix test")
    end

    Mix.shell().info("")
    Mix.shell().info("Next steps:")
    Mix.shell().info("  cd #{result.project_directory}")
    
    if opts[:no_deps] do
      Mix.shell().info("  mix deps.get")
    end
    
    if opts[:no_compile] do
      Mix.shell().info("  mix compile")
    end
    
    if opts[:no_test] do
      Mix.shell().info("  mix test")
    end
    
    Mix.shell().info("  mix phx.server")
    Mix.shell().info("")
    Mix.shell().info("Your Ash.Reactor project is ready! 🚀")
  end

  defp run_in_project(directory, command) do
    original_cwd = File.cwd!()
    
    try do
      File.cd!(directory)
      case System.cmd("sh", ["-c", command], stderr_to_stdout: true) do
        {output, 0} ->
          Mix.shell().info(output)
          :ok
          
        {output, exit_code} ->
          Mix.shell().error("Command failed (exit code #{exit_code}):")
          Mix.shell().error(output)
          :error
      end
    after
      File.cd!(original_cwd)
    end
  end

  defp show_help do
    Mix.shell().info(@moduledoc)
  end
end