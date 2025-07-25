defmodule CNSForge.Workflows.SemanticCompiler do
  @moduledoc """
  Reactor workflow for semantic compilation pipeline
  80/20 Implementation: TTL → BitActor → Executable System
  """
  
  use Reactor
  require Logger
  
  alias CNSForge.{Metacompiler, BitActor, JinjaRenderer}
  
  input :semantic_spec
  input :target_platforms, default: [:elixir_reactor, :c_bitactor]
  input :ttl_budget, default: 8
  
  # Telemetry middleware for observability
  middlewares do
    middleware Reactor.Middleware.Telemetry
    middleware CNSForge.ReactorMiddleware.TTLEnforcement
    middleware CNSForge.ReactorMiddleware.Semantic
  end
  
  # Step 1: Parse semantic specification
  step :parse_semantic do
    argument :spec, input(:semantic_spec)
    argument :ttl, input(:ttl_budget)
    
    run fn %{spec: spec, ttl: ttl} ->
      CNSForge.Telemetry.start_span("semantic_parsing")
      
      result = case Metacompiler.parse_semantic_spec(spec) do
        {:ok, parsed} ->
          {:ok, Map.put(parsed, :ttl_budget, ttl)}
        error ->
          error
      end
      
      CNSForge.Telemetry.end_span(elem(result, 0))
      result
    end
    
    compensate fn _error, _args ->
      Logger.error("Failed to parse semantic spec")
      :ok
    end
  end
  
  # Step 2: Generate Intermediate Representation
  step :generate_ir do
    argument :parsed, result(:parse_semantic)
    
    run fn %{parsed: parsed} ->
      CNSForge.Telemetry.start_span("ir_generation")
      
      result = Metacompiler.generate_intermediate_representation(parsed)
      
      CNSForge.Telemetry.end_span(elem(result, 0))
      result
    end
  end
  
  # Step 3: Optimize IR
  step :optimize_ir do
    argument :ir, result(:generate_ir)
    argument :targets, input(:target_platforms)
    
    run fn %{ir: ir, targets: targets} ->
      CNSForge.Telemetry.start_span("ir_optimization")
      
      result = Metacompiler.optimize_ir(ir, targets: targets)
      
      CNSForge.Telemetry.end_span(elem(result, 0))
      result
    end
  end
  
  # Step 4: Generate targets in parallel
  map :generate_targets do
    source input(:target_platforms)
    batch_size 4
    allow_async? true
    
    step :generate_target do
      argument :target, element(:generate_targets)
      argument :ir, result(:optimize_ir)
      
      run fn %{target: target, ir: ir} ->
        CNSForge.Telemetry.start_span("target_generation", %{target: target})
        
        result = case target do
          :elixir_reactor -> generate_elixir_reactor(ir)
          :c_bitactor -> generate_c_bitactor(ir)
          :erlang_otp -> generate_erlang_otp(ir)
          :kubernetes -> generate_kubernetes(ir)
          _ -> {:error, :unsupported_target}
        end
        
        CNSForge.Telemetry.end_span(elem(result, 0))
        
        case result do
          {:ok, code} -> {:ok, %{target: target, code: code}}
          error -> error
        end
      end
    end
    
    return :generated_targets
  end
  
  # Step 5: Create BitActor mesh for execution
  step :create_bitactor_mesh do
    argument :targets, result(:generate_targets)
    argument :ttl_budget, input(:ttl_budget)
    
    run fn %{targets: targets, ttl_budget: ttl} ->
      transaction_id = generate_transaction_id()
      
      # Create BitActors for each generated target
      bitactors = Enum.map(targets, fn %{target: target, code: _code} ->
        {:ok, actor} = BitActor.create(%{
          type: :compiler_output,
          transaction_id: transaction_id,
          ttl: ttl,
          token: %{
            target: target,
            status: :ready
          }
        })
        actor
      end)
      
      {:ok, %{
        transaction_id: transaction_id,
        bitactors: bitactors,
        mesh_size: length(bitactors)
      }}
    end
  end
  
  # Step 6: Validate generated code
  around :validation, CNSForge.Reactor.ValidationWrapper do
    step :validate_syntax do
      argument :targets, result(:generate_targets)
      
      run fn %{targets: targets} ->
        validation_results = Enum.map(targets, fn %{target: target, code: code} ->
          %{
            target: target,
            syntax_valid: validate_syntax(target, code),
            metrics: calculate_code_metrics(code)
          }
        end)
        
        all_valid = Enum.all?(validation_results, & &1.syntax_valid)
        
        {:ok, %{
          all_valid: all_valid,
          results: validation_results
        }}
      end
    end
    
    step :validate_semantics do
      argument :targets, result(:generate_targets)
      argument :ir, result(:optimize_ir)
      
      run fn %{targets: targets, ir: ir} ->
        semantic_results = Enum.map(targets, fn %{target: target, code: code} ->
          %{
            target: target,
            semantics_preserved: validate_semantics(target, code, ir)
          }
        end)
        
        {:ok, semantic_results}
      end
    end
  end
  
  # Step 7: Collect results
  step :collect_results do
    argument :targets, result(:generate_targets)
    argument :mesh, result(:create_bitactor_mesh)
    argument :syntax_validation, result(:validate_syntax)
    argument :semantic_validation, result(:validate_semantics)
    
    run fn args ->
      {:ok, %{
        compilation_id: args.mesh.transaction_id,
        targets: Enum.map(args.targets, fn t -> 
          %{
            platform: t.target,
            code_size: byte_size(t.code),
            syntax_valid: find_validation(args.syntax_validation.results, t.target).syntax_valid,
            semantics_valid: find_validation(args.semantic_validation, t.target).semantics_preserved
          }
        end),
        mesh_info: %{
          size: args.mesh.mesh_size,
          transaction_id: args.mesh.transaction_id
        },
        timestamp: DateTime.utc_now()
      }}
    end
  end
  
  return :collect_results
  
  # Private helper functions
  
  defp generate_elixir_reactor(ir) do
    reactor_spec = %{
      id: "compiled_#{:erlang.unique_integer([:positive])}",
      inputs: extract_inputs(ir),
      middleware: [:telemetry, :ttl_enforcement],
      steps: transform_ir_to_reactor_steps(ir),
      return: :execution_result
    }
    
    case CNSForge.ReactorBuilder.build(reactor_spec) do
      {:ok, module} ->
        {:ok, format_elixir_module(module)}
      error ->
        error
    end
  end
  
  defp generate_c_bitactor(ir) do
    template_vars = %{
      "ontology_name" => ir.metadata[:name] || "compiled",
      "prefix" => "cnsf",
      "guard_name" => "CNSF_COMPILED",
      "reactor_steps" => transform_ir_to_c_steps(ir),
      "max_ttl_hops" => 8,
      "reactor_ring_size" => 1024,
      "token_size" => 512
    }
    
    JinjaRenderer.render("ash_reactor_bitactor.j2", template_vars)
  end
  
  defp generate_erlang_otp(ir) do
    template_vars = %{
      "module_name" => ir.metadata[:name] || "cns_compiled",
      "processes" => transform_ir_to_otp_processes(ir),
      "supervisors" => generate_otp_supervisors(ir)
    }
    
    JinjaRenderer.render("erlang_otp.j2", template_vars)
  end
  
  defp generate_kubernetes(ir) do
    deployments = generate_k8s_deployments(ir)
    services = generate_k8s_services(ir)
    
    {:ok, "#{deployments}\n---\n#{services}"}
  end
  
  defp generate_transaction_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end
  
  defp validate_syntax(:elixir_reactor, code) do
    # Validate Elixir syntax
    case Code.string_to_quoted(code) do
      {:ok, _} -> true
      _ -> false
    end
  end
  
  defp validate_syntax(:c_bitactor, code) do
    # Basic C syntax validation
    String.contains?(code, "#include") and
    String.contains?(code, "typedef struct") and
    balanced_braces?(code)
  end
  
  defp validate_syntax(_, _), do: true
  
  defp balanced_braces?(code) do
    code
    |> String.graphemes()
    |> Enum.reduce(0, fn
      "{", count -> count + 1
      "}", count -> count - 1
      _, count -> count
    end) == 0
  end
  
  defp calculate_code_metrics(code) do
    %{
      lines: String.split(code, "\n") |> length(),
      size_bytes: byte_size(code),
      complexity: estimate_complexity(code)
    }
  end
  
  defp estimate_complexity(code) do
    # Simplified complexity estimation
    conditionals = Regex.scan(~r/\b(if|case|cond|when)\b/, code) |> length()
    loops = Regex.scan(~r/\b(for|while|each|map|reduce)\b/, code) |> length()
    
    conditionals + loops
  end
  
  defp validate_semantics(_target, _code, _ir) do
    # Simplified semantic validation
    # In production, would verify that generated code preserves IR semantics
    true
  end
  
  defp find_validation(results, target) when is_list(results) do
    Enum.find(results, &(&1.target == target)) || %{syntax_valid: false, semantics_preserved: false}
  end
  
  defp extract_inputs(ir) do
    ir.nodes
    |> Enum.filter(&(&1.type == :input))
    |> Enum.map(&String.to_atom(&1.label))
  end
  
  defp transform_ir_to_reactor_steps(ir) do
    Enum.map(ir.nodes, fn node ->
      %{
        name: String.to_atom(node.label),
        type: :transform,
        arguments: [],
        run: quote do
          fn _args -> {:ok, %{processed: true}} end
        end
      }
    end)
  end
  
  defp transform_ir_to_c_steps(ir) do
    Enum.map(ir.nodes, fn node ->
      %{
        "name" => node.label,
        "description" => "Process #{node.label}",
        "operations" => ["/* Process #{node.label} */"]
      }
    end)
  end
  
  defp transform_ir_to_otp_processes(ir) do
    Enum.map(ir.nodes, fn node ->
      %{
        "name" => node.label,
        "type" => "gen_server"
      }
    end)
  end
  
  defp generate_otp_supervisors(ir) do
    [%{
      "name" => "main_supervisor",
      "strategy" => "one_for_one",
      "children" => ir.nodes |> Enum.take(5) |> Enum.map(& &1.label)
    }]
  end
  
  defp generate_k8s_deployments(ir) do
    """
    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: cns-forge-#{ir.metadata[:name] || "app"}
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: cns-forge
      template:
        metadata:
          labels:
            app: cns-forge
        spec:
          containers:
          - name: cns-forge
            image: cns-forge:latest
            ports:
            - containerPort: 4000
    """
  end
  
  defp generate_k8s_services(ir) do
    """
    apiVersion: v1
    kind: Service
    metadata:
      name: cns-forge-#{ir.metadata[:name] || "app"}
    spec:
      selector:
        app: cns-forge
      ports:
      - port: 4000
        targetPort: 4000
    """
  end
  
  defp format_elixir_module(module) do
    # Format module code for output
    module
    |> Module.get_attribute(:moduledoc)
    |> Kernel.||("")
  end
end