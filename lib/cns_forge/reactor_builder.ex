defmodule CNSForge.ReactorBuilder do
  @moduledoc """
  Dynamic Ash.Reactor workflow builder for CNS Forge
  Builds reactor workflows from semantic specifications
  """
  
  alias CNSForge.{BitActor, ReactorMiddleware}
  require Logger
  
  @doc """
  Build a Reactor module from specification
  """
  def build(reactor_spec) do
    module_name = generate_module_name(reactor_spec.id)
    
    module_ast = quote do
      defmodule unquote(module_name) do
        use Reactor
        
        @workflow_id unquote(reactor_spec.id)
        @created_at DateTime.utc_now()
        
        # Add middleware
        middlewares do
          unquote_splicing(build_middleware_ast(reactor_spec.middleware))
        end
        
        # Add inputs
        unquote_splicing(build_input_ast(reactor_spec.inputs))
        
        # Add steps
        unquote_splicing(build_steps_ast(reactor_spec.steps))
        
        # Set return value
        return unquote(reactor_spec.return)
        
        # Metadata functions
        def workflow_id, do: @workflow_id
        def created_at, do: @created_at
        def spec, do: unquote(Macro.escape(reactor_spec))
      end
    end
    
    # Compile the module
    case compile_module(module_ast) do
      {:ok, module} ->
        Logger.info("Successfully built Reactor workflow: #{inspect(module)}")
        {:ok, module}
        
      error ->
        Logger.error("Failed to build Reactor workflow: #{inspect(error)}")
        error
    end
  end
  
  @doc """
  Build a Reactor workflow for BitActor mesh coordination
  """
  def build_bitactor_mesh_workflow(mesh_config) do
    reactor_spec = %{
      id: "mesh_#{mesh_config.mesh_id}",
      inputs: [:signal_stream, :mesh_topology, :ttl_budget],
      middleware: [:telemetry, :ttl_enforcement, :semantic],
      steps: build_mesh_steps(mesh_config),
      return: :mesh_execution_result
    }
    
    build(reactor_spec)
  end
  
  # Private functions
  
  defp generate_module_name(workflow_id) do
    base_name = workflow_id
    |> String.replace(~r/[^a-zA-Z0-9]/, "_")
    |> Macro.camelize()
    
    Module.concat([CNSForge, Workflows, Generated, base_name])
  end
  
  defp build_middleware_ast(middleware_list) do
    Enum.map(middleware_list, fn middleware ->
      case middleware do
        :telemetry ->
          quote do
            middleware Reactor.Middleware.Telemetry
          end
          
        :semantic ->
          quote do
            middleware CNSForge.SemanticMiddleware
          end
          
        :ttl_enforcement ->
          quote do
            middleware CNSForge.TTLEnforcementMiddleware
          end
          
        {module, opts} when is_atom(module) ->
          quote do
            middleware unquote(module), unquote(opts)
          end
          
        module when is_atom(module) ->
          quote do
            middleware unquote(module)
          end
      end
    end)
  end
  
  defp build_input_ast(inputs) do
    Enum.map(inputs, fn input ->
      case input do
        {name, default} ->
          quote do
            input unquote(name), default: unquote(default)
          end
          
        name when is_atom(name) ->
          quote do
            input unquote(name)
          end
      end
    end)
  end
  
  defp build_steps_ast(steps) do
    Enum.flat_map(steps, &build_step_ast/1)
  end
  
  defp build_step_ast(step) do
    base_ast = case step.type do
      :transform ->
        build_transform_step(step)
        
      :map ->
        build_map_step(step)
        
      :switch ->
        build_switch_step(step)
        
      :compose ->
        build_compose_step(step)
        
      :around ->
        build_around_step(step)
        
      :group ->
        build_group_step(step)
        
      _ ->
        build_generic_step(step)
    end
    
    # Add wait_for if dependencies exist
    if step[:wait_for] && length(step.wait_for) > 0 do
      [base_ast, build_wait_for_ast(step.name, step.wait_for)]
    else
      [base_ast]
    end
  end
  
  defp build_transform_step(step) do
    quote do
      step unquote(step.name), CNSForge.Steps.Transform do
        unquote_splicing(build_arguments_ast(step.arguments))
        
        run unquote(step.run)
        
        unquote_splicing(build_compensate_ast(step))
      end
    end
  end
  
  defp build_map_step(step) do
    quote do
      map unquote(step.name) do
        source unquote(step.source)
        
        unquote_splicing(if step[:batch_size], do: [quote do batch_size unquote(step.batch_size) end], else: [])
        unquote_splicing(if step[:allow_async?], do: [quote do allow_async? true end], else: [])
        
        return unquote(step.return || step.name)
        
        unquote_splicing(build_nested_steps_ast(step.steps))
      end
    end
  end
  
  defp build_switch_step(step) do
    quote do
      switch unquote(step.name) do
        on unquote(step.on)
        
        unquote_splicing(build_switch_matches_ast(step.matches))
        
        default do
          unquote_splicing(build_nested_steps_ast(step.default_steps))
        end
      end
    end
  end
  
  defp build_compose_step(step) do
    quote do
      compose unquote(step.name), unquote(step.workflow) do
        unquote_splicing(build_arguments_ast(step.arguments))
      end
    end
  end
  
  defp build_around_step(step) do
    quote do
      around unquote(step.name), unquote(step.wrapper) do
        unquote_splicing(build_nested_steps_ast(step.steps))
      end
    end
  end
  
  defp build_group_step(step) do
    quote do
      group unquote(step.name) do
        unquote_splicing(build_nested_steps_ast(step.steps))
      end
    end
  end
  
  defp build_generic_step(step) do
    module = step[:module] || CNSForge.Steps.Generic
    
    quote do
      step unquote(step.name), unquote(module) do
        unquote_splicing(build_arguments_ast(step.arguments))
        
        run unquote(step.run)
        
        unquote_splicing(build_compensate_ast(step))
      end
    end
  end
  
  defp build_arguments_ast(arguments) do
    Enum.map(arguments || [], fn {name, source} ->
      quote do
        argument unquote(name), unquote(source)
      end
    end)
  end
  
  defp build_compensate_ast(step) do
    if step[:compensate] do
      [quote do
        compensate unquote(step.compensate)
      end]
    else
      []
    end
  end
  
  defp build_wait_for_ast(step_name, dependencies) do
    quote do
      wait_for unquote(step_name), unquote(dependencies)
    end
  end
  
  defp build_nested_steps_ast(steps) do
    steps
    |> List.wrap()
    |> Enum.flat_map(&build_step_ast/1)
  end
  
  defp build_switch_matches_ast(matches) do
    Enum.map(matches || [], fn match ->
      quote do
        matches? unquote(match.condition) do
          unquote_splicing(build_nested_steps_ast(match.steps))
        end
      end
    end)
  end
  
  defp compile_module(ast) do
    try do
      # Compile the module in memory
      [{module, _}] = Code.compile_quoted(ast)
      {:ok, module}
    rescue
      e in CompileError ->
        {:error, {:compilation_error, e.description}}
      e ->
        {:error, {:unexpected_error, Exception.message(e)}}
    end
  end
  
  defp build_mesh_steps(mesh_config) do
    [
      %{
        name: :validate_mesh_topology,
        type: :transform,
        arguments: [
          {:mesh_topology, quote do input(:mesh_topology) end}
        ],
        run: quote do
          fn %{mesh_topology: topology} ->
            if valid_topology?(topology) do
              {:ok, %{validated: true, topology: topology}}
            else
              {:error, :invalid_topology}
            end
          end
        end
      },
      %{
        name: :initialize_bitactors,
        type: :map,
        source: quote do result(:validate_mesh_topology, [:topology, :bitactors]) end,
        batch_size: 10,
        allow_async?: true,
        return: :initialized_bitactors,
        steps: [
          %{
            name: :spawn_bitactor,
            type: :transform,
            arguments: [
              {:bitactor_spec, quote do element(:initialize_bitactors) end},
              {:ttl_budget, quote do input(:ttl_budget) end}
            ],
            run: quote do
              fn %{bitactor_spec: spec, ttl_budget: ttl} ->
                case CNSForge.BitActor.create(%{
                  type: spec.type,
                  transaction_id: generate_transaction_id(),
                  ttl: ttl,
                  token: spec.initial_token
                }) do
                  {:ok, actor} -> {:ok, %{actor: actor, spec: spec}}
                  error -> error
                end
              end
            end
          }
        ]
      },
      %{
        name: :establish_mesh_connections,
        type: :transform,
        arguments: [
          {:bitactors, quote do result(:initialize_bitactors) end},
          {:topology, quote do result(:validate_mesh_topology, [:topology]) end}
        ],
        wait_for: [:initialize_bitactors],
        run: quote do
          fn %{bitactors: actors, topology: topo} ->
            connections = establish_connections(actors, topo)
            {:ok, %{connections: connections, connected_count: length(connections)}}
          end
        end
      },
      %{
        name: :process_signal_stream,
        type: :map,
        source: quote do input(:signal_stream) end,
        batch_size: 50,
        allow_async?: true,
        return: :processed_signals,
        wait_for: [:establish_mesh_connections],
        steps: [
          %{
            name: :route_signal,
            type: :switch,
            on: quote do element(:process_signal_stream) end,
            matches: [
              %{
                condition: quote do &high_priority?/1 end,
                steps: [
                  %{
                    name: :expedited_processing,
                    type: :transform,
                    arguments: [{:signal, quote do element(:process_signal_stream) end}],
                    run: quote do &process_high_priority_signal/1 end
                  }
                ]
              },
              %{
                condition: quote do &requires_saga?/1 end,
                steps: [
                  %{
                    name: :saga_processing,
                    type: :compose,
                    workflow: CNSForge.Workflows.SagaProcessor,
                    arguments: [{:signal, quote do element(:process_signal_stream) end}]
                  }
                ]
              }
            ],
            default_steps: [
              %{
                name: :standard_processing,
                type: :transform,
                arguments: [{:signal, quote do element(:process_signal_stream) end}],
                run: quote do &process_standard_signal/1 end
              }
            ]
          }
        ]
      },
      %{
        name: :collect_mesh_results,
        type: :transform,
        arguments: [
          {:processed_signals, quote do result(:process_signal_stream) end},
          {:connections, quote do result(:establish_mesh_connections, [:connections]) end}
        ],
        wait_for: [:process_signal_stream],
        run: quote do
          fn %{processed_signals: signals, connections: conns} ->
            stats = calculate_mesh_statistics(signals, conns)
            {:ok, %{
              mesh_execution_result: %{
                signals_processed: length(signals),
                successful: count_successful(signals),
                failed: count_failed(signals),
                average_ttl_usage: calculate_avg_ttl(signals),
                mesh_efficiency: stats.efficiency,
                timestamp: DateTime.utc_now()
              }
            }}
          end
        end
      }
    ]
  end
end