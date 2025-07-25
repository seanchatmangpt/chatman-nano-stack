defmodule CNSForge.Benchmarks do
  @moduledoc """
  Performance benchmarks for CNS Forge components
  Run with: mix run benchmarks/cns_forge_bench.exs
  """
  
  alias CNSForge.{BitActor, MetaCompiler, JinjaRenderer, ReactorBuilder}
  
  # Benchmark configurations
  @bitactor_sizes [1, 10, 100, 1000]
  @ttl_budgets [2, 4, 8, 16]
  @workflow_complexities [:simple, :medium, :complex]
  @template_sizes [:small, :medium, :large]
  
  def run do
    IO.puts("\n=== CNS Forge Performance Benchmarks ===\n")
    
    benchmark_bitactor_creation()
    benchmark_bitactor_hop_execution()
    benchmark_meta_compilation()
    benchmark_jinja_rendering()
    benchmark_reactor_workflows()
    benchmark_end_to_end()
    
    IO.puts("\n=== Benchmark Complete ===\n")
  end
  
  defp benchmark_bitactor_creation do
    IO.puts("## BitActor Creation Benchmarks")
    
    Benchee.run(%{
      "create_minimal" => fn ->
        {:ok, _} = BitActor.create(%{
          type: :benchmark,
          transaction_id: "bench_#{:erlang.unique_integer()}",
          ttl: 8,
          token: %{}
        })
      end,
      
      "create_with_small_token" => fn ->
        {:ok, _} = BitActor.create(%{
          type: :benchmark,
          transaction_id: "bench_#{:erlang.unique_integer()}",
          ttl: 8,
          token: %{data: String.duplicate("x", 100)}
        })
      end,
      
      "create_with_large_token" => fn ->
        {:ok, _} = BitActor.create(%{
          type: :benchmark,
          transaction_id: "bench_#{:erlang.unique_integer()}",
          ttl: 8,
          token: %{data: String.duplicate("x", 10_000)}
        })
      end
    }, time: 10, memory_time: 2)
    
    IO.puts("")
  end
  
  defp benchmark_bitactor_hop_execution do
    IO.puts("## BitActor Hop Execution Benchmarks")
    
    # Pre-create actors for different TTL budgets
    actors = Enum.map(@ttl_budgets, fn ttl ->
      {:ok, actor} = BitActor.create(%{
        type: :hop_benchmark,
        transaction_id: "hop_bench_#{ttl}",
        ttl: ttl,
        token: %{initial: true}
      })
      {"ttl_#{ttl}", actor}
    end) |> Enum.into(%{})
    
    Benchee.run(
      Enum.map(actors, fn {name, actor} ->
        {name, fn ->
          BitActor.execute_hop(actor, %{
            input_token: %{operation: :benchmark},
            operation: :process
          })
        end}
      end) |> Enum.into(%{}),
      time: 10
    )
    
    IO.puts("")
  end
  
  defp benchmark_meta_compilation do
    IO.puts("## MetaCompiler Benchmarks")
    
    # Create test ontologies of different sizes
    ontologies = %{
      "small_ontology" => create_test_ontology(10),
      "medium_ontology" => create_test_ontology(100),
      "large_ontology" => create_test_ontology(1000)
    }
    
    # Write ontologies to files
    paths = Enum.map(ontologies, fn {name, content} ->
      path = "benchmarks/fixtures/#{name}.ttl"
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, content)
      {name, path}
    end) |> Enum.into(%{})
    
    Benchee.run(
      Enum.map(paths, fn {name, path} ->
        {name, fn -> MetaCompiler.compile(path) end}
      end) |> Enum.into(%{}),
      time: 30,
      memory_time: 5
    )
    
    # Cleanup
    Enum.each(paths, fn {_, path} -> File.rm!(path) end)
    
    IO.puts("")
  end
  
  defp benchmark_jinja_rendering do
    IO.puts("## Jinja Template Rendering Benchmarks")
    
    templates = %{
      "simple_template" => """
      #define {{ prefix|upper }}_VERSION {{ version }}
      """,
      
      "loop_template" => """
      {% for item in items %}
      void {{ prefix }}_{{ item.name|c_identifier }}() {
        // {{ item.description }}
      }
      {% endfor %}
      """,
      
      "complex_template" => """
      {% for class in classes %}
      typedef struct {
        {% for prop in class.properties %}
        {{ prop.type }} {{ prop.name }};
        {% endfor %}
      } {{ prefix }}_{{ class.name|c_identifier }}_t;
      
      {% for method in class.methods %}
      {{ method.return_type }} {{ prefix }}_{{ class.name|c_identifier }}_{{ method.name }}(
        {% for param in method.params %}
        {{ param.type }} {{ param.name }}{% if not loop.last %},{% endif %}
        {% endfor %}
      );
      {% endfor %}
      {% endfor %}
      """
    }
    
    contexts = %{
      "small_context" => %{
        prefix: "test",
        version: "1.0.0",
        items: Enum.map(1..5, fn i -> %{name: "item#{i}", description: "Item #{i}"} end)
      },
      
      "large_context" => %{
        prefix: "bench",
        classes: Enum.map(1..50, fn i ->
          %{
            name: "Class#{i}",
            properties: Enum.map(1..10, fn j ->
              %{type: "int", name: "prop#{j}"}
            end),
            methods: Enum.map(1..5, fn k ->
              %{
                name: "method#{k}",
                return_type: "void",
                params: [%{type: "int", name: "param1"}]
              }
            end)
          }
        end)
      }
    }
    
    # Benchmark combinations
    benchmarks = for {template_name, template} <- templates,
                    {context_name, context} <- contexts do
      {"#{template_name}_#{context_name}", fn ->
        JinjaRenderer.render_string(template, context)
      end}
    end |> Enum.into(%{})
    
    Benchee.run(benchmarks, time: 10)
    
    IO.puts("")
  end
  
  defp benchmark_reactor_workflows do
    IO.puts("## Reactor Workflow Benchmarks")
    
    workflows = %{
      "simple_workflow" => build_simple_workflow(),
      "parallel_workflow" => build_parallel_workflow(),
      "complex_workflow" => build_complex_workflow()
    }
    
    # Build workflow modules
    modules = Enum.map(workflows, fn {name, spec} ->
      {:ok, module} = ReactorBuilder.build(spec)
      {name, module}
    end) |> Enum.into(%{})
    
    # Benchmark execution
    Benchee.run(
      Enum.map(modules, fn {name, module} ->
        {name, fn ->
          input = %{data: generate_workflow_input()}
          module.run(input)
        end}
      end) |> Enum.into(%{}),
      time: 20
    )
    
    IO.puts("")
  end
  
  defp benchmark_end_to_end do
    IO.puts("## End-to-End Benchmarks")
    
    Benchee.run(%{
      "ttl_to_bitactor_mesh" => fn ->
        # Create simple ontology
        ttl = """
        @prefix : <http://bench#> .
        :Sensor a owl:Class .
        :Reading a owl:Class .
        """
        path = "benchmarks/e2e_test.ttl"
        File.write!(path, ttl)
        
        # Compile to BitActor mesh
        {:ok, _result} = MetaCompiler.compile(path)
        
        File.rm!(path)
      end,
      
      "semantic_workflow_execution" => fn ->
        # Build and execute semantic workflow
        spec = %{
          id: "semantic_bench_#{:erlang.unique_integer()}",
          inputs: [:semantic_data],
          middleware: [:telemetry],
          steps: [
            %{
              name: :parse,
              type: :transform,
              run: fn input -> {:ok, Map.put(input, :parsed, true)} end,
              arguments: []
            },
            %{
              name: :transform,
              type: :transform,
              run: fn input -> {:ok, Map.put(input, :transformed, true)} end,
              arguments: [],
              wait_for: [:parse]
            }
          ],
          return: :transform
        }
        
        {:ok, module} = ReactorBuilder.build(spec)
        module.run(%{semantic_data: %{test: true}})
      end
    }, time: 30, memory_time: 5)
    
    IO.puts("")
  end
  
  # Helper functions
  
  defp create_test_ontology(class_count) do
    classes = Enum.map(1..class_count, fn i ->
      """
      :Class#{i} a owl:Class ;
        rdfs:label "Test Class #{i}" .
      """
    end) |> Enum.join("\n")
    
    """
    @prefix : <http://benchmark#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    
    :BenchmarkOntology a owl:Ontology .
    
    #{classes}
    """
  end
  
  defp build_simple_workflow do
    %{
      id: "simple_bench",
      inputs: [:data],
      middleware: [],
      steps: [
        %{
          name: :process,
          type: :transform,
          run: fn input -> {:ok, Map.put(input, :processed, true)} end,
          arguments: []
        }
      ],
      return: :process
    }
  end
  
  defp build_parallel_workflow do
    %{
      id: "parallel_bench",
      inputs: [:items],
      middleware: [],
      steps: [
        %{
          name: :process_items,
          type: :map,
          source: quote(do: input(:items)),
          batch_size: 10,
          allow_async?: true,
          return: :processed_items,
          steps: [
            %{
              name: :transform_item,
              type: :transform,
              arguments: [{:item, quote(do: element(:process_items))}],
              run: fn %{item: item} -> {:ok, %{item | processed: true}} end
            }
          ]
        }
      ],
      return: :process_items
    }
  end
  
  defp build_complex_workflow do
    %{
      id: "complex_bench",
      inputs: [:data],
      middleware: [:telemetry],
      steps: Enum.map(1..10, fn i ->
        %{
          name: :"step_#{i}",
          type: :transform,
          run: fn input -> 
            Process.sleep(1)
            {:ok, Map.put(input, :"step_#{i}", :completed)}
          end,
          arguments: [],
          wait_for: if(i > 1, do: [:"step_#{i-1}"], else: [])
        }
      end),
      return: :step_10
    }
  end
  
  defp generate_workflow_input do
    %{
      items: Enum.map(1..100, fn i ->
        %{id: i, value: :rand.uniform(1000), processed: false}
      end),
      metadata: %{
        timestamp: System.system_time(:millisecond),
        source: "benchmark"
      }
    }
  end
end

# Run benchmarks if executed directly
if System.argv() == [] do
  CNSForge.Benchmarks.run()
end