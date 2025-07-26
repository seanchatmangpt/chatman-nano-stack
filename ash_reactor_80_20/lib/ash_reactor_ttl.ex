defmodule AshReactor80_20.TTLReactor do
  @moduledoc """
  Core TTL → Ash.Reactor transformation workflow
  80/20 approach: Simple, working transformation
  """
  
  use Reactor
  
  input :ttl_content
  
  # Step 1: Parse TTL
  step :parse_ttl do
    argument :content, input(:ttl_content)
    
    run fn %{content: content}, _context ->
      # Extract classes with simple regex (80/20 approach)
      class_regex = ~r/(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class/
      
      classes = Regex.scan(class_regex, content)
      |> Enum.map(fn [_, class_uri] ->
        %{
          uri: class_uri,
          name: String.split(class_uri, ":") |> List.last()
        }
      end)
      
      {:ok, %{classes: classes, raw_ttl: content}}
    end
  end
  
  # Step 2: Create Ontology Classes
  step :create_ontology_classes do
    argument :parsed_data, result(:parse_ttl)
    
    run fn %{parsed_data: %{classes: classes}}, context ->
      :telemetry.span(
        [:ash_reactor, :ttl, :create_classes],
        %{class_count: length(classes)},
        fn ->
          created_classes = Enum.map(classes, fn class_data ->
            case AshReactor80_20.Resources.OntologyClass.create_class(class_data) do
              {:ok, class} -> class
              {:error, _} -> nil
            end
          end) |> Enum.filter(& &1)
          
          {:ok, created_classes}
        end
      )
    end
  end
  
  # Step 3: Generate Reactor Workflows
  step :generate_workflows do
    argument :classes, result(:create_ontology_classes)
    
    run fn %{classes: classes}, _context ->
      workflows = Enum.map(classes, fn class ->
        workflow_data = %{
          name: "#{class.name}Workflow",
          ontology_class_id: class.id,
          steps: %{
            "validate" => %{type: "validation", target: class.uri},
            "process" => %{type: "processing", target: class.uri},
            "persist" => %{type: "persistence", target: class.uri}
          }
        }
        
        case AshReactor80_20.Resources.ReactorWorkflow.create_workflow(workflow_data) do
          {:ok, workflow} -> workflow
          {:error, _} -> nil
        end
      end) |> Enum.filter(& &1)
      
      {:ok, workflows}
    end
  end
  
  # Step 4: Execute workflows with OTEL tracing
  step :execute_workflows do
    argument :workflows, result(:generate_workflows)
    
    run fn %{workflows: workflows}, context ->
      :telemetry.span(
        [:ash_reactor, :ttl, :execute],
        %{workflow_count: length(workflows)},
        fn ->
          results = Enum.map(workflows, fn workflow ->
            # Mark workflow as executed
            {:ok, executed} = AshReactor80_20.Resources.ReactorWorkflow.execute_workflow(workflow)
            
            # Mark corresponding class as processed
            {:ok, _} = AshReactor80_20.Resources.OntologyClass.process_class(%{id: workflow.ontology_class_id})
            
            executed
          end)
          
          {:ok, %{
            workflows_executed: length(results),
            execution_time: DateTime.utc_now(),
            results: results
          }}
        end
      )
    end
  end
  
  return :execute_workflows
end

defmodule AshReactor80_20.SimpleDemo do
  @moduledoc """
  Simple demonstration of TTL → Ash.Reactor transformation
  """
  
  def run_demo(ttl_content \\ sample_ttl()) do
    IO.puts("\n🚀 Running Ash.Reactor 80/20 Demo\n")
    
    case Reactor.run(AshReactor80_20.TTLReactor, %{ttl_content: ttl_content}) do
      {:ok, result} ->
        IO.puts("✅ Success!")
        IO.puts("   Workflows executed: #{result.workflows_executed}")
        IO.puts("   Execution time: #{result.execution_time}")
        {:ok, result}
        
      {:error, error} ->
        IO.puts("❌ Error: #{inspect(error)}")
        {:error, error}
    end
  end
  
  defp sample_ttl do
    """
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix ex: <http://example.org/> .
    
    ex:Person a owl:Class .
    ex:Organization a owl:Class .
    ex:Event a owl:Class .
    """
  end
end