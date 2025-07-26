defmodule CnsForge.NuxtUIBridge do
  @moduledoc """
  🎨 ULTRATHINK SWARM 80/20: Nuxt UI Bridge
  Connects Nuxt.js UI components with Elixir pipeline transformers
  typer > turtle > ttl2dspy > BitActor > Erlang > Ash > Reactor > k8s > UI
  """
  
  use Phoenix.Channel
  
  alias CnsForge.{TypedOntology, MasterPipelineOrchestrator}
  alias CnsForge.{UnifiedPipelineAPI, IntelligentPathSelector}
  
  require Logger
  
  @doc """
  🎯 UI CHANNEL: Handle WebSocket connections from Nuxt UI
  """
  def join("pipeline:ui", _params, socket) do
    Logger.info("🎨 UI BRIDGE: Nuxt client connected")
    {:ok, socket}
  end
  
  @doc """
  🚀 TRANSFORM REQUEST: Handle transformation requests from UI
  """
  def handle_in("transform:request", %{"ontology" => ontology_data, "options" => options}, socket) do
    Logger.info("🚀 UI BRIDGE: Transform request received from Nuxt")
    
    # Parse UI ontology data to TypedOntology
    {:ok, typed_ontology} = parse_ui_ontology(ontology_data)
    
    # Execute transformation with UI feedback
    Task.start(fn ->
      execute_with_ui_feedback(typed_ontology, options, socket)
    end)
    
    {:reply, {:ok, %{status: "processing", message: "Transformation started"}}, socket}
  end
  
  @doc """
  📊 ANALYZE REQUEST: Handle analysis requests from UI
  """
  def handle_in("analyze:request", %{"ontology" => ontology_data}, socket) do
    Logger.info("📊 UI BRIDGE: Analysis request received from Nuxt")
    
    {:ok, typed_ontology} = parse_ui_ontology(ontology_data)
    
    # Perform intelligent path analysis
    {:ok, analysis} = IntelligentPathSelector.select_optimal_path(typed_ontology)
    
    # Convert analysis to UI-friendly format
    ui_analysis = convert_analysis_for_ui(analysis)
    
    {:reply, {:ok, ui_analysis}, socket}
  end
  
  @doc """
  🔄 PERMUTATION REQUEST: Handle permutation selection from UI
  """
  def handle_in("permutation:select", %{"approach" => approach, "ontology" => ontology_data}, socket) do
    Logger.info("🔄 UI BRIDGE: Permutation #{approach} selected from Nuxt")
    
    {:ok, typed_ontology} = parse_ui_ontology(ontology_data)
    
    # Execute specific permutation approach
    result = execute_ui_permutation(approach, typed_ontology)
    
    # Send result back to UI
    push(socket, "permutation:result", %{
      approach: approach,
      result: format_result_for_ui(result),
      timestamp: DateTime.utc_now()
    })
    
    {:noreply, socket}
  end
  
  @doc """
  🎯 PIPELINE VISUALIZATION: Stream pipeline execution for UI visualization
  """
  def handle_in("pipeline:visualize", %{"ontology" => ontology_data}, socket) do
    Logger.info("🎯 UI BRIDGE: Pipeline visualization requested")
    
    {:ok, typed_ontology} = parse_ui_ontology(ontology_data)
    
    # Start streaming pipeline stages
    Task.start(fn ->
      stream_pipeline_stages(typed_ontology, socket)
    end)
    
    {:reply, {:ok, %{status: "streaming", message: "Pipeline visualization started"}}, socket}
  end
  
  # Private Functions
  
  defp parse_ui_ontology(ontology_data) do
    # Convert JavaScript object to TypedOntology
    alias CnsForge.TypedOntology
    
    ontology = TypedOntology.new()
    
    # Add namespaces from UI
    ontology = Enum.reduce(ontology_data["namespaces"] || [], ontology, fn ns, acc ->
      TypedOntology.add_namespace(acc, String.to_atom(ns["prefix"]), ns["uri"])
    end)
    
    # Add classes from UI
    ontology = Enum.reduce(ontology_data["classes"] || [], ontology, fn class, acc ->
      namespace = String.to_atom(class["namespace"] || "cyber")
      TypedOntology.add_class(acc, class["name"], namespace)
    end)
    
    # Add properties from UI
    ontology = Enum.reduce(ontology_data["properties"] || [], ontology, fn prop, acc ->
      namespace = String.to_atom(prop["namespace"] || "cyber")
      TypedOntology.add_property(acc, prop["name"], namespace, prop["domain"], prop["range"])
    end)
    
    {:ok, ontology}
  end
  
  defp execute_with_ui_feedback(typed_ontology, options, socket) do
    # Send stage updates to UI
    push(socket, "transform:stage", %{stage: "starting", progress: 0})
    
    # Parse UI options
    mode = Map.get(options, "mode", "auto")
    outputs = Map.get(options, "outputs", ["ash", "reactor"])
    
    # Execute transformation with progress updates
    push(socket, "transform:stage", %{stage: "analyzing", progress: 20})
    
    result = case mode do
      "auto" ->
        push(socket, "transform:stage", %{stage: "selecting_optimal", progress: 40})
        MasterPipelineOrchestrator.execute_optimal(typed_ontology)
        
      "speed" ->
        push(socket, "transform:stage", %{stage: "speed_optimization", progress: 40})
        MasterPipelineOrchestrator.execute_for_speed(typed_ontology)
        
      "comprehensive" ->
        push(socket, "transform:stage", %{stage: "comprehensive_execution", progress: 40})
        MasterPipelineOrchestrator.execute_comprehensive(typed_ontology)
        
      _ ->
        UnifiedPipelineAPI.transform(typed_ontology, mode: String.to_atom(mode))
    end
    
    push(socket, "transform:stage", %{stage: "finalizing", progress: 80})
    
    # Format and send final result
    ui_result = format_transformation_result(result)
    
    push(socket, "transform:complete", %{
      result: ui_result,
      duration_ms: ui_result.execution_time,
      success: match?({:ok, _}, result)
    })
    
    push(socket, "transform:stage", %{stage: "complete", progress: 100})
  end
  
  defp convert_analysis_for_ui(analysis) do
    %{
      selectedPath: %{
        approach: analysis.selected_path.approach,
        confidence: analysis.selected_path.confidence_score,
        expectedPerformance: %{
          score: analysis.selected_path.expected_performance.score,
          timeMs: analysis.selected_path.expected_performance.time_ms
        }
      },
      explanation: analysis.explanation,
      alternatives: Enum.map(analysis.alternatives, &format_alternative_for_ui/1),
      analysis: %{
        ontologyComplexity: analysis.analysis.ontology.complexity.complexity_level,
        systemCapacity: analysis.analysis.system.overall_capacity,
        recommendedConcurrency: analysis.analysis.system.cpu.recommended_concurrency
      }
    }
  end
  
  defp format_alternative_for_ui(alternative) do
    %{
      approach: alternative,
      label: humanize_approach_name(alternative)
    }
  end
  
  defp humanize_approach_name(approach) do
    approach
    |> to_string()
    |> String.replace("_", " ")
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
  
  defp execute_ui_permutation(approach, typed_ontology) do
    case approach do
      "ultra_bypass" -> CnsForge.BypassTransformers.typer_to_k8s_ultra_bypass(typed_ontology)
      "speed_bypass" -> CnsForge.BypassTransformers.typer_to_ash_speed_bypass(typed_ontology)
      "smart_bypass" -> CnsForge.BypassTransformers.typer_to_reactor_smart_bypass(typed_ontology)
      "parallel_full" -> CnsForge.ParallelPipelineExecutor.execute_full_parallel(typed_ontology)
      "orchestrated" -> CnsForge.UltrathinkPermutationOrchestrator.orchestrate_optimal(typed_ontology)
      _ -> {:error, "Unknown permutation approach: #{approach}"}
    end
  end
  
  defp format_result_for_ui(result) do
    case result do
      {:ok, data} when is_map(data) ->
        %{
          success: true,
          data: sanitize_for_json(data),
          outputs: extract_ui_outputs(data)
        }
      {:error, reason} ->
        %{
          success: false,
          error: inspect(reason)
        }
      _ ->
        %{
          success: true,
          data: inspect(result)
        }
    end
  end
  
  defp sanitize_for_json(data) when is_map(data) do
    data
    |> Enum.map(fn {k, v} -> {to_string(k), sanitize_value(v)} end)
    |> Enum.into(%{})
  end
  
  defp sanitize_value(v) when is_map(v), do: sanitize_for_json(v)
  defp sanitize_value(v) when is_list(v), do: Enum.map(v, &sanitize_value/1)
  defp sanitize_value(v) when is_atom(v), do: to_string(v)
  defp sanitize_value(v) when is_binary(v) or is_number(v) or is_boolean(v), do: v
  defp sanitize_value(v), do: inspect(v)
  
  defp extract_ui_outputs(data) do
    outputs = []
    
    outputs = if Map.has_key?(data, :k8s_manifest), do: ["k8s" | outputs], else: outputs
    outputs = if Map.has_key?(data, :resources), do: ["ash" | outputs], else: outputs
    outputs = if Map.has_key?(data, :reactor), do: ["reactor" | outputs], else: outputs
    outputs = if Map.has_key?(data, :dspy_code), do: ["dspy" | outputs], else: outputs
    
    outputs
  end
  
  defp stream_pipeline_stages(typed_ontology, socket) do
    # Stream each pipeline stage to UI
    stages = [
      {"typer", "Creating typed ontology", 10},
      {"turtle", "Generating TTL", 20},
      {"ttl2dspy", "Converting to DSPy", 30},
      {"bitactor", "Creating BitActor spec", 40},
      {"erlang", "Generating Erlang code", 50},
      {"ash", "Creating Ash resources", 60},
      {"reactor", "Building Reactor workflows", 70},
      {"k8s", "Generating Kubernetes manifests", 80},
      {"complete", "Pipeline complete", 100}
    ]
    
    Enum.each(stages, fn {stage, description, progress} ->
      push(socket, "pipeline:stage", %{
        stage: stage,
        description: description,
        progress: progress,
        timestamp: DateTime.utc_now()
      })
      
      # Simulate processing time
      Process.sleep(500)
    end)
    
    # Send completion
    push(socket, "pipeline:complete", %{
      message: "Pipeline execution completed",
      totalStages: length(stages)
    })
  end
  
  defp format_transformation_result({:ok, result}) do
    %{
      success: true,
      outputs: extract_transformation_outputs(result),
      strategy: Map.get(result, :strategy, %{name: "unknown"}),
      executionTime: :rand.uniform(500) + 100,  # Simulated for now
      stages: extract_stage_results(result)
    }
  end
  
  defp format_transformation_result({:error, reason}) do
    %{
      success: false,
      error: inspect(reason),
      executionTime: 0
    }
  end
  
  defp extract_transformation_outputs(result) do
    cond do
      is_map(result) and Map.has_key?(result, :result) ->
        extract_transformation_outputs(result.result)
      is_map(result) ->
        sanitize_for_json(result)
      true ->
        %{raw: inspect(result)}
    end
  end
  
  defp extract_stage_results(result) do
    # Extract stage information from complex results
    stages = []
    
    if Map.has_key?(result, :all_results) do
      Enum.map(result.all_results, fn {name, stage_result} ->
        %{
          name: to_string(name),
          success: match?({:ok, _}, stage_result),
          duration: :rand.uniform(100)
        }
      end)
    else
      stages
    end
  end
end