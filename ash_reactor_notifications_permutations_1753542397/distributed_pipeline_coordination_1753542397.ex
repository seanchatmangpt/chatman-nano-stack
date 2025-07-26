
defmodule CnsForge.DistributedPipelineCoordinator do
  @moduledoc """
  Distributed coordination across pipeline stages using Erlang distribution
  Coordinates: typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
  """
  
  use GenServer
  require Logger
  
  defstruct [:node_registry, :pipeline_state, :coordination_channels]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    # Connect to cluster nodes
    connect_to_cluster()
    
    state = %__MODULE__{
      node_registry: %{},
      pipeline_state: %{},
      coordination_channels: setup_coordination_channels()
    }
    
    {:ok, state}
  end
  
  # Register a pipeline node
  def register_node(node_name, node_type, capabilities) do
    GenServer.call(__MODULE__, {:register_node, node_name, node_type, capabilities})
  end
  
  # Coordinate pipeline execution across nodes
  def coordinate_pipeline(pipeline_id, stages) do
    GenServer.call(__MODULE__, {:coordinate_pipeline, pipeline_id, stages})
  end
  
  def handle_call({:register_node, node_name, node_type, capabilities}, _from, state) do
    node_info = %{
      name: node_name,
      type: node_type,
      capabilities: capabilities,
      status: :ready,
      registered_at: DateTime.utc_now()
    }
    
    new_registry = Map.put(state.node_registry, node_name, node_info)
    
    # Broadcast node registration
    broadcast_to_cluster({:node_registered, node_name, node_info})
    
    {:reply, :ok, %{state | node_registry: new_registry}}
  end
  
  def handle_call({:coordinate_pipeline, pipeline_id, stages}, _from, state) do
    # Create distributed execution plan
    execution_plan = create_execution_plan(stages, state.node_registry)
    
    # Initialize pipeline state
    pipeline_state = %{
      id: pipeline_id,
      stages: stages,
      execution_plan: execution_plan,
      current_stage: 0,
      status: :running,
      started_at: DateTime.utc_now()
    }
    
    new_state = put_in(state.pipeline_state[pipeline_id], pipeline_state)
    
    # Start pipeline execution
    execute_next_stage(pipeline_id, new_state)
    
    {:reply, {:ok, pipeline_id}, new_state}
  end
  
  # Handle stage completion from remote nodes
  def handle_info({:stage_completed, pipeline_id, stage_index, result}, state) do
    case state.pipeline_state[pipeline_id] do
      nil ->
        Logger.warn("Received completion for unknown pipeline: #{pipeline_id}")
        {:noreply, state}
      
      pipeline ->
        updated_pipeline = update_pipeline_progress(pipeline, stage_index, result)
        new_state = put_in(state.pipeline_state[pipeline_id], updated_pipeline)
        
        # Continue with next stage or complete pipeline
        if stage_index + 1 < length(pipeline.stages) do
          execute_next_stage(pipeline_id, new_state)
        else
          complete_pipeline(pipeline_id, new_state)
        end
        
        {:noreply, new_state}
    end
  end
  
  defp connect_to_cluster do
    # Connect to known nodes in the cluster
    cluster_nodes = Application.get_env(:cns_forge, :cluster_nodes, [])
    
    Enum.each(cluster_nodes, fn node ->
      case Node.connect(node) do
        true -> Logger.info("Connected to cluster node: #{node}")
        false -> Logger.warn("Failed to connect to: #{node}")
      end
    end)
  end
  
  defp setup_coordination_channels do
    # Setup Phoenix PubSub channels for coordination
    channels = [
      "coordination:pipeline",
      "coordination:nodes", 
      "coordination:telemetry"
    ]
    
    Enum.each(channels, fn channel ->
      Phoenix.PubSub.subscribe(CnsForge.PubSub, channel)
    end)
    
    channels
  end
  
  defp create_execution_plan(stages, node_registry) do
    # Map stages to available nodes based on capabilities
    Enum.with_index(stages)
    |> Enum.map(fn {stage, index} ->
      suitable_nodes = find_suitable_nodes(stage, node_registry)
      selected_node = select_best_node(suitable_nodes)
      
      %{
        stage: stage,
        index: index,
        assigned_node: selected_node,
        estimated_duration: estimate_duration(stage),
        dependencies: get_stage_dependencies(index, stages)
      }
    end)
  end
  
  defp find_suitable_nodes(stage, node_registry) do
    required_capability = stage_to_capability(stage)
    
    node_registry
    |> Enum.filter(fn {_name, info} ->
      required_capability in info.capabilities and info.status == :ready
    end)
    |> Enum.map(fn {name, info} -> {name, info} end)
  end
  
  defp stage_to_capability(stage) do
    case stage do
      "typer_80_20_input" -> "typer_processing"
      "turtle_generation" -> "turtle_generation"
      "ttl2dspy_transformation" -> "dspy_processing"
      "bitactor_processing" -> "bitactor_execution"
      "erlang_otp_coordination" -> "erlang_coordination"
      "ash_resource_creation" -> "ash_processing"
      "reactor_workflow_execution" -> "reactor_execution"
      "k8s_deployment" -> "k8s_deployment"
    end
  end
  
  defp select_best_node(suitable_nodes) do
    # Select node with best performance characteristics
    case suitable_nodes do
      [] -> nil
      [{name, _info}] -> name
      nodes -> 
        # Select based on load, latency, etc.
        {name, _info} = Enum.random(nodes)
        name
    end
  end
  
  defp execute_next_stage(pipeline_id, state) do
    pipeline = state.pipeline_state[pipeline_id]
    current_stage = Enum.at(pipeline.execution_plan, pipeline.current_stage)
    
    if current_stage && current_stage.assigned_node do
      # Send execution request to assigned node
      execute_on_node(current_stage.assigned_node, pipeline_id, current_stage)
    else
      Logger.error("No suitable node for stage: #{inspect(current_stage)}")
    end
  end
  
  defp execute_on_node(node_name, pipeline_id, stage) do
    # Send execution command to remote node
    GenServer.cast({__MODULE__, node_name}, 
      {:execute_stage, pipeline_id, stage})
    
    # Broadcast stage start
    broadcast_to_cluster({:stage_started, pipeline_id, stage.index, node_name})
  end
  
  defp broadcast_to_cluster(message) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "coordination:pipeline", message)
  end
  
  defp update_pipeline_progress(pipeline, stage_index, result) do
    %{pipeline | 
      current_stage: stage_index + 1,
      stages: List.replace_at(pipeline.stages, stage_index, %{result: result})
    }
  end
  
  defp complete_pipeline(pipeline_id, state) do
    Logger.info("Pipeline completed: #{pipeline_id}")
    broadcast_to_cluster({:pipeline_completed, pipeline_id})
  end
  
  defp estimate_duration(_stage), do: 1000  # milliseconds
  defp get_stage_dependencies(_index, _stages), do: []
end
