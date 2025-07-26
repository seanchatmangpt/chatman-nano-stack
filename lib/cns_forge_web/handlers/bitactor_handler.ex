defmodule CnsForgeWeb.Handlers.BitActorHandler do
  @moduledoc """
  Handler for BitActor distributed coordination stage in the ULTRATHINK pipeline.
  Manages actor spawning, mesh networking, and distributed message routing.
  """
  
  use ChannelHandler.Handler
  
  alias CnsForge.BitActor.{Coordinator, MeshNetwork, ActorRegistry}
  alias CnsForgeWeb.NotificationBroadcaster
  
  require Logger
  
  # Plugs for distributed system checks
  plug CnsForgeWeb.ChannelPlugs.EnsureDistributed
  plug CnsForgeWeb.ChannelPlugs.CheckActorCapacity
  
  # Actor type definitions
  @actor_types ~w(coordinator monitor router metrics error_handler worker specialist)a
  
  @doc """
  Spawn BitActor actors for pipeline coordination
  """
  def spawn_actors(payload, _bindings, socket) do
    Logger.info("Spawning BitActor actors for pipeline #{socket.assigns.pipeline_id}")
    
    actor_config = build_actor_config(payload, socket)
    
    case Coordinator.spawn_actor_pool(actor_config) do
      {:ok, actors} ->
        socket = assign(socket, :bitactors, actors)
        
        # Broadcast actor spawn events
        Enum.each(actors, fn {type, actor} ->
          NotificationBroadcaster.broadcast_stage_event(socket, "bitactor:spawned", %{
            type: type,
            actor_id: actor.id,
            node: actor.node,
            capabilities: actor.capabilities
          })
        end)
        
        {:reply, {:ok, %{
          message: "BitActor pool spawned successfully",
          actors: format_actor_list(actors),
          mesh_topology: actor_config.topology
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: "Failed to spawn actors: #{reason}"}}, socket}
    end
  end
  
  @doc """
  Coordinate actors for distributed task execution
  """
  def coordinate(payload, _bindings, socket) do
    task = Map.get(payload, "task", %{})
    strategy = Map.get(payload, "strategy", "round_robin")
    
    actors = socket.assigns[:bitactors] || %{}
    
    case Coordinator.distribute_task(actors, task, strategy) do
      {:ok, distribution} ->
        # Track coordination
        NotificationBroadcaster.broadcast_stage_event(socket, "bitactor:coordinated", %{
          task_id: distribution.task_id,
          assigned_actors: length(distribution.assignments),
          strategy: strategy
        })
        
        {:reply, {:ok, %{
          message: "Task distributed successfully",
          task_id: distribution.task_id,
          assignments: distribution.assignments
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Handle mesh network operations
  """
  def handle_mesh(payload, %{splat: [operation]}, socket) do
    case operation do
      "join" ->
        join_mesh_network(payload, socket)
        
      "leave" ->
        leave_mesh_network(payload, socket)
        
      "status" ->
        get_mesh_status(socket)
        
      "broadcast" ->
        broadcast_to_mesh(payload, socket)
        
      _ ->
        {:reply, {:error, %{reason: "Unknown mesh operation: #{operation}"}}, socket}
    end
  end
  
  @doc """
  Route messages to specific actor types
  """
  def route_to_actor(actor_type, payload, socket) do
    actor_type_atom = String.to_atom(actor_type)
    
    if actor_type_atom in @actor_types do
      case find_actor_by_type(socket.assigns[:bitactors], actor_type_atom) do
        {:ok, actor} ->
          send_to_actor(actor, payload, socket)
          
        :error ->
          {:reply, {:error, %{reason: "No actor of type #{actor_type} found"}}, socket}
      end
    else
      {:reply, {:error, %{reason: "Invalid actor type: #{actor_type}"}}, socket}
    end
  end
  
  @doc """
  Generic handler for delegated BitActor events
  """
  def handle_in(event, payload, _bindings, socket) do
    Logger.debug("Handling BitActor event: #{event}")
    
    case String.split(event, ":") do
      ["pool", action] ->
        handle_pool_action(action, payload, socket)
        
      ["actor", actor_id, action] ->
        handle_actor_action(actor_id, action, payload, socket)
        
      ["mesh", action] ->
        handle_mesh_action(action, payload, socket)
        
      _ ->
        {:reply, {:error, %{reason: "Unknown BitActor event: #{event}"}}, socket}
    end
  end
  
  # Private functions
  
  defp build_actor_config(payload, socket) do
    %{
      pipeline_id: socket.assigns.pipeline_id,
      topology: Map.get(payload, "topology", :mesh),
      actor_count: Map.get(payload, "actor_count", 5),
      redundancy: Map.get(payload, "redundancy", 2),
      capabilities: extract_capabilities(payload),
      nodes: available_nodes(),
      fault_tolerance: %{
        strategy: :raft,
        heartbeat_interval: 5000,
        election_timeout: 10000
      }
    }
  end
  
  defp extract_capabilities(payload) do
    default_capabilities = %{
      coordinator: [:orchestration, :scheduling, :recovery],
      monitor: [:metrics, :health_check, :alerting],
      router: [:message_routing, :load_balancing, :failover],
      metrics: [:collection, :aggregation, :export],
      error_handler: [:error_detection, :recovery, :escalation],
      worker: [:task_execution, :data_processing],
      specialist: [:ml_inference, :optimization, :analysis]
    }
    
    Map.get(payload, "capabilities", default_capabilities)
  end
  
  defp available_nodes do
    [Node.self() | Node.list()]
    |> Enum.take(3)
    |> Enum.map(&to_string/1)
  end
  
  defp format_actor_list(actors) do
    Enum.map(actors, fn {type, actor} ->
      %{
        type: type,
        id: actor.id,
        node: actor.node,
        status: actor.status,
        capabilities: actor.capabilities
      }
    end)
  end
  
  defp join_mesh_network(payload, socket) do
    node_info = %{
      node: Map.get(payload, "node", Node.self()),
      capabilities: Map.get(payload, "capabilities", []),
      actors: socket.assigns[:bitactors] || %{}
    }
    
    case MeshNetwork.join(socket.assigns.pipeline_id, node_info) do
      {:ok, mesh_state} ->
        socket = assign(socket, :mesh_connected, true)
        
        NotificationBroadcaster.broadcast_stage_event(socket, "mesh:joined", %{
          nodes: length(mesh_state.nodes),
          topology: mesh_state.topology
        })
        
        {:reply, {:ok, %{
          message: "Joined mesh network",
          mesh_id: mesh_state.id,
          peers: mesh_state.nodes
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp leave_mesh_network(_payload, socket) do
    case MeshNetwork.leave(socket.assigns.pipeline_id) do
      :ok ->
        socket = assign(socket, :mesh_connected, false)
        
        NotificationBroadcaster.broadcast_stage_event(socket, "mesh:left", %{
          pipeline_id: socket.assigns.pipeline_id
        })
        
        {:reply, {:ok, %{message: "Left mesh network"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp get_mesh_status(socket) do
    case MeshNetwork.status(socket.assigns.pipeline_id) do
      {:ok, status} ->
        {:reply, {:ok, status}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp broadcast_to_mesh(payload, socket) do
    message = Map.get(payload, "message", %{})
    
    case MeshNetwork.broadcast(socket.assigns.pipeline_id, message) do
      {:ok, broadcast_id} ->
        NotificationBroadcaster.broadcast_stage_event(socket, "mesh:broadcast", %{
          broadcast_id: broadcast_id,
          recipients: :all
        })
        
        {:reply, {:ok, %{
          message: "Broadcast sent",
          broadcast_id: broadcast_id
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp find_actor_by_type(actors, type) when is_map(actors) do
    case Map.get(actors, type) do
      nil -> :error
      actor -> {:ok, actor}
    end
  end
  
  defp find_actor_by_type(_, _), do: :error
  
  defp send_to_actor(actor, payload, socket) do
    case ActorRegistry.send_message(actor.id, payload) do
      :ok ->
        NotificationBroadcaster.broadcast_stage_event(socket, "actor:message_sent", %{
          actor_id: actor.id,
          message_type: Map.get(payload, "type", "unknown")
        })
        
        {:reply, {:ok, %{message: "Message sent to actor #{actor.id}"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_pool_action("scale", payload, socket) do
    scale_factor = Map.get(payload, "factor", 1.5)
    
    case Coordinator.scale_pool(socket.assigns.pipeline_id, scale_factor) do
      {:ok, new_actors} ->
        actors = Map.merge(socket.assigns[:bitactors] || %{}, new_actors)
        socket = assign(socket, :bitactors, actors)
        
        {:reply, {:ok, %{
          message: "Actor pool scaled",
          new_actor_count: map_size(actors)
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_pool_action("health_check", _payload, socket) do
    actors = socket.assigns[:bitactors] || %{}
    
    health_status = Enum.map(actors, fn {type, actor} ->
      {type, ActorRegistry.health_check(actor.id)}
    end)
    |> Enum.into(%{})
    
    {:reply, {:ok, %{health_status: health_status}}, socket}
  end
  
  defp handle_actor_action(actor_id, "ping", _payload, socket) do
    case ActorRegistry.ping(actor_id) do
      {:ok, latency} ->
        {:reply, {:ok, %{actor_id: actor_id, latency: latency}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_actor_action(actor_id, "restart", _payload, socket) do
    case ActorRegistry.restart_actor(actor_id) do
      :ok ->
        NotificationBroadcaster.broadcast_stage_event(socket, "actor:restarted", %{
          actor_id: actor_id
        })
        
        {:reply, {:ok, %{message: "Actor restarted"}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  defp handle_mesh_action("optimize", payload, socket) do
    strategy = Map.get(payload, "strategy", "auto")
    
    case MeshNetwork.optimize_topology(socket.assigns.pipeline_id, strategy) do
      {:ok, optimized} ->
        {:reply, {:ok, %{
          message: "Mesh topology optimized",
          new_topology: optimized.topology,
          improvement: optimized.improvement
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
end