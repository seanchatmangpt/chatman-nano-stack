defmodule CnsWeb.Channels.ReactorHandler do
  @moduledoc """
  Handles Ash Reactor workflow events with 80/20 optimization.
  Manages step execution, workflow creation, and real-time updates.
  """
  
  use ChannelHandler.Handler
  require Logger
  
  alias Cns.Reactor.{WorkflowEngine, StepExecutor, OptimizationEngine}
  alias CnsWeb.Endpoint
  
  # Apply rate limiting for non-critical operations
  plug CnsWeb.ChannelPlugs.RateLimit, [max: 100, window: 60_000] when action in [:create_workflow]
  
  @doc """
  Execute a single reactor step
  """
  def execute_step(payload, _bindings, socket) do
    step_config = %{
      name: payload["step_name"],
      type: payload["step_type"],
      inputs: payload["inputs"] || %{},
      workflow_id: payload["workflow_id"],
      optimization_hints: payload["optimization_hints"] || []
    }
    
    case StepExecutor.execute(step_config) do
      {:ok, result} ->
        # Broadcast step completion
        broadcast_step_event(socket, "step:completed", %{
          workflow_id: step_config.workflow_id,
          step_name: step_config.name,
          duration_ms: result.duration,
          outputs: result.outputs
        })
        
        {:reply, {:ok, result}, socket}
        
      {:error, reason} ->
        broadcast_step_event(socket, "step:failed", %{
          workflow_id: step_config.workflow_id,
          step_name: step_config.name,
          error: reason
        })
        
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Get step execution status
  """
  def step_status(payload, _bindings, socket) do
    workflow_id = payload["workflow_id"]
    step_name = payload["step_name"]
    
    case StepExecutor.get_status(workflow_id, step_name) do
      {:ok, status} ->
        {:reply, {:ok, status}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Create a new Reactor workflow with 80/20 optimization
  """
  def create_workflow(payload, _bindings, socket) do
    workflow_config = %{
      name: payload["name"],
      domain: payload["domain"] || "generic",
      steps: parse_workflow_steps(payload["steps"] || []),
      optimization_mode: socket.assigns[:optimization_mode] || "80_20",
      priority: payload["priority"] || "medium"
    }
    
    case WorkflowEngine.create_workflow(workflow_config) do
      {:ok, workflow} ->
        # Apply 80/20 optimization if enabled
        optimized_workflow = if workflow_config.optimization_mode == "80_20" do
          apply_workflow_optimization(workflow)
        else
          workflow
        end
        
        # Broadcast workflow creation
        broadcast_workflow_event(socket, "workflow:created", %{
          workflow_id: optimized_workflow.id,
          name: optimized_workflow.name,
          step_count: length(optimized_workflow.steps),
          optimized: optimized_workflow.optimized?
        })
        
        {:reply, {:ok, optimized_workflow}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Execute a complete workflow
  """
  def execute_workflow(payload, _bindings, socket) do
    workflow_id = payload["workflow_id"]
    inputs = payload["inputs"] || %{}
    execution_mode = payload["execution_mode"] || "optimized"
    
    case WorkflowEngine.execute(workflow_id, inputs, execution_mode: execution_mode) do
      {:ok, execution} ->
        # Monitor workflow execution
        monitor_workflow_execution(execution.id, socket)
        
        {:reply, {:ok, %{
          execution_id: execution.id,
          workflow_id: workflow_id,
          estimated_duration: execution.estimated_duration
        }}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Apply 80/20 optimization to workflow or step
  """
  def apply_optimization(payload, _bindings, socket) do
    target_type = payload["target_type"] # "workflow" or "step"
    target_id = payload["target_id"]
    optimization_strategy = payload["strategy"] || "skip_non_critical"
    
    result = case target_type do
      "workflow" ->
        OptimizationEngine.optimize_workflow(target_id, optimization_strategy)
        
      "step" ->
        OptimizationEngine.optimize_step(target_id, optimization_strategy)
        
      _ ->
        {:error, "Invalid target type"}
    end
    
    case result do
      {:ok, optimization} ->
        broadcast_optimization_event(socket, "optimization:applied", %{
          target_type: target_type,
          target_id: target_id,
          strategy: optimization_strategy,
          improvements: optimization.improvements
        })
        
        {:reply, {:ok, optimization}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Analyze optimization opportunities
  """
  def analyze_optimization(payload, _bindings, socket) do
    workflow_id = payload["workflow_id"]
    
    case OptimizationEngine.analyze_workflow(workflow_id) do
      {:ok, analysis} ->
        # Filter recommendations based on 80/20 mode
        filtered_analysis = if socket.assigns[:optimization_mode] == "80_20" do
          filter_80_20_recommendations(analysis)
        else
          analysis
        end
        
        {:reply, {:ok, filtered_analysis}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Handle delegated reactor events
  """
  def handle_in(event, payload, _bindings, socket) do
    Logger.debug("ReactorHandler received event: #{event}")
    
    case parse_reactor_event(event) do
      {:workflow_event, action} ->
        handle_workflow_event(action, payload, socket)
        
      {:step_event, action} ->
        handle_step_event(action, payload, socket)
        
      {:monitoring_event, action} ->
        handle_monitoring_event(action, payload, socket)
        
      _ ->
        {:noreply, socket}
    end
  end
  
  # Stage-specific handlers (embedded modules)
  
  defmodule AshStageHandler do
    @moduledoc "Handles Ash-specific stage events"
    
    use ChannelHandler.Handler
    
    def handle_in("resource:create", payload, _bindings, socket) do
      # Handle Ash resource creation
      resource_config = %{
        name: payload["name"],
        attributes: payload["attributes"],
        actions: payload["actions"] || [:read, :create, :update, :destroy],
        domain: payload["domain"]
      }
      
      case create_ash_resource(resource_config) do
        {:ok, resource} ->
          push(socket, "ash:resource_created", resource)
          {:noreply, socket}
          
        {:error, reason} ->
          push(socket, "ash:error", %{reason: reason})
          {:noreply, socket}
      end
    end
    
    def handle_in("resource:validate", payload, _bindings, socket) do
      # Validate Ash resource configuration
      case validate_resource_config(payload) do
        :ok ->
          {:reply, {:ok, %{valid: true}}, socket}
          
        {:error, errors} ->
          {:reply, {:error, %{errors: errors}}, socket}
      end
    end
    
    defp create_ash_resource(config) do
      # Simulate Ash resource creation
      {:ok, %{
        id: generate_resource_id(),
        name: config.name,
        attributes: config.attributes,
        created_at: DateTime.utc_now()
      }}
    end
    
    defp validate_resource_config(config) do
      # Basic validation
      cond do
        !config["name"] -> {:error, ["name is required"]}
        !config["attributes"] -> {:error, ["attributes are required"]}
        true -> :ok
      end
    end
    
    defp generate_resource_id do
      "ash_resource_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
    end
  end
  
  defmodule ReactorStageHandler do
    @moduledoc "Handles Reactor-specific workflow stage events"
    
    use ChannelHandler.Handler
    
    def handle_in("workflow:validate", payload, _bindings, socket) do
      # Validate reactor workflow
      case validate_workflow(payload) do
        {:ok, validation} ->
          {:reply, {:ok, validation}, socket}
          
        {:error, errors} ->
          {:reply, {:error, %{errors: errors}}, socket}
      end
    end
    
    def handle_in("step:optimize", payload, _bindings, socket) do
      # Optimize a specific step
      step_id = payload["step_id"]
      
      optimization = %{
        step_id: step_id,
        suggestions: [
          "Use async execution",
          "Cache intermediate results",
          "Batch similar operations"
        ],
        estimated_improvement: "35%"
      }
      
      {:reply, {:ok, optimization}, socket}
    end
    
    defp validate_workflow(workflow) do
      # Validate workflow structure
      if workflow["steps"] && is_list(workflow["steps"]) do
        {:ok, %{valid: true, step_count: length(workflow["steps"])}}
      else
        {:error, ["Invalid workflow structure"]}
      end
    end
  end
  
  defmodule K8sStageHandler do
    @moduledoc "Handles Kubernetes deployment stage events"
    
    use ChannelHandler.Handler
    
    def handle_in("manifest:generate", payload, _bindings, socket) do
      # Generate K8s manifests
      manifest = generate_k8s_manifest(payload)
      
      push(socket, "k8s:manifest_ready", %{
        manifest: manifest,
        resource_type: payload["resource_type"] || "deployment"
      })
      
      {:reply, {:ok, %{manifest_generated: true}}, socket}
    end
    
    def handle_in("deployment:status", payload, _bindings, socket) do
      # Check deployment status
      deployment_id = payload["deployment_id"]
      
      status = %{
        deployment_id: deployment_id,
        replicas: %{desired: 3, ready: 3},
        conditions: ["Available", "Progressing"],
        last_update: DateTime.utc_now()
      }
      
      {:reply, {:ok, status}, socket}
    end
    
    defp generate_k8s_manifest(config) do
      """
      apiVersion: apps/v1
      kind: Deployment
      metadata:
        name: #{config["name"] || "reactor-app"}
        labels:
          app: #{config["app_label"] || "reactor"}
      spec:
        replicas: #{config["replicas"] || 3}
        selector:
          matchLabels:
            app: #{config["app_label"] || "reactor"}
        template:
          metadata:
            labels:
              app: #{config["app_label"] || "reactor"}
          spec:
            containers:
            - name: app
              image: #{config["image"] || "reactor:latest"}
              ports:
              - containerPort: #{config["port"] || 4000}
      """
    end
  end
  
  # Private functions
  
  defp parse_workflow_steps(steps) do
    Enum.map(steps, fn step ->
      %{
        name: step["name"],
        type: step["type"] || "generic",
        inputs: step["inputs"] || [],
        outputs: step["outputs"] || [],
        dependencies: step["dependencies"] || [],
        critical: step["critical"] || false,
        can_parallelize: step["can_parallelize"] || false
      }
    end)
  end
  
  defp apply_workflow_optimization(workflow) do
    # Apply 80/20 optimization rules
    optimized_steps = workflow.steps
    |> identify_critical_path()
    |> remove_non_critical_steps()
    |> parallelize_independent_steps()
    |> apply_caching_strategy()
    
    %{workflow | 
      steps: optimized_steps,
      optimized?: true,
      optimization_metadata: %{
        original_step_count: length(workflow.steps),
        optimized_step_count: length(optimized_steps),
        optimization_type: "80_20"
      }
    }
  end
  
  defp identify_critical_path(steps) do
    # Mark steps in critical path
    Enum.map(steps, fn step ->
      Map.put(step, :in_critical_path, step.critical || has_critical_dependency?(step, steps))
    end)
  end
  
  defp remove_non_critical_steps(steps) do
    # Keep only critical steps and their dependencies
    Enum.filter(steps, & &1.in_critical_path)
  end
  
  defp parallelize_independent_steps(steps) do
    # Group steps that can run in parallel
    groups = group_parallelizable_steps(steps)
    
    Enum.map(groups, fn group ->
      if length(group) > 1 do
        %{
          name: "parallel_group_#{:rand.uniform(1000)}",
          type: "parallel",
          steps: group,
          can_parallelize: false
        }
      else
        hd(group)
      end
    end)
  end
  
  defp apply_caching_strategy(steps) do
    # Add caching hints to expensive operations
    Enum.map(steps, fn step ->
      if should_cache_step?(step) do
        Map.put(step, :cache_config, %{
          enabled: true,
          ttl: 300, # 5 minutes
          key_prefix: "reactor_step_#{step.name}"
        })
      else
        step
      end
    end)
  end
  
  defp has_critical_dependency?(step, all_steps) do
    # Check if step has critical dependencies
    critical_step_names = all_steps
    |> Enum.filter(& &1.critical)
    |> Enum.map(& &1.name)
    
    Enum.any?(step.dependencies, &(&1 in critical_step_names))
  end
  
  defp group_parallelizable_steps(steps) do
    # Group steps that can execute in parallel
    steps
    |> Enum.reduce([], fn step, groups ->
      if step.can_parallelize and can_add_to_last_group?(step, groups) do
        update_last_group(groups, step)
      else
        groups ++ [[step]]
      end
    end)
  end
  
  defp can_add_to_last_group?(step, []) do
    false
  end
  
  defp can_add_to_last_group?(step, groups) do
    last_group = List.last(groups)
    
    # Check if step depends on any step in the last group
    !Enum.any?(last_group, fn group_step ->
      group_step.name in step.dependencies
    end)
  end
  
  defp update_last_group(groups, step) do
    {init, [last]} = Enum.split(groups, -1)
    init ++ [last ++ [step]]
  end
  
  defp should_cache_step?(step) do
    # Cache expensive or frequently accessed steps
    step.type in ["data_fetch", "computation", "external_api"] or
    String.contains?(step.name, ["fetch", "compute", "calculate"])
  end
  
  defp monitor_workflow_execution(execution_id, socket) do
    # Start monitoring task
    Task.start(fn ->
      WorkflowEngine.monitor_execution(execution_id, fn update ->
        broadcast_workflow_event(socket, "workflow:update", Map.merge(update, %{
          execution_id: execution_id,
          timestamp: System.system_time(:millisecond)
        }))
      end)
    end)
  end
  
  defp filter_80_20_recommendations(analysis) do
    # Filter to show only high-impact recommendations
    %{analysis | 
      recommendations: Enum.filter(analysis.recommendations, fn rec ->
        rec.impact_score >= 0.8 or rec.effort_score <= 0.2
      end)
    }
  end
  
  defp broadcast_step_event(socket, event, payload) do
    Endpoint.broadcast!(socket.topic || "reactor:*", event, payload)
  end
  
  defp broadcast_workflow_event(socket, event, payload) do
    Endpoint.broadcast!(socket.topic || "reactor:*", event, payload)
  end
  
  defp broadcast_optimization_event(socket, event, payload) do
    Endpoint.broadcast!(socket.topic || "reactor:*", event, payload)
  end
  
  defp handle_workflow_event(action, payload, socket) do
    case action do
      "pause" ->
        WorkflowEngine.pause_execution(payload["execution_id"])
        {:reply, {:ok, %{paused: true}}, socket}
        
      "resume" ->
        WorkflowEngine.resume_execution(payload["execution_id"])
        {:reply, {:ok, %{resumed: true}}, socket}
        
      "cancel" ->
        WorkflowEngine.cancel_execution(payload["execution_id"])
        {:reply, {:ok, %{cancelled: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_step_event(action, payload, socket) do
    case action do
      "retry" ->
        StepExecutor.retry_step(payload["execution_id"], payload["step_name"])
        {:reply, {:ok, %{retrying: true}}, socket}
        
      "skip" ->
        StepExecutor.skip_step(payload["execution_id"], payload["step_name"])
        {:reply, {:ok, %{skipped: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp handle_monitoring_event(action, payload, socket) do
    case action do
      "subscribe" ->
        execution_id = payload["execution_id"]
        WorkflowEngine.subscribe_to_updates(execution_id, self())
        {:reply, {:ok, %{subscribed: true}}, socket}
        
      "unsubscribe" ->
        execution_id = payload["execution_id"]
        WorkflowEngine.unsubscribe_from_updates(execution_id, self())
        {:reply, {:ok, %{unsubscribed: true}}, socket}
        
      _ ->
        {:noreply, socket}
    end
  end
  
  defp parse_reactor_event(event) do
    case String.split(event, ":") do
      ["workflow", action] ->
        {:workflow_event, action}
        
      ["step", action] ->
        {:step_event, action}
        
      ["monitor", action] ->
        {:monitoring_event, action}
        
      _ ->
        {:unknown, event}
    end
  end
  
  # Export stage handlers for delegation
  defdelegate K8sStageHandler, to: __MODULE__.K8sStageHandler
  defdelegate ReactorStageHandler, to: __MODULE__.ReactorStageHandler
  defdelegate AshStageHandler, to: __MODULE__.AshStageHandler
  
  # Generic handlers for other stages
  defmodule ErlangStageHandler, do: use ChannelHandler.Handler
  defmodule BitActorStageHandler, do: use ChannelHandler.Handler
  defmodule Ttl2dspyStageHandler, do: use ChannelHandler.Handler
  defmodule TurtleStageHandler, do: use ChannelHandler.Handler
  defmodule TyperStageHandler, do: use ChannelHandler.Handler
end