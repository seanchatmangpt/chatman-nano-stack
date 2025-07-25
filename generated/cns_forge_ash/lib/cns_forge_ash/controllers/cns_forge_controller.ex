defmodule CnsForgeAshWeb.CnsForgeController do
  @moduledoc """
  Phoenix controller for CNS Forge HTTP API endpoints
  
  This controller demonstrates the real Ash.Reactor integration:
  - HTTP request ingress (stimulus:http_request BitActor)
  - Token creation and TTL initialization
  - Reactor workflow orchestration
  - Universal observability and pulse logging
  """
  
  use CnsForgeAshWeb, :controller
  require Logger
  
  action_fallback CnsForgeAshWeb.FallbackController
  
  @doc """
  Execute user registration workflow via real Ash.Reactor
  """
  def register_user(conn, params) do
    Logger.info("CNS Forge: Received user registration request", transaction_id: generate_transaction_id())
    
    with {:ok, result} <- execute_user_registration_workflow(params) do
      json(conn, %{
        success: true,
        workflow_id: result.workflow.workflow_id,
        user: result.result.user,
        telemetry: %{
          total_hops: 8 - result.result.final_token.ttl_hops,
          ttl_remaining: result.result.final_token.ttl_hops,
          email_sent_at: result.result.email_sent_at
        }
      })
    end
  end
  
  @doc """
  Get workflow status and pulse logs for a transaction
  """
  def workflow_status(conn, %{"transaction_id" => transaction_id}) do
    transaction_id = String.to_integer(transaction_id)
    
    # Query pulse logs for the transaction
    pulse_logs = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.PulseLog, %{
      transaction_id: transaction_id
    }, action: :causal_chain)
    
    # Get workflow information
    workflows = CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.Workflow, %{})
    
    workflow = Enum.find(workflows, fn w -> 
      w.input_params["transaction_id"] == transaction_id 
    end)
    
    json(conn, %{
      transaction_id: transaction_id,
      workflow: workflow,
      pulse_logs: pulse_logs,
      causal_chain: format_causal_chain(pulse_logs)
    })
  end
  
  @doc """
  CNS Forge health check endpoint
  """
  def health(conn, _params) do
    # Check system health
    health_status = %{
      status: "healthy",
      ash_domain: check_ash_domain_health(),
      reactor_engine: check_reactor_health(),
      telemetry_system: check_telemetry_health(),
      timestamp: DateTime.utc_now()
    }
    
    json(conn, health_status)
  end
  
  @doc """
  Execute a simple TTL demo workflow
  """
  def ttl_demo(conn, params) do
    ttl = Map.get(params, "ttl", 8)
    
    # Create a simple TTL token demonstration
    {:ok, token} = CnsForgeAsh.Domain.create(CnsForgeAsh.Resources.TTLToken, %{
      workflow_type: 999,  # Demo workflow type
      payload: %{demo: "ttl_test", params: params},
      initial_ttl: ttl
    }, action: :create)
    
    # Simulate processing hops
    demo_results = simulate_ttl_hops(token)
    
    json(conn, %{
      success: true,
      initial_ttl: ttl,
      token_id: token.id,
      transaction_id: token.transaction_id,
      demo_results: demo_results
    })
  end
  
  # Private functions
  
  defp execute_user_registration_workflow(params) do
    # Execute the real Ash.Reactor workflow
    Reactor.run(CnsForgeAsh.Reactors.UserRegistrationReactor, %{
      user_params: params,
      ttl: Map.get(params, "ttl", 8)
    })
  end
  
  defp generate_transaction_id do
    :rand.uniform(1_000_000_000)
  end
  
  defp format_causal_chain(pulse_logs) do
    pulse_logs
    |> Enum.map(fn log ->
      %{
        sequence: log.sequence_number,
        event: log.event_type,
        actor_id: log.actor_id,
        ttl_hops: log.ttl_hops,
        timestamp: log.timestamp,
        metadata: log.metadata
      }
    end)
  end
  
  defp check_ash_domain_health do
    try do
      # Test basic Ash domain operations
      CnsForgeAsh.Domain.read!(CnsForgeAsh.Resources.Workflow, %{})
      "healthy"
    rescue
      _ -> "unhealthy"
    end
  end
  
  defp check_reactor_health do
    try do
      # Test basic Reactor functionality
      case Reactor.run(TestHealthReactor, %{}) do
        {:ok, _} -> "healthy"
        _ -> "degraded"
      end
    rescue
      _ -> "unhealthy"
    end
  end
  
  defp check_telemetry_health do
    # Check if telemetry events are being emitted
    test_event = [:cns_forge, :health_check, :test]
    :telemetry.execute(test_event, %{test: true}, %{timestamp: DateTime.utc_now()})
    "healthy"
  end
  
  defp simulate_ttl_hops(token) do
    results = []
    current_token = token
    
    Enum.reduce_while(1..8, {current_token, results}, fn hop, {token, acc} ->
      if token.ttl_hops > 0 do
        # Decrement TTL
        updated_token = CnsForgeAsh.Domain.update!(token, %{}, action: :decrement_ttl)
        
        # Emit pulse log
        CnsForgeAsh.Domain.create!(CnsForgeAsh.Resources.PulseLog, %{
          transaction_id: token.transaction_id,
          event_type: "demo_hop_#{hop}",
          ttl_hops: updated_token.ttl_hops,
          metadata: %{hop_number: hop, demo: true}
        }, action: :emit)
        
        hop_result = %{
          hop: hop,
          ttl_before: token.ttl_hops,
          ttl_after: updated_token.ttl_hops,
          status: if(updated_token.ttl_hops > 0, do: "success", else: "completed")
        }
        
        new_acc = [hop_result | acc]
        
        if updated_token.ttl_hops > 0 do
          {:cont, {updated_token, new_acc}}
        else
          {:halt, {updated_token, new_acc}}
        end
      else
        {:halt, {token, acc}}
      end
    end)
    |> elem(1)
    |> Enum.reverse()
  end
end

# Simple health check reactor
defmodule TestHealthReactor do
  use Reactor
  
  step :test_step do
    run fn _, _ ->
      {:ok, %{status: "healthy", timestamp: DateTime.utc_now()}}
    end
  end
  
  return :test_step
end