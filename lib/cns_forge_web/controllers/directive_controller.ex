defmodule CNSForgeWeb.DirectiveController do
  @moduledoc """
  HTTP ingress controller for CNS Forge directives
  
  Implements the stimulus:http_request BitActor by translating
  HTTP requests into Reactor workflow execution.
  """
  
  use CNSForgeWeb, :controller
  
  def process(conn, %{"directive" => directive} = params) do
    ttl = Map.get(params, "ttl", 8)
    
    # Create initial token for Reactor workflow
    initial_token = %{
      directive: directive,
      ttl: ttl,
      transaction_id: generate_transaction_id(),
      timestamp: DateTime.utc_now(),
      source: "http_request",
      params: params
    }
    
    case CNSForge.process_directive(directive, ttl) do
      {:ok, result} ->
        json(conn, %{
          status: "success",
          transaction_id: initial_token.transaction_id,
          result: result,
          hops_used: ttl - (result.ttl || 0),
          processing_time_ms: calculate_processing_time(initial_token.timestamp)
        })
        
      {:error, :ttl_expired} ->
        conn
        |> put_status(:request_timeout)
        |> json(%{
          status: "error",
          error: "TTL expired",
          transaction_id: initial_token.transaction_id,
          message: "Directive processing exceeded TTL budget"
        })
        
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{
          status: "error",
          error: reason,
          transaction_id: initial_token.transaction_id,
          message: "Failed to process directive"
        })
    end
  end
  
  def trace(conn, %{"transaction_id" => transaction_id}) do
    # Retrieve complete causal chain for debugging
    case CNSForge.TelemetryFrame.causal_chain(transaction_id) do
      frames when is_list(frames) ->
        causal_chain = CNSForge.TelemetryFrame.reconstruct_ttl_chain(transaction_id)
        integrity_verified = CNSForge.TelemetryFrame.verify_chain_integrity(transaction_id)
        
        json(conn, %{
          status: "success",
          transaction_id: transaction_id,
          total_hops: length(frames),
          causal_chain: causal_chain,
          integrity_verified: integrity_verified,
          frames: frames
        })
        
      _ ->
        conn
        |> put_status(:not_found)
        |> json(%{
          status: "error",
          error: "transaction_not_found",
          transaction_id: transaction_id
        })
    end
  end
  
  defp generate_transaction_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end
  
  defp calculate_processing_time(start_time) do
    DateTime.diff(DateTime.utc_now(), start_time, :millisecond)
  end
end