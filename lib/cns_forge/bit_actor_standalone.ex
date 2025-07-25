defmodule CNSForge.BitActorStandalone do
  @moduledoc """
  Standalone BitActor Implementation - 80/20 Core Functionality
  
  TTL-bounded execution units without Ash framework dependency.
  Implements the critical security fixes for TTL validation.
  
  SECURITY: Strict TTL validation prevents float acceptance vulnerability.
  """

  defstruct [
    :id,
    :type,
    :transaction_id,
    :ttl,
    :token,
    :status,
    :created_at,
    :completed_at,
    :result,
    :error
  ]

  @type t :: %__MODULE__{
    id: String.t(),
    type: :stimulus | :sensor | :motor | :processor,
    transaction_id: String.t(),
    ttl: non_neg_integer(),
    token: map(),
    status: :pending | :running | :completed | :failed | :ttl_expired,
    created_at: DateTime.t(),
    completed_at: DateTime.t() | nil,
    result: map() | nil,
    error: String.t() | nil
  }

  @doc """
  Create a new BitActor with STRICT TTL validation
  
  ## Examples
  
      iex> CNSForge.BitActorStandalone.create(%{
      ...>   type: :processor,
      ...>   transaction_id: "txn_123",
      ...>   ttl: 8,
      ...>   token: %{data: "test"}
      ...> })
      {:ok, %CNSForge.BitActorStandalone{}}
      
      iex> CNSForge.BitActorStandalone.create(%{
      ...>   type: :processor,
      ...>   transaction_id: "txn_123", 
      ...>   ttl: 8.5,  # Float TTL - SECURITY VULNERABILITY
      ...>   token: %{}
      ...> })
      {:error, "TTL must be an integer, got: 8.5 (float)"}
  """
  def create(attrs) do
    with {:ok, validated_attrs} <- validate_creation_attrs(attrs) do
      actor = %__MODULE__{
        id: generate_id(),
        type: validated_attrs.type,
        transaction_id: validated_attrs.transaction_id,
        ttl: validated_attrs.ttl,
        token: validated_attrs.token,
        status: :pending,
        created_at: DateTime.utc_now(),
        completed_at: nil,
        result: nil,
        error: nil
      }
      
      # 80/20: Skip telemetry for standalone operation
      
      {:ok, actor}
    end
  end

  @doc """
  Execute a hop operation with TTL decrement and security validation
  """
  def execute_hop(actor, operation, input_token \\ %{}) do
    cond do
      actor.ttl <= 0 ->
        expired_actor = %{actor | 
          status: :ttl_expired, 
          completed_at: DateTime.utc_now(),
          error: "TTL budget exhausted"
        }
        {:error, :ttl_exhausted, expired_actor}
      
      actor.status != :pending ->
        {:error, :invalid_state}
      
      true ->
        # Execute the hop
        result_token = execute_operation(operation, Map.merge(actor.token, input_token))
        
        updated_actor = %{actor |
          ttl: actor.ttl - 1,
          token: result_token,
          status: if(actor.ttl - 1 <= 0, do: :completed, else: :running),
          completed_at: if(actor.ttl - 1 <= 0, do: DateTime.utc_now(), else: nil),
          result: if(actor.ttl - 1 <= 0, do: result_token, else: nil)
        }
        
        # 80/20: Skip telemetry for standalone operation
        
        {:ok, updated_actor}
    end
  end

  @doc """
  Check TTL status - returns remaining TTL or error if exhausted
  """
  def check_ttl(actor) do
    if actor.ttl > 0 do
      {:ok, actor.ttl}
    else
      {:error, :ttl_exhausted}
    end
  end

  @doc """
  List active actors (mock implementation for testing)
  """
  def list_active(opts \\ []) do
    # In a real implementation, this would query a storage backend
    # For 80/20, we return empty list
    []
  end

  # Private functions

  defp validate_creation_attrs(attrs) do
    with {:ok, type} <- validate_type(Map.get(attrs, :type)),
         {:ok, transaction_id} <- validate_transaction_id(Map.get(attrs, :transaction_id)),
         {:ok, ttl} <- validate_ttl(Map.get(attrs, :ttl, 8)),
         {:ok, token} <- validate_token(Map.get(attrs, :token, %{})) do
      {:ok, %{
        type: type,
        transaction_id: transaction_id,
        ttl: ttl,
        token: token
      }}
    end
  end

  defp validate_type(type) when type in [:stimulus, :sensor, :motor, :processor], do: {:ok, type}
  defp validate_type(type), do: {:error, "Invalid type: #{inspect(type)}. Must be one of: :stimulus, :sensor, :motor, :processor"}

  defp validate_transaction_id(txn_id) when is_binary(txn_id) and byte_size(txn_id) > 0, do: {:ok, txn_id}
  defp validate_transaction_id(txn_id), do: {:error, "Transaction ID must be a non-empty string, got: #{inspect(txn_id)}"}

  # SECURITY: Critical TTL validation to prevent float acceptance vulnerability
  defp validate_ttl(ttl) do
    cond do
      not is_integer(ttl) ->
        type_name = cond do
          is_float(ttl) -> "float"
          is_binary(ttl) -> "string"
          is_atom(ttl) -> "atom"
          is_list(ttl) -> "list"
          is_map(ttl) -> "map"
          true -> "unknown"
        end
        {:error, "TTL must be an integer, got: #{inspect(ttl)} (#{type_name})"}
      
      ttl < 0 ->
        {:error, "TTL must be non-negative, got: #{ttl}"}
      
      ttl > 1000 ->
        {:error, "TTL too large (max 1000), got: #{ttl}"}
      
      true ->
        {:ok, ttl}
    end
  end

  defp validate_token(token) when is_map(token), do: {:ok, token}
  defp validate_token(token), do: {:error, "Token must be a map, got: #{inspect(token)}"}

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp execute_operation(operation, token) do
    # Simulate different operations
    case operation do
      :decode_params -> Map.put(token, :decoded, true)
      :validate_input -> Map.put(token, :validated, true) 
      :process_signal -> Map.put(token, :processed, true)
      :emit_response -> Map.put(token, :emitted, true)
      :semantic_compile -> Map.put(token, :compiled, true)
      _ -> Map.put(token, :operation, operation)
    end
  end
end