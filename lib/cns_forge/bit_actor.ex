defmodule CNSForge.BitActor do
  @moduledoc """
  BitActor as Ash.Resource - represents ephemeral, time-limited execution units
  
  Each BitActor encapsulates one atomic hop of logic within the TTL budget.
  Implemented as declarative Ash actions with STRICT TTL security validation.
  
  SECURITY: TTL must be a non-negative integer to prevent manipulation attacks.
  """
  
  use Ash.Resource,
    domain: CNSForge,
    data_layer: Ash.DataLayer.Mnesia

  attributes do
    uuid_primary_key :id
    
    attribute :type, :atom do
      description "BitActor type (e.g., :stimulus, :sensor, :motor, :processor)"
      allow_nil? false
      constraints [one_of: [:stimulus, :sensor, :motor, :processor]]
    end
    
    attribute :transaction_id, :string do
      description "Correlation ID for tracing causal chains"
      allow_nil? false
    end
    
    attribute :ttl, :integer do
      description "Remaining time-to-live in logical hops (MUST be non-negative integer)"
      allow_nil? false
      default 8
      constraints [min: 0, max: 1000]  # Security: Prevent massive TTL attacks
    end
    
    attribute :token, :map do
      description "Immutable state token passed between hops"
      allow_nil? false
      default %{}
    end
    
    attribute :status, :atom do
      description "Current status: :pending, :running, :completed, :failed, :ttl_expired"
      allow_nil? false
      default :pending
      constraints [one_of: [:pending, :running, :completed, :failed, :ttl_expired]]
    end
    
    attribute :created_at, :utc_datetime_usec do
      description "Creation timestamp"
      allow_nil? false
      default &DateTime.utc_now/0
    end
    
    attribute :completed_at, :utc_datetime_usec do
      description "Completion timestamp"
    end
    
    attribute :result, :map do
      description "Output token from successful execution"
    end
    
    attribute :error, :string do
      description "Error message if execution failed"
    end
  end

  actions do
    defaults [:read, :destroy]
    
    create :create do
      description "Create a new BitActor with STRICT TTL validation"
      
      argument :type, :atom, allow_nil? false
      argument :transaction_id, :string, allow_nil? false  
      argument :token, :map, default: %{}
      argument :ttl, :integer, default: 8
      
      # SECURITY: Strict TTL validation to prevent float acceptance vulnerability
      validate fn changeset, _context ->
        ttl_arg = Ash.Changeset.get_argument(changeset, :ttl)
        
        cond do
          not is_integer(ttl_arg) ->
            type_name = cond do
              is_float(ttl_arg) -> "float"
              is_binary(ttl_arg) -> "string"
              is_atom(ttl_arg) -> "atom"
              is_list(ttl_arg) -> "list"
              is_map(ttl_arg) -> "map"
              true -> "unknown"
            end
            {:error, "TTL must be an integer, got: #{inspect(ttl_arg)} (#{type_name})"}
          
          ttl_arg < 0 ->
            {:error, "TTL must be non-negative, got: #{ttl_arg}"}
          
          ttl_arg > 1000 ->
            {:error, "TTL too large (max 1000), got: #{ttl_arg}"}
          
          true ->
            :ok
        end
      end
      
      change set_attribute(:type, arg(:type))
      change set_attribute(:transaction_id, arg(:transaction_id))
      change set_attribute(:token, arg(:token))
      change set_attribute(:ttl, arg(:ttl))
    end
    
    update :execute_hop do
      description "Execute one atomic hop of logic"
      
      argument :input_token, :map, allow_nil? false
      argument :operation, :atom, allow_nil? false
      
      validate fn changeset, _context ->
        case Ash.Changeset.get_attribute(changeset, :ttl) do
          ttl when ttl <= 0 ->
            {:error, "TTL expired - cannot execute hop"}
          _ ->
            :ok
        end
      end
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.change_attribute(:status, :running)
        |> Ash.Changeset.after_action(fn _changeset, record ->
          # Execute the hop and decrement TTL
          execute_atomic_hop(record, 
            Ash.Changeset.get_argument(changeset, :input_token),
            Ash.Changeset.get_argument(changeset, :operation))
        end)
      end
    end
    
    update :complete do
      description "Mark BitActor as completed"
      
      argument :result_token, :map, allow_nil? false
      
      change set_attribute(:status, :completed)
      change set_attribute(:completed_at, &DateTime.utc_now/0)
      change set_attribute(:result, arg(:result_token))
    end
    
    update :fail do
      description "Mark BitActor as failed"
      
      argument :error_message, :string, allow_nil? false
      
      change set_attribute(:status, :failed)
      change set_attribute(:completed_at, &DateTime.utc_now/0)
      change set_attribute(:error, arg(:error_message))
    end
    
    update :expire_ttl do
      description "Mark BitActor as TTL expired"
      
      change set_attribute(:status, :ttl_expired)
      change set_attribute(:completed_at, &DateTime.utc_now/0)
      change set_attribute(:error, "TTL budget exhausted")
    end
  end

  defp execute_atomic_hop(record, input_token, operation) do
    # Emit telemetry for pulse log
    :telemetry.execute(
      [:cns_forge, :bit_actor, :hop],
      %{ttl_remaining: record.ttl},
      %{
        bit_actor_id: record.id,
        transaction_id: record.transaction_id,
        operation: operation,
        input_token: input_token
      }
    )
    
    # Execute the operation (simplified - would dispatch to actual logic)
    result_token = case operation do
      :decode_params -> Map.put(input_token, :decoded, true)
      :validate_input -> Map.put(input_token, :validated, true)
      :process_signal -> Map.put(input_token, :processed, true)
      :emit_response -> Map.put(input_token, :emitted, true)
      _ -> input_token
    end
    
    # Update with decremented TTL and result
    record
    |> Ash.Changeset.for_update(:complete, %{result_token: result_token})
    |> Ash.Changeset.change_attribute(:ttl, record.ttl - 1)
    |> CNSForge.update!()
  end

  preparations do
    prepare build(load: [:created_at, :completed_at])
  end

  identities do
    identity :transaction_correlation, [:transaction_id, :created_at]
  end
end