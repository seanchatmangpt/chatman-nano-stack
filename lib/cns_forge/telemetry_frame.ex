defmodule CNSForge.TelemetryFrame do
  @moduledoc """
  Telemetry Frame resource for universal observability
  
  Captures every BitActor state transition for complete audit trail
  and time-travel debugging capabilities.
  """
  
  use Ash.Resource,
    domain: CNSForge,
    data_layer: Ash.DataLayer.Mnesia

  attributes do
    uuid_primary_key :id
    
    attribute :transaction_id, :string do
      description "Transaction correlation ID"
      allow_nil? false
    end
    
    attribute :bit_actor_id, :uuid do
      description "BitActor that generated this frame"
      allow_nil? false
    end
    
    attribute :hop_sequence, :integer do
      description "Sequence number of this hop in the transaction"
      allow_nil? false
    end
    
    attribute :operation, :atom do
      description "Operation that was executed"
      allow_nil? false
    end
    
    attribute :input_token, :map do
      description "Input token state before operation"
      allow_nil? false
    end
    
    attribute :output_token, :map do
      description "Output token state after operation"
    end
    
    attribute :ttl_before, :integer do
      description "TTL before operation execution"
      allow_nil? false
    end
    
    attribute :ttl_after, :integer do
      description "TTL after operation execution"
    end
    
    attribute :execution_time_us, :integer do
      description "Execution time in microseconds"
    end
    
    attribute :status, :atom do
      description "Operation status: :success, :failure, :ttl_expired"
      allow_nil? false
    end
    
    attribute :error_details, :string do
      description "Error details if status is :failure"
    end
    
    attribute :timestamp, :utc_datetime_usec do
      description "When this frame was captured"
      allow_nil? false
      default &DateTime.utc_now/0
    end
    
    attribute :blake3_hash, :string do
      description "Blake3 hash for integrity verification"
      allow_nil? false
    end
  end

  actions do
    defaults [:read, :destroy]
    
    create :capture do
      description "Capture a telemetry frame for a BitActor operation"
      
      argument :transaction_id, :string, allow_nil? false
      argument :bit_actor_id, :uuid, allow_nil? false
      argument :hop_sequence, :integer, allow_nil? false
      argument :operation, :atom, allow_nil? false
      argument :input_token, :map, allow_nil? false
      argument :output_token, :map
      argument :ttl_before, :integer, allow_nil? false
      argument :ttl_after, :integer
      argument :execution_time_us, :integer
      argument :status, :atom, allow_nil? false
      argument :error_details, :string
      
      change set_attribute(:transaction_id, arg(:transaction_id))
      change set_attribute(:bit_actor_id, arg(:bit_actor_id))
      change set_attribute(:hop_sequence, arg(:hop_sequence))
      change set_attribute(:operation, arg(:operation))
      change set_attribute(:input_token, arg(:input_token))
      change set_attribute(:output_token, arg(:output_token))
      change set_attribute(:ttl_before, arg(:ttl_before))
      change set_attribute(:ttl_after, arg(:ttl_after))
      change set_attribute(:execution_time_us, arg(:execution_time_us))
      change set_attribute(:status, arg(:status))
      change set_attribute(:error_details, arg(:error_details))
      
      change fn changeset, _context ->
        # Generate Blake3 hash for integrity
        frame_data = %{
          transaction_id: Ash.Changeset.get_attribute(changeset, :transaction_id),
          bit_actor_id: Ash.Changeset.get_attribute(changeset, :bit_actor_id),
          operation: Ash.Changeset.get_attribute(changeset, :operation),
          input_token: Ash.Changeset.get_attribute(changeset, :input_token),
          output_token: Ash.Changeset.get_attribute(changeset, :output_token),
          ttl_before: Ash.Changeset.get_attribute(changeset, :ttl_before),
          timestamp: DateTime.utc_now()
        }
        
        hash = frame_data
        |> :erlang.term_to_binary()
        |> blake3_hash()
        
        Ash.Changeset.change_attribute(changeset, :blake3_hash, hash)
      end
    end
  end

  queries do
    get :by_transaction do
      argument :transaction_id, :string, allow_nil? false
      
      filter expr(transaction_id == ^arg(:transaction_id))
      sort timestamp: :asc
    end
    
    get :causal_chain do
      description "Reconstruct complete causal chain for debugging"
      argument :transaction_id, :string, allow_nil? false
      
      filter expr(transaction_id == ^arg(:transaction_id))
      sort hop_sequence: :asc
    end
  end

  preparations do
    prepare build(load: [:timestamp])
  end

  identities do
    identity :transaction_hop, [:transaction_id, :hop_sequence]
  end

  # Blake3 hashing for cryptographic integrity
  defp blake3_hash(data) do
    # Simplified Blake3 - would use actual Blake3 library in production
    :crypto.hash(:sha256, data) |> Base.encode16(case: :lower)
  end

  @doc """
  Reconstruct TTL state transitions for a transaction
  
  Enables time-travel debugging by replaying the exact sequence
  of state changes that occurred during execution.
  """
  def reconstruct_ttl_chain(transaction_id) do
    transaction_id
    |> __MODULE__.by_transaction!()
    |> Enum.map(fn frame ->
      %{
        hop: frame.hop_sequence,
        operation: frame.operation,
        ttl_before: frame.ttl_before,
        ttl_after: frame.ttl_after,
        input_hash: blake3_hash(:erlang.term_to_binary(frame.input_token)),
        output_hash: blake3_hash(:erlang.term_to_binary(frame.output_token || %{})),
        timestamp: frame.timestamp
      }
    end)
  end

  @doc """
  Verify integrity of telemetry chain using Blake3 hashes
  
  Ensures no tampering with the audit trail.
  """
  def verify_chain_integrity(transaction_id) do
    frames = __MODULE__.by_transaction!(transaction_id)
    
    Enum.all?(frames, fn frame ->
      expected_data = %{
        transaction_id: frame.transaction_id,
        bit_actor_id: frame.bit_actor_id,
        operation: frame.operation,
        input_token: frame.input_token,
        output_token: frame.output_token,
        ttl_before: frame.ttl_before,
        timestamp: frame.timestamp
      }
      
      expected_hash = expected_data
      |> :erlang.term_to_binary()
      |> blake3_hash()
      
      expected_hash == frame.blake3_hash
    end)
  end
end