defmodule CnsForgeAsh.Resources.PulseLog do
  @moduledoc """
  Pulse Log Resource - Universal observability for every BitActor state transition
  
  Implements the "Universal Instrumentation & Observability" epic from CNS Forge:
  - Captures every signal and state transition
  - Enables causal chain reconstruction
  - Provides high-fidelity diagnostics
  """
  
  use Ash.Resource,
    domain: CnsForgeAsh.Domain,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :cns_forge_pulse_logs
  end
  
  actions do
    defaults [:read]
    
    create :emit do
      accept [:transaction_id, :event_type, :actor_id, :ttl_hops, :metadata]
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:timestamp, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:sequence_number, System.unique_integer([:positive]))
      end
    end
    
    read :by_transaction do
      argument :transaction_id, :integer, allow_nil?: false
      
      filter expr(transaction_id == ^arg(:transaction_id))
      
      preparations do
        prepare build(sort: [:sequence_number])
      end
    end
    
    read :causal_chain do
      argument :transaction_id, :integer, allow_nil?: false
      
      filter expr(transaction_id == ^arg(:transaction_id))
      
      preparations do
        prepare build(sort: [:timestamp])
      end
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :transaction_id, :integer do
      allow_nil? false
      public? true
    end
    
    attribute :event_type, :string do
      allow_nil? false
      public? true
    end
    
    attribute :actor_id, :string do
      public? true
    end
    
    attribute :ttl_hops, :integer do
      public? true
    end
    
    attribute :timestamp, :utc_datetime_usec do
      allow_nil? false
      public? true
    end
    
    attribute :sequence_number, :integer do
      allow_nil? false
      public? true
    end
    
    attribute :metadata, :map do
      allow_nil? false
      public? true
      default %{}
    end
  end
  
  # Index for efficient querying by transaction
  identities do
    identity :transaction_sequence, [:transaction_id, :sequence_number]
  end
  
  # Aggregates for analytics
  aggregates do
    count :total_events, :transaction_id
  end
end