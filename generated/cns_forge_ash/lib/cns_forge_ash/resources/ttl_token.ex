defmodule CnsForgeAsh.Resources.TTLToken do
  @moduledoc """
  TTL Token Resource - The data structure that flows through CNS Forge workflows
  
  Implements the "BitActor Signal" concept from the CNS Forge specification:
  - Immutable data payload passed between BitActors (Reactor.Steps)
  - TTL-driven execution with hop-based decrementation
  - Universal observability through pulse logging
  """
  
  use Ash.Resource,
    domain: CnsForgeAsh.Domain,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :cns_forge_ttl_tokens
  end
  
  actions do
    defaults [:read]
    
    create :create do
      accept [:workflow_type, :payload, :initial_ttl]
      
      change fn changeset, _context ->
        transaction_id = :rand.uniform(1_000_000_000_000)
        
        changeset
        |> Ash.Changeset.force_change_attribute(:transaction_id, transaction_id)
        |> Ash.Changeset.force_change_attribute(:ttl_hops, changeset.arguments[:initial_ttl] || 8)
        |> Ash.Changeset.force_change_attribute(:created_at, DateTime.utc_now())
      end
    end
    
    update :decrement_ttl do
      accept []
      
      change fn changeset, _context ->
        current_ttl = Ash.Changeset.get_attribute(changeset, :ttl_hops)
        new_ttl = max(0, current_ttl - 1)
        
        Ash.Changeset.force_change_attribute(changeset, :ttl_hops, new_ttl)
      end
    end
    
    read :expired do
      filter expr(ttl_hops <= 0)
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :transaction_id, :integer do
      allow_nil? false
      public? true
    end
    
    attribute :workflow_type, :integer do
      allow_nil? false
      public? true
    end
    
    attribute :ttl_hops, :integer do
      allow_nil? false
      public? true
      default 8
    end
    
    attribute :payload, :map do
      allow_nil? false
      public? true
      default %{}
    end
    
    attribute :created_at, :utc_datetime_usec do
      allow_nil? false
      public? true
    end
  end
  
  # TTL validation
  validations do
    validate numericality(:ttl_hops, greater_than_or_equal_to: 0, less_than_or_equal_to: 8)
  end
  
  # Automatically emit pulse logs on token changes
  changes do
    change after_action(fn _changeset, result, _context ->
      :telemetry.execute(
        [:cns_forge, :ttl_token, :changed],
        %{ttl_hops: result.ttl_hops},
        %{transaction_id: result.transaction_id}
      )
      
      {:ok, result}
    end)
  end
end