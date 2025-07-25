defmodule CnsForgeAsh.Resources.BitActor do
  @moduledoc """
  BitActor Resource - Ephemeral, time-limited, single-purpose processes
  
  Implements the core BitActor concept from CNS Forge:
  - Single atomic hop of logic
  - TTL-driven execution
  - Integration with existing BitActor C infrastructure via NIFs
  """
  
  use Ash.Resource,
    domain: CnsForgeAsh.Domain,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :cns_forge_bit_actors
  end
  
  actions do
    defaults [:read]
    
    create :spawn do
      accept [:actor_type, :hop_type, :config]
      
      change fn changeset, _context ->
        actor_id = "bitactor_#{:rand.uniform(1_000_000)}"
        
        changeset
        |> Ash.Changeset.force_change_attribute(:actor_id, actor_id)
        |> Ash.Changeset.force_change_attribute(:status, :active)
        |> Ash.Changeset.force_change_attribute(:spawned_at, DateTime.utc_now())
      end
    end
    
    update :process_hop do
      accept [:input_token_id]
      
      change fn changeset, context ->
        # This would integrate with the existing BitActor C infrastructure
        # via NIFs to call bitactor_server:send_message/2
        
        changeset
        |> Ash.Changeset.force_change_attribute(:last_hop_at, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:hops_processed, 
             (Ash.Changeset.get_attribute(changeset, :hops_processed) || 0) + 1)
      end
    end
    
    update :terminate do
      accept []
      
      change fn changeset, _context ->
        Ash.Changeset.force_change_attribute(changeset, :status, :terminated)
      end
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :actor_id, :string do
      allow_nil? false
      public? true
    end
    
    attribute :actor_type, :atom do
      allow_nil? false
      public? true
      constraints [one_of: [:decoder, :workflow_engine, :memory, :actuation, :signal_router]]
    end
    
    attribute :hop_type, :string do
      allow_nil? false
      public? true
    end
    
    attribute :status, :atom do
      allow_nil? false
      public? true
      default :inactive
      constraints [one_of: [:inactive, :active, :terminated, :expired]]
    end
    
    attribute :config, :map do
      allow_nil? false
      public? true
      default %{}
    end
    
    attribute :hops_processed, :integer do
      allow_nil? false
      public? true
      default 0
    end
    
    attribute :spawned_at, :utc_datetime_usec do
      allow_nil? false
      public? true
    end
    
    attribute :last_hop_at, :utc_datetime_usec do
      public? true
    end
  end
  
  # Pulse logging for BitActor lifecycle events
  changes do
    change after_action(fn changeset, result, _context ->
      event = case changeset.action.name do
        :spawn -> :bit_actor_spawned
        :process_hop -> :bit_actor_hop_processed
        :terminate -> :bit_actor_terminated
        _ -> :bit_actor_changed
      end
      
      :telemetry.execute(
        [:cns_forge, :bit_actor, event],
        %{hops_processed: result.hops_processed},
        %{actor_id: result.actor_id, actor_type: result.actor_type}
      )
      
      {:ok, result}
    end)
  end
end