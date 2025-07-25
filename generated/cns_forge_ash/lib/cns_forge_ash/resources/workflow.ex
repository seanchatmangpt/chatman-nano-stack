defmodule CnsForgeAsh.Resources.Workflow do
  @moduledoc """
  Workflow Resource - Manages Reactor workflow executions
  
  Tracks the execution of Ash.Reactor workflows within the CNS Forge system:
  - Workflow lifecycle management
  - TTL budget tracking
  - Success/failure/expiration outcomes
  """
  
  use Ash.Resource,
    domain: CnsForgeAsh.Domain,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :cns_forge_workflows
  end
  
  actions do
    defaults [:read]
    
    create :initiate do
      accept [:workflow_name, :initial_ttl, :input_params]
      
      change fn changeset, _context ->
        workflow_id = "workflow_#{:rand.uniform(1_000_000)}"
        
        changeset
        |> Ash.Changeset.force_change_attribute(:workflow_id, workflow_id)
        |> Ash.Changeset.force_change_attribute(:status, :running)
        |> Ash.Changeset.force_change_attribute(:started_at, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:current_ttl, changeset.arguments[:initial_ttl] || 8)
      end
    end
    
    update :complete do
      accept [:result]
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:status, :completed)
        |> Ash.Changeset.force_change_attribute(:completed_at, DateTime.utc_now())
      end
    end
    
    update :expire do
      accept []
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:status, :expired)
        |> Ash.Changeset.force_change_attribute(:completed_at, DateTime.utc_now())
      end
    end
    
    update :fail do
      accept [:error_reason]
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:status, :failed)
        |> Ash.Changeset.force_change_attribute(:completed_at, DateTime.utc_now())
      end
    end
    
    update :decrement_ttl do
      accept []
      
      change fn changeset, _context ->
        current_ttl = Ash.Changeset.get_attribute(changeset, :current_ttl)
        new_ttl = max(0, current_ttl - 1)
        
        Ash.Changeset.force_change_attribute(changeset, :current_ttl, new_ttl)
      end
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :workflow_id, :string do
      allow_nil? false
      public? true
    end
    
    attribute :workflow_name, :string do
      allow_nil? false
      public? true
    end
    
    attribute :status, :atom do
      allow_nil? false
      public? true
      default :pending
      constraints [one_of: [:pending, :running, :completed, :failed, :expired]]
    end
    
    attribute :initial_ttl, :integer do
      allow_nil? false
      public? true
      default 8
    end
    
    attribute :current_ttl, :integer do
      allow_nil? false
      public? true
      default 8
    end
    
    attribute :input_params, :map do
      allow_nil? false
      public? true
      default %{}
    end
    
    attribute :result, :map do
      public? true
    end
    
    attribute :error_reason, :string do
      public? true
    end
    
    attribute :started_at, :utc_datetime_usec do
      allow_nil? false
      public? true
    end
    
    attribute :completed_at, :utc_datetime_usec do
      public? true
    end
  end
  
  # Workflow event telemetry
  changes do
    change after_action(fn changeset, result, _context ->
      event = case changeset.action.name do
        :initiate -> :workflow_initiated
        :complete -> :workflow_completed
        :expire -> :workflow_expired
        :fail -> :workflow_failed
        _ -> :workflow_updated
      end
      
      :telemetry.execute(
        [:cns_forge, :workflow, event],
        %{current_ttl: result.current_ttl},
        %{workflow_id: result.workflow_id, workflow_name: result.workflow_name}
      )
      
      {:ok, result}
    end)
  end
end