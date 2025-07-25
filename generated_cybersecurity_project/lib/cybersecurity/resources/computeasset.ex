defmodule Cybersecurity.Resources.ComputeAsset do
  @moduledoc """
  Compute Asset Resource
  Computing device or system
  """

  use Ash.Resource,
    domain: Cybersecurity.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "computeassets"
    repo Cybersecurity.Repo
  end

  attributes do
    uuid_primary_key :id
    
    attribute :name, :string do
      allow_nil? false
    end
    
    attribute :description, :string
    
    attribute :status, :atom do
      constraints [one_of: [:active, :inactive, :pending]]
      default :active
    end
    
    attribute :metadata, :map do
      default %{}
    end
    
    timestamps()
  end

  actions do
    defaults [:create, :read, :update, :destroy]
    
    create :create do
      primary? true
      accept [:name, :description, :status, :metadata]
    end
    
    read :read do
      primary? true
    end
    
    read :by_status do
      argument :status, :atom, allow_nil?: false
      filter expr(status == ^arg(:status))
    end
    
    update :update do
      primary? true
      accept [:name, :description, :status, :metadata]
    end
    
    update :activate do
      change set_attribute(:status, :active)
    end
    
    update :deactivate do
      change set_attribute(:status, :inactive)
    end
    
    destroy :destroy do
      primary? true
    end
  end

  identities do
    identity :unique_id, [:id]
  end

  validations do
    # Add domain-specific validations here
  end
end
