defmodule Ultrathink.Resources.Signal do
  @moduledoc """
  Trading Signal Resource
  Market signal with embedded intelligence
  """

  use Ash.Resource,
    domain: Ultrathink.Domain,
    data_layer: Ash.DataLayer.Ets

  # Using ETS for 80/20 approach - no database setup required

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
