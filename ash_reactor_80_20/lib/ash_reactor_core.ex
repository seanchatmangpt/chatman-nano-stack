defmodule AshReactor80_20.Core do
  @moduledoc """
  Minimal viable Ash.Reactor implementation - 80/20 approach
  Focuses on core TTL transformation and execution
  """
  
  use Ash.Domain
  
  resources do
    resource AshReactor80_20.Resources.OntologyClass
    resource AshReactor80_20.Resources.ReactorWorkflow
  end
end

defmodule AshReactor80_20.Resources.OntologyClass do
  @moduledoc "Represents a TTL ontology class"
  
  use Ash.Resource,
    domain: AshReactor80_20.Core,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :ontology_classes
    private? false
  end
  
  actions do
    defaults [:read, :destroy]
    
    create :create do
      accept [:uri, :name, :properties]
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:created_at, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:status, "active")
      end
    end
    
    update :process do
      accept []
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:processed_at, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:status, "processed")
      end
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :uri, :string do
      public? true
      allow_nil? false
    end
    
    attribute :name, :string do
      public? true
      allow_nil? false
    end
    
    attribute :properties, :map do
      public? true
      default %{}
    end
    
    attribute :status, :string do
      public? true
      default "pending"
    end
    
    attribute :created_at, :utc_datetime, public?: true
    attribute :processed_at, :utc_datetime, public?: true
  end
  
  code_interface do
    define :create_class, action: :create
    define :list_classes, action: :read
    define :process_class, action: :process
  end
end

defmodule AshReactor80_20.Resources.ReactorWorkflow do
  @moduledoc "Represents an Ash.Reactor workflow"
  
  use Ash.Resource,
    domain: AshReactor80_20.Core,
    data_layer: Ash.DataLayer.Ets
  
  ets do
    table :reactor_workflows
    private? false
  end
  
  actions do
    defaults [:read, :destroy]
    
    create :create do
      accept [:name, :steps, :ontology_class_id]
    end
    
    update :execute do
      accept []
      
      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.force_change_attribute(:executed_at, DateTime.utc_now())
        |> Ash.Changeset.force_change_attribute(:status, "completed")
      end
    end
  end
  
  attributes do
    uuid_primary_key :id
    
    attribute :name, :string do
      public? true
      allow_nil? false
    end
    
    attribute :steps, :map do
      public? true
      default %{}
    end
    
    attribute :ontology_class_id, :uuid do
      public? true
    end
    
    attribute :status, :string do
      public? true
      default "pending"
    end
    
    attribute :executed_at, :utc_datetime, public?: true
  end
  
  relationships do
    belongs_to :ontology_class, AshReactor80_20.Resources.OntologyClass
  end
  
  code_interface do
    define :create_workflow, action: :create
    define :list_workflows, action: :read
    define :execute_workflow, action: :execute
  end
end