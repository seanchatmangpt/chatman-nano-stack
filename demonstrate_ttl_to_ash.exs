# Demonstration of TTL to Ash generation results
# Shows what the mix task would generate

IO.puts("""
🐢 TTL TO ASH DEMONSTRATION
========================

Input TTL:
----------
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix cyber: <http://cybersecurity.org/> .

cyber:ThreatActor a owl:Class .
cyber:Vulnerability a owl:Class .
cyber:SecurityControl a owl:Class .

Command:
--------
mix cns.gen.from_ttl --file cyber.ttl --domain MyApp.Cybersecurity

Generated Structure:
-------------------
""")

# Show file structure
IO.puts("""
lib/
└── my_app/
    └── cybersecurity/
        ├── cybersecurity.ex             # Ash Domain
        ├── resources/
        │   ├── threat_actor.ex         # Ash Resource
        │   ├── vulnerability.ex        # Ash Resource
        │   └── security_control.ex     # Ash Resource
        └── reactors/
            └── main_workflow.ex        # Ash.Reactor

""")

# Show sample generated resource
IO.puts("Generated Resource Example (threat_actor.ex):")
IO.puts("=" <> String.duplicate("=", 49))
IO.puts(~S"""
defmodule MyApp.Cybersecurity.Resources.ThreatActor do
  @moduledoc \"\"\"
  Ash Resource for ThreatActor
  
  Generated from TTL class: cyber:ThreatActor
  \"\"\"
  
  use Ash.Resource,
    domain: MyApp.Cybersecurity,
    data_layer: Ash.DataLayer.Ets

  attributes do
    uuid_primary_key :id
    
    attribute :ttl_uri, :string do
      description "Original TTL URI for this entity"
      default "cyber:ThreatActor"
      public? true
    end
    
    attribute :name, :string do
      description "Name of the ThreatActor"
      allow_nil? false
      public? true
    end
    
    attribute :description, :string do
      description "Description of the ThreatActor"
      public? true
    end
    
    create_timestamp :created_at
    update_timestamp :updated_at
  end

  actions do
    defaults [:read, :destroy]
    
    create :create do
      primary? true
      accept [:name, :description]
      change set_attribute(:ttl_uri, "cyber:ThreatActor")
    end
    
    update :update do
      primary? true
      accept [:name, :description]
    end
  end

  code_interface do
    define :create, args: [:name]
    define :read
    define :update, args: [:name]
    define :destroy
  end
end
""")

IO.puts("\n\nGenerated Domain (cybersecurity.ex):")
IO.puts("=" <> String.duplicate("=", 39))
IO.puts(~S"""
defmodule MyApp.Cybersecurity do
  @moduledoc \"\"\"
  Ash Domain generated from TTL ontology
  
  This domain contains 3 resources transformed from OWL classes.
  \"\"\"
  
  use Ash.Domain, otp_app: :my_app

  resources do
    resource MyApp.Cybersecurity.Resources.ThreatActor
    resource MyApp.Cybersecurity.Resources.Vulnerability
    resource MyApp.Cybersecurity.Resources.SecurityControl
  end
end
""")

IO.puts("\n\nGenerated Reactor (main_workflow.ex):")
IO.puts("=" <> String.duplicate("=", 39))
IO.puts(~S"""
defmodule MyApp.Cybersecurity.Reactors.MainWorkflow do
  @moduledoc \"\"\"
  Main Ash.Reactor workflow for MyApp.Cybersecurity
  
  Orchestrates operations across TTL-generated resources.
  \"\"\"
  
  use Reactor

  input :operation, :atom do
    description "The operation to perform"
    constraints one_of: [:create, :read, :update, :destroy]
  end

  input :resource_type, :atom do
    description "The type of resource to operate on"
  end

  input :params, :map do
    description "Parameters for the operation"
    default %{}
  end

  step :validate_inputs do
    argument :operation, input(:operation)
    argument :resource_type, input(:resource_type)
    
    run fn args, _context ->
      if args.operation && args.resource_type do
        {:ok, %{valid: true}}
      else
        {:error, "Missing required inputs"}
      end
    end
  end

  step :execute_operation do
    wait_for :validate_inputs
    
    argument :operation, input(:operation)
    argument :resource_type, input(:resource_type)
    argument :params, input(:params)
    
    run fn args, _context ->
      {:ok, %{
        operation: args.operation,
        resource_type: args.resource_type,
        status: :completed
      }}
    end
  end

  return :execute_operation
end
""")

IO.puts("\n\n🎉 Usage Example:")
IO.puts("=" <> String.duplicate("=", 19))
IO.puts("""
# Create a threat actor
{:ok, actor} = MyApp.Cybersecurity.Resources.ThreatActor.create("APT28")

# List all vulnerabilities  
{:ok, vulns} = MyApp.Cybersecurity.Resources.Vulnerability.read()

# Update a security control
{:ok, control} = MyApp.Cybersecurity.Resources.SecurityControl.update(
  control_id, 
  %{name: "Updated Firewall Rules"}
)
""")

IO.puts("\n✅ Mix task would generate all these files automatically from TTL!")