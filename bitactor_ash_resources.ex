defmodule BitActor.Ash.Domain do
  @moduledoc """
  Ash Domain for BitActor system
  """
  use Ash.Domain, otp_app: :bitactor

  resources do
    resource BitActor.Ash.Resources.Actor
    resource BitActor.Ash.Resources.Signal
    resource BitActor.Ash.Resources.TelemetryFrame
    resource BitActor.Ash.Resources.TTLConstraint
    resource BitActor.Ash.Resources.TTLViolation
    resource BitActor.Ash.Resources.SwarmConfiguration
  end

  authorization do
    authorize :when_requested
  end
end

# =============================================================================
# Actor Resource
# =============================================================================

defmodule BitActor.Ash.Resources.Actor do
  @moduledoc """
  Ash Resource for BitActor
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "bitactors"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      constraints max_length: 255
    end

    attribute :status, :atom do
      public? true
      default :inactive
      constraints one_of: [:inactive, :active, :processing, :suspended, :error, :terminated]
    end

    attribute :ttl_budget_ms, :integer do
      public? true
      default 8
      constraints min: 1, max: 100
    end

    attribute :signals_processed, :integer do
      public? true
      default 0
    end

    attribute :signals_failed, :integer do
      public? true
      default 0
    end

    attribute :last_processing_time_ns, :integer do
      public? true
    end

    attribute :last_signal_id, :uuid do
      public? true
    end

    attribute :parent_actor_id, :uuid do
      public? true
    end

    create_timestamp :created_at
    update_timestamp :updated_at
    
    attribute :last_active_at, :utc_datetime_usec do
      public? true
    end
  end

  relationships do
    belongs_to :parent_actor, BitActor.Ash.Resources.Actor do
      source_attribute :parent_actor_id
      destination_attribute :id
    end

    has_many :child_actors, BitActor.Ash.Resources.Actor do
      source_attribute :id
      destination_attribute :parent_actor_id
    end

    has_many :signals, BitActor.Ash.Resources.Signal do
      destination_attribute :target_actor_id
    end

    has_many :telemetry_frames, BitActor.Ash.Resources.TelemetryFrame

    has_one :ttl_constraint, BitActor.Ash.Resources.TTLConstraint

    has_many :ttl_violations, BitActor.Ash.Resources.TTLViolation
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      primary? true
      accept [:name, :ttl_budget_ms, :status, :parent_actor_id]

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.after_action(fn changeset, actor ->
          # Start the GenServer for this actor
          {:ok, pid} = BitActor.Supervisor.start_actor([
            id: actor.id,
            name: actor.name,
            ttl_budget_ms: actor.ttl_budget_ms,
            status: actor.status
          ])
          
          {:ok, actor}
        end)
      end
    end

    update :update do
      primary? true
      accept [:status, :ttl_budget_ms]
    end

    update :process_signal do
      accept []
      argument :signal_id, :uuid, allow_nil?: false

      change fn changeset, context ->
        signal_id = context.arguments.signal_id
        actor = changeset.data

        # Delegate to GenServer
        case BitActor.GenServer.process_signal({:via, Registry, {BitActor.Registry, actor.id}}, %{id: signal_id}) do
          {:ok, result} ->
            changeset
            |> Ash.Changeset.force_change_attribute(:last_signal_id, signal_id)
            |> Ash.Changeset.force_change_attribute(:signals_processed, actor.signals_processed + 1)
            |> Ash.Changeset.force_change_attribute(:last_active_at, DateTime.utc_now())

          {:error, _reason} ->
            changeset
            |> Ash.Changeset.force_change_attribute(:signals_failed, actor.signals_failed + 1)
            |> Ash.Changeset.force_change_attribute(:status, :error)
        end
      end
    end

    read :active do
      filter expr(status == :active)
    end

    read :by_parent do
      argument :parent_id, :uuid

      filter expr(parent_actor_id == ^arg(:parent_id))
    end
  end

  code_interface do
    define :create_actor, action: :create
    define :list_actors, action: :read
    define :list_active_actors, action: :active
    define :process_signal, action: :process_signal
  end

  calculations do
    calculate :success_rate, :float do
      calculation fn records, _context ->
        Enum.map(records, fn actor ->
          total = actor.signals_processed + actor.signals_failed
          if total > 0 do
            actor.signals_processed / total * 100
          else
            0.0
          end
        end)
      end
    end

    calculate :ttl_utilization, :float do
      calculation fn records, _context ->
        Enum.map(records, fn actor ->
          if actor.last_processing_time_ns && actor.ttl_budget_ms > 0 do
            (actor.last_processing_time_ns / (actor.ttl_budget_ms * 1_000_000)) * 100
          else
            0.0
          end
        end)
      end
    end
  end
end

# =============================================================================
# Signal Resource
# =============================================================================

defmodule BitActor.Ash.Resources.Signal do
  @moduledoc """
  Ash Resource for Signals
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "signals"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :type, :atom do
      allow_nil? false
      public? true
      constraints one_of: [:data, :control, :telemetry, :heartbeat, :error]
    end

    attribute :payload, :map do
      public? true
      default %{}
    end

    attribute :priority, :atom do
      public? true
      default :medium
      constraints one_of: [:low, :medium, :high, :critical, :emergency]
    end

    attribute :source_actor_id, :uuid do
      public? true
    end

    attribute :target_actor_id, :uuid do
      public? true
    end

    create_timestamp :created_at
  end

  relationships do
    belongs_to :source_actor, BitActor.Ash.Resources.Actor do
      source_attribute :source_actor_id
      destination_attribute :id
    end

    belongs_to :target_actor, BitActor.Ash.Resources.Actor do
      source_attribute :target_actor_id
      destination_attribute :id
    end

    has_one :ttl_constraint, BitActor.Ash.Resources.TTLConstraint
  end

  actions do
    defaults [:read, :create, :destroy]

    create :send_signal do
      accept [:type, :payload, :priority, :source_actor_id, :target_actor_id]

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.after_action(fn changeset, signal ->
          # Trigger signal processing in target actor
          if signal.target_actor_id do
            BitActor.Ash.Resources.Actor.process_signal!(
              signal.target_actor_id,
              %{signal_id: signal.id}
            )
          end
          
          {:ok, signal}
        end)
      end
    end
  end

  code_interface do
    define :create_signal, action: :create
    define :send_signal, action: :send_signal
    define :list_signals, action: :read
  end
end

# =============================================================================
# TelemetryFrame Resource
# =============================================================================

defmodule BitActor.Ash.Resources.TelemetryFrame do
  @moduledoc """
  Ash Resource for Telemetry Frames
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "telemetry_frames"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :metric_name, :string do
      allow_nil? false
      public? true
      constraints max_length: 100
    end

    attribute :value, :float do
      allow_nil? false
      public? true
    end

    attribute :unit, :atom do
      public? true
      default :count
      constraints one_of: [:count, :ms, :ns, :bytes, :percent, :rate]
    end

    attribute :tags, :map do
      public? true
      default %{}
    end

    attribute :metadata, :map do
      public? true
      default %{}
    end

    create_timestamp :timestamp
  end

  relationships do
    belongs_to :bitactor, BitActor.Ash.Resources.Actor do
      allow_nil? false
    end
  end

  actions do
    defaults [:read, :create]

    read :by_actor do
      argument :actor_id, :uuid, allow_nil?: false
      filter expr(bitactor_id == ^arg(:actor_id))
    end

    read :by_metric do
      argument :metric_name, :string, allow_nil?: false
      filter expr(metric_name == ^arg(:metric_name))
    end
  end

  code_interface do
    define :record_telemetry, action: :create
    define :list_telemetry, action: :read
    define :telemetry_by_actor, action: :by_actor
    define :telemetry_by_metric, action: :by_metric
  end
end

# =============================================================================
# TTL Constraint Resource
# =============================================================================

defmodule BitActor.Ash.Resources.TTLConstraint do
  @moduledoc """
  Ash Resource for TTL Constraints
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "ttl_constraints"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :budget_ns, :integer do
      allow_nil? false
      public? true
      constraints min: 1
    end

    attribute :precision, :atom do
      public? true
      default :nanosecond
      constraints one_of: [:nanosecond, :microsecond, :millisecond, :second]
    end

    attribute :max_budget_ms, :integer do
      public? true
      default 8
    end
  end

  relationships do
    belongs_to :bitactor, BitActor.Ash.Resources.Actor
    belongs_to :signal, BitActor.Ash.Resources.Signal
  end

  actions do
    defaults [:read, :create, :update, :destroy]
  end

  validations do
    validate fn changeset, _context ->
      budget_ns = Ash.Changeset.get_attribute(changeset, :budget_ns)
      max_budget_ms = Ash.Changeset.get_attribute(changeset, :max_budget_ms)
      
      if budget_ns && max_budget_ms && budget_ns > max_budget_ms * 1_000_000 do
        {:error, field: :budget_ns, message: "exceeds maximum budget"}
      else
        :ok
      end
    end
  end
end

# =============================================================================
# TTL Violation Resource
# =============================================================================

defmodule BitActor.Ash.Resources.TTLViolation do
  @moduledoc """
  Ash Resource for TTL Violations
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "ttl_violations"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :expected_ttl_ns, :integer do
      allow_nil? false
      public? true
    end

    attribute :actual_time_ns, :integer do
      allow_nil? false
      public? true
    end

    attribute :violation_amount_ns, :integer do
      allow_nil? false
      public? true
    end

    create_timestamp :timestamp
  end

  relationships do
    belongs_to :actor, BitActor.Ash.Resources.Actor do
      allow_nil? false
    end

    belongs_to :signal, BitActor.Ash.Resources.Signal do
      allow_nil? false
    end
  end

  actions do
    defaults [:read, :create]

    read :by_actor do
      argument :actor_id, :uuid, allow_nil?: false
      filter expr(actor_id == ^arg(:actor_id))
    end
  end

  calculations do
    calculate :violation_percentage, :float do
      calculation fn records, _context ->
        Enum.map(records, fn violation ->
          (violation.violation_amount_ns / violation.expected_ttl_ns) * 100
        end)
      end
    end
  end

  code_interface do
    define :record_violation, action: :create
    define :list_violations, action: :read
    define :violations_by_actor, action: :by_actor
  end
end

# =============================================================================
# Swarm Configuration Resource
# =============================================================================

defmodule BitActor.Ash.Resources.SwarmConfiguration do
  @moduledoc """
  Ash Resource for Swarm Configuration
  """
  use Ash.Resource,
    otp_app: :bitactor,
    domain: BitActor.Ash.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "swarm_configurations"
    repo BitActor.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :topology, :atom do
      allow_nil? false
      public? true
      constraints one_of: [:hierarchical, :mesh, :ring, :star]
    end

    attribute :max_actors, :integer do
      public? true
      default 100
      constraints min: 1, max: 1000
    end

    attribute :strategy, :atom do
      public? true
      default :balanced
      constraints one_of: [:balanced, :specialized, :adaptive]
    end

    attribute :global_ttl_budget_ms, :integer do
      public? true
      default 5000
      constraints min: 1
    end

    attribute :actor_ttl_budget_ms, :integer do
      public? true
      default 8
      constraints min: 1
    end

    attribute :auto_scale, :boolean do
      public? true
      default true
    end

    attribute :min_actors, :integer do
      public? true
      default 1
      constraints min: 1
    end

    attribute :target_cpu_percent, :integer do
      public? true
      default 70
      constraints min: 1, max: 100
    end

    attribute :namespace, :string do
      public? true
      default "bitactor-system"
    end

    attribute :service_name, :string do
      public? true
      default "bitactor-swarm"
    end

    attribute :image, :string do
      public? true
      default "bitactor:latest"
    end

    attribute :replicas, :integer do
      public? true
      default 3
      constraints min: 1
    end

    timestamps()
  end

  relationships do
    has_many :actors, BitActor.Ash.Resources.Actor
  end

  actions do
    defaults [:read, :create, :update, :destroy]
  end

  code_interface do
    define :create_swarm_config, action: :create
    define :list_swarm_configs, action: :read
    define :update_swarm_config, action: :update
  end
end