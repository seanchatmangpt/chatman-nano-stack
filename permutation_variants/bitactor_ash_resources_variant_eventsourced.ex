defmodule BitActor.Ash.Resources.EventSourced do
  @moduledoc """
  Event-Sourced BitActor Ash Resources Variant - CQRS with Event Store
  
  This variant explores event sourcing patterns with:
  - Command-Query Responsibility Segregation (CQRS)
  - Event store for all state changes
  - Aggregate reconstruction from events
  - Event-driven TTL constraint tracking
  - Projection-based read models
  - Event replay and temporal queries
  """
  
  # =============================================================================
  # Event-Sourced Domain
  # =============================================================================
  
  defmodule BitActor.Ash.Domain.EventSourced do
    @moduledoc "Event-sourced BitActor domain with CQRS architecture"
    
    use Ash.Domain,
      validate_config_inclusion?: false
    
    resources do
      resource BitActor.Ash.Resources.EventSourced.BitActorAggregate
      resource BitActor.Ash.Resources.EventSourced.SignalEvent
      resource BitActor.Ash.Resources.EventSourced.TTLConstraintEvent
      resource BitActor.Ash.Resources.EventSourced.TelemetryEvent
      resource BitActor.Ash.Resources.EventSourced.SwarmConfigurationEvent
      resource BitActor.Ash.Resources.EventSourced.TTLViolationEvent
      
      # Read Models (Projections)
      resource BitActor.Ash.Resources.EventSourced.BitActorProjection
      resource BitActor.Ash.Resources.EventSourced.SwarmHealthProjection
      resource BitActor.Ash.Resources.EventSourced.TTLMetricsProjection
      
      # Event Store
      resource BitActor.Ash.Resources.EventSourced.EventStore
      resource BitActor.Ash.Resources.EventSourced.Snapshot
    end
  end
  
  # =============================================================================
  # BitActor Aggregate (Write Side)
  # =============================================================================
  
  defmodule BitActor.Ash.Resources.EventSourced.BitActorAggregate do
    @moduledoc "Event-sourced BitActor aggregate for command processing"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshEventStore.DataLayer,
      notifiers: [BitActor.Ash.EventNotifier]
    
    event_store do
      event_store_name BitActor.EventStore
    end
    
    attributes do
      uuid_primary_key :id
      
      # Aggregate identification
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :name, :string, public?: true, constraints: [min_length: 1, max_length: 255]
      attribute :aggregate_version, :integer, public?: true, default: 0
      
      # Command processing state
      attribute :status, :atom, public?: true, default: :inactive
      attribute :ttl_budget_ms, :integer, public?: true, default: 8, constraints: [min: 1, max: 100]
      
      # Event sourcing metadata
      attribute :last_event_id, :uuid, public?: true
      attribute :last_event_timestamp, :utc_datetime_usec, public?: true
      attribute :events_count, :integer, public?: true, default: 0
      
      # TTL tracking (derived from events)
      attribute :current_ttl_budget_ns, :integer, public?: true
      attribute :ttl_violations_count, :integer, public?: true, default: 0
      attribute :last_processing_time_ns, :integer, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      # Command Actions (Write Side)
      create :create_bitactor do
        accept [:name, :ttl_budget_ms]
        
        change fn changeset, _context ->
          aggregate_id = UUID.uuid4()
          
          changeset
          |> Ash.Changeset.force_change_attribute(:aggregate_id, aggregate_id)
          |> Ash.Changeset.force_change_attribute(:aggregate_version, 1)
          |> Ash.Changeset.force_change_attribute(:current_ttl_budget_ns, 
               changeset.data.ttl_budget_ms * 1_000_000)
        end
        
        # Emit BitActorCreated event
        change {BitActor.Ash.Changes.EmitEvent, event_type: :bitactor_created}
      end
      
      update :process_signal_command do
        accept []
        argument :signal_id, :uuid, allow_nil?: false
        argument :signal_type, :atom, allow_nil?: false
        argument :signal_payload, :map, default: %{}
        argument :ttl_constraint_ns, :integer, allow_nil?: true
        
        change fn changeset, context ->
          signal_id = context.arguments.signal_id
          signal_type = context.arguments.signal_type
          signal_payload = context.arguments.signal_payload
          ttl_constraint_ns = context.arguments.ttl_constraint_ns
          
          start_time = System.monotonic_time(:nanosecond)
          
          # Validate TTL constraint
          current_ttl = changeset.data.current_ttl_budget_ns
          required_ttl = ttl_constraint_ns || (changeset.data.ttl_budget_ms * 1_000_000)
          
          if current_ttl >= required_ttl do
            # Process command and emit events
            processing_time_ns = System.monotonic_time(:nanosecond) - start_time
            new_ttl_budget = current_ttl - processing_time_ns
            new_version = changeset.data.aggregate_version + 1
            
            changeset
            |> Ash.Changeset.force_change_attribute(:aggregate_version, new_version)
            |> Ash.Changeset.force_change_attribute(:current_ttl_budget_ns, new_ttl_budget)
            |> Ash.Changeset.force_change_attribute(:last_processing_time_ns, processing_time_ns)
            |> Ash.Changeset.force_change_attribute(:events_count, changeset.data.events_count + 1)
            |> emit_signal_processed_event(signal_id, signal_type, processing_time_ns)
          else
            # Emit TTL violation event
            changeset
            |> Ash.Changeset.force_change_attribute(:ttl_violations_count, 
                 changeset.data.ttl_violations_count + 1)
            |> emit_ttl_violation_event(signal_id, required_ttl, current_ttl)
            |> Ash.Changeset.add_error(field: :ttl_constraint_ns, message: "Insufficient TTL budget")
          end
        end
      end
      
      update :update_ttl_budget do
        accept []
        argument :new_budget_ms, :integer, allow_nil?: false
        
        change fn changeset, context ->
          new_budget_ms = context.arguments.new_budget_ms
          new_budget_ns = new_budget_ms * 1_000_000
          new_version = changeset.data.aggregate_version + 1
          
          changeset
          |> Ash.Changeset.force_change_attribute(:ttl_budget_ms, new_budget_ms)
          |> Ash.Changeset.force_change_attribute(:current_ttl_budget_ns, new_budget_ns)
          |> Ash.Changeset.force_change_attribute(:aggregate_version, new_version)
          |> emit_ttl_budget_updated_event(new_budget_ms, new_budget_ns)
        end
      end
      
      # Event Replay Action
      action :replay_events do
        argument :from_version, :integer, default: 0
        argument :to_version, :integer, allow_nil?: true
        
        run fn input, context ->
          aggregate_id = input.id
          from_version = input.arguments.from_version
          to_version = input.arguments.to_version
          
          # Query events from event store
          events = BitActor.Ash.Resources.EventSourced.EventStore
          |> Ash.Query.filter(aggregate_id == ^aggregate_id)
          |> Ash.Query.filter(event_version >= ^from_version)
          |> then(fn query ->
            if to_version do
              Ash.Query.filter(query, event_version <= ^to_version)
            else
              query
            end
          end)
          |> Ash.Query.sort(:event_version)
          |> Ash.read!()
          
          # Reconstruct aggregate state from events
          reconstructed_state = Enum.reduce(events, %{}, fn event, state ->
            apply_event_to_state(event, state)
          end)
          
          {:ok, reconstructed_state}
        end
      end
    end
    
    calculations do
      calculate :ttl_utilization_percent, :decimal, expr(
        fragment("CASE WHEN ? > 0 THEN (1.0 - (? / ?::float)) * 100.0 ELSE 0 END",
          ttl_budget_ms, current_ttl_budget_ns, ttl_budget_ms * 1_000_000)
      )
      
      calculate :events_per_hour, :integer, expr(
        fragment("CASE WHEN AGE(NOW(), ?) > INTERVAL '1 hour' 
                  THEN ? / EXTRACT(EPOCH FROM AGE(NOW(), ?)) * 3600 
                  ELSE ? END",
          inserted_at, events_count, inserted_at, events_count)
      )
      
      calculate :average_processing_time_ms, :decimal, expr(
        fragment("? / 1000000.0", last_processing_time_ns)
      )
    end
    
    # Event emission helpers
    defp emit_signal_processed_event(changeset, signal_id, signal_type, processing_time_ns) do
      event_data = %{
        signal_id: signal_id,
        signal_type: signal_type,
        processing_time_ns: processing_time_ns,
        timestamp: DateTime.utc_now()
      }
      
      Ash.Changeset.after_action(changeset, fn changeset_result, _result ->
        BitActor.Ash.Resources.EventSourced.SignalEvent
        |> Ash.Changeset.for_create(:create, %{
          aggregate_id: changeset_result.id,
          event_type: :signal_processed,
          event_data: event_data,
          event_version: changeset_result.aggregate_version
        })
        |> Ash.create!()
        
        changeset_result
      end)
    end
    
    defp emit_ttl_violation_event(changeset, signal_id, required_ttl, current_ttl) do
      event_data = %{
        signal_id: signal_id,
        required_ttl_ns: required_ttl,
        current_ttl_ns: current_ttl,
        violation_type: :insufficient_budget,
        timestamp: DateTime.utc_now()
      }
      
      Ash.Changeset.after_action(changeset, fn changeset_result, _result ->
        BitActor.Ash.Resources.EventSourced.TTLViolationEvent
        |> Ash.Changeset.for_create(:create, %{
          aggregate_id: changeset_result.id,
          event_type: :ttl_violation,
          event_data: event_data,
          event_version: changeset_result.aggregate_version
        })
        |> Ash.create!()
        
        changeset_result
      end)
    end
    
    defp emit_ttl_budget_updated_event(changeset, new_budget_ms, new_budget_ns) do
      event_data = %{
        old_budget_ms: changeset.data.ttl_budget_ms,
        new_budget_ms: new_budget_ms,
        new_budget_ns: new_budget_ns,
        timestamp: DateTime.utc_now()
      }
      
      Ash.Changeset.after_action(changeset, fn changeset_result, _result ->
        BitActor.Ash.Resources.EventSourced.TTLConstraintEvent
        |> Ash.Changeset.for_create(:create, %{
          aggregate_id: changeset_result.id,
          event_type: :ttl_budget_updated,
          event_data: event_data,
          event_version: changeset_result.aggregate_version
        })
        |> Ash.create!()
        
        changeset_result
      end)
    end
    
    defp apply_event_to_state(event, state) do
      case event.event_type do
        :bitactor_created ->
          Map.merge(state, event.event_data)
        
        :signal_processed ->
          state
          |> Map.update(:signals_processed, 1, &(&1 + 1))
          |> Map.put(:last_processing_time_ns, event.event_data.processing_time_ns)
        
        :ttl_violation ->
          Map.update(state, :ttl_violations, 1, &(&1 + 1))
        
        :ttl_budget_updated ->
          state
          |> Map.put(:ttl_budget_ms, event.event_data.new_budget_ms)
          |> Map.put(:current_ttl_budget_ns, event.event_data.new_budget_ns)
        
        _ ->
          state
      end
    end
  end
  
  # =============================================================================
  # Event Store
  # =============================================================================
  
  defmodule BitActor.Ash.Resources.EventSourced.EventStore do
    @moduledoc "Central event store for all BitActor domain events"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_event_store"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      
      # Event identification
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :event_type, :atom, public?: true, allow_nil?: false
      attribute :event_version, :integer, public?: true, allow_nil?: false
      
      # Event payload
      attribute :event_data, :map, public?: true, default: %{}
      attribute :event_metadata, :map, public?: true, default: %{}
      
      # Event timing with nanosecond precision
      attribute :occurred_at, :utc_datetime_usec, public?: true, default: &DateTime.utc_now/0
      attribute :occurred_at_ns, :integer, public?: true, default: &System.monotonic_time/1
      
      # Event causation
      attribute :causation_id, :uuid, public?: true
      attribute :correlation_id, :uuid, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :event_type, :event_version, :event_data, :event_metadata,
                :causation_id, :correlation_id]
        
        change fn changeset, _context ->
          changeset
          |> Ash.Changeset.force_change_attribute(:occurred_at_ns, System.monotonic_time(:nanosecond))
        end
      end
      
      # Temporal queries
      read :events_for_aggregate do
        argument :aggregate_id, :uuid, allow_nil?: false
        argument :from_version, :integer, default: 0
        argument :to_version, :integer, allow_nil?: true
        
        prepare fn query, context ->
          aggregate_id = context.arguments.aggregate_id
          from_version = context.arguments.from_version
          to_version = context.arguments.to_version
          
          query = query
          |> Ash.Query.filter(aggregate_id == ^aggregate_id)
          |> Ash.Query.filter(event_version >= ^from_version)
          |> Ash.Query.sort(:event_version)
          
          if to_version do
            Ash.Query.filter(query, event_version <= ^to_version)
          else
            query
          end
        end
      end
      
      read :events_by_type do
        argument :event_type, :atom, allow_nil?: false
        argument :since, :utc_datetime_usec, allow_nil?: true
        
        prepare fn query, context ->
          event_type = context.arguments.event_type
          since = context.arguments.since
          
          query = Ash.Query.filter(query, event_type == ^event_type)
          
          if since do
            Ash.Query.filter(query, occurred_at >= ^since)
          else
            query
          end
        end
      end
      
      read :events_in_time_range do
        argument :start_time_ns, :integer, allow_nil?: false
        argument :end_time_ns, :integer, allow_nil?: false
        
        prepare fn query, context ->
          start_time = context.arguments.start_time_ns
          end_time = context.arguments.end_time_ns
          
          query
          |> Ash.Query.filter(occurred_at_ns >= ^start_time)
          |> Ash.Query.filter(occurred_at_ns <= ^end_time)
          |> Ash.Query.sort(:occurred_at_ns)
        end
      end
    end
    
    calculations do
      calculate :event_age_ms, :integer, expr(
        fragment("EXTRACT(EPOCH FROM AGE(NOW(), ?)) * 1000", occurred_at)
      )
      
      calculate :processing_latency_ns, :integer, expr(
        fragment("? - ?", 
          fragment("EXTRACT(EPOCH FROM ?) * 1000000000", occurred_at),
          occurred_at_ns)
      )
    end
    
    identities do
      identity :unique_event_per_aggregate_version, [:aggregate_id, :event_version]
    end
  end
  
  # =============================================================================
  # Domain Events
  # =============================================================================
  
  defmodule BitActor.Ash.Resources.EventSourced.SignalEvent do
    @moduledoc "Events related to signal processing"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_signal_events"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :event_type, :atom, public?: true, allow_nil?: false
      attribute :event_version, :integer, public?: true, allow_nil?: false
      attribute :event_data, :map, public?: true, default: %{}
      
      # Signal-specific attributes
      attribute :signal_id, :uuid, public?: true
      attribute :signal_type, :atom, public?: true
      attribute :processing_time_ns, :integer, public?: true
      attribute :ttl_budget_consumed_ns, :integer, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :event_type, :event_version, :event_data,
                :signal_id, :signal_type, :processing_time_ns, :ttl_budget_consumed_ns]
      end
    end
  end
  
  defmodule BitActor.Ash.Resources.EventSourced.TTLConstraintEvent do
    @moduledoc "Events related to TTL constraint changes"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_ttl_constraint_events"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :event_type, :atom, public?: true, allow_nil?: false
      attribute :event_version, :integer, public?: true, allow_nil?: false
      attribute :event_data, :map, public?: true, default: %{}
      
      # TTL-specific attributes
      attribute :old_budget_ns, :integer, public?: true
      attribute :new_budget_ns, :integer, public?: true
      attribute :precision, :atom, public?: true, default: :nanosecond
      attribute :constraint_source, :string, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :event_type, :event_version, :event_data,
                :old_budget_ns, :new_budget_ns, :precision, :constraint_source]
      end
    end
  end
  
  defmodule BitActor.Ash.Resources.EventSourced.TTLViolationEvent do
    @moduledoc "Events for TTL constraint violations"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_ttl_violation_events"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :event_type, :atom, public?: true, allow_nil?: false
      attribute :event_version, :integer, public?: true, allow_nil?: false
      attribute :event_data, :map, public?: true, default: %{}
      
      # Violation-specific attributes
      attribute :violation_type, :atom, public?: true
      attribute :required_ttl_ns, :integer, public?: true
      attribute :actual_ttl_ns, :integer, public?: true
      attribute :violation_severity, :atom, public?: true, default: :warning
      attribute :signal_id, :uuid, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :event_type, :event_version, :event_data,
                :violation_type, :required_ttl_ns, :actual_ttl_ns, 
                :violation_severity, :signal_id]
      end
    end
    
    calculations do
      calculate :ttl_deficit_ns, :integer, expr(required_ttl_ns - actual_ttl_ns)
      calculate :ttl_deficit_percent, :decimal, expr(
        fragment("CASE WHEN ? > 0 THEN ((? - ?) / ?::float) * 100.0 ELSE 0 END",
          required_ttl_ns, required_ttl_ns, actual_ttl_ns, required_ttl_ns)
      )
    end
  end
  
  # =============================================================================
  # Read Models (Projections)
  # =============================================================================
  
  defmodule BitActor.Ash.Resources.EventSourced.BitActorProjection do
    @moduledoc "Optimized read model for BitActor queries"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_projection"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :name, :string, public?: true
      attribute :status, :atom, public?: true
      
      # Aggregated metrics
      attribute :total_signals_processed, :integer, public?: true, default: 0
      attribute :total_ttl_violations, :integer, public?: true, default: 0
      attribute :average_processing_time_ns, :integer, public?: true, default: 0
      attribute :current_ttl_budget_ns, :integer, public?: true
      attribute :ttl_budget_ms, :integer, public?: true
      
      # Performance metrics
      attribute :signals_per_second, :decimal, public?: true, default: 0.0
      attribute :ttl_utilization_percent, :decimal, public?: true, default: 0.0
      attribute :error_rate_percent, :decimal, public?: true, default: 0.0
      
      # Temporal data
      attribute :last_activity_at, :utc_datetime_usec, public?: true
      attribute :projection_updated_at, :utc_datetime_usec, public?: true, default: &DateTime.utc_now/0
      attribute :event_version, :integer, public?: true, default: 0
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :name, :status, :ttl_budget_ms, :current_ttl_budget_ns]
      end
      
      update :update_from_event do
        accept []
        argument :event, :map, allow_nil?: false
        
        change fn changeset, context ->
          event = context.arguments.event
          
          # Update projection based on event type
          case event.event_type do
            :signal_processed ->
              update_signal_processed_metrics(changeset, event)
            
            :ttl_violation ->
              update_ttl_violation_metrics(changeset, event)
            
            :ttl_budget_updated ->
              update_ttl_budget_metrics(changeset, event)
              
            _ ->
              changeset
          end
          |> Ash.Changeset.force_change_attribute(:event_version, event.event_version)
          |> Ash.Changeset.force_change_attribute(:projection_updated_at, DateTime.utc_now())
        end
      end
      
      read :high_performance_actors do
        prepare fn query, _context ->
          query
          |> Ash.Query.filter(signals_per_second > 100.0)
          |> Ash.Query.filter(error_rate_percent < 5.0)
          |> Ash.Query.sort(desc: :signals_per_second)
        end
      end
      
      read :ttl_violation_prone_actors do
        prepare fn query, _context ->
          query
          |> Ash.Query.filter(ttl_utilization_percent > 80.0)
          |> Ash.Query.sort(desc: :ttl_utilization_percent)
        end
      end
    end
    
    defp update_signal_processed_metrics(changeset, event) do
      current_total = changeset.data.total_signals_processed
      current_avg = changeset.data.average_processing_time_ns
      processing_time = event.event_data.processing_time_ns
      
      new_total = current_total + 1
      new_avg = div((current_avg * current_total) + processing_time, new_total)
      
      changeset
      |> Ash.Changeset.force_change_attribute(:total_signals_processed, new_total)
      |> Ash.Changeset.force_change_attribute(:average_processing_time_ns, new_avg)
      |> Ash.Changeset.force_change_attribute(:last_activity_at, DateTime.utc_now())
    end
    
    defp update_ttl_violation_metrics(changeset, _event) do
      new_violations = changeset.data.total_ttl_violations + 1
      
      changeset
      |> Ash.Changeset.force_change_attribute(:total_ttl_violations, new_violations)
    end
    
    defp update_ttl_budget_metrics(changeset, event) do
      changeset
      |> Ash.Changeset.force_change_attribute(:ttl_budget_ms, event.event_data.new_budget_ms)
      |> Ash.Changeset.force_change_attribute(:current_ttl_budget_ns, event.event_data.new_budget_ns)
    end
  end
  
  # =============================================================================
  # Snapshots for Performance
  # =============================================================================
  
  defmodule BitActor.Ash.Resources.EventSourced.Snapshot do
    @moduledoc "Aggregate snapshots for performance optimization"
    
    use Ash.Resource,
      domain: BitActor.Ash.Domain.EventSourced,
      data_layer: AshPostgres.DataLayer
    
    postgres do
      table "bitactor_snapshots"
      repo BitActor.Repo
    end
    
    attributes do
      uuid_primary_key :id
      attribute :aggregate_id, :uuid, public?: true, allow_nil?: false
      attribute :snapshot_version, :integer, public?: true, allow_nil?: false
      attribute :aggregate_state, :map, public?: true, default: %{}
      attribute :events_count_at_snapshot, :integer, public?: true
      
      timestamps()
    end
    
    actions do
      defaults [:read, :destroy]
      
      create :create do
        accept [:aggregate_id, :snapshot_version, :aggregate_state, :events_count_at_snapshot]
      end
      
      read :latest_snapshot_for_aggregate do
        argument :aggregate_id, :uuid, allow_nil?: false
        
        prepare fn query, context ->
          aggregate_id = context.arguments.aggregate_id
          
          query
          |> Ash.Query.filter(aggregate_id == ^aggregate_id)
          |> Ash.Query.sort(desc: :snapshot_version)
          |> Ash.Query.limit(1)
        end
      end
    end
    
    identities do
      identity :unique_snapshot_per_aggregate_version, [:aggregate_id, :snapshot_version]
    end
  end
  
  # =============================================================================
  # Event Notifier
  # =============================================================================
  
  defmodule BitActor.Ash.EventNotifier do
    @moduledoc "Notifier for propagating events to projections"
    
    use Ash.Notifier
    
    @impl Ash.Notifier
    def notify(%Ash.Notifier.Notification{
      resource: BitActor.Ash.Resources.EventSourced.EventStore,
      action: %{type: :create},
      data: event
    }) do
      # Update relevant projections
      update_projections_from_event(event)
      :ok
    end
    
    def notify(_), do: :ok
    
    defp update_projections_from_event(event) do
      # Update BitActor projection
      case BitActor.Ash.Resources.EventSourced.BitActorProjection
           |> Ash.Query.filter(aggregate_id == ^event.aggregate_id)
           |> Ash.read_one() do
        {:ok, projection} when not is_nil(projection) ->
          projection
          |> Ash.Changeset.for_update(:update_from_event, %{event: event})
          |> Ash.update()
        
        _ ->
          # Create projection if it doesn't exist
          BitActor.Ash.Resources.EventSourced.BitActorProjection
          |> Ash.Changeset.for_create(:create, %{
            aggregate_id: event.aggregate_id,
            name: event.event_data[:name] || "UnknownActor",
            status: :active,
            ttl_budget_ms: event.event_data[:ttl_budget_ms] || 8,
            current_ttl_budget_ns: (event.event_data[:ttl_budget_ms] || 8) * 1_000_000
          })
          |> Ash.create()
      end
    end
  end
end