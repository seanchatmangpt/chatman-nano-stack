defmodule CNSForge.Signal do
  @moduledoc """
  Signal resource for routing between BitActors in the mesh
  
  Represents the immutable data payload passed between BitActors.
  Uses Elixir Registry for high-performance signal routing.
  """
  
  use Ash.Resource,
    domain: CNSForge,
    data_layer: Ash.DataLayer.Mnesia

  attributes do
    uuid_primary_key :id
    
    attribute :type, :atom do
      description "Signal type for dispatch routing"
      allow_nil? false
    end
    
    attribute :source_actor_id, :uuid do
      description "ID of the BitActor that sent this signal"
      allow_nil? false
    end
    
    attribute :target_actor_type, :atom do
      description "Target BitActor type for routing"
    end
    
    attribute :transaction_id, :string do
      description "Transaction correlation ID"
      allow_nil? false
    end
    
    attribute :payload, :map do
      description "Signal data payload"
      allow_nil? false
      default %{}
    end
    
    attribute :ttl, :integer do
      description "TTL inherited from source BitActor"
      allow_nil? false
    end
    
    attribute :priority, :atom do
      description "Signal priority: :low, :medium, :high, :critical"
      allow_nil? false
      default :medium
    end
    
    attribute :created_at, :utc_datetime_usec do
      description "Signal creation timestamp"
      allow_nil? false
      default &DateTime.utc_now/0
    end
    
    attribute :routed_at, :utc_datetime_usec do
      description "When signal was successfully routed"
    end
    
    attribute :consumed_at, :utc_datetime_usec do
      description "When signal was consumed by target"
    end
  end

  actions do
    defaults [:read, :destroy]
    
    create :emit do
      description "Emit a new signal from a BitActor"
      
      argument :type, :atom, allow_nil? false
      argument :source_actor_id, :uuid, allow_nil? false
      argument :transaction_id, :string, allow_nil? false
      argument :payload, :map, default: %{}
      argument :ttl, :integer, allow_nil? false
      argument :target_actor_type, :atom
      argument :priority, :atom, default: :medium
      
      change set_attribute(:type, arg(:type))
      change set_attribute(:source_actor_id, arg(:source_actor_id))
      change set_attribute(:transaction_id, arg(:transaction_id))
      change set_attribute(:payload, arg(:payload))
      change set_attribute(:ttl, arg(:ttl))
      change set_attribute(:target_actor_type, arg(:target_actor_type))
      change set_attribute(:priority, arg(:priority))
      
      change after_action(fn _changeset, signal ->
        # Route the signal using Registry
        route_signal(signal)
        {:ok, signal}
      end)
    end
    
    update :mark_routed do
      description "Mark signal as successfully routed"
      
      change set_attribute(:routed_at, &DateTime.utc_now/0)
    end
    
    update :mark_consumed do
      description "Mark signal as consumed by target"
      
      change set_attribute(:consumed_at, &DateTime.utc_now/0)
    end
  end

  preparations do
    prepare build(load: [:created_at, :routed_at, :consumed_at])
  end

  identities do
    identity :transaction_signal, [:transaction_id, :created_at]
  end

  # Signal routing implementation using Elixir Registry
  
  defp route_signal(signal) do
    # Emit telemetry for signal routing
    :telemetry.execute(
      [:cns_forge, :signal, :routed],
      %{ttl: signal.ttl},
      %{
        signal_id: signal.id,
        signal_type: signal.type,
        transaction_id: signal.transaction_id,
        target_type: signal.target_actor_type
      }
    )
    
    case signal.target_actor_type do
      nil ->
        # Broadcast to all consumers of this signal type
        Registry.dispatch(CNSForge.SignalRegistry, {:signal_consumer, signal.type}, fn entries ->
          for {pid, _} <- entries do
            send(pid, {:signal, signal})
          end
        end)
        
      target_type ->
        # Route to specific BitActor type
        case Registry.lookup(CNSForge.SignalRegistry, {:signal_consumer, target_type}) do
          [] ->
            # No consumers registered - signal is dropped
            :telemetry.execute(
              [:cns_forge, :signal, :dropped],
              %{},
              %{signal_id: signal.id, reason: :no_consumers}
            )
            
          consumers ->
            # Send to first available consumer (could implement load balancing)
            [{pid, _}] = Enum.take(consumers, 1)
            send(pid, {:signal, signal})
        end
    end
    
    # Mark as routed
    signal
    |> Ash.Changeset.for_update(:mark_routed)
    |> CNSForge.update!()
  end
end