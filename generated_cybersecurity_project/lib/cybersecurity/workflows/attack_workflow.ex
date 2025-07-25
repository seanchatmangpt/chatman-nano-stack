defmodule Cybersecurity.Workflows.AttackWorkflow do
  @moduledoc """
  Workflow for Attack resource operations
  """

  use Ash.Reactor

  input :action
  input :resource_data, default: %{}
  input :resource_id, default: nil

  step :validate_action do
    argument :action, input(:action)
    
    run fn %{action: action} ->
      if action in [:create, :read, :update, :delete, :list] do
        {:ok, %{validated_action: action}}
      else
        {:error, :invalid_action}
      end
    end
  end

  step :execute_resource_action do
    argument :action, result(:validate_action, [:validated_action])
    argument :data, input(:resource_data)
    argument :id, input(:resource_id)
    
    run fn %{action: action, data: data, id: id} ->
      resource = Cybersecurity.Resources.Attack
      
      case action do
        :create ->
          Ash.create(resource, data)
          
        :read when not is_nil(id) ->
          Ash.get(resource, id)
          
        :list ->
          Ash.read(resource)
          
        :update when not is_nil(id) ->
          case Ash.get(resource, id) do
            {:ok, record} -> Ash.update(record, data)
            error -> error
          end
          
        :delete when not is_nil(id) ->
          case Ash.get(resource, id) do
            {:ok, record} -> Ash.destroy(record)
            error -> error
          end
          
        _ ->
          {:error, :invalid_action_combination}
      end
    end
  end

  step :format_result do
    argument :result, result(:execute_resource_action)
    argument :action, result(:validate_action, [:validated_action])
    
    run fn %{result: result, action: action} ->
      {:ok, %{
        resource: "attack",
        action: action,
        result: result,
        timestamp: DateTime.utc_now()
      }}
    end
  end

  return :format_result
end
