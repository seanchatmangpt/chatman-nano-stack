
defmodule CnsForge.ReactiveNotifications do
  @moduledoc """
  Reactive notification system that adapts to step outcomes
  Uses Ash.Notifier for automatic resource event handling
  """
  
  use GenServer
  require Logger
  
  defstruct [:subscribers, :notification_rules, :escalation_paths]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    state = %__MODULE__{
      subscribers: %{},
      notification_rules: load_notification_rules(),
      escalation_paths: load_escalation_paths()
    }
    {:ok, state}
  end
  
  # Subscribe to reactive notifications
  def subscribe(pattern, callback) do
    GenServer.call(__MODULE__, {:subscribe, pattern, callback})
  end
  
  # Handle reactor step outcomes
  def handle_step_outcome(step_name, outcome, context \\ %{}) do
    GenServer.cast(__MODULE__, {:step_outcome, step_name, outcome, context})
  end
  
  def handle_call({:subscribe, pattern, callback}, _from, state) do
    new_subscribers = Map.put(state.subscribers, pattern, callback)
    {:reply, :ok, %{state | subscribers: new_subscribers}}
  end
  
  def handle_cast({:step_outcome, step_name, outcome, context}, state) do
    # Apply reactive logic based on outcome
    notifications = determine_notifications(step_name, outcome, context, state)
    
    Enum.each(notifications, &send_notification/1)
    
    {:noreply, state}
  end
  
  defp determine_notifications(step_name, outcome, context, state) do
    base_notification = %{
      step: step_name,
      outcome: outcome,
      context: context,
      timestamp: DateTime.utc_now()
    }
    
    case outcome do
      :success -> 
        success_notifications(base_notification, state)
      :warning ->
        warning_notifications(base_notification, state) 
      :error ->
        error_notifications(base_notification, state)
      :milestone ->
        milestone_notifications(base_notification, state)
    end
  end
  
  defp success_notifications(notification, _state) do
    [
      %{notification | type: "continue_pipeline", priority: "normal"},
      %{notification | type: "update_metrics", priority: "low"},
      %{notification | type: "notify_stakeholders", priority: "low"}
    ]
  end
  
  defp warning_notifications(notification, _state) do
    [
      %{notification | type: "escalate_monitoring", priority: "high"},
      %{notification | type: "prepare_fallback", priority: "high"},
      %{notification | type: "alert_operators", priority: "medium"}
    ]
  end
  
  defp error_notifications(notification, _state) do
    [
      %{notification | type: "trigger_compensation", priority: "critical"},
      %{notification | type: "halt_pipeline", priority: "critical"},
      %{notification | type: "emergency_notification", priority: "critical"}
    ]
  end
  
  defp milestone_notifications(notification, _state) do
    [
      %{notification | type: "celebrate_achievement", priority: "medium"},
      %{notification | type: "broadcast_progress", priority: "medium"},
      %{notification | type: "update_dashboard", priority: "low"}
    ]
  end
  
  defp send_notification(notification) do
    # Route notification based on type and priority
    case notification.type do
      "emergency_notification" -> 
        send_emergency_alert(notification)
      "celebrate_achievement" ->
        send_celebration(notification)
      _ ->
        send_standard_notification(notification)
    end
  end
  
  defp send_emergency_alert(notification) do
    # Multiple channels for critical notifications
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "emergency:alerts", notification)
    Logger.error("EMERGENCY: #{notification.step} - #{inspect(notification.context)}")
  end
  
  defp send_celebration(notification) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations", notification)
    Logger.info("🎉 MILESTONE: #{notification.step} - #{inspect(notification.context)}")
  end
  
  defp send_standard_notification(notification) do
    channel = "notifications:#{notification.priority}"
    Phoenix.PubSub.broadcast(CnsForge.PubSub, channel, notification)
  end
  
  defp load_notification_rules do
    # Load from configuration or database
    %{}
  end
  
  defp load_escalation_paths do
    # Define escalation paths for different scenarios
    %{}
  end
end

# Integration with Ash Reactor steps
defmodule CnsForge.ReactiveStepReactor do
  use Reactor
  alias CnsForge.ReactiveNotifications
  
  input :operation
  
  step :validate_operation do
    argument :op, input(:operation)
    
    run fn %{op: op}, _context ->
      case validate_op(op) do
        {:ok, result} ->
          ReactiveNotifications.handle_step_outcome("validate_operation", :success, %{operation: op})
          {:ok, result}
        {:error, reason} ->
          ReactiveNotifications.handle_step_outcome("validate_operation", :error, %{reason: reason})
          {:error, reason}
      end
    end
  end
  
  defp validate_op(op) when is_binary(op), do: {:ok, %{validated: op}}
  defp validate_op(_), do: {:error, "Invalid operation"}
  
  return :validate_operation
end
