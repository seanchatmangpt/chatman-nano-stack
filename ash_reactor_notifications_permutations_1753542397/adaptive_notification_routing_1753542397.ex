
defmodule CnsForge.AdaptiveNotificationRouting do
  @moduledoc """
  Adaptive notification routing based on context, priority, and recipient preferences
  Learns from user behavior to optimize notification delivery
  """
  
  use GenServer
  require Logger
  
  defstruct [:routing_rules, :recipient_preferences, :delivery_history, :learning_engine]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    state = %__MODULE__{
      routing_rules: load_routing_rules(),
      recipient_preferences: load_recipient_preferences(), 
      delivery_history: %{},
      learning_engine: initialize_learning_engine()
    }
    {:ok, state}
  end
  
  # Route a notification based on adaptive intelligence
  def route_notification(notification, context \\ %{}) do
    GenServer.call(__MODULE__, {:route_notification, notification, context})
  end
  
  # Update recipient preferences based on feedback
  def update_preferences(recipient_id, feedback) do
    GenServer.cast(__MODULE__, {:update_preferences, recipient_id, feedback})
  end
  
  # Register a new recipient with initial preferences
  def register_recipient(recipient_id, initial_preferences) do
    GenServer.call(__MODULE__, {:register_recipient, recipient_id, initial_preferences})
  end
  
  def handle_call({:route_notification, notification, context}, _from, state) do
    # Determine optimal routing strategy
    routing_strategy = determine_routing_strategy(notification, context, state)
    
    # Apply routing strategy
    delivery_plan = create_delivery_plan(notification, routing_strategy, state)
    
    # Execute delivery
    delivery_results = execute_delivery_plan(delivery_plan)
    
    # Update learning engine
    new_learning_engine = update_learning(state.learning_engine, notification, delivery_results)
    
    # Update delivery history
    new_history = update_delivery_history(state.delivery_history, notification, delivery_results)
    
    new_state = %{state | 
      learning_engine: new_learning_engine,
      delivery_history: new_history
    }
    
    {:reply, {:ok, delivery_results}, new_state}
  end
  
  def handle_call({:register_recipient, recipient_id, initial_preferences}, _from, state) do
    new_preferences = Map.put(state.recipient_preferences, recipient_id, initial_preferences)
    {:reply, :ok, %{state | recipient_preferences: new_preferences}}
  end
  
  def handle_cast({:update_preferences, recipient_id, feedback}, state) do
    # Learn from feedback to improve future routing
    updated_preferences = apply_feedback_learning(
      state.recipient_preferences[recipient_id], 
      feedback
    )
    
    new_preferences = Map.put(state.recipient_preferences, recipient_id, updated_preferences)
    {:noreply, %{state | recipient_preferences: new_preferences}}
  end
  
  defp determine_routing_strategy(notification, context, state) do
    # Multi-factor routing decision
    factors = %{
      urgency: determine_urgency(notification),
      recipient_availability: check_recipient_availability(context),
      content_type: classify_content_type(notification),
      historical_engagement: analyze_historical_engagement(notification, state),
      current_load: assess_current_system_load()
    }
    
    # Use machine learning model to determine optimal strategy
    apply_routing_intelligence(factors, state.learning_engine)
  end
  
  defp apply_routing_intelligence(factors, learning_engine) do
    # Simple decision tree (could be replaced with ML model)
    cond do
      factors.urgency >= 0.9 ->
        %{strategy: :immediate_multi_channel, priority: :critical}
      
      factors.urgency >= 0.7 and factors.recipient_availability >= 0.6 ->
        %{strategy: :preferred_channel_with_fallback, priority: :high}
      
      factors.historical_engagement >= 0.8 ->
        %{strategy: :learned_preference, priority: :medium}
      
      factors.current_load >= 0.8 ->
        %{strategy: :batch_delivery, priority: :low}
      
      true ->
        %{strategy: :standard_routing, priority: :medium}
    end
  end
  
  defp create_delivery_plan(notification, routing_strategy, state) do
    recipients = determine_recipients(notification, state.routing_rules)
    
    Enum.map(recipients, fn recipient ->
      recipient_preferences = state.recipient_preferences[recipient.id] || %{}
      
      channels = select_channels(routing_strategy, recipient_preferences, recipient)
      delivery_timing = calculate_delivery_timing(routing_strategy, recipient_preferences)
      
      %{
        recipient: recipient,
        channels: channels,
        timing: delivery_timing,
        content: customize_content(notification, recipient_preferences),
        strategy: routing_strategy.strategy
      }
    end)
  end
  
  defp select_channels(routing_strategy, recipient_preferences, recipient) do
    case routing_strategy.strategy do
      :immediate_multi_channel ->
        # Use all available high-priority channels
        ["push_notification", "email", "slack", "sms"]
        |> filter_by_recipient_availability(recipient)
      
      :preferred_channel_with_fallback ->
        primary = Map.get(recipient_preferences, :preferred_channel, "email")
        fallback = Map.get(recipient_preferences, :fallback_channel, "slack")
        [primary, fallback]
      
      :learned_preference ->
        # Use channels with highest engagement for this recipient
        get_highest_engagement_channels(recipient_preferences)
      
      :batch_delivery ->
        # Use low-cost batch channels
        ["email", "dashboard_notification"]
      
      :standard_routing ->
        # Default channel selection
        ["email", "dashboard_notification"]
    end
  end
  
  defp calculate_delivery_timing(routing_strategy, recipient_preferences) do
    base_delay = case routing_strategy.priority do
      :critical -> 0
      :high -> 30_000      # 30 seconds
      :medium -> 300_000   # 5 minutes
      :low -> 1_800_000    # 30 minutes
    end
    
    # Adjust based on recipient preferences
    preferred_hours = Map.get(recipient_preferences, :preferred_hours, {8, 18})
    timezone = Map.get(recipient_preferences, :timezone, "UTC")
    
    adjust_for_recipient_schedule(base_delay, preferred_hours, timezone)
  end
  
  defp customize_content(notification, recipient_preferences) do
    # Customize notification content based on preferences
    content_style = Map.get(recipient_preferences, :content_style, :standard)
    verbosity = Map.get(recipient_preferences, :verbosity, :medium)
    
    case content_style do
      :technical ->
        add_technical_details(notification)
      
      :business ->
        focus_on_business_impact(notification)
      
      :summary ->
        create_summary_version(notification)
      
      :standard ->
        notification
    end
    |> adjust_verbosity(verbosity)
  end
  
  defp execute_delivery_plan(delivery_plan) do
    # Execute delivery for each recipient
    Task.async_stream(delivery_plan, fn delivery ->
      execute_delivery(delivery)
    end)
    |> Enum.map(fn {:ok, result} -> result end)
  end
  
  defp execute_delivery(delivery) do
    results = Enum.map(delivery.channels, fn channel ->
      # Apply delivery timing
      if delivery.timing > 0 do
        Process.sleep(delivery.timing)
      end
      
      case channel do
        "push_notification" ->
          send_push_notification(delivery.recipient, delivery.content)
        
        "email" ->
          send_email_notification(delivery.recipient, delivery.content)
        
        "slack" ->
          send_slack_notification(delivery.recipient, delivery.content)
        
        "sms" ->
          send_sms_notification(delivery.recipient, delivery.content)
        
        "dashboard_notification" ->
          send_dashboard_notification(delivery.recipient, delivery.content)
        
        _ ->
          {:error, "Unknown channel: #{channel}"}
      end
    end)
    
    %{
      recipient: delivery.recipient,
      results: results,
      delivered_at: DateTime.utc_now(),
      strategy_used: delivery.strategy
    }
  end
  
  defp update_learning(learning_engine, notification, delivery_results) do
    # Update machine learning model with delivery results
    success_rate = calculate_success_rate(delivery_results)
    engagement_score = calculate_engagement_score(delivery_results)
    
    # Store learning data
    learning_data = %{
      notification_type: notification.type,
      success_rate: success_rate,
      engagement_score: engagement_score,
      delivery_timestamp: DateTime.utc_now()
    }
    
    # Update model (simplified)
    Map.update(learning_engine, :historical_data, [], fn data ->
      [learning_data | data] |> Enum.take(1000)  # Keep last 1000 entries
    end)
  end
  
  defp apply_feedback_learning(current_preferences, feedback) do
    # Update preferences based on user feedback
    case feedback.action do
      :dismissed ->
        decrease_channel_preference(current_preferences, feedback.channel)
      
      :engaged ->
        increase_channel_preference(current_preferences, feedback.channel)
      
      :complained ->
        blacklist_notification_type(current_preferences, feedback.notification_type)
      
      :requested_more ->
        increase_verbosity_preference(current_preferences)
    end
  end
  
  # Utility functions
  defp determine_urgency(notification) do
    urgency_weights = %{
      "emergency" => 1.0,
      "critical" => 0.9,
      "high" => 0.7,
      "medium" => 0.5,
      "low" => 0.2
    }
    
    Map.get(urgency_weights, notification.priority, 0.5)
  end
  
  defp check_recipient_availability(_context) do
    # Check if recipients are likely to be available
    # Based on time, calendar, status, etc.
    0.7  # Simplified
  end
  
  defp classify_content_type(notification) do
    cond do
      String.contains?(notification.content || "", "error") -> :error_alert
      String.contains?(notification.content || "", "milestone") -> :achievement
      String.contains?(notification.content || "", "deploy") -> :deployment
      true -> :general
    end
  end
  
  defp analyze_historical_engagement(_notification, _state) do
    # Analyze how well this type of notification has performed
    0.6  # Simplified
  end
  
  defp assess_current_system_load do
    # Check current system load to decide on batching
    0.5  # Simplified
  end
  
  defp determine_recipients(notification, routing_rules) do
    # Determine who should receive this notification
    case notification.recipient_criteria do
      :all_engineers ->
        get_engineers()
      
      :on_call ->
        get_on_call_engineers()
      
      :specific ->
        notification.specific_recipients || []
      
      _ ->
        get_default_recipients()
    end
  end
  
  # Placeholder implementations
  defp filter_by_recipient_availability(channels, _recipient), do: channels
  defp get_highest_engagement_channels(_preferences), do: ["email", "slack"]
  defp adjust_for_recipient_schedule(delay, _hours, _timezone), do: delay
  defp add_technical_details(notification), do: notification
  defp focus_on_business_impact(notification), do: notification
  defp create_summary_version(notification), do: notification
  defp adjust_verbosity(notification, _verbosity), do: notification
  
  defp send_push_notification(_recipient, _content), do: {:ok, "sent"}
  defp send_email_notification(_recipient, _content), do: {:ok, "sent"}
  defp send_slack_notification(_recipient, _content), do: {:ok, "sent"}
  defp send_sms_notification(_recipient, _content), do: {:ok, "sent"}
  defp send_dashboard_notification(_recipient, _content), do: {:ok, "sent"}
  
  defp calculate_success_rate(_results), do: 0.85
  defp calculate_engagement_score(_results), do: 0.75
  
  defp decrease_channel_preference(prefs, _channel), do: prefs
  defp increase_channel_preference(prefs, _channel), do: prefs
  defp blacklist_notification_type(prefs, _type), do: prefs
  defp increase_verbosity_preference(prefs), do: prefs
  
  defp get_engineers, do: []
  defp get_on_call_engineers, do: []
  defp get_default_recipients, do: []
  
  defp load_routing_rules, do: %{}
  defp load_recipient_preferences, do: %{}
  defp initialize_learning_engine, do: %{historical_data: []}
  defp update_delivery_history(history, _notification, _results), do: history
end
