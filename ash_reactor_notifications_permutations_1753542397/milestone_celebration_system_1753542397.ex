
defmodule CnsForge.MilestoneCelebration do
  @moduledoc """
  Pipeline milestone celebration system
  Tracks achievements and celebrates team success
  """
  
  use GenServer
  require Logger
  
  defstruct [:milestones, :achievements, :celebration_rules, :team_notifications]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    state = %__MODULE__{
      milestones: load_milestone_definitions(),
      achievements: load_achievement_history(),
      celebration_rules: load_celebration_rules(),
      team_notifications: %{}
    }
    
    # Setup achievement tracking
    setup_achievement_tracking()
    
    {:ok, state}
  end
  
  # Track a pipeline event for milestone calculation
  def track_event(event_type, data \\ %{}) do
    GenServer.cast(__MODULE__, {:track_event, event_type, data})
  end
  
  # Check if a milestone has been reached
  def check_milestone(milestone_id) do
    GenServer.call(__MODULE__, {:check_milestone, milestone_id})
  end
  
  def handle_cast({:track_event, event_type, data}, state) do
    # Update achievement counters
    new_achievements = update_achievements(state.achievements, event_type, data)
    
    # Check for milestone completions
    completed_milestones = check_for_completed_milestones(new_achievements, state.milestones)
    
    # Celebrate any new milestones
    Enum.each(completed_milestones, fn milestone ->
      celebrate_milestone(milestone, state.celebration_rules)
    end)
    
    new_state = %{state | achievements: new_achievements}
    {:noreply, new_state}
  end
  
  def handle_call({:check_milestone, milestone_id}, _from, state) do
    milestone = Map.get(state.milestones, milestone_id)
    achievement_count = get_achievement_count(state.achievements, milestone_id)
    
    status = if milestone && achievement_count >= milestone.target do
      :completed
    else
      :in_progress
    end
    
    {:reply, {status, achievement_count, milestone}, state}
  end
  
  defp load_milestone_definitions do
    %{
      "first_successful_pipeline" => %{
        id: "first_successful_pipeline",
        name: "First Successful Pipeline",
        description: "Complete first end-to-end pipeline execution",
        target: 1,
        event_type: "pipeline_completed",
        celebration_level: :team_announcement
      },
      
      "hundred_pipelines" => %{
        id: "hundred_pipelines", 
        name: "Century Runner",
        description: "Complete 100 pipeline executions",
        target: 100,
        event_type: "pipeline_completed",
        celebration_level: :team_party
      },
      
      "thousand_ttl_files" => %{
        id: "thousand_ttl_files",
        name: "TTL Master",
        description: "Process 1000 TTL files successfully", 
        target: 1000,
        event_type: "ttl_processed",
        celebration_level: :company_announcement
      },
      
      "zero_downtime_week" => %{
        id: "zero_downtime_week",
        name: "Perfect Week",
        description: "One week with zero pipeline failures",
        target: 7,
        event_type: "perfect_day",
        celebration_level: :team_reward
      },
      
      "sub_100ms_pipeline" => %{
        id: "sub_100ms_pipeline",
        name: "Speed Demon", 
        description: "Complete pipeline in under 100ms",
        target: 1,
        event_type: "fast_pipeline",
        celebration_level: :engineering_highlight
      },
      
      "bitactor_efficiency" => %{
        id: "bitactor_efficiency",
        name: "BitActor Champion",
        description: "Achieve 99.9% BitActor execution efficiency",
        target: 1,
        event_type: "high_efficiency",
        celebration_level: :technical_achievement
      }
    }
  end
  
  defp setup_achievement_tracking do
    # Subscribe to pipeline events
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "pipeline:events")
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "reactor:steps")
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "telemetry:performance")
  end
  
  # Handle pipeline events for milestone tracking
  def handle_info({:pipeline_stage, "k8s", "completed", _data}, state) do
    # Pipeline completed successfully
    track_event("pipeline_completed")
    {:noreply, state}
  end
  
  def handle_info({:reactor_step, "parse_ttl", "success"}, state) do
    # TTL file processed successfully
    track_event("ttl_processed")
    {:noreply, state}
  end
  
  def handle_info({:telemetry_stream, "performance", metric}, state) do
    # Check for performance milestones
    if Map.get(metric.measurements, :duration, 1000) < 100 do
      track_event("fast_pipeline", %{duration: metric.measurements.duration})
    end
    {:noreply, state}
  end
  
  defp update_achievements(achievements, event_type, data) do
    current_count = Map.get(achievements, event_type, 0)
    new_count = current_count + 1
    
    updated = Map.put(achievements, event_type, new_count)
    
    # Store achievement with timestamp
    achievement_entry = %{
      count: new_count,
      last_occurrence: DateTime.utc_now(),
      data: data
    }
    
    Map.put(updated, "#{event_type}_details", achievement_entry)
  end
  
  defp check_for_completed_milestones(achievements, milestones) do
    Enum.filter(milestones, fn {_id, milestone} ->
      current_count = Map.get(achievements, milestone.event_type, 0)
      
      current_count >= milestone.target and 
      not milestone_already_celebrated?(milestone.id, achievements)
    end)
    |> Enum.map(fn {_id, milestone} -> milestone end)
  end
  
  defp celebrate_milestone(milestone, celebration_rules) do
    Logger.info("🎉 MILESTONE ACHIEVED: #{milestone.name}")
    
    celebration = create_celebration(milestone)
    
    # Send celebration notifications based on level
    case milestone.celebration_level do
      :team_announcement ->
        send_team_announcement(celebration)
      
      :team_party ->
        send_team_party_notification(celebration)
      
      :company_announcement ->
        send_company_announcement(celebration)
      
      :team_reward ->
        send_team_reward_notification(celebration)
      
      :engineering_highlight ->
        send_engineering_highlight(celebration)
      
      :technical_achievement ->
        send_technical_achievement_notification(celebration)
    end
    
    # Store celebration in history
    store_celebration(celebration)
  end
  
  defp create_celebration(milestone) do
    %{
      milestone_id: milestone.id,
      milestone_name: milestone.name,
      description: milestone.description,
      achieved_at: DateTime.utc_now(),
      celebration_level: milestone.celebration_level,
      celebration_message: generate_celebration_message(milestone)
    }
  end
  
  defp generate_celebration_message(milestone) do
    messages = %{
      "first_successful_pipeline" => "🚀 First pipeline completed! The journey begins!",
      "hundred_pipelines" => "💯 One hundred pipelines completed! Century achieved!",
      "thousand_ttl_files" => "🎯 One thousand TTL files processed! Semantic mastery!",
      "zero_downtime_week" => "⭐ Perfect week! Zero failures for 7 days straight!",
      "sub_100ms_pipeline" => "⚡ Lightning fast! Pipeline completed in under 100ms!",
      "bitactor_efficiency" => "🏆 BitActor champion! 99.9% efficiency achieved!"
    }
    
    Map.get(messages, milestone.id, "🎉 Milestone achieved: #{milestone.name}!")
  end
  
  defp send_team_announcement(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:team", celebration)
    
    # Send to team chat channels
    send_slack_notification(celebration, "#engineering")
  end
  
  defp send_team_party_notification(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:party", celebration)
    
    # Bigger celebration - multiple channels
    send_slack_notification(celebration, "#engineering")
    send_slack_notification(celebration, "#general")
    send_email_celebration(celebration, "team")
  end
  
  defp send_company_announcement(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:company", celebration)
    
    # Company-wide announcement
    send_slack_notification(celebration, "#announcements")
    send_email_celebration(celebration, "company")
    create_blog_post_draft(celebration)
  end
  
  defp send_team_reward_notification(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:reward", celebration)
    
    # Suggest team reward
    reward_suggestion = %{
      type: "team_lunch",
      message: "Perfect week achieved! Time for a team celebration lunch! 🍕",
      celebration: celebration
    }
    
    send_slack_notification(reward_suggestion, "#engineering-leads")
  end
  
  defp send_engineering_highlight(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:engineering", celebration)
    
    # Engineering blog highlight
    engineering_post = %{
      title: "Engineering Achievement: #{celebration.milestone_name}",
      content: celebration.celebration_message,
      technical_details: true
    }
    
    send_slack_notification(engineering_post, "#engineering")
  end
  
  defp send_technical_achievement_notification(celebration) do
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "celebrations:technical", celebration)
    
    # Technical achievement recognition
    recognition = %{
      achievement: celebration.milestone_name,
      impact: "Significant technical milestone achieved",
      celebration: celebration
    }
    
    send_slack_notification(recognition, "#architecture")
  end
  
  # Helper functions
  defp milestone_already_celebrated?(milestone_id, achievements) do
    # Check if we've already celebrated this milestone
    celebration_key = "celebrated_#{milestone_id}"
    Map.has_key?(achievements, celebration_key)
  end
  
  defp get_achievement_count(achievements, milestone_id) do
    milestones = load_milestone_definitions()
    milestone = Map.get(milestones, milestone_id)
    
    if milestone do
      Map.get(achievements, milestone.event_type, 0)
    else
      0
    end
  end
  
  defp load_achievement_history do
    # Load from persistent storage
    %{}
  end
  
  defp load_celebration_rules do
    # Load celebration configuration
    %{}
  end
  
  defp store_celebration(celebration) do
    # Store in database for history
    Logger.info("Storing celebration: #{celebration.milestone_name}")
  end
  
  defp send_slack_notification(data, channel) do
    # Implementation for Slack integration
    Logger.info("Slack notification to #{channel}: #{inspect(data)}")
  end
  
  defp send_email_celebration(celebration, scope) do
    # Implementation for email notifications
    Logger.info("Email celebration (#{scope}): #{celebration.milestone_name}")
  end
  
  defp create_blog_post_draft(celebration) do
    # Create draft blog post for major milestones
    Logger.info("Blog post draft created for: #{celebration.milestone_name}")
  end
end
