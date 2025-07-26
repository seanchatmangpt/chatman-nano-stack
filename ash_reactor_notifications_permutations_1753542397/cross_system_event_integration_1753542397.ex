
defmodule CnsForge.CrossSystemEventIntegration do
  @moduledoc """
  Integration with external systems and event aggregation
  Connects pipeline events with monitoring, alerting, and business systems
  """
  
  use GenServer
  require Logger
  
  defstruct [:integrations, :event_aggregator, :delivery_queues, :retry_policies]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    state = %__MODULE__{
      integrations: setup_integrations(),
      event_aggregator: start_event_aggregator(),
      delivery_queues: %{},
      retry_policies: load_retry_policies()
    }
    
    # Subscribe to all pipeline events
    subscribe_to_pipeline_events()
    
    {:ok, state}
  end
  
  # Send event to external systems
  def send_to_external_systems(event, systems \\ :all) do
    GenServer.cast(__MODULE__, {:external_event, event, systems})
  end
  
  # Register a new external system integration
  def register_integration(system_name, config) do
    GenServer.call(__MODULE__, {:register_integration, system_name, config})
  end
  
  def handle_call({:register_integration, system_name, config}, _from, state) do
    new_integrations = Map.put(state.integrations, system_name, config)
    {:reply, :ok, %{state | integrations: new_integrations}}
  end
  
  def handle_cast({:external_event, event, systems}, state) do
    # Aggregate the event
    aggregated_event = aggregate_event(event, state.event_aggregator)
    
    # Determine target systems
    target_systems = determine_target_systems(systems, state.integrations)
    
    # Queue for delivery to each system
    new_queues = queue_for_delivery(state.delivery_queues, aggregated_event, target_systems)
    
    # Process delivery queues
    process_delivery_queues(new_queues, state.integrations, state.retry_policies)
    
    {:noreply, %{state | delivery_queues: new_queues}}
  end
  
  # Handle pipeline events
  def handle_info({:pipeline_stage, stage, status, data}, state) do
    pipeline_event = %{
      type: "pipeline_stage",
      stage: stage,
      status: status,
      data: data,
      timestamp: DateTime.utc_now(),
      source: "cns_forge_pipeline"
    }
    
    send_to_external_systems(pipeline_event)
    {:noreply, state}
  end
  
  def handle_info({:reactor_step, step_name, result}, state) do
    reactor_event = %{
      type: "reactor_step",
      step: step_name,
      result: result,
      timestamp: DateTime.utc_now(),
      source: "ash_reactor"
    }
    
    send_to_external_systems(reactor_event)
    {:noreply, state}
  end
  
  def handle_info({:system_alert, alert_type, details}, state) do
    alert_event = %{
      type: "system_alert",
      alert_type: alert_type,
      details: details,
      timestamp: DateTime.utc_now(),
      source: "monitoring_system"
    }
    
    # Send alerts to high-priority systems immediately
    send_to_external_systems(alert_event, [:slack, :pagerduty, :email])
    {:noreply, state}
  end
  
  defp setup_integrations do
    %{
      slack: %{
        type: :chat,
        webhook_url: Application.get_env(:cns_forge, :slack_webhook),
        channels: %{
          general: "#engineering",
          alerts: "#alerts",
          deployments: "#deployments"
        },
        enabled: true
      },
      
      email: %{
        type: :email,
        smtp_config: Application.get_env(:cns_forge, :smtp_config),
        templates: %{
          pipeline_success: "pipeline_success_template",
          pipeline_failure: "pipeline_failure_template",
          milestone: "milestone_template"
        },
        enabled: true
      },
      
      pagerduty: %{
        type: :alerting,
        api_key: Application.get_env(:cns_forge, :pagerduty_api_key),
        service_id: Application.get_env(:cns_forge, :pagerduty_service_id),
        escalation_policy: "P1-Critical",
        enabled: true
      },
      
      jira: %{
        type: :ticketing,
        base_url: Application.get_env(:cns_forge, :jira_base_url),
        credentials: Application.get_env(:cns_forge, :jira_credentials),
        project_key: "INFRA",
        enabled: false  # Disabled by default
      },
      
      grafana: %{
        type: :monitoring,
        api_url: Application.get_env(:cns_forge, :grafana_api_url),
        api_key: Application.get_env(:cns_forge, :grafana_api_key),
        dashboards: %{
          pipeline: "pipeline-overview",
          performance: "system-performance"
        },
        enabled: true
      },
      
      datadog: %{
        type: :observability,
        api_key: Application.get_env(:cns_forge, :datadog_api_key),
        app_key: Application.get_env(:cns_forge, :datadog_app_key),
        tags: ["env:production", "service:cns-forge"],
        enabled: true
      }
    }
  end
  
  defp subscribe_to_pipeline_events do
    events = [
      "pipeline:events",
      "reactor:steps", 
      "system:alerts",
      "telemetry:aggregated",
      "celebrations:all"
    ]
    
    Enum.each(events, fn event ->
      Phoenix.PubSub.subscribe(CnsForge.PubSub, event)
    end)
  end
  
  defp start_event_aggregator do
    # Simple event aggregation logic
    %{
      buffer: [],
      last_flush: DateTime.utc_now(),
      aggregation_rules: load_aggregation_rules()
    }
  end
  
  defp aggregate_event(event, aggregator) do
    # Add enrichment and context
    enriched_event = enrich_event(event)
    
    # Apply aggregation rules
    apply_aggregation_rules(enriched_event, aggregator.aggregation_rules)
  end
  
  defp enrich_event(event) do
    # Add common enrichment
    Map.merge(event, %{
      environment: Application.get_env(:cns_forge, :environment, "production"),
      version: Application.spec(:cns_forge, :vsn),
      hostname: System.get_env("HOSTNAME", "unknown"),
      correlation_id: generate_correlation_id()
    })
  end
  
  defp determine_target_systems(:all, integrations) do
    integrations
    |> Enum.filter(fn {_name, config} -> config.enabled end)
    |> Enum.map(fn {name, _config} -> name end)
  end
  
  defp determine_target_systems(specific_systems, integrations) when is_list(specific_systems) do
    specific_systems
    |> Enum.filter(fn system -> 
      config = Map.get(integrations, system)
      config && config.enabled
    end)
  end
  
  defp queue_for_delivery(current_queues, event, target_systems) do
    Enum.reduce(target_systems, current_queues, fn system, queues ->
      system_queue = Map.get(queues, system, [])
      new_system_queue = [event | system_queue]
      Map.put(queues, system, new_system_queue)
    end)
  end
  
  defp process_delivery_queues(queues, integrations, retry_policies) do
    # Process each system's queue
    Enum.each(queues, fn {system, events} ->
      if length(events) > 0 do
        Task.start(fn ->
          process_system_queue(system, events, integrations[system], retry_policies)
        end)
      end
    end)
  end
  
  defp process_system_queue(system, events, integration_config, retry_policies) do
    case system do
      :slack ->
        process_slack_events(events, integration_config)
      
      :email ->
        process_email_events(events, integration_config)
      
      :pagerduty ->
        process_pagerduty_events(events, integration_config)
      
      :jira ->
        process_jira_events(events, integration_config)
      
      :grafana ->
        process_grafana_events(events, integration_config)
      
      :datadog ->
        process_datadog_events(events, integration_config)
      
      _ ->
        Logger.warn("Unknown integration system: #{system}")
    end
  end
  
  defp process_slack_events(events, config) do
    # Group events for better Slack formatting
    grouped_events = group_events_by_type(events)
    
    Enum.each(grouped_events, fn {event_type, event_list} ->
      slack_message = format_slack_message(event_type, event_list)
      channel = determine_slack_channel(event_type, config.channels)
      
      send_slack_message(slack_message, channel, config)
    end)
  end
  
  defp process_email_events(events, config) do
    # Aggregate events into digest emails
    digest = create_email_digest(events)
    recipients = determine_email_recipients(events)
    
    send_email_digest(digest, recipients, config)
  end
  
  defp process_pagerduty_events(events, config) do
    # Only send critical events to PagerDuty
    critical_events = Enum.filter(events, &is_critical_event?/1)
    
    Enum.each(critical_events, fn event ->
      pagerduty_incident = format_pagerduty_incident(event)
      create_pagerduty_incident(pagerduty_incident, config)
    end)
  end
  
  defp process_jira_events(events, config) do
    # Create Jira tickets for certain event types
    ticket_worthy_events = Enum.filter(events, &should_create_ticket?/1)
    
    Enum.each(ticket_worthy_events, fn event ->
      jira_ticket = format_jira_ticket(event)
      create_jira_ticket(jira_ticket, config)
    end)
  end
  
  defp process_grafana_events(events, config) do
    # Send annotations to Grafana
    Enum.each(events, fn event ->
      if should_annotate_grafana?(event) do
        annotation = format_grafana_annotation(event)
        send_grafana_annotation(annotation, config)
      end
    end)
  end
  
  defp process_datadog_events(events, config) do
    # Send events to Datadog
    Enum.each(events, fn event ->
      datadog_event = format_datadog_event(event)
      send_datadog_event(datadog_event, config)
    end)
  end
  
  # Formatting functions
  defp format_slack_message(event_type, events) do
    case event_type do
      "pipeline_stage" ->
        create_pipeline_slack_message(events)
      
      "system_alert" ->
        create_alert_slack_message(events)
      
      "milestone" ->
        create_milestone_slack_message(events)
      
      _ ->
        create_generic_slack_message(events)
    end
  end
  
  defp create_pipeline_slack_message(events) do
    successful = Enum.count(events, fn e -> e.status == "completed" end)
    failed = Enum.count(events, fn e -> e.status == "failed" end)
    
    status_emoji = if failed > 0, do: "⚠️", else: "✅"
    
    %{
      text: "#{status_emoji} Pipeline Update",
      attachments: [
        %{
          color: if(failed > 0, do: "warning", else: "good"),
          fields: [
            %{title: "Successful", value: successful, short: true},
            %{title: "Failed", value: failed, short: true}
          ]
        }
      ]
    }
  end
  
  defp create_alert_slack_message(events) do
    critical_count = Enum.count(events, fn e -> e.alert_type == "critical" end)
    
    %{
      text: "🚨 System Alerts",
      attachments: [
        %{
          color: "danger",
          fields: [
            %{title: "Critical Alerts", value: critical_count, short: true},
            %{title: "Total Alerts", value: length(events), short: true}
          ]
        }
      ]
    }
  end
  
  defp create_milestone_slack_message(events) do
    milestone_names = Enum.map(events, fn e -> e.milestone_name end)
    
    %{
      text: "🎉 Milestones Achieved!",
      attachments: [
        %{
          color: "good",
          text: Enum.join(milestone_names, "
")
        }
      ]
    }
  end
  
  defp create_generic_slack_message(events) do
    %{
      text: "📊 System Events (#{length(events)})",
      attachments: [
        %{
          text: "Multiple system events occurred. Check dashboard for details."
        }
      ]
    }
  end
  
  # Delivery functions (simplified implementations)
  defp send_slack_message(message, channel, config) do
    # Implementation for Slack webhook
    Logger.info("Slack message to #{channel}: #{inspect(message)}")
  end
  
  defp send_email_digest(digest, recipients, config) do
    # Implementation for email sending
    Logger.info("Email digest to #{inspect(recipients)}: #{digest.subject}")
  end
  
  defp create_pagerduty_incident(incident, config) do
    # Implementation for PagerDuty API
    Logger.info("PagerDuty incident: #{incident.summary}")
  end
  
  defp create_jira_ticket(ticket, config) do
    # Implementation for Jira API
    Logger.info("Jira ticket: #{ticket.summary}")
  end
  
  defp send_grafana_annotation(annotation, config) do
    # Implementation for Grafana API
    Logger.info("Grafana annotation: #{annotation.text}")
  end
  
  defp send_datadog_event(event, config) do
    # Implementation for Datadog API
    Logger.info("Datadog event: #{event.title}")
  end
  
  # Helper functions
  defp group_events_by_type(events) do
    Enum.group_by(events, fn event -> event.type end)
  end
  
  defp determine_slack_channel(event_type, channels) do
    case event_type do
      "system_alert" -> channels.alerts
      "pipeline_stage" -> channels.deployments
      _ -> channels.general
    end
  end
  
  defp create_email_digest(events) do
    %{
      subject: "CNS Forge System Digest - #{length(events)} events",
      body: "System events summary...",
      events: events
    }
  end
  
  defp determine_email_recipients(_events) do
    ["engineering@company.com"]
  end
  
  defp is_critical_event?(event) do
    event.type == "system_alert" and event.alert_type == "critical"
  end
  
  defp should_create_ticket?(event) do
    event.type == "system_alert" and event.alert_type in ["critical", "high"]
  end
  
  defp should_annotate_grafana?(event) do
    event.type in ["pipeline_stage", "deployment", "milestone"]
  end
  
  defp format_pagerduty_incident(event) do
    %{
      summary: "CNS Forge Alert: #{event.alert_type}",
      details: event.details,
      severity: "critical"
    }
  end
  
  defp format_jira_ticket(event) do
    %{
      summary: "System Alert: #{event.alert_type}",
      description: "Alert details: #{inspect(event.details)}",
      priority: "High"
    }
  end
  
  defp format_grafana_annotation(event) do
    %{
      text: "#{event.type}: #{event.stage || event.step}",
      tags: ["cns-forge", event.type]
    }
  end
  
  defp format_datadog_event(event) do
    %{
      title: "CNS Forge: #{event.type}",
      text: "Event: #{inspect(event)}",
      tags: ["service:cns-forge", "env:production"]
    }
  end
  
  defp generate_correlation_id do
    System.unique_integer([:positive]) |> to_string()
  end
  
  defp load_aggregation_rules, do: %{}
  defp load_retry_policies, do: %{}
  defp apply_aggregation_rules(event, _rules), do: event
end
