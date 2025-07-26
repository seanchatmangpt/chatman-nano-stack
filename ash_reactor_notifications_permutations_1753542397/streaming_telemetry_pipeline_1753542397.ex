
defmodule CnsForge.StreamingTelemetry do
  @moduledoc """
  Streaming telemetry pipeline with real-time analytics
  Collects and streams metrics from every pipeline stage
  """
  
  use GenServer
  require Logger
  
  defstruct [:metrics_buffer, :stream_subscribers, :analytics_engine]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    # Setup telemetry handlers for all pipeline stages
    setup_telemetry_handlers()
    
    state = %__MODULE__{
      metrics_buffer: %{},
      stream_subscribers: %{},
      analytics_engine: start_analytics_engine()
    }
    
    # Start periodic metrics aggregation
    schedule_metrics_aggregation()
    
    {:ok, state}
  end
  
  # Subscribe to telemetry streams
  def subscribe_to_stream(stream_name, subscriber_pid) do
    GenServer.call(__MODULE__, {:subscribe, stream_name, subscriber_pid})
  end
  
  # Emit a telemetry event
  def emit_metric(stage, metric_name, value, metadata \\ %{}) do
    :telemetry.execute([:cns_forge, :pipeline, stage], %{metric_name => value}, metadata)
  end
  
  def handle_call({:subscribe, stream_name, subscriber_pid}, _from, state) do
    current_subscribers = Map.get(state.stream_subscribers, stream_name, [])
    new_subscribers = [subscriber_pid | current_subscribers] |> Enum.uniq()
    
    new_stream_subscribers = Map.put(state.stream_subscribers, stream_name, new_subscribers)
    
    {:reply, :ok, %{state | stream_subscribers: new_stream_subscribers}}
  end
  
  # Handle telemetry events
  def handle_info({:telemetry_event, event_name, measurements, metadata}, state) do
    # Process and buffer the metric
    processed_metric = process_telemetry_event(event_name, measurements, metadata)
    new_buffer = update_metrics_buffer(state.metrics_buffer, processed_metric)
    
    # Stream to real-time subscribers
    stream_to_subscribers(processed_metric, state.stream_subscribers)
    
    # Update analytics
    update_analytics(state.analytics_engine, processed_metric)
    
    {:noreply, %{state | metrics_buffer: new_buffer}}
  end
  
  def handle_info(:aggregate_metrics, state) do
    # Aggregate buffered metrics
    aggregated = aggregate_metrics(state.metrics_buffer)
    
    # Send aggregated metrics to dashboard
    send_aggregated_metrics(aggregated)
    
    # Clear buffer
    schedule_metrics_aggregation()
    {:noreply, %{state | metrics_buffer: %{}}}
  end
  
  defp setup_telemetry_handlers do
    # Attach handlers for each pipeline stage
    stages = [
      :typer_processing,
      :turtle_generation, 
      :ttl2dspy_transformation,
      :bitactor_processing,
      :erlang_coordination,
      :ash_resources,
      :reactor_execution,
      :k8s_deployment
    ]
    
    Enum.each(stages, fn stage ->
      :telemetry.attach(
        "cns-forge-#{stage}",
        [:cns_forge, :pipeline, stage],
        &handle_telemetry_event/4,
        %{stage: stage}
      )
    end)
  end
  
  defp handle_telemetry_event(event_name, measurements, metadata, config) do
    send(__MODULE__, {:telemetry_event, event_name, measurements, metadata})
  end
  
  defp process_telemetry_event(event_name, measurements, metadata) do
    %{
      event: event_name,
      measurements: measurements,
      metadata: metadata,
      timestamp: DateTime.utc_now(),
      processed_at: System.monotonic_time(:microsecond)
    }
  end
  
  defp update_metrics_buffer(buffer, metric) do
    stage = extract_stage_from_event(metric.event)
    stage_metrics = Map.get(buffer, stage, [])
    
    # Keep last 100 metrics per stage
    new_stage_metrics = [metric | stage_metrics] |> Enum.take(100)
    
    Map.put(buffer, stage, new_stage_metrics)
  end
  
  defp stream_to_subscribers(metric, subscribers) do
    stream_name = determine_stream_name(metric)
    subscriber_list = Map.get(subscribers, stream_name, [])
    
    # Send to all subscribers
    Enum.each(subscriber_list, fn subscriber_pid ->
      send(subscriber_pid, {:telemetry_stream, stream_name, metric})
    end)
    
    # Also broadcast via Phoenix PubSub
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "telemetry:#{stream_name}", metric)
  end
  
  defp determine_stream_name(metric) do
    stage = extract_stage_from_event(metric.event)
    
    cond do
      has_performance_metrics?(metric) -> "performance"
      has_error_metrics?(metric) -> "errors"
      has_business_metrics?(metric) -> "business"
      true -> "general"
    end
  end
  
  defp aggregate_metrics(buffer) do
    Enum.map(buffer, fn {stage, metrics} ->
      {stage, %{
        count: length(metrics),
        avg_duration: calculate_avg_duration(metrics),
        error_rate: calculate_error_rate(metrics),
        throughput: calculate_throughput(metrics),
        p95_latency: calculate_p95_latency(metrics)
      }}
    end)
    |> Enum.into(%{})
  end
  
  defp send_aggregated_metrics(aggregated) do
    # Send to dashboard via WebSocket
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "dashboard:metrics", %{
      type: "aggregated_metrics",
      data: aggregated,
      timestamp: DateTime.utc_now()
    })
    
    # Store in time-series database (if configured)
    store_in_timeseries(aggregated)
  end
  
  defp start_analytics_engine do
    # Simple analytics engine for real-time processing
    %{
      running_averages: %{},
      trend_analysis: %{},
      anomaly_detection: %{}
    }
  end
  
  defp update_analytics(engine, metric) do
    # Update running averages
    # Perform trend analysis
    # Check for anomalies
    # (Implementation details...)
    engine
  end
  
  defp schedule_metrics_aggregation do
    Process.send_after(self(), :aggregate_metrics, 5000)  # Every 5 seconds
  end
  
  # Helper functions
  defp extract_stage_from_event(event_name) do
    event_name
    |> List.last()
    |> to_string()
  end
  
  defp has_performance_metrics?(metric) do
    Map.has_key?(metric.measurements, :duration) or
    Map.has_key?(metric.measurements, :memory_usage)
  end
  
  defp has_error_metrics?(metric) do
    Map.has_key?(metric.measurements, :error_count) or
    Map.get(metric.metadata, :status) == :error
  end
  
  defp has_business_metrics?(metric) do
    Map.has_key?(metric.measurements, :items_processed) or
    Map.has_key?(metric.measurements, :revenue_impact)
  end
  
  defp calculate_avg_duration(metrics) do
    durations = Enum.map(metrics, fn m -> 
      Map.get(m.measurements, :duration, 0) 
    end)
    
    case durations do
      [] -> 0
      _ -> Enum.sum(durations) / length(durations)
    end
  end
  
  defp calculate_error_rate(metrics) do
    total = length(metrics)
    errors = Enum.count(metrics, fn m ->
      Map.get(m.metadata, :status) == :error
    end)
    
    if total > 0, do: errors / total * 100, else: 0
  end
  
  defp calculate_throughput(metrics) do
    # Metrics per second
    if length(metrics) > 0 do
      time_span = get_time_span(metrics)
      length(metrics) / time_span
    else
      0
    end
  end
  
  defp calculate_p95_latency(metrics) do
    durations = Enum.map(metrics, fn m -> 
      Map.get(m.measurements, :duration, 0) 
    end)
    |> Enum.sort()
    
    case durations do
      [] -> 0
      _ -> 
        p95_index = round(length(durations) * 0.95)
        Enum.at(durations, p95_index - 1, 0)
    end
  end
  
  defp get_time_span(metrics) do
    timestamps = Enum.map(metrics, fn m -> m.processed_at end)
    case {Enum.min(timestamps), Enum.max(timestamps)} do
      {min_time, max_time} -> 
        (max_time - min_time) / 1_000_000  # Convert to seconds
    end
  end
  
  defp store_in_timeseries(_aggregated) do
    # Store in InfluxDB, TimescaleDB, or similar
    :ok
  end
end

# WebSocket handler for real-time telemetry streaming
defmodule CnsForge.TelemetryChannel do
  @moduledoc """
  Phoenix Channel for real-time telemetry streaming
  """
  
  use Phoenix.Channel
  alias CnsForge.StreamingTelemetry
  
  def join("telemetry:" <> stream_name, _payload, socket) do
    # Subscribe to telemetry stream
    StreamingTelemetry.subscribe_to_stream(stream_name, self())
    
    socket = assign(socket, :stream_name, stream_name)
    {:ok, socket}
  end
  
  def handle_info({:telemetry_stream, stream_name, metric}, socket) do
    push(socket, "metric", %{
      stream: stream_name,
      data: metric,
      timestamp: DateTime.utc_now()
    })
    
    {:noreply, socket}
  end
  
  def terminate(_reason, socket) do
    # Cleanup subscription
    :ok
  end
end
