defmodule CnsWeb.Channels.TelemetryHandler do
  @moduledoc """
  Handles telemetry streaming and metrics aggregation with 80/20 optimization.
  Manages real-time data streams, pattern detection, and smart aggregation.
  """
  
  use ChannelHandler.Handler
  
  alias Cns.Telemetry.{Collector, Aggregator, PatternDetector, Streamer}
  
  # Apply telemetry optimization
  plug &apply_telemetry_optimization/4
  plug &enforce_streaming_limits/4 when action in [:stream_critical, :stream_aggregated]
  
  @critical_metrics ~w(cpu memory latency error_rate throughput)
  @aggregation_window 5000  # 5 seconds for 80/20 mode
  @full_aggregation_window 1000  # 1 second for full mode
  
  @doc """
  Subscribe to telemetry streams with 80/20 optimization
  """
  def subscribe(payload, _bindings, socket) do
    metrics = Map.get(payload, "metrics", @critical_metrics)
    stream_mode = Map.get(payload, "mode", "aggregated")
    
    # Apply 80/20 optimization
    subscription = if socket.assigns.optimization_mode == "80_20" do
      %{
        metrics: filter_critical_metrics(metrics),
        mode: "aggregated",
        window: @aggregation_window,
        sample_rate: 0.2,  # 20% sampling for non-critical
        compression: true
      }
    else
      %{
        metrics: metrics,
        mode: stream_mode,
        window: Map.get(payload, "window", @full_aggregation_window),
        sample_rate: Map.get(payload, "sample_rate", 1.0),
        compression: Map.get(payload, "compression", false)
      }
    end
    
    # Start telemetry stream
    case Streamer.start_stream(socket.assigns.swarm_id, self(), subscription) do
      {:ok, stream_id} ->
        socket = assign(socket, :telemetry_stream, stream_id)
        {:reply, {:ok, %{stream_id: stream_id, config: subscription}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Get current metrics with 80/20 filtering
  """
  def get_metrics(payload, _bindings, socket) do
    metric_types = Map.get(payload, "types", @critical_metrics)
    time_range = Map.get(payload, "range", "5m")
    
    # Filter metrics based on optimization mode
    filtered_types = if socket.assigns.optimization_mode == "80_20" do
      filter_critical_metrics(metric_types)
    else
      metric_types
    end
    
    metrics = Collector.get_metrics(
      socket.assigns.swarm_id,
      filtered_types,
      time_range
    )
    
    # Apply 80/20 aggregation if needed
    processed_metrics = if socket.assigns.optimization_mode == "80_20" do
      aggregate_80_20_metrics(metrics)
    else
      metrics
    end
    
    {:reply, {:ok, processed_metrics}, socket}
  end
  
  @doc """
  Detect patterns in telemetry data
  """
  def detect_patterns(payload, _bindings, socket) do
    pattern_types = Map.get(payload, "patterns", ["anomaly", "trend", "threshold"])
    data_window = Map.get(payload, "window", "10m")
    
    case PatternDetector.analyze(
      socket.assigns.swarm_id,
      pattern_types,
      data_window
    ) do
      {:ok, patterns} ->
        # Filter patterns by importance in 80/20 mode
        filtered_patterns = if socket.assigns.optimization_mode == "80_20" do
          filter_critical_patterns(patterns)
        else
          patterns
        end
        
        {:reply, {:ok, filtered_patterns}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Stream only critical telemetry data
  """
  def stream_critical(_payload, _bindings, socket) do
    # Always stream critical metrics regardless of optimization mode
    critical_stream = %{
      metrics: @critical_metrics,
      mode: "real_time",
      filters: ["threshold_breach", "error_spike", "performance_degradation"],
      priority: "high"
    }
    
    case Streamer.start_critical_stream(socket.assigns.swarm_id, self(), critical_stream) do
      {:ok, stream_id} ->
        socket = assign(socket, :critical_stream, stream_id)
        
        # Send immediate status
        push(socket, "telemetry:critical:started", %{
          stream_id: stream_id,
          metrics: @critical_metrics
        })
        
        {:noreply, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Stream aggregated telemetry for efficiency
  """
  def stream_aggregated(payload, _bindings, socket) do
    aggregation_config = if socket.assigns.optimization_mode == "80_20" do
      %{
        window: @aggregation_window,
        functions: ["avg", "max", "p95"],  # Key percentiles only
        grouping: ["stage", "type"],
        compress: true
      }
    else
      %{
        window: Map.get(payload, "window", @full_aggregation_window),
        functions: Map.get(payload, "functions", ["avg", "min", "max", "p50", "p95", "p99"]),
        grouping: Map.get(payload, "grouping", ["stage", "type", "instance"]),
        compress: Map.get(payload, "compress", false)
      }
    end
    
    case Aggregator.start_stream(socket.assigns.swarm_id, self(), aggregation_config) do
      {:ok, stream_id} ->
        socket = assign(socket, :aggregated_stream, stream_id)
        {:reply, {:ok, %{stream_id: stream_id}}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  @doc """
  Handle delegated telemetry events
  """
  def handle_in(event, payload, bindings, socket) do
    case parse_telemetry_event(event) do
      {:metric_event, metric_type} ->
        handle_metric_event(metric_type, payload, bindings, socket)
        
      {:stream_event, action} ->
        handle_stream_action(action, payload, socket)
        
      {:aggregation_event, action} ->
        handle_aggregation_action(action, payload, socket)
        
      {:pattern_event, action} ->
        handle_pattern_action(action, payload, socket)
        
      :unknown ->
        {:reply, {:error, %{reason: "Unknown telemetry event: #{event}"}}, socket}
    end
  end
  
  # Info message handlers for real-time streaming
  
  handle_info {:telemetry_data, data}, fn socket ->
    # Process incoming telemetry data
    processed_data = if socket.assigns.optimization_mode == "80_20" do
      compress_telemetry_data(data)
    else
      data
    end
    
    push(socket, "telemetry:data", processed_data)
    {:noreply, socket}
  end
  
  handle_info {:critical_alert, alert}, fn socket ->
    # Always push critical alerts immediately
    push(socket, "telemetry:critical:alert", alert)
    {:noreply, socket}
  end
  
  handle_info {:pattern_detected, pattern}, fn socket ->
    # Send pattern detection results
    if pattern.confidence >= 0.8 or socket.assigns.optimization_mode != "80_20" do
      push(socket, "telemetry:pattern", pattern)
    end
    
    {:noreply, socket}
  end
  
  handle_info {:aggregated_metrics, metrics}, fn socket ->
    # Send aggregated metrics
    push(socket, "telemetry:aggregated", metrics)
    {:noreply, socket}
  end
  
  # Private functions
  
  defp apply_telemetry_optimization(socket, payload, bindings, _opts) do
    # Add optimization metadata to payload
    optimized_payload = if socket.assigns.optimization_mode == "80_20" do
      payload
      |> Map.put("optimization", "80_20")
      |> Map.put("sample_rate", 0.2)
      |> Map.put("compress", true)
    else
      payload
    end
    
    {:cont, socket, optimized_payload, bindings}
  end
  
  defp enforce_streaming_limits(socket, payload, bindings, _opts) do
    # Apply rate limiting for telemetry streams
    key = "telemetry_stream:#{socket.assigns.swarm_id}"
    
    # Different limits for different modes
    limit = if socket.assigns.optimization_mode == "80_20" do
      {200, 60_000}  # 200 messages per minute for 80/20
    else
      {1000, 60_000}  # 1000 messages per minute for full
    end
    
    case Hammer.check_rate(key, elem(limit, 1), elem(limit, 0)) do
      {:allow, _count} ->
        {:cont, socket, payload, bindings}
        
      {:deny, _limit} ->
        {:reply, {:error, %{reason: "Telemetry rate limit exceeded"}}, socket}
    end
  end
  
  defp filter_critical_metrics(metrics) do
    if "all" in metrics do
      @critical_metrics
    else
      Enum.filter(metrics, &(&1 in @critical_metrics))
    end
  end
  
  defp aggregate_80_20_metrics(metrics) do
    # Focus on the 20% of metrics that provide 80% of insight
    metrics
    |> Enum.group_by(& &1.type)
    |> Enum.map(fn {type, type_metrics} ->
      {type, calculate_critical_aggregations(type_metrics)}
    end)
    |> Enum.into(%{})
  end
  
  defp calculate_critical_aggregations(metrics) do
    %{
      avg: calculate_average(metrics),
      max: Enum.max_by(metrics, & &1.value),
      p95: calculate_percentile(metrics, 95),
      trend: calculate_trend(metrics),
      anomalies: detect_anomalies(metrics)
    }
  end
  
  defp filter_critical_patterns(patterns) do
    patterns
    |> Enum.filter(fn pattern ->
      pattern.confidence >= 0.8 or
      pattern.type in ["threshold_breach", "error_spike", "performance_degradation"] or
      pattern.impact >= 80
    end)
    |> Enum.sort_by(& &1.confidence, :desc)
    |> Enum.take(10)  # Top 10 patterns only
  end
  
  defp compress_telemetry_data(data) do
    # Compress data for 80/20 mode
    data
    |> remove_non_critical_fields()
    |> round_decimal_values()
    |> sample_data_points()
  end
  
  defp remove_non_critical_fields(data) do
    critical_fields = ~w(timestamp type value stage instance)
    Map.take(data, critical_fields)
  end
  
  defp round_decimal_values(data) do
    Map.update(data, :value, 0, fn val ->
      if is_float(val), do: Float.round(val, 2), else: val
    end)
  end
  
  defp sample_data_points(data) when is_list(data) do
    # Keep every 5th data point for 20% sampling
    data
    |> Enum.with_index()
    |> Enum.filter(fn {_item, index} -> rem(index, 5) == 0 end)
    |> Enum.map(fn {item, _index} -> item end)
  end
  
  defp sample_data_points(data), do: data
  
  defp parse_telemetry_event(event) do
    case String.split(event, ":", parts: 2) do
      ["metric", type] ->
        {:metric_event, type}
        
      ["stream", action] ->
        {:stream_event, action}
        
      ["aggregate", action] ->
        {:aggregation_event, action}
        
      ["pattern", action] ->
        {:pattern_event, action}
        
      _ ->
        :unknown
    end
  end
  
  defp handle_metric_event(metric_type, payload, _bindings, socket) do
    case Collector.get_metric(socket.assigns.swarm_id, metric_type, payload) do
      {:ok, metric_data} ->
        # Apply 80/20 filtering
        filtered_data = if socket.assigns.optimization_mode == "80_20" do
          compress_telemetry_data(metric_data)
        else
          metric_data
        end
        
        {:reply, {:ok, filtered_data}, socket}
        
      {:error, reason} ->
        {:reply, {:error, reason}, socket}
    end
  end
  
  defp handle_stream_action("pause", _payload, socket) do
    case Streamer.pause_stream(socket.assigns[:telemetry_stream]) do
      :ok -> {:reply, :ok, socket}
      error -> {:reply, error, socket}
    end
  end
  
  defp handle_stream_action("resume", _payload, socket) do
    case Streamer.resume_stream(socket.assigns[:telemetry_stream]) do
      :ok -> {:reply, :ok, socket}
      error -> {:reply, error, socket}
    end
  end
  
  defp handle_stream_action("stop", _payload, socket) do
    case Streamer.stop_stream(socket.assigns[:telemetry_stream]) do
      :ok ->
        socket = assign(socket, :telemetry_stream, nil)
        {:reply, :ok, socket}
        
      error ->
        {:reply, error, socket}
    end
  end
  
  defp handle_aggregation_action("configure", payload, socket) do
    config = Map.fetch!(payload, "config")
    
    case Aggregator.update_config(socket.assigns[:aggregated_stream], config) do
      {:ok, new_config} -> {:reply, {:ok, new_config}, socket}
      error -> {:reply, error, socket}
    end
  end
  
  defp handle_pattern_action("analyze", payload, socket) do
    analysis_config = Map.get(payload, "config", %{})
    
    case PatternDetector.run_analysis(socket.assigns.swarm_id, analysis_config) do
      {:ok, results} -> {:reply, {:ok, results}, socket}
      error -> {:reply, error, socket}
    end
  end
  
  # Helper calculation functions
  
  defp calculate_average(metrics) do
    if Enum.empty?(metrics) do
      0
    else
      sum = Enum.reduce(metrics, 0, fn metric, acc -> acc + metric.value end)
      sum / length(metrics)
    end
  end
  
  defp calculate_percentile(metrics, percentile) do
    sorted_values = metrics
    |> Enum.map(& &1.value)
    |> Enum.sort()
    
    if Enum.empty?(sorted_values) do
      0
    else
      index = ceil(length(sorted_values) * percentile / 100) - 1
      Enum.at(sorted_values, max(0, index))
    end
  end
  
  defp calculate_trend(metrics) do
    if length(metrics) < 2 do
      "insufficient_data"
    else
      recent = Enum.take(metrics, -5)
      older = Enum.take(metrics, 5)
      
      recent_avg = calculate_average(recent)
      older_avg = calculate_average(older)
      
      cond do
        recent_avg > older_avg * 1.1 -> "increasing"
        recent_avg < older_avg * 0.9 -> "decreasing" 
        true -> "stable"
      end
    end
  end
  
  defp detect_anomalies(metrics) do
    # Simple anomaly detection - values beyond 2 standard deviations
    if length(metrics) < 3 do
      []
    else
      mean = calculate_average(metrics)
      variance = calculate_variance(metrics, mean)
      std_dev = :math.sqrt(variance)
      
      Enum.filter(metrics, fn metric ->
        abs(metric.value - mean) > 2 * std_dev
      end)
    end
  end
  
  defp calculate_variance(metrics, mean) do
    sum_squared_diff = Enum.reduce(metrics, 0, fn metric, acc ->
      diff = metric.value - mean
      acc + diff * diff
    end)
    
    sum_squared_diff / length(metrics)
  end
end