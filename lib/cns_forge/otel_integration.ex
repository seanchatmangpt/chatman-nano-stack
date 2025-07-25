defmodule CNSForge.OtelIntegration do
  @moduledoc """
  Comprehensive OpenTelemetry integration for CNS Forge
  Provides distributed tracing, metrics, and logging
  """
  
  require Logger
  
  @tracer :cns_forge_tracer
  
  def setup do
    # Configure OpenTelemetry
    :opentelemetry.register_tracer(@tracer, "1.0.0")
    
    # Set up semantic conventions
    resource = %{
      "service.name" => "cns-forge",
      "service.version" => Application.spec(:cns_forge, :vsn) |> to_string(),
      "service.namespace" => "production",
      "deployment.environment" => Application.get_env(:cns_forge, :environment, "development"),
      "telemetry.sdk.name" => "opentelemetry-erlang",
      "telemetry.sdk.language" => "erlang",
      "telemetry.sdk.version" => Application.spec(:opentelemetry, :vsn) |> to_string()
    }
    
    :opentelemetry.set_resource(resource)
    
    # Attach telemetry handlers
    attach_telemetry_handlers()
    
    Logger.info("OpenTelemetry integration initialized", 
      service: "cns-forge",
      otel_enabled: true
    )
  end
  
  @doc """
  Start a new trace span
  """
  def start_span(name, attributes \\ %{}) do
    ctx = :otel_ctx.get_current()
    
    span_attrs = Map.merge(%{
      "component" => "cns-forge",
      "span.kind" => "internal"
    }, attributes)
    
    span = :otel_tracer.start_span(
      ctx,
      @tracer,
      name,
      %{attributes: span_attrs}
    )
    
    ctx = :otel_ctx.set_current(:otel_tracer.set_current_span(ctx, span))
    {span, ctx}
  end
  
  @doc """
  End span with status
  """
  def end_span(span, status \\ :ok, attributes \\ %{}) do
    # Add any final attributes
    Enum.each(attributes, fn {k, v} ->
      :otel_span.set_attribute(span, k, v)
    end)
    
    # Set status
    case status do
      :ok ->
        :otel_span.set_status(span, :ok)
      {:error, reason} ->
        :otel_span.set_status(span, :error, inspect(reason))
        :otel_span.record_exception(span, reason)
    end
    
    :otel_span.end_span(span)
  end
  
  @doc """
  Wrap function execution in a span
  """
  def with_span(name, attributes \\ %{}, fun) do
    {span, ctx} = start_span(name, attributes)
    
    try do
      :otel_ctx.attach(ctx)
      result = fun.()
      end_span(span, :ok)
      result
    rescue
      error ->
        end_span(span, {:error, error})
        reraise error, __STACKTRACE__
    after
      :otel_ctx.detach(ctx)
    end
  end
  
  @doc """
  Record a metric
  """
  def record_metric(name, value, attributes \\ %{}) do
    labels = attributes_to_labels(attributes)
    
    case Application.get_env(:cns_forge, :metrics_backend, :prometheus) do
      :prometheus ->
        :prometheus_histogram.observe(name, labels, value)
      :otlp ->
        :otel_meter.record(name, value, attributes)
      _ ->
        :ok
    end
  end
  
  @doc """
  Increment a counter
  """
  def increment_counter(name, value \\ 1, attributes \\ %{}) do
    labels = attributes_to_labels(attributes)
    
    case Application.get_env(:cns_forge, :metrics_backend, :prometheus) do
      :prometheus ->
        :prometheus_counter.inc(name, labels, value)
      :otlp ->
        :otel_meter.add(name, value, attributes)
      _ ->
        :ok
    end
  end
  
  @doc """
  Set a gauge value
  """
  def set_gauge(name, value, attributes \\ %{}) do
    labels = attributes_to_labels(attributes)
    
    case Application.get_env(:cns_forge, :metrics_backend, :prometheus) do
      :prometheus ->
        :prometheus_gauge.set(name, labels, value)
      :otlp ->
        :otel_meter.record(name, value, attributes)
      _ ->
        :ok
    end
  end
  
  # Telemetry handlers
  
  defp attach_telemetry_handlers do
    handlers = [
      # BitActor events
      {[:cns_forge, :bit_actor, :hop], &handle_bitactor_hop/4},
      {[:cns_forge, :bit_actor, :created], &handle_bitactor_created/4},
      {[:cns_forge, :bit_actor, :completed], &handle_bitactor_completed/4},
      
      # Reactor workflow events
      {[:cns_forge, :reactor, :workflow, :started], &handle_workflow_started/4},
      {[:cns_forge, :reactor, :workflow, :completed], &handle_workflow_completed/4},
      {[:cns_forge, :reactor, :step, :executed], &handle_step_executed/4},
      
      # Semantic compilation events
      {[:cns_forge, :semantic, :compilation, :started], &handle_compilation_started/4},
      {[:cns_forge, :semantic, :compilation, :completed], &handle_compilation_completed/4},
      
      # System events
      {[:cns_forge, :system], &handle_system_metrics/4},
      {[:cns_forge, :mesh], &handle_mesh_metrics/4},
      {[:cns_forge, :performance], &handle_performance_metrics/4}
    ]
    
    Enum.each(handlers, fn {event, handler} ->
      :telemetry.attach(
        "otel-#{inspect(event)}",
        event,
        handler,
        nil
      )
    end)
  end
  
  # BitActor handlers
  
  defp handle_bitactor_hop(_event, measurements, metadata, _config) do
    span_name = "bitactor.hop"
    
    with_span span_name, %{
      "bitactor.id" => metadata.bit_actor_id,
      "bitactor.operation" => to_string(metadata.operation),
      "bitactor.transaction_id" => metadata.transaction_id,
      "bitactor.ttl_remaining" => measurements.ttl_remaining
    } do
      # Record metrics
      increment_counter("cns_forge_bitactor_hops_total", 1, %{
        operation: metadata.operation
      })
      
      record_metric("cns_forge_bitactor_ttl_remaining", measurements.ttl_remaining, %{
        operation: metadata.operation
      })
    end
  end
  
  defp handle_bitactor_created(_event, _measurements, metadata, _config) do
    increment_counter("cns_forge_bitactor_created_total", 1, %{
      type: metadata.type
    })
  end
  
  defp handle_bitactor_completed(_event, _measurements, metadata, _config) do
    increment_counter("cns_forge_bitactor_completed_total", 1, %{
      status: metadata.status
    })
  end
  
  # Reactor workflow handlers
  
  defp handle_workflow_started(_event, _measurements, metadata, _config) do
    span_name = "workflow.#{metadata.workflow_type}"
    
    {span, ctx} = start_span(span_name, %{
      "workflow.id" => metadata.workflow_id,
      "workflow.type" => metadata.workflow_type
    })
    
    # Store span context for correlation
    Process.put({:workflow_span, metadata.workflow_id}, {span, ctx})
    
    increment_counter("cns_forge_workflow_started_total", 1, %{
      workflow_type: metadata.workflow_type
    })
  end
  
  defp handle_workflow_completed(_event, measurements, metadata, _config) do
    # Retrieve and end the workflow span
    case Process.get({:workflow_span, metadata.workflow_id}) do
      {span, _ctx} ->
        end_span(span, metadata.status, %{
          "workflow.duration_ms" => measurements.duration
        })
        Process.delete({:workflow_span, metadata.workflow_id})
        
      _ ->
        :ok
    end
    
    increment_counter("cns_forge_workflow_completed_total", 1, %{
      workflow_type: metadata.workflow_type,
      status: metadata.status
    })
    
    record_metric("cns_forge_workflow_duration_milliseconds", measurements.duration, %{
      workflow_type: metadata.workflow_type
    })
  end
  
  defp handle_step_executed(_event, measurements, metadata, _config) do
    with_span "workflow.step.#{metadata.step_name}", %{
      "step.name" => metadata.step_name,
      "step.type" => metadata.step_type
    } do
      increment_counter("cns_forge_workflow_steps_total", 1, %{
        step_name: metadata.step_name,
        step_type: metadata.step_type
      })
      
      if measurements[:duration] do
        record_metric("cns_forge_step_duration_microseconds", measurements.duration, %{
          step_name: metadata.step_name
        })
      end
    end
  end
  
  # Semantic compilation handlers
  
  defp handle_compilation_started(_event, _measurements, metadata, _config) do
    span_name = "compilation.#{metadata.language}"
    
    {span, ctx} = start_span(span_name, %{
      "compilation.language" => metadata.language
    })
    
    Process.put({:compilation_span, metadata.compilation_id}, {span, ctx})
    
    increment_counter("cns_forge_compilation_started_total", 1, %{
      language: metadata.language
    })
  end
  
  defp handle_compilation_completed(_event, measurements, metadata, _config) do
    case Process.get({:compilation_span, metadata.compilation_id}) do
      {span, _ctx} ->
        end_span(span, metadata.status, %{
          "compilation.duration_ms" => measurements.duration,
          "compilation.targets" => inspect(metadata.targets)
        })
        Process.delete({:compilation_span, metadata.compilation_id})
        
      _ ->
        :ok
    end
    
    increment_counter("cns_forge_compilation_completed_total", 1, %{
      language: metadata.language,
      status: metadata.status
    })
    
    record_metric("cns_forge_compilation_duration_milliseconds", measurements.duration, %{
      language: metadata.language
    })
  end
  
  # System metrics handlers
  
  defp handle_system_metrics(_event, measurements, metadata, _config) do
    set_gauge("cns_forge_memory_usage_bytes", measurements.memory_usage)
    set_gauge("cns_forge_process_count", measurements.process_count)
    set_gauge("cns_forge_port_count", measurements.port_count)
    set_gauge("cns_forge_schedulers", measurements.schedulers)
    
    # Log if memory usage is high
    memory_mb = measurements.memory_usage / 1024 / 1024
    if memory_mb > 2048 do
      Logger.warning("High memory usage detected", 
        memory_mb: memory_mb,
        node: metadata.node
      )
    end
  end
  
  defp handle_mesh_metrics(_event, measurements, metadata, _config) do
    set_gauge("cns_forge_mesh_active_bitactors", measurements.active_bitactors, %{
      mesh_id: metadata.mesh_id
    })
    
    set_gauge("cns_forge_mesh_active_workflows", measurements.active_workflows, %{
      mesh_id: metadata.mesh_id
    })
  end
  
  defp handle_performance_metrics(_event, measurements, _metadata, _config) do
    record_metric("cns_forge_throughput_ops_per_second", measurements.throughput)
    record_metric("cns_forge_latency_microseconds", measurements.avg_latency)
  end
  
  # Helper functions
  
  defp attributes_to_labels(attributes) do
    attributes
    |> Enum.map(fn {k, v} -> {k, to_string(v)} end)
    |> Enum.into([])
  end
  
  @doc """
  Create distributed trace context for cross-service calls
  """
  def inject_trace_context(headers \\ []) do
    ctx = :otel_ctx.get_current()
    :otel_propagator_text_map.inject(ctx, headers)
  end
  
  @doc """
  Extract trace context from incoming request
  """
  def extract_trace_context(headers) do
    ctx = :otel_propagator_text_map.extract(headers)
    :otel_ctx.attach(ctx)
  end
  
  @doc """
  Log with trace correlation
  """
  def log_with_trace(level, message, metadata \\ []) do
    span = :otel_tracer.current_span(:otel_ctx.get_current())
    
    trace_metadata = if span != :undefined do
      span_ctx = :otel_span.ctx(span)
      [
        trace_id: :otel_trace_id.to_hex(:otel_span_ctx.trace_id(span_ctx)),
        span_id: :otel_span_id.to_hex(:otel_span_ctx.span_id(span_ctx))
      ]
    else
      []
    end
    
    Logger.log(level, message, trace_metadata ++ metadata)
  end
end