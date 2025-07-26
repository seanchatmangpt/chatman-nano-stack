defmodule CnsForge.AshSwarmTracer do
  @moduledoc """
  🔗 ASH SWARM TRACER - CORRELATION INTELLIGENCE
  ============================================
  
  ADVERSARIAL FIX: Custom Ash.Tracer that was MISSING
  - Implements the Ash.Tracer behaviour
  - Adds correlation IDs to ALL Ash operations
  - Connects to telemetry swarm for emergent intelligence
  - 20% code enables 80% observability
  
  THIS COMPLETES THE HYPER-INTELLIGENT TELEMETRY SWARM!
  """
  
  @behaviour Ash.Tracer
  
  require Logger
  
  @trace_types [
    :custom,
    :action, 
    :changeset,
    :validation,
    :change,
    :calculation,
    :before_transaction,
    :before_action,
    :after_transaction,
    :after_action,
    :request_step,
    :query,
    :notifier,
    :preparation
  ]
  
  @impl Ash.Tracer
  def start_span(type, name, context) do
    # Generate or get correlation ID
    correlation_id = get_or_create_correlation_id(context)
    
    # Create span context
    span_context = %{
      trace_id: generate_trace_id(),
      span_id: generate_span_id(),
      correlation_id: correlation_id,
      start_time: System.monotonic_time(:nanosecond),
      type: type,
      name: name
    }
    
    # Store in process dictionary for correlation
    Process.put({:ash_span, span_context.span_id}, span_context)
    Process.put(:current_correlation_id, correlation_id)
    
    # Return span context
    span_context
  end
  
  @impl Ash.Tracer
  def stop_span(span_context, outcome, context) do
    # Calculate duration
    duration = System.monotonic_time(:nanosecond) - span_context.start_time
    
    # Determine status
    status = case outcome do
      {:ok, _} -> :success
      {:error, _} -> :error
      _ -> :unknown
    end
    
    # Emit telemetry for the span
    :telemetry.execute(
      [:ash, :trace, span_context.type],
      %{
        duration: duration,
        duration_ms: duration / 1_000_000
      },
      %{
        trace_id: span_context.trace_id,
        span_id: span_context.span_id,
        correlation_id: span_context.correlation_id,
        name: span_context.name,
        status: status,
        outcome: outcome
      }
    )
    
    # Clean up process dictionary
    Process.delete({:ash_span, span_context.span_id})
    
    :ok
  end
  
  @impl Ash.Tracer
  def get_span_context do
    # Return current correlation ID for cross-process tracing
    Process.get(:current_correlation_id)
  end
  
  @impl Ash.Tracer
  def set_span_context(correlation_id) do
    # Set correlation ID for incoming traces
    Process.put(:current_correlation_id, correlation_id)
    :ok
  end
  
  @impl Ash.Tracer
  def trace(type, name, context, fun) when type in @trace_types do
    # Start span
    span_context = start_span(type, name, context)
    
    # Execute function with tracing
    try do
      result = fun.()
      
      # Stop span with success
      stop_span(span_context, {:ok, result}, context)
      
      result
    rescue
      error ->
        # Stop span with error
        stop_span(span_context, {:error, error}, context)
        
        # Re-raise
        reraise error, __STACKTRACE__
    end
  end
  
  @impl Ash.Tracer
  def trace(type, name, context, fun) do
    # For unknown trace types, just execute
    Logger.debug("Unknown trace type: #{inspect(type)}")
    fun.()
  end
  
  # Helper functions
  
  defp get_or_create_correlation_id(context) do
    # Check various sources for correlation ID
    context[:correlation_id] ||
    Process.get(:current_correlation_id) ||
    Process.get(:otel_correlation_id) ||
    generate_correlation_id()
  end
  
  defp generate_correlation_id do
    "ash-trace-#{System.unique_integer([:positive])}-#{:rand.uniform(999999)}"
  end
  
  defp generate_trace_id do
    # 128-bit trace ID (32 hex chars)
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end
  
  defp generate_span_id do
    # 64-bit span ID (16 hex chars)
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end
end

defmodule CnsForge.AshTracerConfig do
  @moduledoc """
  Configuration module to enable the Ash Swarm Tracer
  
  Add to your config:
  ```
  config :ash, :tracer, CnsForge.AshSwarmTracer
  ```
  
  Or configure per-domain:
  ```
  defmodule YourDomain do
    use Ash.Domain,
      short_name: :your_domain,
      default_tracer: CnsForge.AshSwarmTracer
  end
  ```
  """
  
  def configure! do
    # Set globally
    Application.put_env(:ash, :tracer, CnsForge.AshSwarmTracer)
    
    # Log configuration
    Logger.info("🔗 Ash Swarm Tracer configured globally")
  end
end