defmodule CNSForge.ReactorMiddleware do
  @moduledoc """
  Reactor middleware for universal BitActor instrumentation
  
  Implements the telemetry requirements by wrapping every Reactor step
  with pulse log generation and TTL enforcement.
  """
  
  @behaviour Reactor.Middleware

  @impl Reactor.Middleware
  def init(options), do: {:ok, options}

  @impl Reactor.Middleware
  def before_step(step, arguments, context, options) do
    # Record step start
    start_time = System.monotonic_time(:microsecond)
    
    # Extract TTL from context or arguments
    ttl = extract_ttl(arguments, context)
    transaction_id = extract_transaction_id(arguments, context)
    
    # Check TTL budget before execution
    if ttl <= 0 do
      emit_ttl_expired_event(step, transaction_id)
      {:halt, {:error, :ttl_expired}}
    else
      # Store timing info for after_step
      enhanced_context = Map.put(context, :cns_forge_timing, %{
        step_name: step.name,
        start_time: start_time,
        ttl_before: ttl,
        transaction_id: transaction_id
      })
      
      {:continue, arguments, enhanced_context, options}
    end
  end

  @impl Reactor.Middleware
  def after_step(step, arguments, result, context, options) do
    case context[:cns_forge_timing] do
      nil ->
        {:continue, result, context, options}
        
      timing_info ->
        end_time = System.monotonic_time(:microsecond)
        execution_time = end_time - timing_info.start_time
        
        # Extract result TTL
        ttl_after = extract_ttl_from_result(result)
        
        # Create telemetry frame
        create_telemetry_frame(%{
          transaction_id: timing_info.transaction_id,
          step_name: step.name,
          execution_time_us: execution_time,
          ttl_before: timing_info.ttl_before,
          ttl_after: ttl_after,
          input_arguments: arguments,
          output_result: result,
          status: determine_status(result)
        })
        
        # Emit pulse log event
        :telemetry.execute(
          [:cns_forge, :bit_actor, :hop],
          %{
            execution_time_us: execution_time,
            ttl_remaining: ttl_after
          },
          %{
            step_name: step.name,
            transaction_id: timing_info.transaction_id,
            ttl_before: timing_info.ttl_before,
            ttl_after: ttl_after,
            status: determine_status(result)
          }
        )
        
        {:continue, result, context, options}
    end
  end

  @impl Reactor.Middleware
  def after_compensate(step, error, arguments, context, options) do
    case context[:cns_forge_timing] do
      nil ->
        {:continue, context, options}
        
      timing_info ->
        # Record compensation in telemetry
        create_telemetry_frame(%{
          transaction_id: timing_info.transaction_id,
          step_name: step.name,
          execution_time_us: 0,
          ttl_before: timing_info.ttl_before,
          ttl_after: timing_info.ttl_before, # TTL unchanged on compensation
          input_arguments: arguments,
          output_result: nil,
          status: :compensated,
          error_details: inspect(error)
        })
        
        :telemetry.execute(
          [:cns_forge, :bit_actor, :compensate],
          %{},
          %{
            step_name: step.name,
            transaction_id: timing_info.transaction_id,
            error: inspect(error)
          }
        )
        
        {:continue, context, options}
    end
  end

  @impl Reactor.Middleware
  def after_undo(step, reason, arguments, context, options) do
    case context[:cns_forge_timing] do
      nil ->
        {:continue, context, options}
        
      timing_info ->
        # Record undo in telemetry
        create_telemetry_frame(%{
          transaction_id: timing_info.transaction_id,
          step_name: step.name,
          execution_time_us: 0,
          ttl_before: timing_info.ttl_before,
          ttl_after: timing_info.ttl_before, # TTL unchanged on undo
          input_arguments: arguments,
          output_result: nil,
          status: :undone,
          error_details: inspect(reason)
        })
        
        :telemetry.execute(
          [:cns_forge, :bit_actor, :undo],
          %{},
          %{
            step_name: step.name,
            transaction_id: timing_info.transaction_id,
            reason: inspect(reason)
          }
        )
        
        {:continue, context, options}
    end
  end

  # Private helper functions

  defp extract_ttl(arguments, context) do
    cond do
      is_map(arguments) and Map.has_key?(arguments, :ttl) ->
        arguments.ttl
        
      is_map(context) and Map.has_key?(context, :ttl) ->
        context.ttl
        
      is_map(arguments) and Map.has_key?(arguments, :token) and 
      is_map(arguments.token) and Map.has_key?(arguments.token, :ttl) ->
        arguments.token.ttl
        
      true ->
        8 # Default TTL
    end
  end

  defp extract_transaction_id(arguments, context) do
    cond do
      is_map(arguments) and Map.has_key?(arguments, :transaction_id) ->
        arguments.transaction_id
        
      is_map(context) and Map.has_key?(context, :transaction_id) ->
        context.transaction_id
        
      is_map(arguments) and Map.has_key?(arguments, :token) and 
      is_map(arguments.token) and Map.has_key?(arguments.token, :transaction_id) ->
        arguments.token.transaction_id
        
      true ->
        "unknown"
    end
  end

  defp extract_ttl_from_result({:ok, result}) when is_map(result) do
    case result do
      %{ttl: ttl} -> ttl
      %{token: %{ttl: ttl}} -> ttl
      %{result: %{ttl: ttl}} -> ttl
      _ -> 0
    end
  end
  
  defp extract_ttl_from_result(result) when is_map(result) do
    case result do
      %{ttl: ttl} -> ttl
      %{token: %{ttl: ttl}} -> ttl
      _ -> 0
    end
  end
  
  defp extract_ttl_from_result(_), do: 0

  defp determine_status({:ok, _}), do: :success
  defp determine_status({:error, _}), do: :failure
  defp determine_status(_), do: :success

  defp emit_ttl_expired_event(step, transaction_id) do
    :telemetry.execute(
      [:cns_forge, :bit_actor, :ttl_expired],
      %{},
      %{
        step_name: step.name,
        transaction_id: transaction_id
      }
    )
  end

  defp create_telemetry_frame(params) do
    # Create telemetry frame asynchronously to avoid blocking execution
    Task.start(fn ->
      try do
        CNSForge.TelemetryFrame.capture!(params)
      rescue
        error ->
          :telemetry.execute(
            [:cns_forge, :telemetry, :frame_error],
            %{},
            %{error: inspect(error), params: params}
          )
      end
    end)
  end
end