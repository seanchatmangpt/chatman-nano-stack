defmodule Pipeline.Workflows.MainProcess do
  use Reactor
  
  input :raw_data
  
  # Initialize data stream
  step :init_stream do
    argument :data, input(:raw_data)
    
    run fn args, _context ->
      {:ok, %{stream: args.data, status: "initialized"}}
    end
  end
  
  # Process with each type
  step :datastream do
    argument :stream, result(:init_stream)
    
    run fn args, _context ->
      # Process DataStream
      # Available attributes: id, source, format, rate
      result = %{
        type: "DataStream",
        processed: true,
        timestamp: DateTime.utc_now()
      }
      {:ok, result}
    end
  end
  step :processor do
    argument :stream, result(:init_stream)
    
    run fn args, _context ->
      # Process Processor
      # Available attributes: id, type, config, capacity
      result = %{
        type: "Processor",
        processed: true,
        timestamp: DateTime.utc_now()
      }
      {:ok, result}
    end
  end
  step :pattern do
    argument :stream, result(:init_stream)
    
    run fn args, _context ->
      # Process Pattern
      # Available attributes: id, expression, severity
      result = %{
        type: "Pattern",
        processed: true,
        timestamp: DateTime.utc_now()
      }
      {:ok, result}
    end
  end
  step :alert do
    argument :stream, result(:init_stream)
    
    run fn args, _context ->
      # Process Alert
      # Available attributes: id, message, timestamp, priority
      result = %{
        type: "Alert",
        processed: true,
        timestamp: DateTime.utc_now()
      }
      {:ok, result}
    end
  end
  
  # Final collection step
  step :collect_results do
    argument :datastream_result, result(:datastream)
    argument :processor_result, result(:processor)
    argument :pattern_result, result(:pattern)
    argument :alert_result, result(:alert)
    
    run fn args, _context ->
      results = %{
        datastream: args.datastream_result,
        processor: args.processor_result,
        pattern: args.pattern_result,
        alert: args.alert_result,
      }
      {:ok, results}
    end
  end
  
  return :collect_results
end
