
defmodule BitActorSemantic.Steps.SignalStep do
  @moduledoc """
  BitActor integration step for Signal
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, signal_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Signal
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Signal processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to Signal
    :ok
  end
end


defmodule BitActorSemantic.Steps.SemanticSignalStep do
  @moduledoc """
  BitActor integration step for SemanticSignal
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, semanticsignal_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SemanticSignal
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SemanticSignal processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to SemanticSignal
    :ok
  end
end


defmodule BitActorSemantic.Steps.ValidationSignalStep do
  @moduledoc """
  BitActor integration step for ValidationSignal
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, validationsignal_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ValidationSignal
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ValidationSignal processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to ValidationSignal
    :ok
  end
end


defmodule BitActorSemantic.Steps.QuerySignalStep do
  @moduledoc """
  BitActor integration step for QuerySignal
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, querysignal_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for QuerySignal
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo QuerySignal processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to QuerySignal
    :ok
  end
end


defmodule BitActorSemantic.Steps.EngineStep do
  @moduledoc """
  BitActor integration step for Engine
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, engine_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Engine
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Engine processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to Engine
    :ok
  end
end


defmodule BitActorSemantic.Steps.HandlerStep do
  @moduledoc """
  BitActor integration step for Handler
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, handler_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Handler
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Handler processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to Handler
    :ok
  end
end


defmodule BitActorSemantic.Steps.SemanticHandlerStep do
  @moduledoc """
  BitActor integration step for SemanticHandler
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, semantichandler_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SemanticHandler
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SemanticHandler processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to SemanticHandler
    :ok
  end
end


defmodule BitActorSemantic.Steps.TickBudgetStep do
  @moduledoc """
  BitActor integration step for TickBudget
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, tickbudget_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for TickBudget
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo TickBudget processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to TickBudget
    :ok
  end
end


defmodule BitActorSemantic.Steps.ExecutionResultStep do
  @moduledoc """
  BitActor integration step for ExecutionResult
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, executionresult_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ExecutionResult
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ExecutionResult processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to ExecutionResult
    :ok
  end
end


defmodule BitActorSemantic.Steps.TripleStoreStep do
  @moduledoc """
  BitActor integration step for TripleStore
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, triplestore_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for TripleStore
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo TripleStore processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to TripleStore
    :ok
  end
end


defmodule BitActorSemantic.Steps.SHACLShapeStep do
  @moduledoc """
  BitActor integration step for SHACLShape
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, shaclshape_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SHACLShape
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SHACLShape processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to SHACLShape
    :ok
  end
end


defmodule BitActorSemantic.Steps.SPARQLQueryStep do
  @moduledoc """
  BitActor integration step for SPARQLQuery
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, sparqlquery_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SPARQLQuery
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SPARQLQuery processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to SPARQLQuery
    :ok
  end
end


defmodule BitActorSemantic.Steps.OptimizedHandlerStep do
  @moduledoc """
  BitActor integration step for OptimizedHandler
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, optimizedhandler_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for OptimizedHandler
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo OptimizedHandler processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to OptimizedHandler
    :ok
  end
end


defmodule BitActorSemantic.Steps.CachedQueryStep do
  @moduledoc """
  BitActor integration step for CachedQuery
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, cachedquery_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CachedQuery
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CachedQuery processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to CachedQuery
    :ok
  end
end


defmodule BitActorSemantic.Steps.FastValidationStep do
  @moduledoc """
  BitActor integration step for FastValidation
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, fastvalidation_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for FastValidation
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo FastValidation processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to FastValidation
    :ok
  end
end


defmodule BitActorSemantic.Steps.MemoryPoolStep do
  @moduledoc """
  BitActor integration step for MemoryPool
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, memorypool_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MemoryPool
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MemoryPool processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {output, 0} -> 
        Jason.decode(output)
      {error, _} -> 
        {:error, error}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to MemoryPool
    :ok
  end
end
