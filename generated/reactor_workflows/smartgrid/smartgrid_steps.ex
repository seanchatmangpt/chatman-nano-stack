
defmodule SmartGrid.Steps.GridNodeStep do
  @moduledoc """
  BitActor integration step for GridNode
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, gridnode_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GridNode
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GridNode processing
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
    # Cleanup logic specific to GridNode
    :ok
  end
end


defmodule SmartGrid.Steps.PowerPlantStep do
  @moduledoc """
  BitActor integration step for PowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, powerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PowerPlant processing
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
    # Cleanup logic specific to PowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.RenewablePowerPlantStep do
  @moduledoc """
  BitActor integration step for RenewablePowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, renewablepowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for RenewablePowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo RenewablePowerPlant processing
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
    # Cleanup logic specific to RenewablePowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.SolarPowerPlantStep do
  @moduledoc """
  BitActor integration step for SolarPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, solarpowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SolarPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SolarPowerPlant processing
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
    # Cleanup logic specific to SolarPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.WindPowerPlantStep do
  @moduledoc """
  BitActor integration step for WindPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, windpowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for WindPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo WindPowerPlant processing
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
    # Cleanup logic specific to WindPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.HydroPowerPlantStep do
  @moduledoc """
  BitActor integration step for HydroPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, hydropowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for HydroPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo HydroPowerPlant processing
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
    # Cleanup logic specific to HydroPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.GeothermalPowerPlantStep do
  @moduledoc """
  BitActor integration step for GeothermalPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, geothermalpowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GeothermalPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GeothermalPowerPlant processing
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
    # Cleanup logic specific to GeothermalPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.ThermalPowerPlantStep do
  @moduledoc """
  BitActor integration step for ThermalPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, thermalpowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ThermalPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ThermalPowerPlant processing
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
    # Cleanup logic specific to ThermalPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.NuclearPowerPlantStep do
  @moduledoc """
  BitActor integration step for NuclearPowerPlant
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, nuclearpowerplant_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NuclearPowerPlant
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NuclearPowerPlant processing
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
    # Cleanup logic specific to NuclearPowerPlant
    :ok
  end
end


defmodule SmartGrid.Steps.SubstationStep do
  @moduledoc """
  BitActor integration step for Substation
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, substation_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Substation
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Substation processing
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
    # Cleanup logic specific to Substation
    :ok
  end
end


defmodule SmartGrid.Steps.TransmissionLineStep do
  @moduledoc """
  BitActor integration step for TransmissionLine
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, transmissionline_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for TransmissionLine
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo TransmissionLine processing
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
    # Cleanup logic specific to TransmissionLine
    :ok
  end
end


defmodule SmartGrid.Steps.DistributionLineStep do
  @moduledoc """
  BitActor integration step for DistributionLine
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, distributionline_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DistributionLine
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DistributionLine processing
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
    # Cleanup logic specific to DistributionLine
    :ok
  end
end


defmodule SmartGrid.Steps.TransformerStep do
  @moduledoc """
  BitActor integration step for Transformer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, transformer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Transformer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Transformer processing
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
    # Cleanup logic specific to Transformer
    :ok
  end
end


defmodule SmartGrid.Steps.CircuitBreakerStep do
  @moduledoc """
  BitActor integration step for CircuitBreaker
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, circuitbreaker_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CircuitBreaker
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CircuitBreaker processing
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
    # Cleanup logic specific to CircuitBreaker
    :ok
  end
end


defmodule SmartGrid.Steps.SmartMeterStep do
  @moduledoc """
  BitActor integration step for SmartMeter
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, smartmeter_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SmartMeter
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SmartMeter processing
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
    # Cleanup logic specific to SmartMeter
    :ok
  end
end


defmodule SmartGrid.Steps.EnergyStorageStep do
  @moduledoc """
  BitActor integration step for EnergyStorage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, energystorage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EnergyStorage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EnergyStorage processing
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
    # Cleanup logic specific to EnergyStorage
    :ok
  end
end


defmodule SmartGrid.Steps.BatteryStorageStep do
  @moduledoc """
  BitActor integration step for BatteryStorage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, batterystorage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for BatteryStorage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo BatteryStorage processing
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
    # Cleanup logic specific to BatteryStorage
    :ok
  end
end


defmodule SmartGrid.Steps.PumpedHydroStorageStep do
  @moduledoc """
  BitActor integration step for PumpedHydroStorage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, pumpedhydrostorage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PumpedHydroStorage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PumpedHydroStorage processing
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
    # Cleanup logic specific to PumpedHydroStorage
    :ok
  end
end


defmodule SmartGrid.Steps.CompressedAirStorageStep do
  @moduledoc """
  BitActor integration step for CompressedAirStorage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, compressedairstorage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CompressedAirStorage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CompressedAirStorage processing
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
    # Cleanup logic specific to CompressedAirStorage
    :ok
  end
end


defmodule SmartGrid.Steps.FlywheelStorageStep do
  @moduledoc """
  BitActor integration step for FlywheelStorage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, flywheelstorage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for FlywheelStorage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo FlywheelStorage processing
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
    # Cleanup logic specific to FlywheelStorage
    :ok
  end
end


defmodule SmartGrid.Steps.EnergyConsumerStep do
  @moduledoc """
  BitActor integration step for EnergyConsumer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, energyconsumer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EnergyConsumer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EnergyConsumer processing
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
    # Cleanup logic specific to EnergyConsumer
    :ok
  end
end


defmodule SmartGrid.Steps.ResidentialConsumerStep do
  @moduledoc """
  BitActor integration step for ResidentialConsumer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, residentialconsumer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ResidentialConsumer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ResidentialConsumer processing
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
    # Cleanup logic specific to ResidentialConsumer
    :ok
  end
end


defmodule SmartGrid.Steps.CommercialConsumerStep do
  @moduledoc """
  BitActor integration step for CommercialConsumer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, commercialconsumer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CommercialConsumer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CommercialConsumer processing
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
    # Cleanup logic specific to CommercialConsumer
    :ok
  end
end


defmodule SmartGrid.Steps.IndustrialConsumerStep do
  @moduledoc """
  BitActor integration step for IndustrialConsumer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, industrialconsumer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for IndustrialConsumer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo IndustrialConsumer processing
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
    # Cleanup logic specific to IndustrialConsumer
    :ok
  end
end


defmodule SmartGrid.Steps.ElectricVehicleStep do
  @moduledoc """
  BitActor integration step for ElectricVehicle
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, electricvehicle_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ElectricVehicle
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ElectricVehicle processing
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
    # Cleanup logic specific to ElectricVehicle
    :ok
  end
end


defmodule SmartGrid.Steps.GridControllerStep do
  @moduledoc """
  BitActor integration step for GridController
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, gridcontroller_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GridController
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GridController processing
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
    # Cleanup logic specific to GridController
    :ok
  end
end


defmodule SmartGrid.Steps.SCADAStep do
  @moduledoc """
  BitActor integration step for SCADA
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, scada_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SCADA
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SCADA processing
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
    # Cleanup logic specific to SCADA
    :ok
  end
end


defmodule SmartGrid.Steps.EnergyManagementSystemStep do
  @moduledoc """
  BitActor integration step for EnergyManagementSystem
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, energymanagementsystem_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EnergyManagementSystem
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EnergyManagementSystem processing
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
    # Cleanup logic specific to EnergyManagementSystem
    :ok
  end
end


defmodule SmartGrid.Steps.DemandResponseProgramStep do
  @moduledoc """
  BitActor integration step for DemandResponseProgram
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, demandresponseprogram_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DemandResponseProgram
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DemandResponseProgram processing
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
    # Cleanup logic specific to DemandResponseProgram
    :ok
  end
end


defmodule SmartGrid.Steps.LoadBalancerStep do
  @moduledoc """
  BitActor integration step for LoadBalancer
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, loadbalancer_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for LoadBalancer
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo LoadBalancer processing
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
    # Cleanup logic specific to LoadBalancer
    :ok
  end
end


defmodule SmartGrid.Steps.ElectricalMeasurementStep do
  @moduledoc """
  BitActor integration step for ElectricalMeasurement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, electricalmeasurement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ElectricalMeasurement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ElectricalMeasurement processing
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
    # Cleanup logic specific to ElectricalMeasurement
    :ok
  end
end


defmodule SmartGrid.Steps.VoltageMeasurementStep do
  @moduledoc """
  BitActor integration step for VoltageMeasurement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, voltagemeasurement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VoltageMeasurement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VoltageMeasurement processing
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
    # Cleanup logic specific to VoltageMeasurement
    :ok
  end
end


defmodule SmartGrid.Steps.CurrentMeasurementStep do
  @moduledoc """
  BitActor integration step for CurrentMeasurement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, currentmeasurement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CurrentMeasurement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CurrentMeasurement processing
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
    # Cleanup logic specific to CurrentMeasurement
    :ok
  end
end


defmodule SmartGrid.Steps.PowerMeasurementStep do
  @moduledoc """
  BitActor integration step for PowerMeasurement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, powermeasurement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PowerMeasurement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PowerMeasurement processing
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
    # Cleanup logic specific to PowerMeasurement
    :ok
  end
end


defmodule SmartGrid.Steps.FrequencyMeasurementStep do
  @moduledoc """
  BitActor integration step for FrequencyMeasurement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, frequencymeasurement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for FrequencyMeasurement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo FrequencyMeasurement processing
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
    # Cleanup logic specific to FrequencyMeasurement
    :ok
  end
end


defmodule SmartGrid.Steps.EnergyReadingStep do
  @moduledoc """
  BitActor integration step for EnergyReading
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, energyreading_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EnergyReading
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EnergyReading processing
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
    # Cleanup logic specific to EnergyReading
    :ok
  end
end


defmodule SmartGrid.Steps.GridEventStep do
  @moduledoc """
  BitActor integration step for GridEvent
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, gridevent_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GridEvent
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GridEvent processing
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
    # Cleanup logic specific to GridEvent
    :ok
  end
end


defmodule SmartGrid.Steps.OutageStep do
  @moduledoc """
  BitActor integration step for Outage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, outage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Outage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Outage processing
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
    # Cleanup logic specific to Outage
    :ok
  end
end


defmodule SmartGrid.Steps.VoltageAnomalySagStep do
  @moduledoc """
  BitActor integration step for VoltageAnomalySag
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, voltageanomalysag_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VoltageAnomalySag
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VoltageAnomalySag processing
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
    # Cleanup logic specific to VoltageAnomalySag
    :ok
  end
end


defmodule SmartGrid.Steps.VoltageAnomalySwellStep do
  @moduledoc """
  BitActor integration step for VoltageAnomalySwell
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, voltageanomalyswell_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VoltageAnomalySwell
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VoltageAnomalySwell processing
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
    # Cleanup logic specific to VoltageAnomalySwell
    :ok
  end
end


defmodule SmartGrid.Steps.FrequencyDeviationStep do
  @moduledoc """
  BitActor integration step for FrequencyDeviation
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, frequencydeviation_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for FrequencyDeviation
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo FrequencyDeviation processing
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
    # Cleanup logic specific to FrequencyDeviation
    :ok
  end
end


defmodule SmartGrid.Steps.OverloadStep do
  @moduledoc """
  BitActor integration step for Overload
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, overload_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Overload
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Overload processing
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
    # Cleanup logic specific to Overload
    :ok
  end
end


defmodule SmartGrid.Steps.GridProtectionTripStep do
  @moduledoc """
  BitActor integration step for GridProtectionTrip
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, gridprotectiontrip_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GridProtectionTrip
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GridProtectionTrip processing
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
    # Cleanup logic specific to GridProtectionTrip
    :ok
  end
end
