
defmodule HealthcareCore.Steps.PatientStep do
  @moduledoc """
  BitActor integration step for Patient
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, patient_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Patient
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Patient processing
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
    # Cleanup logic specific to Patient
    :ok
  end
end


defmodule HealthcareCore.Steps.HealthcareProviderStep do
  @moduledoc """
  BitActor integration step for HealthcareProvider
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, healthcareprovider_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for HealthcareProvider
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo HealthcareProvider processing
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
    # Cleanup logic specific to HealthcareProvider
    :ok
  end
end


defmodule HealthcareCore.Steps.PhysicianStep do
  @moduledoc """
  BitActor integration step for Physician
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, physician_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Physician
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Physician processing
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
    # Cleanup logic specific to Physician
    :ok
  end
end


defmodule HealthcareCore.Steps.NurseStep do
  @moduledoc """
  BitActor integration step for Nurse
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, nurse_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Nurse
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Nurse processing
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
    # Cleanup logic specific to Nurse
    :ok
  end
end


defmodule HealthcareCore.Steps.SpecialistStep do
  @moduledoc """
  BitActor integration step for Specialist
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, specialist_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Specialist
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Specialist processing
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
    # Cleanup logic specific to Specialist
    :ok
  end
end


defmodule HealthcareCore.Steps.TechnicianStep do
  @moduledoc """
  BitActor integration step for Technician
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, technician_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Technician
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Technician processing
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
    # Cleanup logic specific to Technician
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicalDeviceStep do
  @moduledoc """
  BitActor integration step for MedicalDevice
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicaldevice_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicalDevice
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicalDevice processing
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
    # Cleanup logic specific to MedicalDevice
    :ok
  end
end


defmodule HealthcareCore.Steps.MonitoringDeviceStep do
  @moduledoc """
  BitActor integration step for MonitoringDevice
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, monitoringdevice_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MonitoringDevice
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MonitoringDevice processing
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
    # Cleanup logic specific to MonitoringDevice
    :ok
  end
end


defmodule HealthcareCore.Steps.ECGMonitorStep do
  @moduledoc """
  BitActor integration step for ECGMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ecgmonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ECGMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ECGMonitor processing
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
    # Cleanup logic specific to ECGMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.VitalSignsMonitorStep do
  @moduledoc """
  BitActor integration step for VitalSignsMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, vitalsignsmonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VitalSignsMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VitalSignsMonitor processing
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
    # Cleanup logic specific to VitalSignsMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.PulseOximeterStep do
  @moduledoc """
  BitActor integration step for PulseOximeter
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, pulseoximeter_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PulseOximeter
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PulseOximeter processing
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
    # Cleanup logic specific to PulseOximeter
    :ok
  end
end


defmodule HealthcareCore.Steps.BloodPressureMonitorStep do
  @moduledoc """
  BitActor integration step for BloodPressureMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, bloodpressuremonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for BloodPressureMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo BloodPressureMonitor processing
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
    # Cleanup logic specific to BloodPressureMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.RespiratoryMonitorStep do
  @moduledoc """
  BitActor integration step for RespiratoryMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, respiratorymonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for RespiratoryMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo RespiratoryMonitor processing
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
    # Cleanup logic specific to RespiratoryMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.TemperatureMonitorStep do
  @moduledoc """
  BitActor integration step for TemperatureMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, temperaturemonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for TemperatureMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo TemperatureMonitor processing
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
    # Cleanup logic specific to TemperatureMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.GlucoseMonitorStep do
  @moduledoc """
  BitActor integration step for GlucoseMonitor
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, glucosemonitor_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for GlucoseMonitor
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo GlucoseMonitor processing
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
    # Cleanup logic specific to GlucoseMonitor
    :ok
  end
end


defmodule HealthcareCore.Steps.InfusionPumpStep do
  @moduledoc """
  BitActor integration step for InfusionPump
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, infusionpump_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for InfusionPump
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo InfusionPump processing
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
    # Cleanup logic specific to InfusionPump
    :ok
  end
end


defmodule HealthcareCore.Steps.VentilatorStep do
  @moduledoc """
  BitActor integration step for Ventilator
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ventilator_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Ventilator
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Ventilator processing
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
    # Cleanup logic specific to Ventilator
    :ok
  end
end


defmodule HealthcareCore.Steps.DefibrillatorStep do
  @moduledoc """
  BitActor integration step for Defibrillator
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, defibrillator_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Defibrillator
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Defibrillator processing
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
    # Cleanup logic specific to Defibrillator
    :ok
  end
end


defmodule HealthcareCore.Steps.VitalSignStep do
  @moduledoc """
  BitActor integration step for VitalSign
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, vitalsign_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VitalSign
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VitalSign processing
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
    # Cleanup logic specific to VitalSign
    :ok
  end
end


defmodule HealthcareCore.Steps.HeartRateStep do
  @moduledoc """
  BitActor integration step for HeartRate
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, heartrate_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for HeartRate
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo HeartRate processing
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
    # Cleanup logic specific to HeartRate
    :ok
  end
end


defmodule HealthcareCore.Steps.BloodPressureStep do
  @moduledoc """
  BitActor integration step for BloodPressure
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, bloodpressure_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for BloodPressure
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo BloodPressure processing
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
    # Cleanup logic specific to BloodPressure
    :ok
  end
end


defmodule HealthcareCore.Steps.RespiratoryRateStep do
  @moduledoc """
  BitActor integration step for RespiratoryRate
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, respiratoryrate_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for RespiratoryRate
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo RespiratoryRate processing
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
    # Cleanup logic specific to RespiratoryRate
    :ok
  end
end


defmodule HealthcareCore.Steps.BodyTemperatureStep do
  @moduledoc """
  BitActor integration step for BodyTemperature
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, bodytemperature_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for BodyTemperature
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo BodyTemperature processing
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
    # Cleanup logic specific to BodyTemperature
    :ok
  end
end


defmodule HealthcareCore.Steps.OxygenSaturationStep do
  @moduledoc """
  BitActor integration step for OxygenSaturation
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, oxygensaturation_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for OxygenSaturation
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo OxygenSaturation processing
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
    # Cleanup logic specific to OxygenSaturation
    :ok
  end
end


defmodule HealthcareCore.Steps.BloodGlucoseStep do
  @moduledoc """
  BitActor integration step for BloodGlucose
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, bloodglucose_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for BloodGlucose
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo BloodGlucose processing
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
    # Cleanup logic specific to BloodGlucose
    :ok
  end
end


defmodule HealthcareCore.Steps.PainLevelStep do
  @moduledoc """
  BitActor integration step for PainLevel
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, painlevel_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PainLevel
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PainLevel processing
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
    # Cleanup logic specific to PainLevel
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicalConditionStep do
  @moduledoc """
  BitActor integration step for MedicalCondition
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicalcondition_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicalCondition
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicalCondition processing
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
    # Cleanup logic specific to MedicalCondition
    :ok
  end
end


defmodule HealthcareCore.Steps.ChronicConditionStep do
  @moduledoc """
  BitActor integration step for ChronicCondition
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, chroniccondition_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ChronicCondition
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ChronicCondition processing
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
    # Cleanup logic specific to ChronicCondition
    :ok
  end
end


defmodule HealthcareCore.Steps.AcuteConditionStep do
  @moduledoc """
  BitActor integration step for AcuteCondition
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, acutecondition_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for AcuteCondition
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo AcuteCondition processing
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
    # Cleanup logic specific to AcuteCondition
    :ok
  end
end


defmodule HealthcareCore.Steps.CardiacConditionStep do
  @moduledoc """
  BitActor integration step for CardiacCondition
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, cardiaccondition_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CardiacCondition
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CardiacCondition processing
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
    # Cleanup logic specific to CardiacCondition
    :ok
  end
end


defmodule HealthcareCore.Steps.RespiratoryConditionStep do
  @moduledoc """
  BitActor integration step for RespiratoryCondition
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, respiratorycondition_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for RespiratoryCondition
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo RespiratoryCondition processing
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
    # Cleanup logic specific to RespiratoryCondition
    :ok
  end
end


defmodule HealthcareCore.Steps.DiabetesStep do
  @moduledoc """
  BitActor integration step for Diabetes
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, diabetes_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Diabetes
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Diabetes processing
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
    # Cleanup logic specific to Diabetes
    :ok
  end
end


defmodule HealthcareCore.Steps.HypertensionStep do
  @moduledoc """
  BitActor integration step for Hypertension
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, hypertension_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Hypertension
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Hypertension processing
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
    # Cleanup logic specific to Hypertension
    :ok
  end
end


defmodule HealthcareCore.Steps.ArrhythmiaStep do
  @moduledoc """
  BitActor integration step for Arrhythmia
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, arrhythmia_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Arrhythmia
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Arrhythmia processing
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
    # Cleanup logic specific to Arrhythmia
    :ok
  end
end


defmodule HealthcareCore.Steps.COPDStep do
  @moduledoc """
  BitActor integration step for COPD
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, copd_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for COPD
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo COPD processing
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
    # Cleanup logic specific to COPD
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicationStep do
  @moduledoc """
  BitActor integration step for Medication
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medication_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Medication
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Medication processing
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
    # Cleanup logic specific to Medication
    :ok
  end
end


defmodule HealthcareCore.Steps.PrescriptionStep do
  @moduledoc """
  BitActor integration step for Prescription
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, prescription_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Prescription
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Prescription processing
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
    # Cleanup logic specific to Prescription
    :ok
  end
end


defmodule HealthcareCore.Steps.DosageStep do
  @moduledoc """
  BitActor integration step for Dosage
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, dosage_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Dosage
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Dosage processing
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
    # Cleanup logic specific to Dosage
    :ok
  end
end


defmodule HealthcareCore.Steps.TreatmentStep do
  @moduledoc """
  BitActor integration step for Treatment
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, treatment_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Treatment
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Treatment processing
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
    # Cleanup logic specific to Treatment
    :ok
  end
end


defmodule HealthcareCore.Steps.TherapyStep do
  @moduledoc """
  BitActor integration step for Therapy
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, therapy_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Therapy
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Therapy processing
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
    # Cleanup logic specific to Therapy
    :ok
  end
end


defmodule HealthcareCore.Steps.SurgeryStep do
  @moduledoc """
  BitActor integration step for Surgery
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, surgery_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Surgery
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Surgery processing
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
    # Cleanup logic specific to Surgery
    :ok
  end
end


defmodule HealthcareCore.Steps.ClinicalEventStep do
  @moduledoc """
  BitActor integration step for ClinicalEvent
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, clinicalevent_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ClinicalEvent
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ClinicalEvent processing
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
    # Cleanup logic specific to ClinicalEvent
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicalAlertStep do
  @moduledoc """
  BitActor integration step for MedicalAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicalalert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicalAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicalAlert processing
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
    # Cleanup logic specific to MedicalAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.CriticalAlertStep do
  @moduledoc """
  BitActor integration step for CriticalAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, criticalalert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CriticalAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CriticalAlert processing
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
    # Cleanup logic specific to CriticalAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.VitalSignAlertStep do
  @moduledoc """
  BitActor integration step for VitalSignAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, vitalsignalert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VitalSignAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VitalSignAlert processing
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
    # Cleanup logic specific to VitalSignAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicationAlertStep do
  @moduledoc """
  BitActor integration step for MedicationAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicationalert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicationAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicationAlert processing
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
    # Cleanup logic specific to MedicationAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.DeviceAlertStep do
  @moduledoc """
  BitActor integration step for DeviceAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, devicealert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DeviceAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DeviceAlert processing
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
    # Cleanup logic specific to DeviceAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.PatientDeteriorationAlertStep do
  @moduledoc """
  BitActor integration step for PatientDeteriorationAlert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, patientdeteriorationalert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PatientDeteriorationAlert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PatientDeteriorationAlert processing
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
    # Cleanup logic specific to PatientDeteriorationAlert
    :ok
  end
end


defmodule HealthcareCore.Steps.CodeBlueStep do
  @moduledoc """
  BitActor integration step for CodeBlue
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, codeblue_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CodeBlue
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CodeBlue processing
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
    # Cleanup logic specific to CodeBlue
    :ok
  end
end


defmodule HealthcareCore.Steps.HealthcareFacilityStep do
  @moduledoc """
  BitActor integration step for HealthcareFacility
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, healthcarefacility_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for HealthcareFacility
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo HealthcareFacility processing
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
    # Cleanup logic specific to HealthcareFacility
    :ok
  end
end


defmodule HealthcareCore.Steps.HospitalStep do
  @moduledoc """
  BitActor integration step for Hospital
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, hospital_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Hospital
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Hospital processing
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
    # Cleanup logic specific to Hospital
    :ok
  end
end


defmodule HealthcareCore.Steps.ClinicStep do
  @moduledoc """
  BitActor integration step for Clinic
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, clinic_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Clinic
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Clinic processing
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
    # Cleanup logic specific to Clinic
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicalUnitStep do
  @moduledoc """
  BitActor integration step for MedicalUnit
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicalunit_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicalUnit
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicalUnit processing
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
    # Cleanup logic specific to MedicalUnit
    :ok
  end
end


defmodule HealthcareCore.Steps.ICUStep do
  @moduledoc """
  BitActor integration step for ICU
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, icu_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ICU
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ICU processing
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
    # Cleanup logic specific to ICU
    :ok
  end
end


defmodule HealthcareCore.Steps.EmergencyDepartmentStep do
  @moduledoc """
  BitActor integration step for EmergencyDepartment
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, emergencydepartment_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EmergencyDepartment
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EmergencyDepartment processing
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
    # Cleanup logic specific to EmergencyDepartment
    :ok
  end
end


defmodule HealthcareCore.Steps.MedicalSurgicalUnitStep do
  @moduledoc """
  BitActor integration step for MedicalSurgicalUnit
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, medicalsurgicalunit_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for MedicalSurgicalUnit
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo MedicalSurgicalUnit processing
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
    # Cleanup logic specific to MedicalSurgicalUnit
    :ok
  end
end


defmodule HealthcareCore.Steps.CardiologyUnitStep do
  @moduledoc """
  BitActor integration step for CardiologyUnit
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, cardiologyunit_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CardiologyUnit
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CardiologyUnit processing
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
    # Cleanup logic specific to CardiologyUnit
    :ok
  end
end


defmodule HealthcareCore.Steps.NICUStep do
  @moduledoc """
  BitActor integration step for NICU
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, nicu_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NICU
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NICU processing
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
    # Cleanup logic specific to NICU
    :ok
  end
end
