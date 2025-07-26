
defmodule TestValidation179420e1.Steps.AssetStep do
  @moduledoc """
  BitActor integration step for Asset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, asset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Asset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Asset processing
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
    # Cleanup logic specific to Asset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.NetworkAssetStep do
  @moduledoc """
  BitActor integration step for NetworkAsset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, networkasset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NetworkAsset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NetworkAsset processing
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
    # Cleanup logic specific to NetworkAsset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ComputeAssetStep do
  @moduledoc """
  BitActor integration step for ComputeAsset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, computeasset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ComputeAsset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ComputeAsset processing
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
    # Cleanup logic specific to ComputeAsset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.DataAssetStep do
  @moduledoc """
  BitActor integration step for DataAsset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, dataasset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DataAsset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DataAsset processing
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
    # Cleanup logic specific to DataAsset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.UserAssetStep do
  @moduledoc """
  BitActor integration step for UserAsset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, userasset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for UserAsset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo UserAsset processing
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
    # Cleanup logic specific to UserAsset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ApplicationAssetStep do
  @moduledoc """
  BitActor integration step for ApplicationAsset
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, applicationasset_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ApplicationAsset
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ApplicationAsset processing
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
    # Cleanup logic specific to ApplicationAsset
    :ok
  end
end


defmodule TestValidation179420e1.Steps.NetworkDeviceStep do
  @moduledoc """
  BitActor integration step for NetworkDevice
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, networkdevice_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NetworkDevice
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NetworkDevice processing
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
    # Cleanup logic specific to NetworkDevice
    :ok
  end
end


defmodule TestValidation179420e1.Steps.RouterStep do
  @moduledoc """
  BitActor integration step for Router
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, router_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Router
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Router processing
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
    # Cleanup logic specific to Router
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SwitchStep do
  @moduledoc """
  BitActor integration step for Switch
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, switch_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Switch
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Switch processing
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
    # Cleanup logic specific to Switch
    :ok
  end
end


defmodule TestValidation179420e1.Steps.FirewallStep do
  @moduledoc """
  BitActor integration step for Firewall
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, firewall_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Firewall
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Firewall processing
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
    # Cleanup logic specific to Firewall
    :ok
  end
end


defmodule TestValidation179420e1.Steps.LoadBalancerStep do
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


defmodule TestValidation179420e1.Steps.VPNStep do
  @moduledoc """
  BitActor integration step for VPN
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, vpn_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for VPN
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo VPN processing
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
    # Cleanup logic specific to VPN
    :ok
  end
end


defmodule TestValidation179420e1.Steps.NetworkSegmentStep do
  @moduledoc """
  BitActor integration step for NetworkSegment
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, networksegment_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NetworkSegment
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NetworkSegment processing
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
    # Cleanup logic specific to NetworkSegment
    :ok
  end
end


defmodule TestValidation179420e1.Steps.DMZStep do
  @moduledoc """
  BitActor integration step for DMZ
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, dmz_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DMZ
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DMZ processing
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
    # Cleanup logic specific to DMZ
    :ok
  end
end


defmodule TestValidation179420e1.Steps.InternalNetworkStep do
  @moduledoc """
  BitActor integration step for InternalNetwork
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, internalnetwork_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for InternalNetwork
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo InternalNetwork processing
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
    # Cleanup logic specific to InternalNetwork
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PublicNetworkStep do
  @moduledoc """
  BitActor integration step for PublicNetwork
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, publicnetwork_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PublicNetwork
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PublicNetwork processing
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
    # Cleanup logic specific to PublicNetwork
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ThreatStep do
  @moduledoc """
  BitActor integration step for Threat
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, threat_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Threat
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Threat processing
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
    # Cleanup logic specific to Threat
    :ok
  end
end


defmodule TestValidation179420e1.Steps.MalwareStep do
  @moduledoc """
  BitActor integration step for Malware
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, malware_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Malware
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Malware processing
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
    # Cleanup logic specific to Malware
    :ok
  end
end


defmodule TestValidation179420e1.Steps.VirusStep do
  @moduledoc """
  BitActor integration step for Virus
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, virus_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Virus
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Virus processing
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
    # Cleanup logic specific to Virus
    :ok
  end
end


defmodule TestValidation179420e1.Steps.WormStep do
  @moduledoc """
  BitActor integration step for Worm
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, worm_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Worm
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Worm processing
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
    # Cleanup logic specific to Worm
    :ok
  end
end


defmodule TestValidation179420e1.Steps.TrojanStep do
  @moduledoc """
  BitActor integration step for Trojan
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, trojan_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Trojan
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Trojan processing
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
    # Cleanup logic specific to Trojan
    :ok
  end
end


defmodule TestValidation179420e1.Steps.RansomwareStep do
  @moduledoc """
  BitActor integration step for Ransomware
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ransomware_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Ransomware
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Ransomware processing
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
    # Cleanup logic specific to Ransomware
    :ok
  end
end


defmodule TestValidation179420e1.Steps.RootkitStep do
  @moduledoc """
  BitActor integration step for Rootkit
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, rootkit_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Rootkit
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Rootkit processing
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
    # Cleanup logic specific to Rootkit
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SpywareStep do
  @moduledoc """
  BitActor integration step for Spyware
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, spyware_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Spyware
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Spyware processing
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
    # Cleanup logic specific to Spyware
    :ok
  end
end


defmodule TestValidation179420e1.Steps.BotnetStep do
  @moduledoc """
  BitActor integration step for Botnet
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, botnet_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Botnet
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Botnet processing
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
    # Cleanup logic specific to Botnet
    :ok
  end
end


defmodule TestValidation179420e1.Steps.AttackStep do
  @moduledoc """
  BitActor integration step for Attack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, attack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Attack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Attack processing
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
    # Cleanup logic specific to Attack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.NetworkAttackStep do
  @moduledoc """
  BitActor integration step for NetworkAttack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, networkattack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for NetworkAttack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo NetworkAttack processing
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
    # Cleanup logic specific to NetworkAttack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.DDoSAttackStep do
  @moduledoc """
  BitActor integration step for DDoSAttack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ddosattack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DDoSAttack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DDoSAttack processing
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
    # Cleanup logic specific to DDoSAttack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ManInTheMiddleAttackStep do
  @moduledoc """
  BitActor integration step for ManInTheMiddleAttack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, maninthemiddleattack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ManInTheMiddleAttack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ManInTheMiddleAttack processing
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
    # Cleanup logic specific to ManInTheMiddleAttack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PacketSniffingStep do
  @moduledoc """
  BitActor integration step for PacketSniffing
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, packetsniffing_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PacketSniffing
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PacketSniffing processing
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
    # Cleanup logic specific to PacketSniffing
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PortScanStep do
  @moduledoc """
  BitActor integration step for PortScan
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, portscan_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PortScan
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PortScan processing
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
    # Cleanup logic specific to PortScan
    :ok
  end
end


defmodule TestValidation179420e1.Steps.WebAttackStep do
  @moduledoc """
  BitActor integration step for WebAttack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, webattack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for WebAttack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo WebAttack processing
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
    # Cleanup logic specific to WebAttack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SQLInjectionStep do
  @moduledoc """
  BitActor integration step for SQLInjection
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, sqlinjection_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SQLInjection
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SQLInjection processing
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
    # Cleanup logic specific to SQLInjection
    :ok
  end
end


defmodule TestValidation179420e1.Steps.XSSStep do
  @moduledoc """
  BitActor integration step for XSS
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, xss_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for XSS
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo XSS processing
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
    # Cleanup logic specific to XSS
    :ok
  end
end


defmodule TestValidation179420e1.Steps.CSRFStep do
  @moduledoc """
  BitActor integration step for CSRF
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, csrf_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CSRF
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CSRF processing
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
    # Cleanup logic specific to CSRF
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SocialEngineeringStep do
  @moduledoc """
  BitActor integration step for SocialEngineering
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, socialengineering_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SocialEngineering
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SocialEngineering processing
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
    # Cleanup logic specific to SocialEngineering
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PhishingAttackStep do
  @moduledoc """
  BitActor integration step for PhishingAttack
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, phishingattack_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PhishingAttack
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PhishingAttack processing
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
    # Cleanup logic specific to PhishingAttack
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SpearPhishingStep do
  @moduledoc """
  BitActor integration step for SpearPhishing
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, spearphishing_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SpearPhishing
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SpearPhishing processing
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
    # Cleanup logic specific to SpearPhishing
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PrivilegeEscalationStep do
  @moduledoc """
  BitActor integration step for PrivilegeEscalation
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, privilegeescalation_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PrivilegeEscalation
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PrivilegeEscalation processing
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
    # Cleanup logic specific to PrivilegeEscalation
    :ok
  end
end


defmodule TestValidation179420e1.Steps.LateralMovementStep do
  @moduledoc """
  BitActor integration step for LateralMovement
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, lateralmovement_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for LateralMovement
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo LateralMovement processing
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
    # Cleanup logic specific to LateralMovement
    :ok
  end
end


defmodule TestValidation179420e1.Steps.DataExfiltrationStep do
  @moduledoc """
  BitActor integration step for DataExfiltration
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, dataexfiltration_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DataExfiltration
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DataExfiltration processing
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
    # Cleanup logic specific to DataExfiltration
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SecurityEventStep do
  @moduledoc """
  BitActor integration step for SecurityEvent
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, securityevent_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SecurityEvent
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SecurityEvent processing
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
    # Cleanup logic specific to SecurityEvent
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SecurityIncidentStep do
  @moduledoc """
  BitActor integration step for SecurityIncident
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, securityincident_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SecurityIncident
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SecurityIncident processing
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
    # Cleanup logic specific to SecurityIncident
    :ok
  end
end


defmodule TestValidation179420e1.Steps.VulnerabilityStep do
  @moduledoc """
  BitActor integration step for Vulnerability
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, vulnerability_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Vulnerability
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Vulnerability processing
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
    # Cleanup logic specific to Vulnerability
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ZeroDayVulnerabilityStep do
  @moduledoc """
  BitActor integration step for ZeroDayVulnerability
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, zerodayvulnerability_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ZeroDayVulnerability
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ZeroDayVulnerability processing
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
    # Cleanup logic specific to ZeroDayVulnerability
    :ok
  end
end


defmodule TestValidation179420e1.Steps.AlertStep do
  @moduledoc """
  BitActor integration step for Alert
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, alert_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Alert
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Alert processing
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
    # Cleanup logic specific to Alert
    :ok
  end
end


defmodule TestValidation179420e1.Steps.ThreatIntelligenceStep do
  @moduledoc """
  BitActor integration step for ThreatIntelligence
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, threatintelligence_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for ThreatIntelligence
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo ThreatIntelligence processing
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
    # Cleanup logic specific to ThreatIntelligence
    :ok
  end
end


defmodule TestValidation179420e1.Steps.IOCStep do
  @moduledoc """
  BitActor integration step for IOC
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ioc_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for IOC
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo IOC processing
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
    # Cleanup logic specific to IOC
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SecurityControlStep do
  @moduledoc """
  BitActor integration step for SecurityControl
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, securitycontrol_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SecurityControl
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SecurityControl processing
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
    # Cleanup logic specific to SecurityControl
    :ok
  end
end


defmodule TestValidation179420e1.Steps.PreventiveControlStep do
  @moduledoc """
  BitActor integration step for PreventiveControl
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, preventivecontrol_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for PreventiveControl
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo PreventiveControl processing
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
    # Cleanup logic specific to PreventiveControl
    :ok
  end
end


defmodule TestValidation179420e1.Steps.DetectiveControlStep do
  @moduledoc """
  BitActor integration step for DetectiveControl
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, detectivecontrol_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for DetectiveControl
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo DetectiveControl processing
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
    # Cleanup logic specific to DetectiveControl
    :ok
  end
end


defmodule TestValidation179420e1.Steps.CorrectiveControlStep do
  @moduledoc """
  BitActor integration step for CorrectiveControl
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, correctivecontrol_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for CorrectiveControl
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo CorrectiveControl processing
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
    # Cleanup logic specific to CorrectiveControl
    :ok
  end
end


defmodule TestValidation179420e1.Steps.IDSStep do
  @moduledoc """
  BitActor integration step for IDS
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ids_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for IDS
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo IDS processing
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
    # Cleanup logic specific to IDS
    :ok
  end
end


defmodule TestValidation179420e1.Steps.IPSStep do
  @moduledoc """
  BitActor integration step for IPS
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, ips_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for IPS
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo IPS processing
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
    # Cleanup logic specific to IPS
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SIEMStep do
  @moduledoc """
  BitActor integration step for SIEM
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, siem_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SIEM
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SIEM processing
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
    # Cleanup logic specific to SIEM
    :ok
  end
end


defmodule TestValidation179420e1.Steps.AntivirusStep do
  @moduledoc """
  BitActor integration step for Antivirus
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, antivirus_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for Antivirus
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo Antivirus processing
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
    # Cleanup logic specific to Antivirus
    :ok
  end
end


defmodule TestValidation179420e1.Steps.EDRStep do
  @moduledoc """
  BitActor integration step for EDR
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, edr_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for EDR
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo EDR processing
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
    # Cleanup logic specific to EDR
    :ok
  end
end


defmodule TestValidation179420e1.Steps.SOARStep do
  @moduledoc """
  BitActor integration step for SOAR
  Performance Requirements: {}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, soar_handler) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, "BitActor failed: #{reason}"}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for SOAR
    case reason do
      %{timeout: _} -> :retry
      %{bitactor_error: _} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo SOAR processing
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
    # Cleanup logic specific to SOAR
    :ok
  end
end
