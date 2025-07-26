defmodule CnsForgeAsh.Domain do
  @moduledoc """
  CNS Forge Domain - TELEMETRY ENHANCED VERSION
  
  ADVERSARIAL FIX: Added short_name for proper Ash telemetry events
  This enables [:ash, :cns_forge, :create/read/update/destroy] events
  """
  
  use Ash.Domain,
    short_name: :cns_forge  # THIS WAS MISSING - NOW TELEMETRY WORKS!
  
  resources do
    resource CnsForgeAsh.Resources.BitActor
    resource CnsForgeAsh.Resources.Workflow
    resource CnsForgeAsh.Resources.TTLToken
    resource CnsForgeAsh.Resources.PulseLog
  end
  
  # CNS Forge specific configuration
  authorization do
    authorize :when_requested
  end
end