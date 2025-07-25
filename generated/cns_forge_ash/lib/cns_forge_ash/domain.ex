defmodule CnsForgeAsh.Domain do
  @moduledoc """
  CNS Forge Domain - The "ecosystem composer" for orchestrating digital reality
  
  This domain contains all CNS Forge resources and reactors, implementing
  the actual Ash.Reactor architecture as specified in the CNS Forge document.
  """
  
  use Ash.Domain
  
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