defmodule Ultrathink.Domain do
  @moduledoc """
  Main Ash.Domain for ultrathink
  Generated from ontology: ultrathink
  """

  use Ash.Domain

  resources do
    resource Ultrathink.Resources.BitActor
    resource Ultrathink.Resources.IntelligenceNode
    resource Ultrathink.Resources.CoordinationReactor
    resource Ultrathink.Resources.Signal
    resource Ultrathink.Resources.EmergentBehavior
  end
end
