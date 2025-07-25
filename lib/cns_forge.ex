defmodule CNSForge do
  @moduledoc """
  CNS Forge - Ecosystem Composer using Ash/Reactor Architecture
  
  Implements the BitActor Mesh as Reactor workflows with:
  - TTL-driven execution (8 hops max)
  - Saga orchestration for atomicity
  - Universal observability via telemetry
  - Declarative resource-oriented architecture
  """
  
  use Ash.Domain,
    validate_config_inclusion?: false

  alias CNSForge.{BitActor, Signal, TelemetryFrame}

  resources do
    resource BitActor
    resource Signal
    resource TelemetryFrame
  end

  def process_directive(directive, initial_ttl \\ 8) do
    %{
      directive: directive,
      ttl: initial_ttl,
      transaction_id: generate_transaction_id(),
      timestamp: DateTime.utc_now()
    }
    |> CNSForge.Workflows.ProcessDirective.run()
  end

  defp generate_transaction_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end
end