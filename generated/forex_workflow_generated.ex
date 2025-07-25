
defmodule CNSForge.GeneratedForexWorkflow do
  use Ash.Reactor
  
  ash do
    default_domain CNSForge.Domain
  end
  
  # Inputs from parsed directive
  input :forex_pair, default: "EUR/USD"
  input :latency_target_ns, default: 42
  
  # Trading workflow steps
  action :execute_trade, CNSForge.ForexTrader do
    inputs %{
      pair: input(:forex_pair),
      target_latency: input(:latency_target_ns)
    }
    undo_action :compensate_trade
    undo :always
  end
  
  return :execute_trade
end
