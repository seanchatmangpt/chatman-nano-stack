#!/usr/bin/env elixir
# Run with: elixir demonstrate_otel_swarm.exs

Mix.install([
  {:ash, "~> 3.0"},
  {:reactor, "~> 0.9"},
  {:telemetry, "~> 1.2"}
])

# Load our modules
Code.require_file("lib/cns_forge/telemetry_swarm_reactor.ex")
Code.require_file("lib/cns_forge/otel_ash_orchestrator.ex") 
Code.require_file("lib/cns_forge/ash_swarm_tracer.ex")

# Configure logging
require Logger
Logger.configure(level: :info)

defmodule OtelSwarmDemo do
  @moduledoc """
  Demonstrates the OTEL Hyper Intelligence Swarm in action
  """
  
  def run do
    IO.puts("\n🧠 OTEL HYPER INTELLIGENCE SWARM DEMONSTRATION")
    IO.puts("=" * 60)
    
    # Start the orchestrator
    {:ok, _pid} = CnsForge.OtelAshOrchestrator.start_link()
    
    # Configure Ash tracer
    CnsForge.AshTracerConfig.configure!()
    
    Process.sleep(500)
    
    IO.puts("\n📡 Generating telemetry events...")
    
    # Simulate various telemetry patterns
    simulate_resource_lifecycle()
    simulate_ttl_violations()
    simulate_high_volume_events()
    simulate_traced_operations()
    
    # Let the swarm process
    IO.puts("\n🔄 Swarm processing events...")
    Process.sleep(2000)
    
    # Get swarm intelligence
    intelligence = CnsForge.OtelAshOrchestrator.get_swarm_intelligence()
    
    # Display results
    display_swarm_intelligence(intelligence)
  end
  
  defp simulate_resource_lifecycle do
    IO.puts("   • Simulating resource lifecycle...")
    
    correlation_id = "demo-lifecycle-#{System.unique_integer()}"
    
    # Create
    :telemetry.execute([:ash, :cns_forge, :create, :start], %{}, %{
      correlation_id: correlation_id,
      resource: "DemoResource"
    })
    
    :telemetry.execute([:ash, :changeset], %{}, %{
      correlation_id: correlation_id,
      resource: "DemoResource"
    })
    
    :telemetry.execute([:ash, :validation], %{duration: 100}, %{
      correlation_id: correlation_id,
      validation: "required_fields"
    })
    
    :telemetry.execute([:ash, :cns_forge, :create, :stop], %{duration: 500}, %{
      correlation_id: correlation_id,
      resource: "DemoResource"
    })
    
    # Update
    :telemetry.execute([:ash, :cns_forge, :update, :start], %{}, %{
      correlation_id: correlation_id,
      resource: "DemoResource"
    })
    
    :telemetry.execute([:ash, :cns_forge, :update, :stop], %{duration: 300}, %{
      correlation_id: correlation_id,
      resource: "DemoResource"
    })
  end
  
  defp simulate_ttl_violations do
    IO.puts("   • Simulating TTL violations...")
    
    for i <- 1..5 do
      :telemetry.execute([:cns_forge, :ttl, :resource_processed], 
        %{processing_time: 5000},
        %{
          resource: "SlowResource#{i}",
          ttl_compliant: false,
          ttl_budget: 1000
        }
      )
    end
    
    # Some compliant ones
    for i <- 1..10 do
      :telemetry.execute([:cns_forge, :ttl, :resource_processed],
        %{processing_time: 500},
        %{
          resource: "FastResource#{i}",
          ttl_compliant: true,
          ttl_budget: 1000
        }
      )
    end
  end
  
  defp simulate_high_volume_events do
    IO.puts("   • Simulating high-volume event storm...")
    
    correlation_id = "storm-#{System.unique_integer()}"
    
    for i <- 1..100 do
      event_type = Enum.random([:create, :read, :update, :destroy])
      
      :telemetry.execute([:ash, :cns_forge, event_type, :start], %{}, %{
        correlation_id: correlation_id,
        index: i
      })
      
      :telemetry.execute([:ash, :cns_forge, event_type, :stop], 
        %{duration: :rand.uniform(100)},
        %{
          correlation_id: correlation_id,
          index: i
        }
      )
    end
  end
  
  defp simulate_traced_operations do
    IO.puts("   • Simulating traced operations...")
    
    # Use the tracer
    CnsForge.AshSwarmTracer.trace(:action, "demo_operation", %{}, fn ->
      Process.sleep(10)
      
      CnsForge.AshSwarmTracer.trace(:changeset, "nested_operation", %{}, fn ->
        :telemetry.execute([:cns_forge, :bit_actor, :bit_actor_spawned],
          %{spawn_time: 50},
          %{actor_id: "demo-actor"}
        )
      end)
      
      {:ok, "completed"}
    end)
  end
  
  defp display_swarm_intelligence(intelligence) do
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("🧠 SWARM INTELLIGENCE ANALYSIS")
    IO.puts(String.duplicate("=", 60))
    
    # Metrics
    IO.puts("\n📊 METRICS:")
    IO.puts("   • Reactor Runs: #{intelligence.metrics.reactor_runs}")
    IO.puts("   • Emergence Factor: #{Float.round(intelligence.swarm_state.emergence_factor, 3)}")
    IO.puts("   • TTL Compliance: #{Float.round(intelligence.swarm_state.ttl_compliance_rate * 100, 1)}%")
    IO.puts("   • Correlations Active: #{map_size(intelligence.swarm_state.correlations)}")
    IO.puts("   • Patterns Detected: #{map_size(intelligence.swarm_state.patterns)}")
    IO.puts("   • System Health: #{intelligence.health}")
    
    # Patterns
    if map_size(intelligence.swarm_state.patterns) > 0 do
      IO.puts("\n🔍 DETECTED PATTERNS:")
      Enum.each(intelligence.swarm_state.patterns, fn {pattern, instances} ->
        IO.puts("   • #{pattern}: #{length(instances)} instances")
      end)
    end
    
    # Recommendations
    if length(intelligence.swarm_state.optimization_queue) > 0 do
      IO.puts("\n💡 OPTIMIZATION RECOMMENDATIONS:")
      Enum.each(intelligence.swarm_state.optimization_queue, fn rec ->
        IO.puts("   • #{rec}")
      end)
    end
    
    # Intelligence Level
    emergence = intelligence.swarm_state.emergence_factor
    level = cond do
      emergence >= 0.9 -> "🧠 HYPER INTELLIGENT"
      emergence >= 0.7 -> "🎯 HIGHLY INTELLIGENT"
      emergence >= 0.5 -> "✨ INTELLIGENT"
      emergence >= 0.3 -> "📈 LEARNING"
      true -> "🌱 NASCENT"
    end
    
    IO.puts("\n🎭 INTELLIGENCE LEVEL: #{level}")
    
    # Summary
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("✅ OTEL SWARM OPERATIONAL - Monitoring #{intelligence.metrics.reactor_runs} events")
    IO.puts("🧠 Artificial Intelligence emerging from telemetry patterns!")
    IO.puts(String.duplicate("=", 60) <> "\n")
  end
end

# Run the demo
OtelSwarmDemo.run()