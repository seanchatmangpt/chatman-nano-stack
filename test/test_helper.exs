ExUnit.start()

# Configure test coverage
if System.get_env("COVERAGE") do
  # Use ExCoveralls for coverage reporting
  # Add to mix.exs: {:excoveralls, "~> 0.18", only: :test}
end

# Configure Ash testing
Ash.Test.start()

# Set up test database/data layer
Application.put_env(:ash, :disable_async, false)

# Configure telemetry collection for tests
:telemetry.attach_many(
  "test-telemetry-handler",
  [
    [:ash_reactor, :step, :start],
    [:ash_reactor, :step, :stop],
    [:ash_reactor, :step, :exception],
    [:cns_forge, :ttl, :resource_processed],
    [:ttl_swarm, :ai_inference]
  ],
  &CnsForge.TestTelemetryHandler.handle_event/4,
  nil
)

defmodule CnsForge.TestTelemetryHandler do
  @moduledoc """
  Collects telemetry events during tests for coverage analysis
  """
  
  def handle_event(event, measurements, metadata, _config) do
    # Store events for test assertions
    events = Process.get(:telemetry_events, [])
    Process.put(:telemetry_events, [{event, measurements, metadata} | events])
  end
end

defmodule CnsForge.TestHelpers do
  @moduledoc """
  Common test helpers for Ash.Reactor testing
  """
  
  def create_test_reactor(name, steps) do
    # Dynamically create a test reactor
    Module.create(
      name,
      quote do
        use Ash.Reactor
        
        reactor do
          unquote(Enum.map(steps, &create_step/1))
        end
      end,
      Macro.Env.location(__ENV__)
    )
  end
  
  defp create_step(%{name: name, type: type, inputs: inputs}) do
    quote do
      step unquote(name), unquote(type) do
        unquote(
          Enum.map(inputs, fn {key, value} ->
            quote do
              input %{unquote(key) => unquote(value)}
            end
          end)
        )
      end
    end
  end
  
  def assert_reactor_success(reactor, input) do
    assert {:ok, result} = Ash.Reactor.run(reactor, input)
    result
  end
  
  def assert_telemetry_emitted(event_name) do
    events = Process.get(:telemetry_events, [])
    
    assert Enum.any?(events, fn {event, _, _} ->
      event == event_name
    end), "Expected telemetry event #{inspect(event_name)} was not emitted"
  end
end

# Import helpers in all tests
ExUnit.configure(
  exclude: [:pending],
  include: [bdd: true, unit: true, integration: true],
  max_failures: 5
)