defmodule AshReactor80_20.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("Starting Ash.Reactor 80/20 application")
    
    # Configure OpenTelemetry
    :opentelemetry.set_default_tracer(:ash_reactor_80_20)
    
    children = [
      # Telemetry supervisor
      {Telemetry.Supervisor, name: AshReactor80_20.TelemetrySupervisor},
      
      # Add any other supervisors here
    ]

    opts = [strategy: :one_for_one, name: AshReactor80_20.Supervisor]
    
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("Ash.Reactor 80/20 started successfully")
        {:ok, pid}
        
      error ->
        Logger.error("Failed to start Ash.Reactor 80/20: #{inspect(error)}")
        error
    end
  end
end

defmodule Telemetry.Supervisor do
  @moduledoc false
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: opts[:name])
  end

  @impl true
  def init(_opts) do
    children = [
      # Telemetry poller for metrics
      {:telemetry_poller,
       measurements: periodic_measurements(),
       period: 10_000,
       name: :ash_reactor_telemetry_poller}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp periodic_measurements do
    [
      {AshReactor80_20.Telemetry, :dispatch_metrics, []}
    ]
  end
end

defmodule AshReactor80_20.Telemetry do
  @moduledoc false
  
  def dispatch_metrics do
    :telemetry.execute(
      [:ash_reactor_80_20, :vm, :memory],
      %{total: :erlang.memory(:total)},
      %{}
    )
  end
end