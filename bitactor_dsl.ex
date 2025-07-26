defmodule BitActorDSL do
  @moduledoc """
  BitActor DSL - Generated from TTL Ontology
  Defines the domain-specific language for BitActor with TTL constraints
  """

  use Spark.Dsl

  # =============================================================================
  # DSL Sections
  # =============================================================================

  @bitactor %Spark.Dsl.Section{
    name: :bitactor,
    describe: "Configure BitActor properties and TTL constraints",
    schema: [
      name: [
        type: :string,
        required: true,
        doc: "Human-readable name of the BitActor"
      ],
      ttl_budget_ms: [
        type: :integer,
        default: 8,
        doc: "TTL budget in milliseconds (1-100)"
      ],
      status: [
        type: {:in, [:inactive, :active, :processing, :suspended, :error, :terminated]},
        default: :inactive,
        doc: "Initial operational status"
      ],
      parent_actor: [
        type: :atom,
        doc: "Reference to parent BitActor module"
      ]
    ],
    entities: [
      ttl_constraint: [
        Spark.Dsl.Entity,
        schema: [
          budget_ns: [
            type: :integer,
            required: true,
            doc: "TTL budget in nanoseconds"
          ],
          precision: [
            type: {:in, [:nanosecond, :microsecond, :millisecond, :second]},
            default: :nanosecond,
            doc: "Time precision level"
          ],
          max_budget_ms: [
            type: :integer,
            default: 8,
            doc: "Maximum budget in milliseconds"
          ]
        ]
      ]
    ]
  }

  @signals %Spark.Dsl.Section{
    name: :signals,
    describe: "Define signal processing capabilities",
    entities: [
      process: [
        Spark.Dsl.Entity,
        args: [:type],
        schema: [
          type: [
            type: {:in, [:data, :control, :telemetry, :heartbeat, :error]},
            required: true,
            doc: "Type of signal to process"
          ],
          priority: [
            type: {:in, [:low, :medium, :high, :critical, :emergency]},
            default: :medium,
            doc: "Processing priority"
          ],
          handler: [
            type: {:fun, 2},
            required: true,
            doc: "Function to handle the signal (signal, context)"
          ],
          ttl_override: [
            type: :integer,
            doc: "Override TTL for this signal type (nanoseconds)"
          ]
        ]
      ]
    ]
  }

  @telemetry %Spark.Dsl.Section{
    name: :telemetry,
    describe: "Configure telemetry emission",
    entities: [
      metric: [
        Spark.Dsl.Entity,
        args: [:name],
        schema: [
          name: [
            type: :string,
            required: true,
            doc: "Metric name"
          ],
          unit: [
            type: {:in, [:count, :ms, :ns, :bytes, :percent, :rate]},
            default: :count,
            doc: "Unit of measurement"
          ],
          interval_ms: [
            type: :integer,
            default: 1000,
            doc: "Emission interval in milliseconds"
          ],
          aggregation: [
            type: {:in, [:sum, :avg, :max, :min, :last]},
            default: :last,
            doc: "Aggregation method"
          ]
        ]
      ]
    ]
  }

  @swarm %Spark.Dsl.Section{
    name: :swarm,
    describe: "Swarm coordination configuration",
    schema: [
      topology: [
        type: {:in, [:hierarchical, :mesh, :ring, :star]},
        default: :hierarchical,
        doc: "Swarm network topology"
      ],
      max_actors: [
        type: :integer,
        default: 100,
        doc: "Maximum actors in swarm"
      ],
      strategy: [
        type: {:in, [:balanced, :specialized, :adaptive]},
        default: :balanced,
        doc: "Actor distribution strategy"
      ],
      auto_scale: [
        type: :boolean,
        default: true,
        doc: "Enable auto-scaling"
      ]
    ]
  }

  @ttl_violations %Spark.Dsl.Section{
    name: :ttl_violations,
    describe: "TTL violation handling",
    schema: [
      action: [
        type: {:in, [:log, :alert, :compensate, :terminate]},
        default: :log,
        doc: "Action on TTL violation"
      ],
      threshold_percent: [
        type: :integer,
        default: 10,
        doc: "Violation threshold as percentage of budget"
      ],
      handler: [
        type: {:fun, 1},
        doc: "Custom violation handler function"
      ]
    ]
  }

  # =============================================================================
  # DSL Definition
  # =============================================================================

  use Spark.Dsl.Extension,
    sections: [@bitactor, @signals, @telemetry, @swarm, @ttl_violations],
    transformers: [
      BitActorDSL.Transformers.ValidateTTL,
      BitActorDSL.Transformers.GenerateHandlers,
      BitActorDSL.Transformers.SetupTelemetry
    ]
end

defmodule BitActorDSL.Transformers.ValidateTTL do
  @moduledoc """
  Validates TTL constraints are consistent
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, ttl_budget_ms} <- Spark.Dsl.Transformer.get_option(dsl_state, [:bitactor], :ttl_budget_ms),
         {:ok, constraints} <- Spark.Dsl.Transformer.get_entities(dsl_state, [:bitactor, :ttl_constraint]) do
      
      case constraints do
        [] ->
          # Add default constraint if none specified
          budget_ns = ttl_budget_ms * 1_000_000
          
          entity = %Spark.Dsl.Entity{
            name: :ttl_constraint,
            args: [],
            opts: [
              budget_ns: budget_ns,
              precision: :nanosecond,
              max_budget_ms: ttl_budget_ms
            ]
          }
          
          {:ok, Spark.Dsl.Transformer.add_entity(dsl_state, [:bitactor], entity)}
          
        [constraint] ->
          # Validate existing constraint
          if constraint.opts[:budget_ns] > ttl_budget_ms * 1_000_000 do
            {:error, "TTL constraint budget exceeds actor TTL budget"}
          else
            {:ok, dsl_state}
          end
          
        _ ->
          {:error, "Only one TTL constraint allowed per BitActor"}
      end
    end
  end
end

defmodule BitActorDSL.Transformers.GenerateHandlers do
  @moduledoc """
  Generates signal handler functions
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, signals} <- Spark.Dsl.Transformer.get_entities(dsl_state, [:signals]) do
      # Generate handler module code
      handlers = Enum.map(signals, fn signal ->
        generate_handler(signal)
      end)
      
      # Store generated handlers in DSL state
      dsl_state = Spark.Dsl.Transformer.set_option(dsl_state, [:bitactor], :generated_handlers, handlers)
      
      {:ok, dsl_state}
    end
  end
  
  defp generate_handler(signal) do
    %{
      type: signal.opts[:type],
      priority: signal.opts[:priority],
      handler: signal.opts[:handler],
      ttl_override: signal.opts[:ttl_override]
    }
  end
end

defmodule BitActorDSL.Transformers.SetupTelemetry do
  @moduledoc """
  Sets up telemetry emission
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, metrics} <- Spark.Dsl.Transformer.get_entities(dsl_state, [:telemetry]) do
      # Configure telemetry metrics
      telemetry_config = Enum.map(metrics, fn metric ->
        %{
          name: metric.opts[:name],
          unit: metric.opts[:unit],
          interval_ms: metric.opts[:interval_ms],
          aggregation: metric.opts[:aggregation]
        }
      end)
      
      dsl_state = Spark.Dsl.Transformer.set_option(dsl_state, [:telemetry], :config, telemetry_config)
      
      {:ok, dsl_state}
    end
  end
end

# =============================================================================
# Example BitActor Definition
# =============================================================================

defmodule ExampleBitActor do
  @moduledoc """
  Example BitActor using the DSL
  """
  use BitActorDSL

  bitactor do
    name "SignalProcessor"
    ttl_budget_ms 10
    status :active
    
    ttl_constraint do
      budget_ns 10_000_000
      precision :nanosecond
      max_budget_ms 10
    end
  end

  signals do
    process :data do
      priority :high
      handler &handle_data_signal/2
      ttl_override 5_000_000
    end
    
    process :control do
      priority :critical
      handler &handle_control_signal/2
    end
    
    process :telemetry do
      priority :low
      handler &handle_telemetry_signal/2
    end
  end

  telemetry do
    metric "signals_processed" do
      unit :count
      interval_ms 1000
      aggregation :sum
    end
    
    metric "processing_time" do
      unit :ns
      interval_ms 100
      aggregation :avg
    end
    
    metric "ttl_utilization" do
      unit :percent
      interval_ms 1000
      aggregation :avg
    end
  end

  swarm do
    topology :mesh
    max_actors 50
    strategy :adaptive
    auto_scale true
  end

  ttl_violations do
    action :alert
    threshold_percent 20
    handler &handle_ttl_violation/1
  end

  # Handler implementations
  defp handle_data_signal(signal, context) do
    # Process data signal
    {:ok, %{processed: true, duration_ns: 1_500_000}}
  end

  defp handle_control_signal(signal, context) do
    # Process control signal
    {:ok, %{action_taken: signal.payload.action}}
  end

  defp handle_telemetry_signal(signal, context) do
    # Process telemetry signal
    {:ok, %{metrics_recorded: true}}
  end

  defp handle_ttl_violation(violation) do
    # Handle TTL violation
    Logger.warning("TTL violation: #{violation.violation_amount_ns}ns over budget")
  end
end