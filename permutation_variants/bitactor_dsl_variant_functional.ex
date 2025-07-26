defmodule FunctionalBitActorDSL do
  @moduledoc """
  Functional BitActor DSL Variant - Immutable State with Message Passing
  
  This variant explores functional programming patterns with:
  - Immutable actor state
  - Pure function signal processing
  - Message passing concurrency
  - Functional composition of TTL constraints
  - Monadic error handling
  """

  use Spark.Dsl

  # =============================================================================
  # Functional DSL Sections
  # =============================================================================

  @functional_actor %Spark.Dsl.Section{
    name: :functional_actor,
    describe: "Configure functional BitActor with immutable state",
    schema: [
      name: [
        type: :string,
        required: true,
        doc: "Immutable actor name"
      ],
      initial_state: [
        type: {:or, [:map, :keyword_list]},
        default: %{},
        doc: "Initial immutable state"
      ],
      ttl_budget_ms: [
        type: :integer,
        default: 8,
        doc: "TTL budget with functional composition"
      ],
      pure_functions_only: [
        type: :boolean,
        default: true,
        doc: "Enforce pure function constraints"
      ],
      state_transitions: [
        type: {:list, :atom},
        default: [:inactive, :active, :processing, :completed, :error],
        doc: "Valid state transitions"
      ]
    ],
    entities: [
      immutable_constraint: [
        Spark.Dsl.Entity,
        schema: [
          budget_ns: [
            type: :integer,
            required: true,
            doc: "Immutable TTL budget in nanoseconds"
          ],
          composition_strategy: [
            type: {:in, [:monadic, :applicative, :functor, :lens]},
            default: :monadic,
            doc: "Functional composition strategy"
          ],
          purity_level: [
            type: {:in, [:strict, :lenient, :effectful]},
            default: :strict,
            doc: "Function purity enforcement level"
          ]
        ]
      ]
    ]
  }

  @message_patterns %Spark.Dsl.Section{
    name: :message_patterns,
    describe: "Define immutable message processing patterns",
    entities: [
      pattern: [
        Spark.Dsl.Entity,
        args: [:name],
        schema: [
          name: [
            type: :atom,
            required: true,
            doc: "Pattern name"
          ],
          matcher: [
            type: {:fun, 1},
            required: true,
            doc: "Pattern matching function"
          ],
          transformer: [
            type: {:fun, 2},
            required: true,
            doc: "Pure transformation function (message, state) -> {new_state, result}"
          ],
          guard_conditions: [
            type: {:list, {:fun, 2}},
            default: [],
            doc: "List of guard functions"
          ],
          ttl_transform: [
            type: {:fun, 1},
            doc: "TTL constraint transformation function"
          ]
        ]
      ]
    ]
  }

  @state_management %Spark.Dsl.Section{
    name: :state_management,
    describe: "Configure immutable state management",
    schema: [
      persistence_strategy: [
        type: {:in, [:memory, :immutable_log, :event_sourcing, :snapshot]},
        default: :memory,
        doc: "State persistence strategy"
      ],
      snapshot_frequency: [
        type: :integer,
        default: 100,
        doc: "Frequency of state snapshots"
      ],
      max_history: [
        type: :integer,
        default: 1000,
        doc: "Maximum state history to maintain"
      ]
    ],
    entities: [
      lens: [
        Spark.Dsl.Entity,
        args: [:path],
        schema: [
          path: [
            type: {:list, :atom},
            required: true,
            doc: "Path into state structure"
          ],
          getter: [
            type: {:fun, 1},
            required: true,
            doc: "Pure getter function"
          ],
          setter: [
            type: {:fun, 2},
            required: true,
            doc: "Pure setter function returning new state"
          ]
        ]
      ]
    ]
  }

  @composition_rules %Spark.Dsl.Section{
    name: :composition_rules,
    describe: "Define functional composition rules",
    entities: [
      compose: [
        Spark.Dsl.Entity,
        args: [:name],
        schema: [
          name: [
            type: :atom,
            required: true,
            doc: "Composition rule name"
          ],
          functions: [
            type: {:list, {:fun, 1}},
            required: true,
            doc: "List of functions to compose"
          ],
          composition_type: [
            type: {:in, [:pipe, :curry, :partial, :monadic_bind]},
            default: :pipe,
            doc: "Type of function composition"
          ],
          error_handling: [
            type: {:in, [:maybe, :either, :exception, :tuple]},
            default: :maybe,
            doc: "Error handling strategy"
          ]
        ]
      ]
    ]
  }

  @monadic_operations %Spark.Dsl.Section{
    name: :monadic_operations,
    describe: "Define monadic operations for error handling",
    entities: [
      monad: [
        Spark.Dsl.Entity,
        args: [:type],
        schema: [
          type: [
            type: {:in, [:maybe, :either, :io, :state, :reader, :writer]},
            required: true,
            doc: "Monad type"
          ],
          bind_function: [
            type: {:fun, 2},
            required: true,
            doc: "Monadic bind operation"
          ],
          return_function: [
            type: {:fun, 1},
            required: true,
            doc: "Monadic return operation"
          ],
          error_transform: [
            type: {:fun, 1},
            doc: "Error transformation function"
          ]
        ]
      ]
    ]
  }

  # =============================================================================
  # DSL Extension Definition
  # =============================================================================

  use Spark.Dsl.Extension,
    sections: [
      @functional_actor, 
      @message_patterns, 
      @state_management, 
      @composition_rules, 
      @monadic_operations
    ],
    transformers: [
      FunctionalBitActorDSL.Transformers.ValidateImmutability,
      FunctionalBitActorDSL.Transformers.GenerateFunctionalHandlers,
      FunctionalBitActorDSL.Transformers.ComposeStateMachine,
      FunctionalBitActorDSL.Transformers.OptimizeComposition
    ]
end

# =============================================================================
# Functional Transformers
# =============================================================================

defmodule FunctionalBitActorDSL.Transformers.ValidateImmutability do
  @moduledoc """
  Validates that all functions are pure and state is immutable
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, pure_functions_only} <- 
           Spark.Dsl.Transformer.get_option(dsl_state, [:functional_actor], :pure_functions_only),
         {:ok, patterns} <- 
           Spark.Dsl.Transformer.get_entities(dsl_state, [:message_patterns]) do
      
      if pure_functions_only do
        validate_function_purity(patterns)
      else
        {:ok, dsl_state}
      end
    end
  end

  defp validate_function_purity(patterns) do
    # In a real implementation, this would analyze function ASTs
    # for side effects, mutations, etc.
    impure_functions = Enum.filter(patterns, &has_side_effects?/1)
    
    if Enum.empty?(impure_functions) do
      {:ok, :validated}
    else
      {:error, "Impure functions detected: #{inspect(impure_functions)}"}
    end
  end

  # Placeholder for side effect detection
  defp has_side_effects?(_pattern), do: false
end

defmodule FunctionalBitActorDSL.Transformers.GenerateFunctionalHandlers do
  @moduledoc """
  Generates pure functional message handlers
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, patterns} <- Spark.Dsl.Transformer.get_entities(dsl_state, [:message_patterns]) do
      handlers = Enum.map(patterns, &generate_pure_handler/1)
      
      dsl_state = Spark.Dsl.Transformer.set_option(
        dsl_state, 
        [:functional_actor], 
        :generated_handlers, 
        handlers
      )
      
      {:ok, dsl_state}
    end
  end

  defp generate_pure_handler(pattern) do
    %{
      name: pattern.opts[:name],
      matcher: pattern.opts[:matcher],
      transformer: pattern.opts[:transformer],
      guard_conditions: pattern.opts[:guard_conditions] || [],
      pure_function: true,
      composition_ready: true
    }
  end
end

defmodule FunctionalBitActorDSL.Transformers.ComposeStateMachine do
  @moduledoc """
  Composes state transitions as pure functions
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, state_transitions} <- 
           Spark.Dsl.Transformer.get_option(dsl_state, [:functional_actor], :state_transitions) do
      
      state_machine = generate_state_machine(state_transitions)
      
      dsl_state = Spark.Dsl.Transformer.set_option(
        dsl_state,
        [:functional_actor],
        :state_machine,
        state_machine
      )
      
      {:ok, dsl_state}
    end
  end

  defp generate_state_machine(transitions) do
    %{
      initial_state: :inactive,
      transitions: build_transition_map(transitions),
      pure_transitions: true,
      immutable_state: true
    }
  end

  defp build_transition_map(transitions) do
    # Generate valid state transition mappings
    for from <- transitions, to <- transitions, from != to, into: %{} do
      {{from, to}, &validate_transition/2}
    end
  end

  defp validate_transition(from_state, to_state) do
    # Pure function to validate state transitions
    case {from_state, to_state} do
      {:inactive, :active} -> {:ok, :valid_transition}
      {:active, :processing} -> {:ok, :valid_transition}
      {:processing, :completed} -> {:ok, :valid_transition}
      {:processing, :error} -> {:ok, :valid_transition}
      {_, :inactive} -> {:ok, :valid_transition}  # Can always go back to inactive
      _ -> {:error, :invalid_transition}
    end
  end
end

defmodule FunctionalBitActorDSL.Transformers.OptimizeComposition do
  @moduledoc """
  Optimizes functional composition chains
  """
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    with {:ok, composition_rules} <- 
           Spark.Dsl.Transformer.get_entities(dsl_state, [:composition_rules]) do
      
      optimized_rules = Enum.map(composition_rules, &optimize_composition_rule/1)
      
      dsl_state = Spark.Dsl.Transformer.set_option(
        dsl_state,
        [:composition_rules],
        :optimized_compositions,
        optimized_rules
      )
      
      {:ok, dsl_state}
    end
  end

  defp optimize_composition_rule(rule) do
    functions = rule.opts[:functions]
    composition_type = rule.opts[:composition_type]
    
    %{
      name: rule.opts[:name],
      optimized_function: create_optimized_composition(functions, composition_type),
      original_function_count: length(functions),
      composition_type: composition_type
    }
  end

  defp create_optimized_composition(functions, :pipe) do
    # Create a single optimized function from the composition
    fn input ->
      Enum.reduce(functions, input, fn f, acc -> f.(acc) end)
    end
  end

  defp create_optimized_composition(functions, :curry) do
    # Create curried composition
    Enum.reduce(functions, &Function.curry(&2, &1))
  end

  defp create_optimized_composition(functions, :monadic_bind) do
    # Create monadic composition
    fn input ->
      Enum.reduce(functions, {:ok, input}, fn f, {:ok, acc} ->
        f.(acc)
      other ->
        other
      end)
    end
  end
end

# =============================================================================
# Functional Utilities
# =============================================================================

defmodule FunctionalBitActorDSL.Maybe do
  @moduledoc """
  Maybe monad implementation for error handling
  """

  defstruct [:value]

  def some(value), do: %__MODULE__{value: value}
  def none(), do: %__MODULE__{value: nil}

  def bind(%__MODULE__{value: nil}, _f), do: none()
  def bind(%__MODULE__{value: value}, f), do: f.(value)

  def map(%__MODULE__{value: nil}, _f), do: none()
  def map(%__MODULE__{value: value}, f), do: some(f.(value))

  def is_some?(%__MODULE__{value: nil}), do: false
  def is_some?(%__MODULE__{}), do: true

  def unwrap(%__MODULE__{value: value}, default \\ nil) do
    if value, do: value, else: default
  end
end

defmodule FunctionalBitActorDSL.State do
  @moduledoc """
  Immutable state management utilities
  """

  defstruct [:data, :history, :version]

  def new(initial_data \\ %{}) do
    %__MODULE__{
      data: initial_data,
      history: [initial_data],
      version: 1
    }
  end

  def update(%__MODULE__{} = state, update_fn) when is_function(update_fn, 1) do
    new_data = update_fn.(state.data)
    
    %__MODULE__{
      data: new_data,
      history: [new_data | state.history],
      version: state.version + 1
    }
  end

  def get(%__MODULE__{data: data}, path) when is_list(path) do
    get_in(data, path)
  end

  def get(%__MODULE__{data: data}, key) do
    Map.get(data, key)
  end

  def set(%__MODULE__{} = state, path, value) when is_list(path) do
    update(state, fn data -> put_in(data, path, value) end)
  end

  def set(%__MODULE__{} = state, key, value) do
    update(state, fn data -> Map.put(data, key, value) end)
  end

  def rollback(%__MODULE__{history: [_current | previous | _rest]} = state) do
    %__MODULE__{
      data: previous,
      history: state.history,
      version: state.version + 1
    }
  end

  def rollback(%__MODULE__{} = state), do: state  # Can't rollback further
end

defmodule FunctionalBitActorDSL.Lens do
  @moduledoc """
  Lens implementation for functional state access
  """

  defstruct [:getter, :setter]

  def new(getter, setter) do
    %__MODULE__{getter: getter, setter: setter}
  end

  def view(%__MODULE__{getter: getter}, data) do
    getter.(data)
  end

  def set(%__MODULE__{setter: setter}, data, value) do
    setter.(data, value)
  end

  def over(%__MODULE__{} = lens, data, update_fn) do
    current_value = view(lens, data)
    new_value = update_fn.(current_value)
    set(lens, data, new_value)
  end

  # Lens composition
  def compose(%__MODULE__{} = outer, %__MODULE__{} = inner) do
    new(
      fn data -> inner.getter.(outer.getter.(data)) end,
      fn data, value ->
        outer_value = outer.getter.(data)
        new_outer_value = inner.setter.(outer_value, value)
        outer.setter.(data, new_outer_value)
      end
    )
  end
end

# =============================================================================
# Example Functional BitActor
# =============================================================================

defmodule ExampleFunctionalBitActor do
  @moduledoc """
  Example functional BitActor with immutable state and pure functions
  """
  use FunctionalBitActorDSL

  alias FunctionalBitActorDSL.{Maybe, State, Lens}

  functional_actor do
    name "FunctionalThreatProcessor"
    initial_state %{
      threats_processed: 0,
      threat_severity_sum: 0,
      last_threat_id: nil,
      processing_state: :ready
    }
    ttl_budget_ms 10
    pure_functions_only true
    state_transitions [:inactive, :ready, :processing, :completed, :error]

    immutable_constraint do
      budget_ns 10_000_000
      composition_strategy :monadic
      purity_level :strict
    end
  end

  message_patterns do
    pattern :threat_detection do
      matcher(&match_threat_signal/1)
      transformer(&process_threat_signal/2)
      guard_conditions [&validate_threat_format/2, &check_ttl_budget/2]
      ttl_transform(&apply_threat_ttl/1)
    end

    pattern :heartbeat do
      matcher(&match_heartbeat_signal/1)
      transformer(&process_heartbeat/2)
      guard_conditions [&validate_heartbeat_timing/2]
    end

    pattern :error_recovery do
      matcher(&match_error_signal/1)
      transformer(&handle_error_recovery/2)
      guard_conditions [&validate_error_state/2]
    end
  end

  state_management do
    persistence_strategy :immutable_log
    snapshot_frequency 50
    max_history 500

    lens :threats_count do
      path [:threats_processed]
      getter(&Map.get(&1, :threats_processed, 0))
      setter(&Map.put(&1, :threats_processed, &2))
    end

    lens :severity_average do
      path [:threat_severity_sum, :threats_processed]
      getter(&calculate_severity_average/1)
      setter(&update_severity_data/2)
    end
  end

  composition_rules do
    compose :threat_processing_pipeline do
      functions [
        &validate_threat_input/1,
        &extract_threat_features/1,
        &calculate_severity_score/1,
        &update_threat_statistics/1,
        &generate_response/1
      ]
      composition_type :monadic_bind
      error_handling :maybe
    end

    compose :state_update_chain do
      functions [
        &increment_counter/1,
        &update_severity_sum/1,
        &set_last_threat_id/1,
        &transition_to_completed/1
      ]
      composition_type :pipe
      error_handling :tuple
    end
  end

  monadic_operations do
    monad :maybe do
      bind_function(&Maybe.bind/2)
      return_function(&Maybe.some/1)
      error_transform(&handle_maybe_error/1)
    end

    monad :either do
      bind_function(&bind_either/2)
      return_function(&return_either/1)
      error_transform(&transform_either_error/1)
    end
  end

  # =============================================================================
  # Pure Function Implementations
  # =============================================================================

  # Message matching functions
  defp match_threat_signal(%{type: :threat_detection}), do: true
  defp match_threat_signal(_), do: false

  defp match_heartbeat_signal(%{type: :heartbeat}), do: true
  defp match_heartbeat_signal(_), do: false

  defp match_error_signal(%{type: :error}), do: true
  defp match_error_signal(_), do: false

  # Message processing functions (pure)
  defp process_threat_signal(signal, state) do
    threat_id = signal.payload[:threat_id]
    severity = signal.payload[:severity] || 1
    
    # Create threat processing pipeline
    case run_threat_pipeline(signal) do
      {:ok, processed_threat} ->
        new_state = state
        |> State.update(fn data ->
          data
          |> Map.update(:threats_processed, 0, &(&1 + 1))
          |> Map.update(:threat_severity_sum, 0, &(&1 + severity))
          |> Map.put(:last_threat_id, threat_id)
          |> Map.put(:processing_state, :completed)
        end)
        
        {new_state, {:ok, processed_threat}}
      
      {:error, reason} ->
        error_state = State.update(state, fn data ->
          Map.put(data, :processing_state, :error)
        end)
        
        {error_state, {:error, reason}}
    end
  end

  defp process_heartbeat(signal, state) do
    timestamp = signal.payload[:timestamp] || DateTime.utc_now()
    
    new_state = State.update(state, fn data ->
      Map.put(data, :last_heartbeat, timestamp)
    end)
    
    {new_state, {:ok, :heartbeat_processed}}
  end

  defp handle_error_recovery(signal, state) do
    error_type = signal.payload[:error_type]
    
    new_state = State.update(state, fn data ->
      data
      |> Map.put(:processing_state, :ready)
      |> Map.put(:last_error_recovery, DateTime.utc_now())
    end)
    
    {new_state, {:ok, {:recovered_from, error_type}}}
  end

  # Guard condition functions (pure)
  defp validate_threat_format(signal, _state) do
    required_fields = [:threat_id, :severity, :source]
    Enum.all?(required_fields, &Map.has_key?(signal.payload, &1))
  end

  defp check_ttl_budget(signal, _state) do
    ttl_remaining = signal.ttl_constraint[:budget_ns] || 10_000_000
    ttl_remaining > 1_000_000  # At least 1ms remaining
  end

  defp validate_heartbeat_timing(signal, state) do
    last_heartbeat = State.get(state, :last_heartbeat)
    current_time = signal.payload[:timestamp] || DateTime.utc_now()
    
    case last_heartbeat do
      nil -> true  # First heartbeat
      last -> DateTime.diff(current_time, last, :millisecond) >= 1000
    end
  end

  defp validate_error_state(signal, state) do
    current_state = State.get(state, :processing_state)
    error_type = signal.payload[:error_type]
    
    current_state == :error and not is_nil(error_type)
  end

  # TTL transformation function (pure)
  defp apply_threat_ttl(signal) do
    base_ttl = signal.ttl_constraint[:budget_ns] || 10_000_000
    severity = signal.payload[:severity] || 1
    
    # Higher severity threats get more processing time
    adjusted_ttl = base_ttl * min(severity, 3)
    
    put_in(signal.ttl_constraint[:budget_ns], adjusted_ttl)
  end

  # Threat processing pipeline functions (pure)
  defp run_threat_pipeline(signal) do
    Maybe.some(signal)
    |> Maybe.bind(&validate_threat_input/1)
    |> Maybe.bind(&extract_threat_features/1)  
    |> Maybe.bind(&calculate_severity_score/1)
    |> Maybe.bind(&generate_response/1)
    |> case do
      %Maybe{value: nil} -> {:error, :pipeline_failed}
      %Maybe{value: result} -> {:ok, result}
    end
  end

  defp validate_threat_input(signal) do
    if Map.has_key?(signal.payload, :threat_id) do
      Maybe.some(signal)
    else
      Maybe.none()
    end
  end

  defp extract_threat_features(signal) do
    features = %{
      threat_id: signal.payload[:threat_id],
      severity: signal.payload[:severity] || 1,
      source: signal.payload[:source] || "unknown",
      timestamp: DateTime.utc_now()
    }
    
    Maybe.some(Map.put(signal, :features, features))
  end

  defp calculate_severity_score(signal) do
    base_severity = signal.features[:severity]
    source_multiplier = case signal.features[:source] do
      "external" -> 2.0
      "internal" -> 1.5
      _ -> 1.0
    end
    
    final_score = base_severity * source_multiplier
    
    Maybe.some(put_in(signal.features[:calculated_severity], final_score))
  end

  defp generate_response(signal) do
    response = %{
      threat_id: signal.features[:threat_id],
      severity_score: signal.features[:calculated_severity],
      recommended_action: determine_action(signal.features[:calculated_severity]),
      processed_at: DateTime.utc_now()
    }
    
    Maybe.some(response)
  end

  # Helper functions (pure)
  defp determine_action(severity) when severity >= 5.0, do: :immediate_response
  defp determine_action(severity) when severity >= 3.0, do: :escalate
  defp determine_action(severity) when severity >= 1.5, do: :monitor
  defp determine_action(_), do: :log

  defp calculate_severity_average(data) do
    processed = Map.get(data, :threats_processed, 0)
    total_severity = Map.get(data, :threat_severity_sum, 0)
    
    if processed > 0, do: total_severity / processed, else: 0.0
  end

  defp update_severity_data(data, {new_sum, new_count}) do
    data
    |> Map.put(:threat_severity_sum, new_sum)
    |> Map.put(:threats_processed, new_count)
  end

  # Monadic operations
  defp bind_either({:ok, value}, f), do: f.(value)
  defp bind_either({:error, _} = error, _f), do: error

  defp return_either(value), do: {:ok, value}

  defp handle_maybe_error(_error), do: Maybe.none()
  defp transform_either_error(error), do: {:error, error}

  # State update functions (pure)
  defp increment_counter(data), do: Map.update(data, :threats_processed, 0, &(&1 + 1))
  defp update_severity_sum(data), do: data  # Implementation would update sum
  defp set_last_threat_id(data), do: data   # Implementation would set ID
  defp transition_to_completed(data), do: Map.put(data, :processing_state, :completed)
end