defmodule CNSForge.TTLParser.Streaming do
  @moduledoc """
  Streaming TTL Parser Variant - Incremental RDF Processing
  
  This variant explores streaming parsing patterns with:
  - Incremental TTL parsing with backpressure
  - Stream-based constraint extraction
  - Memory-efficient large ontology processing
  - Real-time TTL constraint validation
  - Chunked processing with nanosecond precision
  - Event-driven parsing with TTL deadline enforcement
  """
  
  use GenServer
  require Logger
  
  alias CNSForge.TTLParser.Streaming.{Lexer, Parser, ConstraintExtractor, StreamBuffer}
  
  # Streaming Parser State
  defstruct [
    :parser_id,
    :stream_buffer,
    :parsing_state,
    :constraint_accumulator,
    :ttl_deadline_ns,
    :chunk_size,
    :backpressure_threshold,
    :streaming_metrics,
    :error_recovery_mode,
    :incremental_results
  ]
  
  @type parsing_state :: :idle | :streaming | :paused | :backpressure | :deadline_exceeded | :completed
  
  @type stream_chunk :: %{
    data: binary(),
    sequence: non_neg_integer(),
    timestamp_ns: non_neg_integer(),
    size_bytes: non_neg_integer()
  }
  
  @type constraint_event :: %{
    type: :constraint_discovered | :constraint_validated | :constraint_violation,
    constraint: map(),
    timestamp_ns: non_neg_integer(),
    source_location: {non_neg_integer(), non_neg_integer()}
  }
  
  # =============================================================================
  # Streaming Parser API
  # =============================================================================
  
  @spec start_streaming_parser(keyword()) :: {:ok, pid()} | {:error, term()}
  def start_streaming_parser(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: via_parser_id(opts[:parser_id]))
  end
  
  @spec stream_ttl_chunk(pid(), binary()) :: :ok | {:error, term()}
  def stream_ttl_chunk(parser_pid, ttl_chunk) when is_binary(ttl_chunk) do
    chunk = create_stream_chunk(ttl_chunk)
    GenServer.cast(parser_pid, {:stream_chunk, chunk})
  end
  
  @spec stream_ttl_file(pid(), String.t()) :: {:ok, reference()} | {:error, term()}
  def stream_ttl_file(parser_pid, file_path) do
    GenServer.call(parser_pid, {:stream_file, file_path})
  end
  
  @spec get_streaming_constraints(pid()) :: {:ok, [map()]} | {:error, term()}
  def get_streaming_constraints(parser_pid) do
    GenServer.call(parser_pid, :get_streaming_constraints)
  end
  
  @spec set_ttl_deadline(pid(), non_neg_integer()) :: :ok
  def set_ttl_deadline(parser_pid, deadline_ns) do
    GenServer.cast(parser_pid, {:set_ttl_deadline, deadline_ns})
  end
  
  @spec pause_streaming(pid()) :: :ok
  def pause_streaming(parser_pid) do
    GenServer.cast(parser_pid, :pause_streaming)
  end
  
  @spec resume_streaming(pid()) :: :ok
  def resume_streaming(parser_pid) do
    GenServer.cast(parser_pid, :resume_streaming)
  end
  
  @spec get_streaming_metrics(pid()) :: {:ok, map()} | {:error, term()}
  def get_streaming_metrics(parser_pid) do
    GenServer.call(parser_pid, :get_streaming_metrics)
  end
  
  # =============================================================================
  # GenServer Implementation
  # =============================================================================
  
  @impl GenServer
  def init(opts) do
    state = %__MODULE__{
      parser_id: Keyword.get(opts, :parser_id, UUID.uuid4()),
      stream_buffer: StreamBuffer.new(Keyword.get(opts, :buffer_size, 1024 * 1024)),  # 1MB buffer
      parsing_state: :idle,
      constraint_accumulator: ConstraintExtractor.new(),
      ttl_deadline_ns: Keyword.get(opts, :ttl_deadline_ns, 30_000_000_000),  # 30 seconds
      chunk_size: Keyword.get(opts, :chunk_size, 8192),  # 8KB chunks
      backpressure_threshold: Keyword.get(opts, :backpressure_threshold, 0.8),
      streaming_metrics: initialize_streaming_metrics(),
      error_recovery_mode: Keyword.get(opts, :error_recovery_mode, :skip_invalid),
      incremental_results: []
    }
    
    Logger.info("Streaming TTL parser started: #{state.parser_id}")
    {:ok, state}
  end
  
  @impl GenServer
  def handle_cast({:stream_chunk, chunk}, state) do
    case state.parsing_state do
      :paused ->
        # Queue chunk for later processing
        updated_buffer = StreamBuffer.enqueue(state.stream_buffer, chunk)
        {:noreply, %{state | stream_buffer: updated_buffer}}
      
      :backpressure ->
        # Drop chunk or apply backpressure
        Logger.warning("Backpressure active, dropping chunk")
        {:noreply, update_streaming_metrics(state, :chunks_dropped, 1)}
      
      _ ->
        # Process chunk immediately
        handle_streaming_chunk(chunk, state)
    end
  end
  
  @impl GenServer
  def handle_cast({:set_ttl_deadline, deadline_ns}, state) do
    Logger.info("TTL deadline updated: #{deadline_ns}ns")
    {:noreply, %{state | ttl_deadline_ns: deadline_ns}}
  end
  
  @impl GenServer
  def handle_cast(:pause_streaming, state) do
    Logger.info("Streaming parser paused")
    {:noreply, %{state | parsing_state: :paused}}
  end
  
  @impl GenServer
  def handle_cast(:resume_streaming, state) do
    Logger.info("Streaming parser resumed")
    new_state = %{state | parsing_state: :streaming}
    
    # Process any queued chunks
    process_queued_chunks(new_state)
  end
  
  @impl GenServer
  def handle_call({:stream_file, file_path}, from, state) do
    # Start streaming file in background
    file_stream_ref = make_ref()
    spawn_link(fn -> stream_file_chunks(file_path, self(), file_stream_ref) end)
    
    new_state = %{state |
      parsing_state: :streaming,
      streaming_metrics: Map.put(state.streaming_metrics, :file_stream_ref, file_stream_ref)
    }
    
    {:reply, {:ok, file_stream_ref}, new_state}
  end
  
  @impl GenServer
  def handle_call(:get_streaming_constraints, _from, state) do
    constraints = ConstraintExtractor.get_all_constraints(state.constraint_accumulator)
    {:reply, {:ok, constraints}, state}
  end
  
  @impl GenServer
  def handle_call(:get_streaming_metrics, _from, state) do
    {:reply, {:ok, state.streaming_metrics}, state}
  end
  
  @impl GenServer
  def handle_info({:file_chunk, ref, chunk}, state) do
    if Map.get(state.streaming_metrics, :file_stream_ref) == ref do
      handle_streaming_chunk(chunk, state)
    else
      # Ignore chunks from old file streams
      {:noreply, state}
    end
  end
  
  @impl GenServer
  def handle_info({:file_complete, ref}, state) do
    if Map.get(state.streaming_metrics, :file_stream_ref) == ref do
      Logger.info("File streaming completed")
      finalize_streaming_parsing(state)
    else
      {:noreply, state}
    end
  end
  
  @impl GenServer
  def handle_info(:check_ttl_deadline, state) do
    current_time_ns = System.monotonic_time(:nanosecond)
    parsing_start_ns = Map.get(state.streaming_metrics, :parsing_start_ns, current_time_ns)
    elapsed_ns = current_time_ns - parsing_start_ns
    
    if elapsed_ns >= state.ttl_deadline_ns do
      Logger.error("TTL deadline exceeded during streaming parsing")
      handle_ttl_deadline_exceeded(state)
    else
      # Schedule next deadline check
      schedule_ttl_deadline_check()
      {:noreply, state}
    end
  end
  
  # =============================================================================
  # Streaming Processing Logic
  # =============================================================================
  
  defp handle_streaming_chunk(chunk, state) do
    start_time_ns = System.monotonic_time(:nanosecond)
    
    # Check backpressure
    buffer_utilization = StreamBuffer.utilization(state.stream_buffer)
    
    if buffer_utilization >= state.backpressure_threshold do
      Logger.warning("Backpressure threshold reached: #{buffer_utilization}")
      new_state = %{state | parsing_state: :backpressure}
      {:noreply, new_state}
    else
      # Process chunk with TTL enforcement
      case process_chunk_with_ttl(chunk, state, start_time_ns) do
        {:ok, updated_state} ->
          {:noreply, updated_state}
        
        {:error, :ttl_exceeded} ->
          handle_ttl_deadline_exceeded(state)
        
        {:error, reason} ->
          handle_parsing_error(reason, chunk, state)
      end
    end
  end
  
  defp process_chunk_with_ttl(chunk, state, start_time_ns) do
    # Check TTL deadline before processing
    current_time_ns = System.monotonic_time(:nanosecond)
    parsing_start_ns = Map.get(state.streaming_metrics, :parsing_start_ns, start_time_ns)
    elapsed_ns = current_time_ns - parsing_start_ns
    
    if elapsed_ns >= state.ttl_deadline_ns do
      {:error, :ttl_exceeded}
    else
      # Parse chunk incrementally
      case Lexer.tokenize_chunk(chunk.data) do
        {:ok, tokens} ->
          process_tokens_streaming(tokens, chunk, state, start_time_ns)
        
        {:error, reason} ->
          {:error, reason}
      end
    end
  end
  
  defp process_tokens_streaming(tokens, chunk, state, start_time_ns) do
    # Process tokens with streaming parser
    parsing_result = Parser.parse_tokens_streaming(
      tokens,
      state.constraint_accumulator,
      %{
        chunk_info: chunk,
        ttl_budget_ns: state.ttl_deadline_ns,
        start_time_ns: start_time_ns
      }
    )
    
    case parsing_result do
      {:ok, {updated_accumulator, constraint_events}} ->
        # Update state with parsed results
        end_time_ns = System.monotonic_time(:nanosecond)
        chunk_processing_time = end_time_ns - start_time_ns
        
        updated_state = %{state |
          constraint_accumulator: updated_accumulator,
          incremental_results: constraint_events ++ state.incremental_results,
          streaming_metrics: update_chunk_metrics(state.streaming_metrics, chunk, chunk_processing_time)
        }
        
        # Emit constraint events
        Enum.each(constraint_events, fn event ->
          emit_constraint_event(event, state.parser_id)
        end)
        
        {:ok, updated_state}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp process_queued_chunks(state) do
    case StreamBuffer.dequeue(state.stream_buffer) do
      {:ok, chunk, updated_buffer} ->
        updated_state = %{state | stream_buffer: updated_buffer}
        
        case handle_streaming_chunk(chunk, updated_state) do
          {:noreply, new_state} ->
            # Continue processing queued chunks
            process_queued_chunks(new_state)
          
          error ->
            error
        end
      
      {:empty, _buffer} ->
        {:noreply, state}
    end
  end
  
  defp finalize_streaming_parsing(state) do
    # Finalize any remaining parsing state
    final_constraints = ConstraintExtractor.finalize(state.constraint_accumulator)
    
    final_metrics = state.streaming_metrics
    |> Map.put(:parsing_completed_at, System.monotonic_time(:nanosecond))
    |> Map.put(:total_constraints_extracted, length(final_constraints))
    
    final_state = %{state |
      parsing_state: :completed,
      streaming_metrics: final_metrics
    }
    
    Logger.info("Streaming TTL parsing completed: #{length(final_constraints)} constraints extracted")
    {:noreply, final_state}
  end
  
  # =============================================================================
  # TTL Deadline Handling
  # =============================================================================
  
  defp handle_ttl_deadline_exceeded(state) do
    Logger.error("TTL deadline exceeded for streaming parser #{state.parser_id}")
    
    # Attempt graceful degradation
    partial_constraints = ConstraintExtractor.get_partial_constraints(state.constraint_accumulator)
    
    deadline_exceeded_state = %{state |
      parsing_state: :deadline_exceeded,
      incremental_results: partial_constraints,
      streaming_metrics: Map.put(state.streaming_metrics, :ttl_exceeded, true)
    }
    
    {:noreply, deadline_exceeded_state}
  end
  
  defp schedule_ttl_deadline_check() do
    Process.send_after(self(), :check_ttl_deadline, 1000)  # Check every second
  end
  
  # =============================================================================
  # Error Handling
  # =============================================================================
  
  defp handle_parsing_error(reason, chunk, state) do
    Logger.warning("Parsing error for chunk #{chunk.sequence}: #{inspect(reason)}")
    
    case state.error_recovery_mode do
      :skip_invalid ->
        # Skip this chunk and continue
        updated_metrics = update_streaming_metrics(state.streaming_metrics, :chunks_skipped, 1)
        {:noreply, %{state | streaming_metrics: updated_metrics}}
      
      :halt_on_error ->
        # Stop streaming
        error_state = %{state | parsing_state: :error}
        {:noreply, error_state}
      
      :partial_recovery ->
        # Try to extract partial constraints from chunk
        case attempt_partial_chunk_recovery(chunk, state) do
          {:ok, partial_constraints} ->
            updated_accumulator = ConstraintExtractor.add_partial_constraints(
              state.constraint_accumulator,
              partial_constraints
            )
            
            updated_state = %{state | constraint_accumulator: updated_accumulator}
            {:noreply, updated_state}
          
          {:error, _} ->
            # Fall back to skipping
            updated_metrics = update_streaming_metrics(state.streaming_metrics, :chunks_skipped, 1)
            {:noreply, %{state | streaming_metrics: updated_metrics}}
        end
    end
  end
  
  defp attempt_partial_chunk_recovery(chunk, _state) do
    # Attempt to extract constraints from malformed chunk
    # This is a simplified implementation
    case String.contains?(chunk.data, "bitactor:") do
      true ->
        # Try to extract basic BitActor constraints
        constraints = extract_basic_bitactor_constraints(chunk.data)
        {:ok, constraints}
      
      false ->
        {:error, :no_recoverable_constraints}
    end
  end
  
  defp extract_basic_bitactor_constraints(data) do
    # Basic pattern matching for BitActor constraints
    budget_pattern = ~r/bitactor:budgetNs\s+(\d+)/
    precision_pattern = ~r/bitactor:precision\s+"(\w+)"/
    
    budget_matches = Regex.scan(budget_pattern, data)
    precision_matches = Regex.scan(precision_pattern, data)
    
    constraints = []
    
    constraints = if length(budget_matches) > 0 do
      [_, budget_str] = hd(budget_matches)
      budget_ns = String.to_integer(budget_str)
      
      [%{
        type: :ttl_constraint,
        property: :budget_ns,
        value: budget_ns,
        source: :partial_recovery
      } | constraints]
    else
      constraints
    end
    
    constraints = if length(precision_matches) > 0 do
      [_, precision_str] = hd(precision_matches)
      
      [%{
        type: :ttl_constraint,
        property: :precision,
        value: precision_str,
        source: :partial_recovery
      } | constraints]
    else
      constraints
    end
    
    constraints
  end
  
  # =============================================================================
  # Streaming Utilities
  # =============================================================================
  
  defp create_stream_chunk(data) when is_binary(data) do
    %{
      data: data,
      sequence: :erlang.unique_integer([:positive]),
      timestamp_ns: System.monotonic_time(:nanosecond),
      size_bytes: byte_size(data)
    }
  end
  
  defp stream_file_chunks(file_path, parser_pid, ref) do
    case File.open(file_path, [:read, :binary]) do
      {:ok, file} ->
        stream_file_chunks_loop(file, parser_pid, ref, 0)
        File.close(file)
        send(parser_pid, {:file_complete, ref})
      
      {:error, reason} ->
        Logger.error("Failed to open file #{file_path}: #{inspect(reason)}")
    end
  end
  
  defp stream_file_chunks_loop(file, parser_pid, ref, sequence) do
    case IO.binread(file, 8192) do  # 8KB chunks
      :eof ->
        :ok
      
      {:error, reason} ->
        Logger.error("File read error: #{inspect(reason)}")
      
      data ->
        chunk = %{
          data: data,
          sequence: sequence,
          timestamp_ns: System.monotonic_time(:nanosecond),
          size_bytes: byte_size(data)
        }
        
        send(parser_pid, {:file_chunk, ref, chunk})
        stream_file_chunks_loop(file, parser_pid, ref, sequence + 1)
    end
  end
  
  defp initialize_streaming_metrics() do
    %{
      parsing_start_ns: System.monotonic_time(:nanosecond),
      chunks_processed: 0,
      chunks_skipped: 0,
      chunks_dropped: 0,
      total_bytes_processed: 0,
      constraints_discovered: 0,
      average_chunk_processing_ns: 0,
      backpressure_events: 0,
      ttl_exceeded: false
    }
  end
  
  defp update_streaming_metrics(metrics, :chunks_dropped, count) do
    Map.update(metrics, :chunks_dropped, count, &(&1 + count))
  end
  
  defp update_streaming_metrics(metrics, :chunks_skipped, count) do
    Map.update(metrics, :chunks_skipped, count, &(&1 + count))
  end
  
  defp update_chunk_metrics(metrics, chunk, processing_time_ns) do
    current_processed = Map.get(metrics, :chunks_processed, 0)
    current_avg = Map.get(metrics, :average_chunk_processing_ns, 0)
    
    new_processed = current_processed + 1
    new_avg = div((current_avg * current_processed) + processing_time_ns, new_processed)
    
    metrics
    |> Map.put(:chunks_processed, new_processed)
    |> Map.put(:average_chunk_processing_ns, new_avg)
    |> Map.update(:total_bytes_processed, chunk.size_bytes, &(&1 + chunk.size_bytes))
  end
  
  defp emit_constraint_event(event, parser_id) do
    # Emit constraint discovery event
    Logger.debug("Constraint event: #{event.type} from parser #{parser_id}")
    
    # In real implementation, would emit to event bus
    GenServer.cast(:constraint_event_handler, {:constraint_event, parser_id, event})
  end
  
  defp via_parser_id(nil), do: __MODULE__
  defp via_parser_id(parser_id), do: {:via, Registry, {BitActor.StreamingParserRegistry, parser_id}}
  
  # =============================================================================
  # Supporting Modules for Streaming Components
  # =============================================================================
  
  defmodule CNSForge.TTLParser.Streaming.StreamBuffer do
    @moduledoc "Ring buffer for streaming chunks with backpressure"
    
    defstruct [:buffer, :head, :tail, :size, :capacity, :count]
    
    def new(capacity \\ 1024) do
      %__MODULE__{
        buffer: :array.new(capacity, default: nil),
        head: 0,
        tail: 0,
        size: capacity,
        capacity: capacity,
        count: 0
      }
    end
    
    def enqueue(%__MODULE__{count: count, capacity: capacity} = buffer, _item) when count >= capacity do
      {:error, :buffer_full}
    end
    
    def enqueue(%__MODULE__{} = buffer, item) do
      updated_array = :array.set(buffer.tail, item, buffer.buffer)
      new_tail = rem(buffer.tail + 1, buffer.capacity)
      
      %{buffer |
        buffer: updated_array,
        tail: new_tail,
        count: buffer.count + 1
      }
    end
    
    def dequeue(%__MODULE__{count: 0} = buffer) do
      {:empty, buffer}
    end
    
    def dequeue(%__MODULE__{} = buffer) do
      item = :array.get(buffer.head, buffer.buffer)
      new_head = rem(buffer.head + 1, buffer.capacity)
      
      updated_buffer = %{buffer |
        head: new_head,
        count: buffer.count - 1
      }
      
      {:ok, item, updated_buffer}
    end
    
    def utilization(%__MODULE__{count: count, capacity: capacity}) do
      count / capacity
    end
  end
  
  defmodule CNSForge.TTLParser.Streaming.Lexer do
    @moduledoc "Streaming lexer for TTL tokens"
    
    def tokenize_chunk(data) when is_binary(data) do
      # Simplified TTL tokenization
      tokens = data
      |> String.split(~r/\s+/)
      |> Enum.reject(&(&1 == ""))
      |> Enum.with_index()
      |> Enum.map(fn {token, index} ->
        %{
          type: classify_token(token),
          value: token,
          position: index
        }
      end)
      
      {:ok, tokens}
    end
    
    defp classify_token(token) do
      cond do
        String.starts_with?(token, "@prefix") -> :prefix_directive
        String.starts_with?(token, "bitactor:") -> :bitactor_property
        String.ends_with?(token, ">") -> :iri
        String.starts_with?(token, "\"") -> :literal
        String.contains?(token, ":") -> :prefixed_name
        String.match?(token, ~r/^\d+$/) -> :integer
        String.match?(token, ~r/^\d+\.\d+$/) -> :decimal
        true -> :unknown
      end
    end
  end
  
  defmodule CNSForge.TTLParser.Streaming.Parser do
    @moduledoc "Streaming RDF parser with constraint extraction"
    
    def parse_tokens_streaming(tokens, accumulator, context) do
      start_time_ns = Map.get(context, :start_time_ns)
      ttl_budget_ns = Map.get(context, :ttl_budget_ns)
      
      # Parse tokens with TTL enforcement
      {updated_accumulator, constraint_events, parsing_time} = 
        parse_tokens_with_timing(tokens, accumulator, start_time_ns)
      
      if parsing_time > ttl_budget_ns do
        {:error, :ttl_exceeded}
      else
        {:ok, {updated_accumulator, constraint_events}}
      end
    end
    
    defp parse_tokens_with_timing(tokens, accumulator, start_time_ns) do
      constraint_events = []
      
      # Simplified parsing - look for BitActor constraint patterns
      constraint_events = Enum.reduce(tokens, constraint_events, fn token, events ->
        case extract_constraint_from_token(token) do
          {:ok, constraint} ->
            event = %{
              type: :constraint_discovered,
              constraint: constraint,
              timestamp_ns: System.monotonic_time(:nanosecond),
              source_location: {token.position, token.position}
            }
            [event | events]
          
          :none ->
            events
        end
      end)
      
      end_time_ns = System.monotonic_time(:nanosecond)
      parsing_time = end_time_ns - start_time_ns
      
      # Update accumulator with discovered constraints
      updated_accumulator = add_constraints_to_accumulator(accumulator, constraint_events)
      
      {updated_accumulator, Enum.reverse(constraint_events), parsing_time}
    end
    
    defp extract_constraint_from_token(token) do
      case token.type do
        :bitactor_property ->
          case token.value do
            "bitactor:budgetNs" -> {:ok, %{type: :ttl_budget, property: :budget_ns}}
            "bitactor:precision" -> {:ok, %{type: :ttl_precision, property: :precision}}
            "bitactor:maxBudgetMs" -> {:ok, %{type: :ttl_max_budget, property: :max_budget_ms}}
            _ -> :none
          end
        
        :integer ->
          # Could be a constraint value
          {:ok, %{type: :constraint_value, value: String.to_integer(token.value)}}
        
        _ ->
          :none
      end
    end
    
    defp add_constraints_to_accumulator(accumulator, constraint_events) do
      Enum.reduce(constraint_events, accumulator, fn event, acc ->
        ConstraintExtractor.add_constraint(acc, event.constraint)
      end)
    end
  end
  
  defmodule CNSForge.TTLParser.Streaming.ConstraintExtractor do
    @moduledoc "Accumulates and validates TTL constraints during streaming"
    
    defstruct [:constraints, :partial_constraints, :validation_rules]
    
    def new() do
      %__MODULE__{
        constraints: [],
        partial_constraints: [],
        validation_rules: default_validation_rules()
      }
    end
    
    def add_constraint(%__MODULE__{} = extractor, constraint) do
      case validate_constraint(constraint, extractor.validation_rules) do
        {:ok, validated_constraint} ->
          %{extractor | constraints: [validated_constraint | extractor.constraints]}
        
        {:error, _reason} ->
          %{extractor | partial_constraints: [constraint | extractor.partial_constraints]}
      end
    end
    
    def add_partial_constraints(%__MODULE__{} = extractor, partial_constraints) do
      %{extractor | partial_constraints: partial_constraints ++ extractor.partial_constraints}
    end
    
    def get_all_constraints(%__MODULE__{constraints: constraints}) do
      constraints
    end
    
    def get_partial_constraints(%__MODULE__{constraints: constraints, partial_constraints: partial}) do
      constraints ++ partial
    end
    
    def finalize(%__MODULE__{constraints: constraints}) do
      # Apply final validation and cleanup
      Enum.filter(constraints, &is_complete_constraint?/1)
    end
    
    defp default_validation_rules() do
      %{
        ttl_budget: %{min: 1, max: 1_000_000_000},  # 1ns to 1s
        ttl_precision: %{allowed: ["nanosecond", "microsecond", "millisecond"]},
        ttl_max_budget: %{min: 1, max: 300_000}  # 1ms to 5 minutes
      }
    end
    
    defp validate_constraint(constraint, rules) do
      case constraint.type do
        :ttl_budget ->
          validate_numeric_constraint(constraint, Map.get(rules, :ttl_budget))
        
        :ttl_precision ->
          validate_enum_constraint(constraint, Map.get(rules, :ttl_precision))
        
        :ttl_max_budget ->
          validate_numeric_constraint(constraint, Map.get(rules, :ttl_max_budget))
        
        _ ->
          {:ok, constraint}  # Pass through unknown constraints
      end
    end
    
    defp validate_numeric_constraint(constraint, %{min: min, max: max}) do
      case constraint.value do
        value when is_integer(value) and value >= min and value <= max ->
          {:ok, Map.put(constraint, :validated, true)}
        
        _ ->
          {:error, :invalid_numeric_value}
      end
    end
    
    defp validate_enum_constraint(constraint, %{allowed: allowed_values}) do
      if constraint.value in allowed_values do
        {:ok, Map.put(constraint, :validated, true)}
      else
        {:error, :invalid_enum_value}
      end
    end
    
    defp is_complete_constraint?(constraint) do
      Map.get(constraint, :validated, false)
    end
  end
end