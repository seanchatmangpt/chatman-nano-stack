defmodule CnsForge.Generators.Patterns.RecursivePattern do
  @moduledoc """
  🔄 RECURSIVE PATTERN ENGINE: Core patterns for infinite generation
  
  This module implements fundamental recursive patterns that can be applied
  to any generator, creating infinite depth and self-referential capabilities.
  
  ## Supported Patterns
  
  1. **Fractal**: Self-similar structures at different scales
  2. **Infinite**: Unlimited depth generation with convergence
  3. **Self-Referential**: Structures that reference themselves
  4. **Meta**: Patterns that generate other patterns
  5. **Adaptive**: Patterns that learn and evolve
  
  ## Pattern Composition
  
  Patterns can be composed together to create complex recursive behaviors:
  
      fractal_pattern = RecursivePattern.fractal(scale: 0.5, depth: 10)
      infinite_pattern = RecursivePattern.infinite(convergence: :similarity)
      
      composed = RecursivePattern.compose([fractal_pattern, infinite_pattern])
      RecursivePattern.apply(data, composed)
  """
  
  alias CnsForge.Generators.Patterns.RecursivePattern
  
  @type pattern :: %{
    type: atom(),
    config: map(),
    depth: non_neg_integer(),
    metadata: map()
  }
  
  @type application_result :: {:ok, term()} | {:error, term()} | {:continue, term()}
  
  defstruct [
    :type,
    :config,
    :depth,
    :metadata,
    :composition_rules,
    :convergence_criteria,
    :learning_enabled
  ]
  
  ## Pattern Constructors
  
  @doc """
  Create a fractal pattern that generates self-similar structures
  """
  @spec fractal(keyword()) :: pattern()
  def fractal(opts \\ []) do
    %RecursivePattern{
      type: :fractal,
      config: %{
        scale_factor: Keyword.get(opts, :scale, 0.5),
        max_depth: Keyword.get(opts, :depth, 10),
        similarity_threshold: Keyword.get(opts, :similarity, 0.8),
        branch_factor: Keyword.get(opts, :branches, 2)
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id()
      }
    }
  end
  
  @doc """
  Create an infinite pattern that can generate unlimited variations
  """
  @spec infinite(keyword()) :: pattern()
  def infinite(opts \\ []) do
    %RecursivePattern{
      type: :infinite,
      config: %{
        convergence: Keyword.get(opts, :convergence, :never),
        variation_function: Keyword.get(opts, :variation, &default_variation/1),
        termination_condition: Keyword.get(opts, :termination, &never_terminate/1),
        memory_limit: Keyword.get(opts, :memory_limit, :unlimited)
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id()
      }
    }
  end
  
  @doc """
  Create a self-referential pattern that references itself
  """
  @spec self_referential(keyword()) :: pattern()
  def self_referential(opts \\ []) do
    %RecursivePattern{
      type: :self_referential,
      config: %{
        reference_depth: Keyword.get(opts, :reference_depth, 3),
        circular_detection: Keyword.get(opts, :circular_detection, true),
        break_cycles: Keyword.get(opts, :break_cycles, true),
        self_modification: Keyword.get(opts, :self_modification, false)
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id(),
        self_references: []
      }
    }
  end
  
  @doc """
  Create a meta-pattern that generates other patterns
  """
  @spec meta(keyword()) :: pattern()
  def meta(opts \\ []) do
    %RecursivePattern{
      type: :meta,
      config: %{
        target_patterns: Keyword.get(opts, :targets, [:fractal, :infinite]),
        composition_strategy: Keyword.get(opts, :composition, :sequential),
        pattern_evolution: Keyword.get(opts, :evolution, false),
        meta_depth: Keyword.get(opts, :meta_depth, 2)
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id(),
        generated_patterns: []
      }
    }
  end
  
  @doc """
  Create an adaptive pattern that learns and evolves
  """
  @spec adaptive(keyword()) :: pattern()
  def adaptive(opts \\ []) do
    %RecursivePattern{
      type: :adaptive,
      config: %{
        learning_rate: Keyword.get(opts, :learning_rate, 0.1),
        adaptation_threshold: Keyword.get(opts, :adaptation_threshold, 0.7),
        memory_size: Keyword.get(opts, :memory_size, 100),
        evolution_strategy: Keyword.get(opts, :evolution, :gradient_based)
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id(),
        learning_history: [],
        adaptation_count: 0
      },
      learning_enabled: true
    }
  end
  
  ## Pattern Application
  
  @doc """
  Apply a recursive pattern to data
  """
  @spec apply(term(), pattern()) :: application_result()
  def apply(data, pattern) do
    case pattern.type do
      :fractal -> apply_fractal(data, pattern)
      :infinite -> apply_infinite(data, pattern)
      :self_referential -> apply_self_referential(data, pattern)
      :meta -> apply_meta(data, pattern)
      :adaptive -> apply_adaptive(data, pattern)
      _ -> {:error, {:unknown_pattern_type, pattern.type}}
    end
  end
  
  @doc """
  Apply pattern recursively with depth tracking
  """
  @spec apply_recursive(term(), pattern(), non_neg_integer()) :: application_result()
  def apply_recursive(data, pattern, depth \\ 0) do
    updated_pattern = %{pattern | depth: depth}
    
    case should_continue_recursion?(data, updated_pattern) do
      true ->
        case apply(data, updated_pattern) do
          {:ok, result} ->
            if should_recurse_further?(result, updated_pattern) do
              apply_recursive(result, updated_pattern, depth + 1)
            else
              {:ok, result}
            end
          
          {:continue, intermediate_result} ->
            apply_recursive(intermediate_result, updated_pattern, depth + 1)
          
          error -> error
        end
      
      false ->
        {:ok, data}
    end
  end
  
  @doc """
  Compose multiple patterns together
  """
  @spec compose([pattern()]) :: pattern()
  def compose(patterns) when is_list(patterns) do
    %RecursivePattern{
      type: :composed,
      config: %{
        patterns: patterns,
        composition_order: :sequential,
        failure_strategy: :stop_on_error
      },
      depth: 0,
      metadata: %{
        created_at: DateTime.utc_now(),
        pattern_id: generate_pattern_id(),
        composed_from: Enum.map(patterns, & &1.metadata.pattern_id)
      }
    }
  end
  
  @doc """
  Apply composed patterns
  """
  @spec apply_composed(term(), pattern()) :: application_result()
  def apply_composed(data, %{type: :composed, config: %{patterns: patterns}}) do
    Enum.reduce_while(patterns, {:ok, data}, fn pattern, {:ok, acc_data} ->
      case apply_recursive(acc_data, pattern) do
        {:ok, result} -> {:cont, {:ok, result}}
        {:continue, result} -> {:cont, {:ok, result}}
        error -> {:halt, error}
      end
    end)
  end
  
  ## Pattern-Specific Implementations
  
  defp apply_fractal(data, pattern) do
    config = pattern.config
    depth = pattern.depth
    
    if depth >= config.max_depth do
      {:ok, data}
    else
      # Generate fractal structure
      scaled_data = scale_data(data, config.scale_factor)
      
      # Create branches
      branches = 
        1..config.branch_factor
        |> Enum.map(fn _branch ->
          branch_pattern = %{pattern | depth: depth + 1}
          case apply_fractal(scaled_data, branch_pattern) do
            {:ok, result} -> result
            _ -> scaled_data
          end
        end)
      
      # Combine fractal elements
      fractal_structure = combine_fractal_elements(scaled_data, branches)
      
      if fractal_converged?(fractal_structure, config.similarity_threshold) do
        {:ok, fractal_structure}
      else
        {:continue, fractal_structure}
      end
    end
  end
  
  defp apply_infinite(data, pattern) do
    config = pattern.config
    
    case config.termination_condition.(data) do
      true -> {:ok, data}
      false ->
        # Apply variation function
        varied_data = config.variation_function.(data)
        
        # Check convergence
        if config.convergence != :never and converged?(data, varied_data, config.convergence) do
          {:ok, varied_data}
        else
          {:continue, varied_data}
        end
    end
  end
  
  defp apply_self_referential(data, pattern) do
    config = pattern.config
    depth = pattern.depth
    
    if depth >= config.reference_depth do
      {:ok, data}
    else
      # Add self-reference
      self_ref_data = add_self_reference(data, pattern)
      
      # Check for circular references
      if config.circular_detection and has_circular_reference?(self_ref_data, pattern) do
        if config.break_cycles do
          {:ok, break_circular_reference(self_ref_data)}
        else
          {:error, :circular_reference_detected}
        end
      else
        # Continue recursion with self-reference
        {:continue, self_ref_data}
      end
    end
  end
  
  defp apply_meta(data, pattern) do
    config = pattern.config
    
    # Generate patterns based on target patterns
    generated_patterns = 
      config.target_patterns
      |> Enum.map(&generate_pattern_from_template(&1, data))
      |> Enum.filter(fn {:ok, _} -> true; _ -> false end)
      |> Enum.map(fn {:ok, p} -> p end)
    
    # Apply generated patterns based on composition strategy
    case config.composition_strategy do
      :sequential ->
        apply_patterns_sequentially(data, generated_patterns)
      :parallel ->
        apply_patterns_in_parallel(data, generated_patterns)
      :adaptive ->
        apply_patterns_adaptively(data, generated_patterns)
    end
  end
  
  defp apply_adaptive(data, pattern) do
    config = pattern.config
    metadata = pattern.metadata
    
    # Learn from previous applications
    if pattern.learning_enabled and length(metadata.learning_history) > 0 do
      # Adapt pattern based on learning history
      adapted_config = adapt_configuration(config, metadata.learning_history)
      adapted_pattern = %{pattern | config: adapted_config}
      
      # Apply adapted pattern
      result = apply_base_adaptive_transformation(data, adapted_pattern)
      
      # Update learning history
      updated_metadata = update_learning_history(metadata, data, result)
      updated_pattern = %{adapted_pattern | metadata: updated_metadata}
      
      {:ok, result, updated_pattern}
    else
      # First application - no learning yet
      result = apply_base_adaptive_transformation(data, pattern)
      
      # Initialize learning history
      initial_metadata = initialize_learning_history(metadata, data, result)
      updated_pattern = %{pattern | metadata: initial_metadata}
      
      {:ok, result, updated_pattern}
    end
  end
  
  ## Helper Functions
  
  defp generate_pattern_id do
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end
  
  defp should_continue_recursion?(data, pattern) do
    # Basic recursion continuation logic
    case pattern.type do
      :fractal -> pattern.depth < pattern.config.max_depth
      :infinite -> not pattern.config.termination_condition.(data)
      :self_referential -> pattern.depth < pattern.config.reference_depth
      :meta -> pattern.depth < pattern.config.meta_depth
      :adaptive -> true # Adaptive patterns can always continue
      _ -> true
    end
  end
  
  defp should_recurse_further?(result, pattern) do
    # Determine if recursion should continue based on result
    case pattern.type do
      :fractal -> not fractal_converged?(result, pattern.config.similarity_threshold)
      :infinite -> not converged?(result, result, pattern.config.convergence)
      _ -> false
    end
  end
  
  defp scale_data(data, scale_factor) when is_number(scale_factor) do
    # Generic data scaling - override in specific implementations
    cond do
      is_number(data) -> data * scale_factor
      is_binary(data) -> String.slice(data, 0, round(String.length(data) * scale_factor))
      is_list(data) -> Enum.take(data, round(length(data) * scale_factor))
      is_map(data) -> 
        keys_to_keep = data |> Map.keys() |> Enum.take(round(map_size(data) * scale_factor))
        Map.take(data, keys_to_keep)
      true -> data
    end
  end
  
  defp combine_fractal_elements(base, branches) do
    # Combine base element with fractal branches
    %{
      base: base,
      branches: branches,
      fractal_level: length(branches),
      combined_structure: merge_elements(base, branches)
    }
  end
  
  defp merge_elements(base, branches) when is_list(branches) do
    case base do
      data when is_map(data) ->
        branches
        |> Enum.with_index()
        |> Enum.reduce(data, fn {branch, index}, acc ->
          Map.put(acc, :"branch_#{index}", branch)
        end)
      
      data when is_list(data) ->
        data ++ List.flatten(branches)
      
      data ->
        [data | branches]
    end
  end
  
  defp fractal_converged?(structure, threshold) do
    # Check if fractal structure has converged
    case structure do
      %{base: base, branches: branches} ->
        similarity = calculate_similarity(base, branches)
        similarity >= threshold
      _ -> true
    end
  end
  
  defp calculate_similarity(base, branches) do
    # Calculate similarity between base and branches
    if Enum.empty?(branches) do
      1.0
    else
      similarities = Enum.map(branches, &element_similarity(base, &1))
      Enum.sum(similarities) / length(similarities)
    end
  end
  
  defp element_similarity(elem1, elem2) do
    # Basic similarity calculation - can be enhanced
    cond do
      elem1 == elem2 -> 1.0
      is_map(elem1) and is_map(elem2) -> map_similarity(elem1, elem2)
      is_list(elem1) and is_list(elem2) -> list_similarity(elem1, elem2)
      is_binary(elem1) and is_binary(elem2) -> string_similarity(elem1, elem2)
      true -> 0.0
    end
  end
  
  defp map_similarity(map1, map2) do
    common_keys = MapSet.intersection(MapSet.new(Map.keys(map1)), MapSet.new(Map.keys(map2)))
    total_keys = MapSet.union(MapSet.new(Map.keys(map1)), MapSet.new(Map.keys(map2)))
    
    if MapSet.size(total_keys) == 0 do
      1.0
    else
      MapSet.size(common_keys) / MapSet.size(total_keys)
    end
  end
  
  defp list_similarity(list1, list2) do
    common_elements = MapSet.intersection(MapSet.new(list1), MapSet.new(list2))
    total_elements = MapSet.union(MapSet.new(list1), MapSet.new(list2))
    
    if MapSet.size(total_elements) == 0 do
      1.0
    else
      MapSet.size(common_elements) / MapSet.size(total_elements)
    end
  end
  
  defp string_similarity(str1, str2) do
    # Simple string similarity using common characters
    set1 = str1 |> String.graphemes() |> MapSet.new()
    set2 = str2 |> String.graphemes() |> MapSet.new()
    
    common = MapSet.intersection(set1, set2)
    total = MapSet.union(set1, set2)
    
    if MapSet.size(total) == 0 do
      1.0
    else
      MapSet.size(common) / MapSet.size(total)
    end
  end
  
  defp default_variation(data) do
    # Default variation function - can be overridden
    case data do
      n when is_number(n) -> n + :rand.uniform() - 0.5
      str when is_binary(str) -> str <> "_variant_#{:rand.uniform(1000)}"
      list when is_list(list) -> Enum.shuffle(list)
      map when is_map(map) -> Map.put(map, :variant_id, :rand.uniform(1000))
      other -> other
    end
  end
  
  defp never_terminate(_data), do: false
  
  defp converged?(data1, data2, convergence_type) do
    case convergence_type do
      :never -> false
      :exact -> data1 == data2
      :similarity -> element_similarity(data1, data2) > 0.95
      {:threshold, threshold} -> element_similarity(data1, data2) >= threshold
      _ -> false
    end
  end
  
  defp add_self_reference(data, pattern) do
    pattern_id = pattern.metadata.pattern_id
    
    case data do
      map when is_map(map) ->
        Map.put(map, :self_reference, %{
          pattern_id: pattern_id,
          depth: pattern.depth,
          reference_type: :map_self_reference
        })
      
      list when is_list(list) ->
        self_ref = %{
          pattern_id: pattern_id,
          depth: pattern.depth,
          reference_type: :list_self_reference,
          original_data: data
        }
        [self_ref | list]
      
      other ->
        %{
          original_data: other,
          self_reference: %{
            pattern_id: pattern_id,
            depth: pattern.depth,
            reference_type: :wrapper_self_reference
          }
        }
    end
  end
  
  defp has_circular_reference?(data, pattern) do
    pattern_id = pattern.metadata.pattern_id
    existing_refs = pattern.metadata[:self_references] || []
    
    # Check if this pattern already appears in the reference chain
    pattern_id in existing_refs or detect_circular_structure(data, pattern_id)
  end
  
  defp detect_circular_structure(data, pattern_id) do
    # Detect circular references in data structure
    case data do
      %{self_reference: %{pattern_id: ^pattern_id}} -> true
      %{self_reference: %{pattern_id: other_id}} when other_id != pattern_id -> false
      map when is_map(map) ->
        Enum.any?(map, fn {_k, v} -> detect_circular_structure(v, pattern_id) end)
      list when is_list(list) ->
        Enum.any?(list, &detect_circular_structure(&1, pattern_id))
      _ -> false
    end
  end
  
  defp break_circular_reference(data) do
    # Break circular references by removing or modifying them
    case data do
      %{self_reference: _} = map ->
        Map.put(map, :self_reference, :circular_reference_broken)
      
      list when is_list(list) ->
        Enum.map(list, fn
          %{reference_type: _} = ref -> Map.put(ref, :reference_type, :broken_reference)
          other -> other
        end)
      
      other -> other
    end
  end
  
  defp generate_pattern_from_template(template_type, data) do
    # Generate new patterns based on templates and data
    case template_type do
      :fractal ->
        {:ok, fractal(scale: analyze_optimal_scale(data), depth: analyze_optimal_depth(data))}
      
      :infinite ->
        {:ok, infinite(variation: &create_variation_function(data, &1), convergence: :similarity)}
      
      :self_referential ->
        {:ok, self_referential(reference_depth: analyze_reference_depth(data))}
      
      :adaptive ->
        {:ok, adaptive(learning_rate: 0.1, memory_size: 50)}
      
      _ ->
        {:error, {:unknown_template, template_type}}
    end
  end
  
  defp apply_patterns_sequentially(data, patterns) do
    Enum.reduce_while(patterns, {:ok, data}, fn pattern, {:ok, acc_data} ->
      case apply_recursive(acc_data, pattern) do
        {:ok, result} -> {:cont, {:ok, result}}
        error -> {:halt, error}
      end
    end)
  end
  
  defp apply_patterns_in_parallel(data, patterns) do
    # Apply patterns in parallel and combine results
    results = 
      patterns
      |> Task.async_stream(fn pattern -> apply_recursive(data, pattern) end, timeout: 30_000)
      |> Enum.map(fn {:ok, result} -> result end)
      |> Enum.filter(fn {:ok, _} -> true; _ -> false end)
      |> Enum.map(fn {:ok, result} -> result end)
    
    {:ok, combine_parallel_results(results)}
  end
  
  defp apply_patterns_adaptively(data, patterns) do
    # Apply patterns adaptively based on data characteristics
    best_pattern = select_best_pattern_for_data(data, patterns)
    apply_recursive(data, best_pattern)
  end
  
  defp apply_base_adaptive_transformation(data, pattern) do
    # Base adaptive transformation logic
    config = pattern.config
    
    # Apply transformation based on evolution strategy
    case config.evolution_strategy do
      :gradient_based -> apply_gradient_transformation(data, config)
      :genetic -> apply_genetic_transformation(data, config)
      :simulated_annealing -> apply_annealing_transformation(data, config)
      _ -> data
    end
  end
  
  defp update_learning_history(metadata, input_data, result) do
    learning_entry = %{
      input: input_data,
      output: result,
      timestamp: DateTime.utc_now(),
      performance_score: calculate_performance_score(input_data, result)
    }
    
    updated_history = [learning_entry | metadata.learning_history]
    |> Enum.take(metadata[:memory_size] || 100)
    
    %{metadata | 
      learning_history: updated_history,
      adaptation_count: metadata.adaptation_count + 1
    }
  end
  
  defp initialize_learning_history(metadata, input_data, result) do
    initial_entry = %{
      input: input_data,
      output: result,
      timestamp: DateTime.utc_now(),
      performance_score: 0.5 # neutral starting score
    }
    
    %{metadata | learning_history: [initial_entry]}
  end
  
  defp adapt_configuration(config, learning_history) do
    # Adapt configuration based on learning history
    avg_performance = 
      learning_history
      |> Enum.map(& &1.performance_score)
      |> Enum.sum()
      |> Kernel./(length(learning_history))
    
    # Adjust learning rate based on performance
    new_learning_rate = 
      if avg_performance > 0.7 do
        config.learning_rate * 0.9  # Slow down if performing well
      else
        config.learning_rate * 1.1  # Speed up if performing poorly
      end
    
    %{config | learning_rate: new_learning_rate}
  end
  
  # Additional helper functions for analysis and optimization
  
  defp analyze_optimal_scale(data) do
    # Analyze data to determine optimal fractal scale
    case data do
      data when is_map(data) -> 1.0 / map_size(data)
      data when is_list(data) -> 1.0 / max(length(data), 1)
      data when is_binary(data) -> 1.0 / max(String.length(data), 1)
      _ -> 0.5 # default scale
    end
  end
  
  defp analyze_optimal_depth(data) do
    # Analyze data to determine optimal recursion depth
    case data do
      data when is_map(data) -> min(map_size(data), 10)
      data when is_list(data) -> min(length(data), 10)
      _ -> 5 # default depth
    end
  end
  
  defp analyze_reference_depth(data) do
    # Analyze data to determine optimal self-reference depth
    case data do
      data when is_map(data) -> min(map_size(data) + 2, 5)
      _ -> 3 # default reference depth
    end
  end
  
  defp create_variation_function(original_data, current_data) do
    # Create a variation function based on original data characteristics
    fn data ->
      case original_data do
        data when is_map(data) -> add_random_map_key(data)
        data when is_list(data) -> add_random_list_element(data)
        _ -> default_variation(data)
      end
    end
  end
  
  defp add_random_map_key(data) when is_map(data) do
    random_key = :"random_#{:rand.uniform(1000)}"
    random_value = :rand.uniform(100)
    Map.put(data, random_key, random_value)
  end
  
  defp add_random_list_element(data) when is_list(data) do
    random_element = "random_#{:rand.uniform(1000)}"
    [random_element | data]
  end
  
  defp combine_parallel_results(results) do
    # Combine results from parallel pattern applications
    %{
      combined_results: results,
      result_count: length(results),
      combination_strategy: :merge_all
    }
  end
  
  defp select_best_pattern_for_data(data, patterns) do
    # Select the best pattern for the given data
    pattern_scores = 
      Enum.map(patterns, fn pattern ->
        score = calculate_pattern_fitness(data, pattern)
        {pattern, score}
      end)
    
    {best_pattern, _score} = Enum.max_by(pattern_scores, fn {_pattern, score} -> score end)
    best_pattern
  end
  
  defp calculate_pattern_fitness(data, pattern) do
    # Calculate how well a pattern fits the data
    case {data, pattern.type} do
      {data, :fractal} when is_map(data) -> map_size(data) / 10.0
      {data, :infinite} when is_list(data) -> length(data) / 100.0
      {data, :self_referential} when is_binary(data) -> String.length(data) / 1000.0
      _ -> 0.5 # neutral fitness
    end
  end
  
  defp apply_gradient_transformation(data, config) do
    # Apply gradient-based transformation
    learning_rate = config.learning_rate
    
    case data do
      n when is_number(n) -> n * (1 + learning_rate * (:rand.uniform() - 0.5))
      str when is_binary(str) -> str <> "_gradient_#{trunc(learning_rate * 1000)}"
      other -> other
    end
  end
  
  defp apply_genetic_transformation(data, _config) do
    # Apply genetic algorithm-based transformation
    case data do
      list when is_list(list) -> Enum.shuffle(list) |> Enum.take(max(1, length(list) - 1))
      map when is_map(map) -> 
        keys = Map.keys(map)
        if length(keys) > 1 do
          key_to_remove = Enum.random(keys)
          Map.delete(map, key_to_remove)
        else
          map
        end
      other -> other
    end
  end
  
  defp apply_annealing_transformation(data, config) do
    # Apply simulated annealing transformation
    temperature = config[:temperature] || 1.0
    
    case data do
      n when is_number(n) -> 
        noise = :rand.normal() * temperature
        n + noise
      other -> other
    end
  end
  
  defp calculate_performance_score(input, output) do
    # Calculate performance score for learning
    input_complexity = calculate_complexity(input)
    output_complexity = calculate_complexity(output)
    
    # Performance is better when output complexity is appropriately higher than input
    complexity_ratio = output_complexity / max(input_complexity, 1)
    min(complexity_ratio / 2.0, 1.0)
  end
  
  defp calculate_complexity(data) do
    case data do
      n when is_number(n) -> abs(n)
      str when is_binary(str) -> String.length(str)
      list when is_list(list) -> length(list)
      map when is_map(map) -> map_size(map)
      _ -> 1
    end
  end
end