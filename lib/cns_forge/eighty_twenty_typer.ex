defmodule CnsForge.EightyTwentyTyper do
  @moduledoc """
  80/20 Type Optimizer - Implements Pareto Principle for Type Systems
  
  Identifies the 20% of types and relationships that provide 80% of the value
  in a semantic model, optimizing for maximum impact with minimum complexity.
  """
  
  require Logger
  
  @doc """
  Analyzes a semantic model and identifies the critical 20% using various heuristics
  """
  def optimize_types(semantic_model) do
    Logger.info("Starting 80/20 type optimization")
    
    with {:ok, analyzed} <- analyze_usage_patterns(semantic_model),
         {:ok, ranked} <- rank_by_importance(analyzed),
         {:ok, selected} <- select_critical_twenty_percent(ranked),
         {:ok, optimized} <- optimize_relationships(selected) do
      
      {:ok, %{
        original_count: count_types(semantic_model),
        optimized_count: count_types(optimized),
        reduction_percentage: calculate_reduction(semantic_model, optimized),
        critical_types: optimized
      }}
    end
  end
  
  defp analyze_usage_patterns(model) do
    patterns = %{
      reference_count: count_references(model),
      relationship_density: calculate_relationship_density(model),
      attribute_richness: measure_attribute_richness(model),
      constraint_complexity: analyze_constraints(model)
    }
    
    {:ok, Map.put(model, :usage_patterns, patterns)}
  end
  
  defp rank_by_importance(model) do
    ranked_types = model.types
    |> Enum.map(fn type ->
      score = calculate_importance_score(type, model.usage_patterns)
      {type, score}
    end)
    |> Enum.sort_by(fn {_type, score} -> score end, :desc)
    
    {:ok, Map.put(model, :ranked_types, ranked_types)}
  end
  
  defp select_critical_twenty_percent(model) do
    total_types = length(model.ranked_types)
    twenty_percent_count = max(1, round(total_types * 0.2))
    
    critical_types = model.ranked_types
    |> Enum.take(twenty_percent_count)
    |> Enum.map(fn {type, _score} -> type end)
    
    # Include types that are strongly connected to critical types
    extended_critical = extend_with_dependencies(critical_types, model)
    
    {:ok, %{model | types: extended_critical}}
  end
  
  defp optimize_relationships(model) do
    # Remove relationships that don't involve critical types
    optimized_relationships = model.relationships
    |> Enum.filter(fn rel ->
      Enum.member?(model.types, rel.source) and
      Enum.member?(model.types, rel.target)
    end)
    
    {:ok, %{model | relationships: optimized_relationships}}
  end
  
  defp calculate_importance_score(type, patterns) do
    # Weighted scoring based on multiple factors
    ref_score = Map.get(patterns.reference_count, type.name, 0) * 0.3
    rel_score = Map.get(patterns.relationship_density, type.name, 0) * 0.3
    attr_score = Map.get(patterns.attribute_richness, type.name, 0) * 0.2
    const_score = Map.get(patterns.constraint_complexity, type.name, 0) * 0.2
    
    ref_score + rel_score + attr_score + const_score
  end
  
  defp count_references(model) do
    # Count how many times each type is referenced
    Enum.reduce(model.relationships, %{}, fn rel, acc ->
      acc
      |> Map.update(rel.source, 1, &(&1 + 1))
      |> Map.update(rel.target, 1, &(&1 + 1))
    end)
  end
  
  defp calculate_relationship_density(model) do
    # Calculate relationship density per type
    model.types
    |> Enum.map(fn type ->
      rel_count = Enum.count(model.relationships, fn rel ->
        rel.source == type.name or rel.target == type.name
      end)
      {type.name, rel_count}
    end)
    |> Map.new()
  end
  
  defp measure_attribute_richness(model) do
    # Measure how many attributes each type has
    model.types
    |> Enum.map(fn type ->
      {type.name, length(type.attributes || [])}
    end)
    |> Map.new()
  end
  
  defp analyze_constraints(model) do
    # Analyze constraint complexity
    model.types
    |> Enum.map(fn type ->
      constraint_count = length(type.constraints || [])
      {type.name, constraint_count}
    end)
    |> Map.new()
  end
  
  defp extend_with_dependencies(critical_types, model) do
    # Include types that are mandatory dependencies
    extended = Enum.reduce(critical_types, critical_types, fn type, acc ->
      deps = find_mandatory_dependencies(type, model)
      Enum.uniq(acc ++ deps)
    end)
    
    extended
  end
  
  defp find_mandatory_dependencies(type, model) do
    # Find types that this type cannot function without
    model.relationships
    |> Enum.filter(fn rel ->
      rel.source == type.name and rel.required == true
    end)
    |> Enum.map(fn rel -> 
      Enum.find(model.types, fn t -> t.name == rel.target end)
    end)
    |> Enum.reject(&is_nil/1)
  end
  
  defp count_types(model) do
    length(model.types || [])
  end
  
  defp calculate_reduction(original, optimized) do
    original_count = count_types(original)
    optimized_count = count_types(optimized)
    
    if original_count > 0 do
      ((original_count - optimized_count) / original_count * 100)
      |> Float.round(2)
    else
      0.0
    end
  end
end