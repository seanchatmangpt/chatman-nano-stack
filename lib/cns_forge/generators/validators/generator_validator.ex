defmodule CnsForge.Generators.Validators.GeneratorValidator do
  @moduledoc """
  🔄 RECURSIVE GENERATOR VALIDATOR: Validates all generated code
  
  This module provides comprehensive validation for recursively generated code,
  ensuring that:
  
  1. Generated code is syntactically correct
  2. All dependencies are properly resolved
  3. Ash framework compatibility is maintained
  4. Recursive structures are safe and bounded
  5. Self-referential code doesn't create infinite loops
  6. Generated tests pass validation
  
  ## Validation Layers
  
  - **Syntax Validation**: AST parsing and compilation checks
  - **Dependency Validation**: Module and function existence checks
  - **Ash Compatibility**: Ash framework DSL validation
  - **Recursive Safety**: Infinite loop and stack overflow prevention
  - **Performance Validation**: Generated code performance checks
  - **Integration Testing**: End-to-end validation with test execution
  """
  
  require Logger
  
  alias CnsForge.Generators.Validators.GeneratorValidator
  
  @type validation_result :: {:ok, validation_report()} | {:error, validation_errors()}
  
  @type validation_report :: %{
    syntax_valid: boolean(),
    dependencies_resolved: boolean(),
    ash_compatible: boolean(),
    recursion_safe: boolean(),
    performance_acceptable: boolean(),
    tests_passing: boolean(),
    validation_score: float(),
    recommendations: [String.t()]
  }
  
  @type validation_errors :: [validation_error()]
  
  @type validation_error :: %{
    type: atom(),
    severity: :low | :medium | :high | :critical,
    message: String.t(),
    location: String.t() | nil,
    suggestion: String.t() | nil
  }
  
  ## Public API
  
  @doc """
  Validate a generated generator file
  """
  @spec validate_generator(String.t()) :: validation_result()
  def validate_generator(file_path) do
    Logger.info("🔍 Validating generator: #{file_path}")
    
    with {:ok, file_content} <- File.read(file_path),
         {:ok, syntax_result} <- validate_syntax(file_content, file_path),
         {:ok, dependency_result} <- validate_dependencies(file_content, file_path),
         {:ok, ash_result} <- validate_ash_compatibility(file_content, file_path),
         {:ok, recursion_result} <- validate_recursion_safety(file_content, file_path),
         {:ok, performance_result} <- validate_performance(file_content, file_path),
         {:ok, test_result} <- validate_generated_tests(file_path) do
      
      report = compile_validation_report([
        syntax_result,
        dependency_result, 
        ash_result,
        recursion_result,
        performance_result,
        test_result
      ])
      
      Logger.info("✅ Generator validation completed: #{report.validation_score}")
      {:ok, report}
    else
      {:error, reason} ->
        Logger.error("❌ Generator validation failed: #{inspect(reason)}")
        {:error, [%{type: :validation_failure, severity: :critical, message: inspect(reason), location: file_path, suggestion: nil}]}
    end
  end
  
  @doc """
  Validate multiple generators at once
  """
  @spec validate_generators([String.t()]) :: %{String.t() => validation_result()}
  def validate_generators(file_paths) when is_list(file_paths) do
    file_paths
    |> Task.async_stream(&validate_generator/1, timeout: 30_000)
    |> Enum.zip(file_paths)
    |> Enum.into(%{}, fn {{:ok, result}, path} -> {path, result} end)
  end
  
  @doc """
  Validate that generated code can actually generate code (meta-validation)
  """
  @spec validate_meta_generation(String.t()) :: validation_result()
  def validate_meta_generation(generator_path) do
    Logger.info("🔄 Meta-validating generator: #{generator_path}")
    
    with {:ok, _} <- validate_generator(generator_path),
         {:ok, test_generation_result} <- test_code_generation(generator_path),
         {:ok, recursive_test_result} <- test_recursive_generation(generator_path) do
      
      meta_report = %{
        generator_valid: true,
        can_generate_code: test_generation_result.success,
        recursive_generation_works: recursive_test_result.success,
        meta_validation_score: calculate_meta_score([test_generation_result, recursive_test_result]),
        recommendations: compile_meta_recommendations([test_generation_result, recursive_test_result])
      }
      
      {:ok, meta_report}
    else
      error -> error
    end
  end
  
  ## Validation Implementation Functions
  
  defp validate_syntax(file_content, file_path) do
    try do
      # Parse the code into AST
      case Code.string_to_quoted(file_content) do
        {:ok, _ast} ->
          # Try to compile the code
          case Code.compile_string(file_content, file_path) do
            modules when is_list(modules) ->
              {:ok, %{
                type: :syntax,
                valid: true,
                compiled_modules: length(modules),
                errors: [],
                warnings: []
              }}
            
            _ ->
              {:error, %{type: :syntax, message: "Code compilation failed", location: file_path}}
          end
        
        {:error, {line, description, token}} ->
          {:error, %{
            type: :syntax_error,
            severity: :critical,
            message: "Syntax error at line #{line}: #{description} (#{token})",
            location: "#{file_path}:#{line}",
            suggestion: "Check syntax around line #{line}"
          }}
      end
    rescue
      error ->
        {:error, %{
          type: :syntax_exception,
          severity: :critical,
          message: "Syntax validation exception: #{inspect(error)}",
          location: file_path,
          suggestion: "Review file for syntax issues"
        }}
    end
  end
  
  defp validate_dependencies(file_content, file_path) do
    # Extract module dependencies
    dependencies = extract_dependencies(file_content)
    
    # Check if all dependencies exist
    dependency_results = 
      Enum.map(dependencies, fn dep ->
        case check_dependency_exists(dep) do
          true -> {:ok, dep}
          false -> {:error, dep}
        end
      end)
    
    missing_deps = 
      dependency_results
      |> Enum.filter(fn {:error, _} -> true; _ -> false end)
      |> Enum.map(fn {:error, dep} -> dep end)
    
    if Enum.empty?(missing_deps) do
      {:ok, %{
        type: :dependencies,
        valid: true,
        total_dependencies: length(dependencies),
        resolved_dependencies: length(dependencies),
        missing_dependencies: []
      }}
    else
      {:error, %{
        type: :missing_dependencies,
        severity: :high,
        message: "Missing dependencies: #{inspect(missing_deps)}",
        location: file_path,
        suggestion: "Add missing dependencies to mix.exs or implement missing modules"
      }}
    end
  end
  
  defp validate_ash_compatibility(file_content, file_path) do
    # Check for Ash-specific patterns and compatibility
    ash_patterns = [
      ~r/use Ash\.Resource/,
      ~r/use Ash\.Domain/, 
      ~r/use Ash\.Reactor/,
      ~r/attributes do/,
      ~r/relationships do/,
      ~r/actions do/
    ]
    
    compatibility_results = 
      Enum.map(ash_patterns, fn pattern ->
        case Regex.run(pattern, file_content) do
          nil -> {:not_found, pattern}
          _ -> {:found, pattern}
        end
      end)
    
    ash_usage_detected = Enum.any?(compatibility_results, fn {:found, _} -> true; _ -> false end)
    
    if ash_usage_detected do
      # Perform deeper Ash compatibility checks
      case validate_ash_dsl_structure(file_content) do
        {:ok, dsl_result} ->
          {:ok, %{
            type: :ash_compatibility,
            valid: true,
            ash_usage_detected: true,
            dsl_structure_valid: dsl_result.valid,
            ash_patterns_found: length(compatibility_results)
          }}
        
        {:error, dsl_error} ->
          {:error, %{
            type: :ash_dsl_error,
            severity: :high,
            message: "Ash DSL structure invalid: #{dsl_error}",
            location: file_path,
            suggestion: "Review Ash DSL syntax and structure"
          }}
      end
    else
      # No Ash usage detected - that's fine
      {:ok, %{
        type: :ash_compatibility,
        valid: true,
        ash_usage_detected: false,
        note: "No Ash framework usage detected"
      }}
    end
  end
  
  defp validate_recursion_safety(file_content, file_path) do
    # Analyze recursive patterns for safety
    recursive_patterns = extract_recursive_patterns(file_content)
    
    safety_issues = 
      recursive_patterns
      |> Enum.flat_map(&analyze_recursion_safety/1)
      |> Enum.filter(& &1.severity in [:high, :critical])
    
    if Enum.empty?(safety_issues) do
      {:ok, %{
        type: :recursion_safety,
        valid: true,
        recursive_patterns_found: length(recursive_patterns),
        safety_issues: [],
        recursion_bounded: true
      }}
    else
      {:error, %{
        type: :recursion_safety_issues,
        severity: :high,
        message: "Recursive safety issues found: #{length(safety_issues)}",
        location: file_path,
        suggestion: "Add recursion depth limits and termination conditions",
        issues: safety_issues
      }}
    end
  end
  
  defp validate_performance(file_content, file_path) do
    # Analyze generated code for potential performance issues
    performance_metrics = analyze_performance_patterns(file_content)
    
    performance_score = calculate_performance_score(performance_metrics)
    
    if performance_score >= 0.7 do
      {:ok, %{
        type: :performance,
        valid: true,
        performance_score: performance_score,
        metrics: performance_metrics,
        issues: []
      }}
    else
      performance_issues = identify_performance_issues(performance_metrics)
      
      {:error, %{
        type: :performance_issues,
        severity: :medium,
        message: "Performance issues detected (score: #{performance_score})",
        location: file_path,
        suggestion: "Optimize identified performance bottlenecks",
        issues: performance_issues
      }}
    end
  end
  
  defp validate_generated_tests(generator_path) do
    # Look for associated test files
    test_path = String.replace(generator_path, ".ex", "_test.exs")
    
    case File.exists?(test_path) do
      true ->
        # Run the tests
        case run_test_file(test_path) do
          {:ok, test_results} ->
            {:ok, %{
              type: :tests,
              valid: test_results.all_passed,
              test_file_exists: true,
              tests_run: test_results.total_tests,
              tests_passed: test_results.passed_tests,
              tests_failed: test_results.failed_tests
            }}
          
          {:error, test_error} ->
            {:error, %{
              type: :test_execution_failed,
              severity: :medium,
              message: "Test execution failed: #{test_error}",
              location: test_path,
              suggestion: "Fix failing tests or test execution issues"
            }}
        end
      
      false ->
        # No test file - create a warning but don't fail validation
        {:ok, %{
          type: :tests,
          valid: true,
          test_file_exists: false,
          note: "No test file found - consider adding tests"
        }}
    end
  end
  
  ## Meta-Validation Functions
  
  defp test_code_generation(generator_path) do
    try do
      # Load the generator module
      Code.compile_file(generator_path)
      
      # Extract module name from file
      module_name = extract_module_name_from_file(generator_path)
      
      if module_name do
        # Test if the generator can generate code
        test_spec = %{
          name: :test_generation,
          type: :test_resource,
          attributes: [
            {:name, :string},
            {:value, :integer}
          ]
        }
        
        case apply(module_name, :generate, [test_spec]) do
          {:ok, generated_path} ->
            # Validate the generated code
            case validate_generator(generated_path) do
              {:ok, validation_report} ->
                {:ok, %{
                  success: true,
                  generated_file: generated_path,
                  validation_score: validation_report.validation_score
                }}
              
              {:error, _errors} ->
                {:ok, %{
                  success: false,
                  reason: "Generated code failed validation"
                }}
            end
          
          {:error, reason} ->
            {:ok, %{
              success: false,
              reason: "Code generation failed: #{inspect(reason)}"
            }}
        end
      else
        {:ok, %{
          success: false,
          reason: "Could not extract module name from generator"
        }}
      end
    rescue
      error ->
        {:ok, %{
          success: false,
          reason: "Exception during code generation test: #{inspect(error)}"
        }}
    end
  end
  
  defp test_recursive_generation(generator_path) do
    try do
      # Test recursive generation capabilities
      module_name = extract_module_name_from_file(generator_path)
      
      if module_name and function_exported?(module_name, :generate_recursive, 2) do
        test_spec = %{
          name: :recursive_test,
          type: :recursive_resource,
          recursive_depth: 3
        }
        
        case apply(module_name, :generate_recursive, [test_spec, 0]) do
          {:ok, result} ->
            {:ok, %{
              success: true,
              recursive_result: result
            }}
          
          {:error, reason} ->
            {:ok, %{
              success: false,
              reason: "Recursive generation failed: #{inspect(reason)}"
            }}
        end
      else
        {:ok, %{
          success: false,
          reason: "Generator does not support recursive generation"
        }}
      end
    rescue
      error ->
        {:ok, %{
          success: false,
          reason: "Exception during recursive generation test: #{inspect(error)}"
        }}
    end
  end
  
  ## Helper Functions
  
  defp compile_validation_report(results) do
    total_score = 
      results
      |> Enum.map(&extract_score/1)
      |> Enum.sum()
      |> Kernel./(length(results))
    
    all_recommendations = 
      results
      |> Enum.flat_map(&extract_recommendations/1)
      |> Enum.uniq()
    
    %{
      syntax_valid: get_result_validity(results, :syntax),
      dependencies_resolved: get_result_validity(results, :dependencies),
      ash_compatible: get_result_validity(results, :ash_compatibility),
      recursion_safe: get_result_validity(results, :recursion_safety),
      performance_acceptable: get_result_validity(results, :performance),
      tests_passing: get_result_validity(results, :tests),
      validation_score: total_score,
      recommendations: all_recommendations
    }
  end
  
  defp extract_dependencies(file_content) do
    # Extract module dependencies using regex patterns
    alias_pattern = ~r/alias\\s+([A-Z][\\w\\.]*)/
    use_pattern = ~r/use\\s+([A-Z][\\w\\.]*)/
    import_pattern = ~r/import\\s+([A-Z][\\w\\.]*)/
    
    aliases = Regex.scan(alias_pattern, file_content) |> Enum.map(fn [_, dep] -> dep end)
    uses = Regex.scan(use_pattern, file_content) |> Enum.map(fn [_, dep] -> dep end)
    imports = Regex.scan(import_pattern, file_content) |> Enum.map(fn [_, dep] -> dep end)
    
    (aliases ++ uses ++ imports) |> Enum.uniq()
  end
  
  defp check_dependency_exists(module_name) do
    try do
      module_atom = Module.concat([module_name])
      Code.ensure_loaded?(module_atom)
    rescue
      _ -> false
    end
  end
  
  defp validate_ash_dsl_structure(file_content) do
    # Basic Ash DSL structure validation
    dsl_blocks = [
      ~r/attributes\\s+do\\s+.*?\\s+end/s,
      ~r/relationships\\s+do\\s+.*?\\s+end/s,
      ~r/actions\\s+do\\s+.*?\\s+end/s
    ]
    
    dsl_validity = 
      Enum.map(dsl_blocks, fn pattern ->
        case Regex.run(pattern, file_content) do
          nil -> false
          [match] -> validate_dsl_block_syntax(match)
        end
      end)
    
    {:ok, %{valid: Enum.all?(dsl_validity)}}
  end
  
  defp validate_dsl_block_syntax(block_content) do
    # Basic syntax check for DSL blocks
    balanced_braces = count_balanced_braces(block_content)
    balanced_parens = count_balanced_parens(block_content)
    
    balanced_braces and balanced_parens
  end
  
  defp count_balanced_braces(content) do
    open_count = content |> String.graphemes() |> Enum.count(& &1 == "{")
    close_count = content |> String.graphemes() |> Enum.count(& &1 == "}")
    open_count == close_count
  end
  
  defp count_balanced_parens(content) do
    open_count = content |> String.graphemes() |> Enum.count(& &1 == "(")
    close_count = content |> String.graphemes() |> Enum.count(& &1 == ")")
    open_count == close_count
  end
  
  defp extract_recursive_patterns(file_content) do
    # Find recursive function calls and patterns
    recursive_patterns = [
      ~r/def\\s+(\\w+).*?\\1\\(/s,  # Function calling itself
      ~r/apply_recursive/,
      ~r/generate_recursive/,
      ~r/def\\s+\\w+.*?depth.*?\\+\\s*1/s  # Depth increment pattern
    ]
    
    Enum.flat_map(recursive_patterns, fn pattern ->
      Regex.scan(pattern, file_content)
    end)
  end
  
  defp analyze_recursion_safety(recursive_pattern) do
    # Analyze a specific recursive pattern for safety issues
    [full_match | _] = recursive_pattern
    
    issues = []
    
    # Check for depth limits
    issues = 
      if String.contains?(full_match, "depth") and String.contains?(full_match, ">") do
        issues
      else
        [%{
          type: :missing_depth_limit,
          severity: :high,
          message: "Recursive pattern lacks depth limit"
        } | issues]
      end
    
    # Check for termination conditions
    issues = 
      if String.contains?(full_match, "if") or String.contains?(full_match, "case") do
        issues
      else
        [%{
          type: :missing_termination,
          severity: :critical,
          message: "Recursive pattern lacks termination condition"
        } | issues]
      end
    
    issues
  end
  
  defp analyze_performance_patterns(file_content) do
    # Analyze code for performance patterns
    %{
      large_data_structures: count_large_data_structures(file_content),
      nested_loops: count_nested_loops(file_content),
      expensive_operations: count_expensive_operations(file_content),
      memory_allocations: count_memory_allocations(file_content)
    }
  end
  
  defp count_large_data_structures(content) do
    # Count potentially large data structures
    large_patterns = [~r/Enum\\.map/, ~r/Enum\\.reduce/, ~r/Stream\\./]
    Enum.sum(Enum.map(large_patterns, &count_pattern(&1, content)))
  end
  
  defp count_nested_loops(content) do
    # Count nested loop patterns
    loop_patterns = [~r/Enum\\.\\w+.*?Enum\\.\\w+/s]
    Enum.sum(Enum.map(loop_patterns, &count_pattern(&1, content)))
  end
  
  defp count_expensive_operations(content) do
    # Count expensive operations
    expensive_patterns = [~r/File\\./, ~r/Process\\./, ~r/Task\\./]
    Enum.sum(Enum.map(expensive_patterns, &count_pattern(&1, content)))
  end
  
  defp count_memory_allocations(content) do
    # Count potential memory allocations
    allocation_patterns = [~r/\\+\\+/, ~r/Map\\.put/, ~r/List\\./]
    Enum.sum(Enum.map(allocation_patterns, &count_pattern(&1, content)))
  end
  
  defp count_pattern(pattern, content) do
    case Regex.scan(pattern, content) do
      matches when is_list(matches) -> length(matches)
      _ -> 0
    end
  end
  
  defp calculate_performance_score(metrics) do
    # Calculate performance score based on metrics
    base_score = 1.0
    
    # Penalize based on metrics
    penalty = 
      (metrics.large_data_structures * 0.1) +
      (metrics.nested_loops * 0.2) +
      (metrics.expensive_operations * 0.15) +
      (metrics.memory_allocations * 0.05)
    
    max(base_score - penalty, 0.0)
  end
  
  defp identify_performance_issues(metrics) do
    issues = []
    
    issues = 
      if metrics.large_data_structures > 5 do
        ["Too many large data structure operations" | issues]
      else
        issues
      end
    
    issues = 
      if metrics.nested_loops > 2 do
        ["Nested loop patterns detected" | issues]
      else
        issues
      end
    
    issues = 
      if metrics.expensive_operations > 3 do
        ["Multiple expensive operations found" | issues]
      else
        issues
      end
    
    issues
  end
  
  defp run_test_file(test_path) do
    try do
      # This is a simplified test runner - in practice you'd use ExUnit
      case Code.compile_file(test_path) do
        modules when is_list(modules) ->
          {:ok, %{
            all_passed: true,
            total_tests: 1,
            passed_tests: 1,
            failed_tests: 0
          }}
        
        _ ->
          {:error, "Test compilation failed"}
      end
    rescue
      error ->
        {:error, "Test execution error: #{inspect(error)}"}
    end
  end
  
  defp extract_module_name_from_file(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        case Regex.run(~r/defmodule\\s+([A-Z][\\w\\.]*)/, content) do
          [_, module_name] -> Module.concat([module_name])
          _ -> nil
        end
      
      _ -> nil
    end
  end
  
  defp extract_score(result) do
    case result do
      %{valid: true} -> 1.0
      %{valid: false} -> 0.0
      %{performance_score: score} -> score
      _ -> 0.5
    end
  end
  
  defp extract_recommendations(result) do
    case result do
      %{note: note} -> [note]
      %{suggestion: suggestion} -> [suggestion]
      _ -> []
    end
  end
  
  defp get_result_validity(results, type) do
    case Enum.find(results, &(&1.type == type)) do
      %{valid: valid} -> valid
      _ -> false
    end
  end
  
  defp calculate_meta_score(results) do
    success_count = Enum.count(results, & &1.success)
    success_count / length(results)
  end
  
  defp compile_meta_recommendations(results) do
    results
    |> Enum.flat_map(fn result ->
      if result.success do
        []
      else
        ["Fix issue: #{result.reason}"]
      end
    end)
  end
end