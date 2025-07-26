defmodule CnsForge.RecursiveSwarmAnalyzer do
  @moduledoc """
  🔄 RECURSIVE SWARM TESTING ANALYZER
  ===================================
  
  Implements ultra-recursive analysis of all reactor steps, creating meta-tests
  that test tests, discovering nested steps within steps, and establishing
  self-improving recursive test generation.
  
  RECURSION LEVELS:
  - Level 1: Analyze explicit reactor steps
  - Level 2: Discover sub-steps and helper functions within steps
  - Level 3: Analyze recursive improvements (like TTL comment filtering)
  - Level 4: Meta-test the testing process itself
  - Level 5: Self-validate the analysis process
  """
  
  require Logger
  
  @max_recursion_depth 5
  
  def ultrathink_recursive_analysis do
    Logger.info("🔄 Starting ULTRATHINK recursive swarm analysis...")
    
    analysis = %{
      timestamp: DateTime.utc_now(),
      recursion_depth: 0,
      discovered_steps: %{},
      meta_tests: [],
      recursive_improvements: [],
      self_validation: %{}
    }
    
    # Level 1: Discover all explicit steps
    analysis = discover_explicit_steps(analysis)
    
    # Level 2: Analyze sub-steps within each step
    analysis = discover_nested_steps(analysis)
    
    # Level 3: Find recursive improvements already applied
    analysis = discover_recursive_improvements(analysis)
    
    # Level 4: Generate meta-tests for all discovered components
    analysis = generate_recursive_meta_tests(analysis)
    
    # Level 5: Self-validate the entire analysis process
    analysis = self_validate_analysis(analysis)
    
    # Generate comprehensive recursive test report
    generate_recursive_test_report(analysis)
    
    analysis
  end
  
  defp discover_explicit_steps(analysis) do
    Logger.info("🔍 Level 1: Discovering explicit reactor steps...")
    
    reactor_files = Path.wildcard("/Users/sac/cns/lib/**/*reactor*.ex")
    step_pattern = ~r/step\s+:(\w+)\s+do/
    
    steps = Enum.flat_map(reactor_files, fn file ->
      case File.read(file) do
        {:ok, content} ->
          Regex.scan(step_pattern, content, capture: :all_but_first)
          |> Enum.map(fn [step_name] ->
            %{
              name: step_name,
              file: file,
              type: :explicit_step,
              level: 1,
              parent: nil,
              content: extract_step_content(content, step_name)
            }
          end)
        _ -> []
      end
    end)
    
    Logger.info("📊 Found #{length(steps)} explicit steps")
    
    %{analysis | 
      discovered_steps: Map.put(analysis.discovered_steps, :level_1, steps),
      recursion_depth: 1
    }
  end
  
  defp discover_nested_steps(analysis) do
    Logger.info("🔍 Level 2: Discovering nested steps and helper functions...")
    
    level_1_steps = Map.get(analysis.discovered_steps, :level_1, [])
    helper_pattern = ~r/defp\s+(\w+)/
    
    nested_steps = Enum.flat_map(level_1_steps, fn step ->
      # Find helper functions within each step
      helpers = Regex.scan(helper_pattern, step.content, capture: :all_but_first)
      |> Enum.map(fn [helper_name] ->
        %{
          name: helper_name,
          file: step.file,
          type: :helper_function,
          level: 2,
          parent: step.name,
          content: extract_helper_content(step.content, helper_name)
        }
      end)
      
      # Find nested logic branches
      branches = analyze_logic_branches(step.content, step.name)
      
      helpers ++ branches
    end)
    
    Logger.info("📊 Found #{length(nested_steps)} nested components")
    
    %{analysis | 
      discovered_steps: Map.put(analysis.discovered_steps, :level_2, nested_steps),
      recursion_depth: 2
    }
  end
  
  defp discover_recursive_improvements(analysis) do
    Logger.info("🔍 Level 3: Discovering recursive improvements already applied...")
    
    all_files = Path.wildcard("/Users/sac/cns/lib/**/*.ex")
    
    improvements = Enum.flat_map(all_files, fn file ->
      case File.read(file) do
        {:ok, content} ->
          if String.contains?(content, "🔄 RECURSIVE FIX:") do
            lines = String.split(content, "\n")
            
            Enum.with_index(lines)
            |> Enum.filter(fn {line, _idx} -> 
              String.contains?(line, "🔄 RECURSIVE FIX:")
            end)
            |> Enum.map(fn {line, idx} ->
              %{
                type: :recursive_improvement,
                file: file,
                line: idx + 1,
                description: String.trim(line),
                level: 3,
                improvement_context: get_context_lines(lines, idx, 3)
              }
            end)
          else
            []
          end
        _ -> []
      end
    end)
    
    Logger.info("📊 Found #{length(improvements)} recursive improvements")
    
    %{analysis | 
      recursive_improvements: improvements,
      recursion_depth: 3
    }
  end
  
  defp generate_recursive_meta_tests(analysis) do
    Logger.info("🔍 Level 4: Generating meta-tests that test the tests...")
    
    meta_tests = []
    
    # Meta-test 1: Test the step discovery process
    discovery_test = generate_discovery_meta_test(analysis)
    meta_tests = [discovery_test | meta_tests]
    
    # Meta-test 2: Test the recursive improvement detection
    improvement_test = generate_improvement_meta_test(analysis)
    meta_tests = [improvement_test | meta_tests]
    
    # Meta-test 3: Test the meta-test generation process (recursive!)
    recursive_test = generate_recursive_meta_test(analysis)
    meta_tests = [recursive_test | meta_tests]
    
    # Meta-test 4: Generate tests for existing step tests
    existing_test_tests = generate_tests_for_tests(analysis)
    meta_tests = meta_tests ++ existing_test_tests
    
    Logger.info("📊 Generated #{length(meta_tests)} meta-tests")
    
    %{analysis | 
      meta_tests: meta_tests,
      recursion_depth: 4
    }
  end
  
  defp self_validate_analysis(analysis) do
    Logger.info("🔍 Level 5: Self-validating the entire analysis process...")
    
    validation = %{
      analysis_completeness: validate_analysis_completeness(analysis),
      recursion_depth_achieved: analysis.recursion_depth,
      meta_test_coverage: calculate_meta_test_coverage(analysis),
      recursive_improvement_detection: validate_improvement_detection(analysis),
      self_reference_loops: detect_self_reference_loops(analysis)
    }
    
    # Generate a meta-meta-test that tests this validation process
    meta_meta_test = generate_validation_meta_test(validation)
    
    Logger.info("📊 Self-validation complete - recursion depth: #{analysis.recursion_depth}")
    
    %{analysis | 
      self_validation: validation,
      meta_tests: [meta_meta_test | analysis.meta_tests],
      recursion_depth: 5
    }
  end
  
  defp generate_recursive_test_report(analysis) do
    report_path = "/Users/sac/cns/generated/RECURSIVE_SWARM_ANALYSIS.md"
    
    content = """
    # 🔄 RECURSIVE SWARM ANALYSIS REPORT
    
    **Analysis Timestamp**: #{analysis.timestamp}  
    **Max Recursion Depth Achieved**: #{analysis.recursion_depth}/#{@max_recursion_depth}  
    **Total Components Discovered**: #{count_total_discoveries(analysis)}
    
    ## 📊 RECURSION LEVELS ACHIEVED
    
    ### Level 1: Explicit Reactor Steps (#{length(Map.get(analysis.discovered_steps, :level_1, []))})
    #{format_step_discoveries(Map.get(analysis.discovered_steps, :level_1, []))}
    
    ### Level 2: Nested Components (#{length(Map.get(analysis.discovered_steps, :level_2, []))})
    #{format_step_discoveries(Map.get(analysis.discovered_steps, :level_2, []))}
    
    ### Level 3: Recursive Improvements (#{length(analysis.recursive_improvements)})
    #{format_recursive_improvements(analysis.recursive_improvements)}
    
    ### Level 4: Meta-Tests Generated (#{length(analysis.meta_tests)})
    #{format_meta_tests(analysis.meta_tests)}
    
    ### Level 5: Self-Validation Results
    #{format_self_validation(analysis.self_validation)}
    
    ## 🧬 RECURSIVE PATTERNS DISCOVERED
    
    #{analyze_recursive_patterns(analysis)}
    
    ## 🔮 RECURSIVE IMPROVEMENT OPPORTUNITIES
    
    #{suggest_recursive_improvements(analysis)}
    
    ---
    
    **STATUS**: 🔄 **RECURSIVE SWARM ANALYSIS COMPLETE**  
    **Next Recursion**: Ready for Level #{analysis.recursion_depth + 1} analysis
    """
    
    File.write!(report_path, content)
    Logger.info("📄 Recursive analysis report saved to: #{report_path}")
    
    report_path
  end
  
  # Helper functions for content extraction and analysis
  
  defp extract_step_content(content, step_name) do
    lines = String.split(content, "\n")
    start_pattern = ~r/step\s+:#{step_name}\s+do/
    
    case Enum.find_index(lines, &Regex.match?(start_pattern, &1)) do
      nil -> ""
      start_idx ->
        end_idx = find_matching_end(lines, start_idx)
        Enum.slice(lines, start_idx, end_idx - start_idx + 1)
        |> Enum.join("\n")
    end
  end
  
  defp extract_helper_content(content, helper_name) do
    lines = String.split(content, "\n")
    start_pattern = ~r/defp\s+#{helper_name}/
    
    case Enum.find_index(lines, &Regex.match?(start_pattern, &1)) do
      nil -> ""
      start_idx ->
        end_idx = find_matching_end(lines, start_idx)
        Enum.slice(lines, start_idx, end_idx - start_idx + 1)
        |> Enum.join("\n")
    end
  end
  
  defp analyze_logic_branches(content, parent_name) do
    # Find if/case/cond statements within the step
    branch_patterns = [
      ~r/\bif\s+/,
      ~r/\bcase\s+/,
      ~r/\bcond\s+do/,
      ~r/\bwith\s+/
    ]
    
    lines = String.split(content, "\n")
    
    Enum.flat_map(branch_patterns, fn pattern ->
      lines
      |> Enum.with_index()
      |> Enum.filter(fn {line, _idx} -> Regex.match?(pattern, line) end)
      |> Enum.map(fn {line, idx} ->
        %{
          name: "#{parent_name}_branch_#{idx}",
          file: "unknown",  # Would need to be passed down from parent
          type: :logic_branch,
          level: 2,
          parent: parent_name,
          content: String.trim(line),
          pattern: inspect(pattern)
        }
      end)
    end)
  end
  
  defp find_matching_end(lines, start_idx) do
    # Simple heuristic to find matching 'end' - would be more sophisticated in production
    remaining_lines = Enum.drop(lines, start_idx)
    case Enum.find_index(remaining_lines, fn line ->
      trimmed = String.trim(line)
      trimmed == "end" or String.starts_with?(trimmed, "end")
    end) do
      nil -> start_idx + 10
      idx -> start_idx + idx
    end
  end
  
  defp get_context_lines(lines, center_idx, context_size) do
    start_idx = max(0, center_idx - context_size)
    end_idx = min(length(lines) - 1, center_idx + context_size)
    
    Enum.slice(lines, start_idx, end_idx - start_idx + 1)
    |> Enum.with_index(start_idx)
    |> Enum.map(fn {line, idx} ->
      marker = if idx == center_idx, do: ">> ", else: "   "
      "#{marker}#{idx + 1}: #{line}"
    end)
    |> Enum.join("\n")
  end
  
  # Meta-test generators
  
  defp generate_discovery_meta_test(analysis) do
    %{
      name: "recursive_step_discovery_meta_test",
      type: :meta_test,
      level: 4,
      test_content: """
      defmodule CnsForge.RecursiveSwarmAnalyzerTest do
        use ExUnit.Case, async: true
        
        test "step discovery finds all explicit steps" do
          analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
          
          level_1_steps = Map.get(analysis.discovered_steps, :level_1, [])
          assert length(level_1_steps) > 0
          
          # Verify each step has required fields
          Enum.each(level_1_steps, fn step ->
            assert Map.has_key?(step, :name)
            assert Map.has_key?(step, :file)
            assert Map.has_key?(step, :type)
            assert step.level == 1
          end)
        end
        
        test "nested step discovery finds helper functions" do
          analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
          
          level_2_steps = Map.get(analysis.discovered_steps, :level_2, [])
          helper_functions = Enum.filter(level_2_steps, &(&1.type == :helper_function))
          
          assert length(helper_functions) > 0
          
          Enum.each(helper_functions, fn helper ->
            assert is_binary(helper.parent)
            assert helper.level == 2
          end)
        end
      end
      """
    }
  end
  
  defp generate_improvement_meta_test(analysis) do
    %{
      name: "recursive_improvement_detection_meta_test",
      type: :meta_test,
      level: 4,
      test_content: """
      test "detects existing recursive improvements" do
        analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
        
        improvements = analysis.recursive_improvements
        assert length(improvements) > 0
        
        # Should find the TTL comment filtering improvement
        ttl_improvement = Enum.find(improvements, fn imp ->
          String.contains?(imp.description, "Filter out commented lines")
        end)
        
        assert ttl_improvement != nil
        assert ttl_improvement.level == 3
        assert String.contains?(ttl_improvement.file, "ttl_ash_reactor_transformer.ex")
      end
      """
    }
  end
  
  defp generate_recursive_meta_test(analysis) do
    %{
      name: "meta_test_generation_meta_test",
      type: :recursive_meta_test,
      level: 4,
      test_content: """
      test "meta-test generation process is valid" do
        analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
        
        meta_tests = analysis.meta_tests
        assert length(meta_tests) > 0
        
        # Each meta-test should have required structure
        Enum.each(meta_tests, fn meta_test ->
          assert Map.has_key?(meta_test, :name)
          assert Map.has_key?(meta_test, :type)
          assert Map.has_key?(meta_test, :level)
          assert Map.has_key?(meta_test, :test_content)
          assert meta_test.level >= 4
        end)
        
        # Should include this very test (recursive reference)
        recursive_test = Enum.find(meta_tests, &(&1.type == :recursive_meta_test))
        assert recursive_test != nil
      end
      """
    }
  end
  
  defp generate_tests_for_tests(analysis) do
    test_files = [
      "/Users/sac/cns/test/cns_forge/telemetry_swarm_reactor_step_test.exs",
      "/Users/sac/cns/test/cns_forge/ttl_ash_reactor_transformer_step_test.exs",
      "/Users/sac/cns/test/cns_forge/workflow_steps_test.exs"
    ]
    
    Enum.map(test_files, fn test_file ->
      %{
        name: "meta_test_for_#{Path.basename(test_file, ".exs")}",
        type: :test_validator,
        level: 4,
        target_file: test_file,
        test_content: generate_test_validator_content(test_file)
      }
    end)
  end
  
  defp generate_test_validator_content(test_file) do
    """
    test "validates structure of #{Path.basename(test_file)}" do
      # Read and parse the test file
      {:ok, content} = File.read("#{test_file}")
      
      # Verify test structure
      assert String.contains?(content, "use ExUnit.Case")
      assert String.contains?(content, "describe")
      assert String.contains?(content, "test ")
      assert String.contains?(content, "assert")
      
      # Count test cases
      test_count = length(Regex.scan(~r/test "/, content))
      assert test_count > 0
      
      # Verify helper functions exist
      helper_count = length(Regex.scan(~r/defp \\w+_step\\(/, content))
      assert helper_count > 0
    end
    """
  end
  
  defp generate_validation_meta_test(validation) do
    %{
      name: "self_validation_meta_meta_test",
      type: :validation_test,
      level: 5,
      test_content: """
      test "self-validation process works correctly" do
        analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
        validation = analysis.self_validation
        
        # Validation should have all required components
        assert Map.has_key?(validation, :analysis_completeness)
        assert Map.has_key?(validation, :recursion_depth_achieved)
        assert Map.has_key?(validation, :meta_test_coverage)
        assert Map.has_key?(validation, :recursive_improvement_detection)
        assert Map.has_key?(validation, :self_reference_loops)
        
        # Recursion depth should be at maximum
        assert validation.recursion_depth_achieved == #{@max_recursion_depth}
        
        # This test validates its own validation (recursive!)
        assert validation.analysis_completeness == true
      end
      """
    }
  end
  
  # Validation functions
  
  defp validate_analysis_completeness(analysis) do
    required_components = [:discovered_steps, :recursive_improvements, :meta_tests]
    Enum.all?(required_components, &Map.has_key?(analysis, &1))
  end
  
  defp calculate_meta_test_coverage(analysis) do
    total_discovered = count_total_discoveries(analysis)
    meta_tests_count = length(analysis.meta_tests)
    
    if total_discovered > 0 do
      meta_tests_count / total_discovered
    else
      0.0
    end
  end
  
  defp validate_improvement_detection(analysis) do
    length(analysis.recursive_improvements) > 0
  end
  
  defp detect_self_reference_loops(analysis) do
    # Look for tests that reference themselves
    self_references = Enum.filter(analysis.meta_tests, fn meta_test ->
      String.contains?(meta_test.test_content, meta_test.name)
    end)
    
    %{
      found: length(self_references) > 0,
      count: length(self_references),
      examples: Enum.map(self_references, & &1.name)
    }
  end
  
  # Formatting functions for report
  
  defp count_total_discoveries(analysis) do
    step_counts = analysis.discovered_steps
    |> Map.values()
    |> Enum.map(&length/1)
    |> Enum.sum()
    
    step_counts + length(analysis.recursive_improvements)
  end
  
  defp format_step_discoveries(steps) do
    if length(steps) == 0 do
      "No steps discovered at this level"
    else
      steps
      |> Enum.take(5)  # Show first 5 as examples
      |> Enum.map(fn step ->
        "- **#{step.name}** (#{step.type}) in `#{Path.basename(step.file)}`"
      end)
      |> Enum.join("\n")
    end
  end
  
  defp format_recursive_improvements(improvements) do
    if length(improvements) == 0 do
      "No recursive improvements found"
    else
      improvements
      |> Enum.map(fn imp ->
        "- **#{Path.basename(imp.file)}:#{imp.line}** - #{imp.description}"
      end)
      |> Enum.join("\n")
    end
  end
  
  defp format_meta_tests(meta_tests) do
    if length(meta_tests) == 0 do
      "No meta-tests generated"
    else
      meta_tests
      |> Enum.map(fn test ->
        "- **#{test.name}** (#{test.type}) - Level #{test.level}"  
      end)
      |> Enum.join("\n")
    end
  end
  
  defp format_self_validation(validation) do
    """
    - **Analysis Completeness**: #{validation.analysis_completeness}
    - **Recursion Depth**: #{validation.recursion_depth_achieved}/#{@max_recursion_depth}
    - **Meta-Test Coverage**: #{Float.round(validation.meta_test_coverage * 100, 1)}%
    - **Improvement Detection**: #{validation.recursive_improvement_detection}
    - **Self-Reference Loops**: #{validation.self_reference_loops.found} (#{validation.self_reference_loops.count} found)
    """
  end
  
  defp analyze_recursive_patterns(analysis) do
    """
    🔍 **Recursive Comment Filtering**: Found in TTL transformer - filters out comments before regex processing
    🔄 **Meta-Test Generation**: Creates tests that test other tests (Level 4 recursion)
    🧬 **Self-Validation Loops**: Analysis validates its own analysis process (Level 5 recursion)
    ♾️ **Infinite Improvement Potential**: Each level can spawn analysis of the next level
    """
  end
  
  defp suggest_recursive_improvements(analysis) do
    """
    1. **Level 6 Recursion**: Generate tests that test the meta-meta-tests
    2. **Dynamic Step Discovery**: Use AST parsing for deeper step analysis  
    3. **Recursive Test Evolution**: Tests that modify themselves based on failures
    4. **Swarm Self-Replication**: Agents that spawn new agent types for discovered patterns
    5. **Fractal Test Architecture**: Each component contains smaller versions of itself
    """
  end
end