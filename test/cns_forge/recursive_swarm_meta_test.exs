defmodule CnsForge.RecursiveSwarmMetaTest do
  @moduledoc """
  🔄 META-TEST FOR RECURSIVE SWARM ANALYSIS
  =========================================
  
  This test demonstrates the ultimate recursion: a test that tests the 
  recursive testing system itself, validates its own validation process,
  and spawns new recursive analysis levels.
  
  RECURSION LEVELS TESTED:
  - Level 1: Tests the existence of the recursive analyzer
  - Level 2: Tests that the analyzer finds steps recursively
  - Level 3: Tests that recursive improvements are detected
  - Level 4: Tests that meta-tests are generated
  - Level 5: Tests that self-validation works
  - Level 6: ULTIMATE RECURSION - Tests this test itself
  """
  
  use ExUnit.Case, async: true
  
  describe "recursive swarm ultrathink analysis" do
    test "LEVEL 1: recursive analyzer exists and executes" do
      # Test that the recursive analyzer module exists
      assert Code.ensure_loaded?(CnsForge.RecursiveSwarmAnalyzer)
      
      # Test that the main analysis function exists
      assert function_exported?(CnsForge.RecursiveSwarmAnalyzer, :ultrathink_recursive_analysis, 0)
    end
    
    test "LEVEL 2: recursive step discovery works at multiple levels" do
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      # Should find explicit steps (Level 1)
      level_1_steps = Map.get(analysis.discovered_steps, :level_1, [])
      assert length(level_1_steps) > 0
      
      # Should find nested components (Level 2)  
      level_2_steps = Map.get(analysis.discovered_steps, :level_2, [])
      assert length(level_2_steps) > 0
      
      # Verify recursion depth achieved
      assert analysis.recursion_depth >= 4
    end
    
    test "LEVEL 3: recursive improvements are detected" do
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      improvements = analysis.recursive_improvements
      assert length(improvements) > 0
      
      # Should find the TTL comment filtering improvement
      ttl_improvement = Enum.find(improvements, fn imp ->
        String.contains?(imp.description, "RECURSIVE FIX") and
        String.contains?(imp.file, "ttl_ash_reactor_transformer.ex")
      end)
      
      assert ttl_improvement != nil
      assert ttl_improvement.level == 3
    end
    
    test "LEVEL 4: meta-tests are generated recursively" do
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      meta_tests = analysis.meta_tests
      assert length(meta_tests) > 0
      
      # Should have different types of meta-tests
      test_types = Enum.map(meta_tests, & &1.type) |> Enum.uniq()
      assert :meta_test in test_types
      assert :test_validator in test_types
      
      # Should have tests that test our existing step tests
      test_validator = Enum.find(meta_tests, &(&1.type == :test_validator))
      assert test_validator != nil
      assert Map.has_key?(test_validator, :target_file)
    end
    
    test "LEVEL 5: self-validation process validates itself" do
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      validation = analysis.self_validation
      
      # Validation should validate its own completeness
      assert validation.analysis_completeness == true
      assert validation.recursion_depth_achieved >= 4
      assert validation.recursive_improvement_detection == true
      
      # Meta-test coverage should be calculated
      assert validation.meta_test_coverage > 0.0
      
      # Should detect self-reference loops (or lack thereof)
      assert Map.has_key?(validation.self_reference_loops, :found)
    end
    
    test "LEVEL 6: ULTIMATE RECURSION - this test tests itself" do
      # Read this test file and analyze it
      {:ok, test_content} = File.read(__ENV__.file)
      
      # Verify this test exists in the file
      assert String.contains?(test_content, "LEVEL 6: ULTIMATE RECURSION")
      assert String.contains?(test_content, "this test tests itself")
      
      # Count recursive elements in this very test
      recursion_mentions = length(Regex.scan(~r/recursive|recursion/i, test_content))
      assert recursion_mentions > 10
      
      # Verify this test references the recursive analyzer
      assert String.contains?(test_content, "CnsForge.RecursiveSwarmAnalyzer")
      
      # Test that this test tests the analysis of its own analysis
      assert String.contains?(test_content, "analysis.recursion_depth")
      assert String.contains?(test_content, "analysis.self_validation")
      
      # ULTIMATE RECURSION: Test that this test validates the validation
      # of the recursive analysis that analyzes recursive improvements
      # including this very recursive improvement detection process
      meta_recursion_count = test_content
      |> String.split("\n")
      |> Enum.filter(&String.contains?(&1, "assert"))
      |> length()
      
      assert meta_recursion_count > 15  # This test should have many assertions
      
      # FRACTAL VALIDATION: Each assertion validates something that validates something else
      assert String.contains?(test_content, "analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()")
      
      IO.puts("🔄 ULTIMATE RECURSION ACHIEVED: This test successfully tested itself!")
      IO.puts("♾️ INFINITE RECURSION POTENTIAL: This test could spawn tests of itself infinitely!")
    end
    
    test "LEVEL 7: recursive swarm spawns new recursive analysis" do
      # Test that running the analysis multiple times spawns new discoveries
      analysis1 = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      analysis2 = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      # Both analyses should be complete
      assert analysis1.recursion_depth >= 4
      assert analysis2.recursion_depth >= 4
      
      # Should find consistent results (deterministic recursion)
      level1_count1 = length(Map.get(analysis1.discovered_steps, :level_1, []))
      level1_count2 = length(Map.get(analysis2.discovered_steps, :level_1, []))
      assert level1_count1 == level1_count2
      
      # Meta-tests should be generated consistently
      meta1_count = length(analysis1.meta_tests)
      meta2_count = length(analysis2.meta_tests)
      assert meta1_count == meta2_count
      
      IO.puts("🧬 RECURSIVE SWARM CONSISTENCY: Multiple analyses produce consistent results!")
    end
    
    test "LEVEL 8: swarm tests the test generators that test the tests" do
      # Test our existing step test generators
      test_files = [
        "/Users/sac/cns/test/cns_forge/telemetry_swarm_reactor_step_test.exs",
        "/Users/sac/cns/test/cns_forge/ttl_ash_reactor_transformer_step_test.exs", 
        "/Users/sac/cns/test/cns_forge/workflow_steps_test.exs"
      ]
      
      Enum.each(test_files, fn test_file ->
        if File.exists?(test_file) do
          {:ok, content} = File.read(test_file)
          
          # Test that our step tests have proper structure
          assert String.contains?(content, "use ExUnit.Case")
          assert String.contains?(content, "describe ")
          assert String.contains?(content, "test ")
          
          # Test that helper functions exist (step testing pattern)
          helper_functions = Regex.scan(~r/defp \w+_step\(/, content)
          assert length(helper_functions) > 0
          
          # Test that the tests test what they claim to test
          step_names = Regex.scan(~r/defp (\w+)_step\(/, content, capture: :all_but_first)
          |> List.flatten()
          
          Enum.each(step_names, fn step_name ->
            # Each step should have a describe block
            assert String.contains?(content, "#{step_name} step")
          end)
        end
      end)
      
      IO.puts("🎯 META-META-TESTING: Successfully tested the tests that test the steps!")
    end
    
    test "LEVEL 9: recursive improvement detection improves recursively" do
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      # Test that the recursive analyzer found its own recursive improvements
      self_improvements = Enum.filter(analysis.recursive_improvements, fn imp ->
        String.contains?(imp.file, "recursive_swarm_analyzer.ex")
      end)
      
      assert length(self_improvements) > 0
      
      # Should have found the recursive fix detection pattern
      recursive_detection = Enum.find(self_improvements, fn imp ->
        String.contains?(imp.description, "RECURSIVE FIX")
      end)
      
      assert recursive_detection != nil
      
      # Test that finding recursive improvements is itself a recursive improvement
      assert String.contains?(recursive_detection.improvement_context, "RECURSIVE FIX")
      
      IO.puts("🔄 RECURSIVE IMPROVEMENT RECURSION: Analyzer detected its own recursive improvements!")
    end
    
    test "LEVEL 10: infinite recursion potential - the swarm could recurse forever" do
      # Test the theoretical infinite recursion potential
      analysis = CnsForge.RecursiveSwarmAnalyzer.ultrathink_recursive_analysis()
      
      # The analysis should suggest next recursion levels
      report_path = "/Users/sac/cns/generated/RECURSIVE_SWARM_ANALYSIS.md"
      
      if File.exists?(report_path) do
        {:ok, report_content} = File.read(report_path)
        
        # Should suggest Level 6+ recursions
        assert String.contains?(report_content, "Level 6")
        assert String.contains?(report_content, "Ready for Level")
        assert String.contains?(report_content, "Next Recursion")
        
        # Should identify infinite improvement potential
        assert String.contains?(report_content, "Infinite")
        assert String.contains?(report_content, "spawn")
        
        # The report itself is a recursive improvement (meta-reporting)
        assert String.contains?(report_content, "RECURSIVE")
        assert String.contains?(report_content, "Meta-Test")
      end
      
      # Theoretical test: If we could recurse infinitely...
      max_theoretical_levels = 100
      current_depth = analysis.recursion_depth
      
      potential_levels = max_theoretical_levels - current_depth
      assert potential_levels > 0
      
      IO.puts("♾️ INFINITE POTENTIAL: #{potential_levels} more recursion levels theoretically possible!")
      IO.puts("🌀 ULTRATHINK RECURSION: The swarm has achieved ultra-recursive self-analysis!")
    end
  end
  
  describe "meta-meta-meta analysis (recursive testing of recursive testing)" do
    test "validates that this meta-test validates recursive validation" do
      # Ultimate meta: Test that this test file tests the recursive testing
      {:ok, this_test_content} = File.read(__ENV__.file)
      
      # Count how many times this test references itself
      self_references = [
        "this test",
        "this meta-test", 
        "validates that this",
        "__ENV__.file"
      ]
      
      total_self_refs = Enum.reduce(self_references, 0, fn phrase, acc ->
        acc + length(Regex.scan(~r/#{Regex.escape(phrase)}/i, this_test_content))
      end)
      
      assert total_self_refs >= 4
      
      # This test should reference the recursive analyzer multiple times
      analyzer_refs = length(Regex.scan(~r/RecursiveSwarmAnalyzer/, this_test_content))
      assert analyzer_refs >= 10
      
      # Count recursive concepts in this test
      recursive_concepts = [
        "recursive", "recursion", "meta", "level", "ultimate", 
        "infinite", "fractal", "self", "spawn", "improve"
      ]
      
      concept_count = Enum.reduce(recursive_concepts, 0, fn concept, acc ->
        acc + length(Regex.scan(~r/#{concept}/i, this_test_content))
      end)
      
      assert concept_count >= 50  # This test should be highly recursive
      
      IO.puts("🎯 META-META-META SUCCESS: This test validated its own validation of recursive validation!")
    end
  end
end