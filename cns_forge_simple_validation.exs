#!/usr/bin/env elixir

# CNS Forge Simple Validation System
# ==================================
# 
# Focused validation that tests the core TTL → Generation pipeline
# without relying on complex dependencies or compilation

defmodule CNSForge.SimpleValidation do
  @moduledoc """
  Simple validation system for CNS Forge TTL pipeline.
  Tests core functionality without complex dependencies.
  """
  
  require Logger
  
  @base_path "/Users/sac/cns"
  @test_output_path "/Users/sac/cns/validation_output"
  @validation_ttl_path "/Users/sac/cns/ontologies/cybersecurity_core.ttl"
  
  def run_validation() do
    Logger.info("Starting CNS Forge Simple Validation")
    
    validation_id = generate_validation_id()
    start_time = System.monotonic_time(:millisecond)
    
    results = %{
      validation_id: validation_id,
      start_time: start_time,
      steps: [],
      status: :running
    }
    
    results
    |> validate_ttl_file_exists()
    |> test_ttl_parsing()
    |> test_project_generation()
    |> test_file_generation()
    |> finalize_validation()
  end
  
  defp validate_ttl_file_exists(results) do
    Logger.info("Step 1: Validating TTL file exists")
    step_start = System.monotonic_time(:millisecond)
    
    if File.exists?(@validation_ttl_path) do
      file_size = File.stat!(@validation_ttl_path).size
      Logger.info("✅ TTL file found: #{@validation_ttl_path} (#{file_size} bytes)")
      
      step_end = System.monotonic_time(:millisecond)
      add_step(results, "ttl_file_check", :passed, step_end - step_start, %{
        file_path: @validation_ttl_path,
        file_size: file_size
      })
    else
      Logger.error("❌ TTL file not found: #{@validation_ttl_path}")
      
      step_end = System.monotonic_time(:millisecond)
      results
      |> add_step("ttl_file_check", :failed, step_end - step_start, %{
        error: "File not found"
      })
      |> Map.put(:status, :failed)
    end
  end
  
  defp test_ttl_parsing(results) do
    if results.status == :failed, do: results, else: do_test_ttl_parsing(results)
  end
  
  defp do_test_ttl_parsing(results) do
    Logger.info("Step 2: Testing TTL parsing")
    step_start = System.monotonic_time(:millisecond)
    
    try do
      {output, exit_code} = System.cmd("python3", [
        Path.join(@base_path, "ttl_to_reactor_workflows.py"),
        "--parse-only",
        "--ttl-file", @validation_ttl_path
      ], stderr_to_stdout: true)
      
      step_end = System.monotonic_time(:millisecond)
      
      if exit_code == 0 do
        concepts_extracted = extract_concept_count(output)
        Logger.info("✅ TTL parsing successful: #{concepts_extracted} concepts")
        
        add_step(results, "ttl_parsing", :passed, step_end - step_start, %{
          concepts_extracted: concepts_extracted,
          parser_output_lines: length(String.split(output, "\n"))
        })
      else
        Logger.error("❌ TTL parsing failed: #{String.slice(output, 0, 200)}...")
        
        results
        |> add_step("ttl_parsing", :failed, step_end - step_start, %{
          error: String.slice(output, 0, 500),
          exit_code: exit_code
        })
        |> Map.put(:status, :failed)
      end
    rescue
      error ->
        step_end = System.monotonic_time(:millisecond)
        Logger.error("❌ TTL parsing error: #{inspect(error)}")
        
        results
        |> add_step("ttl_parsing", :failed, step_end - step_start, %{
          error: inspect(error)
        })
        |> Map.put(:status, :failed)
    end
  end
  
  defp test_project_generation(results) do
    if results.status == :failed, do: results, else: do_test_project_generation(results)
  end
  
  defp do_test_project_generation(results) do
    Logger.info("Step 3: Testing project generation")
    step_start = System.monotonic_time(:millisecond)
    
    project_name = "TestValidation#{results.validation_id}"
    project_path = Path.join(@test_output_path, String.downcase(project_name))
    
    # Ensure output directory exists
    File.mkdir_p!(@test_output_path)
    
    try do
      {output, exit_code} = System.cmd("python3", [
        Path.join(@base_path, "ttl_to_reactor_workflows.py"),
        "--generate-project",
        "--ttl-file", @validation_ttl_path,
        "--project-name", project_name,
        "--output-path", project_path
      ], stderr_to_stdout: true)
      
      step_end = System.monotonic_time(:millisecond)
      
      if exit_code == 0 and File.dir?(project_path) do
        generated_files = count_generated_files(project_path)
        Logger.info("✅ Project generation successful: #{generated_files} files")
        
        add_step(results, "project_generation", :passed, step_end - step_start, %{
          project_path: project_path,
          generated_files: generated_files,
          project_name: project_name
        })
      else
        Logger.error("❌ Project generation failed: #{String.slice(output, 0, 200)}...")
        
        results
        |> add_step("project_generation", :failed, step_end - step_start, %{
          error: String.slice(output, 0, 500),
          exit_code: exit_code
        })
        |> Map.put(:status, :failed)
      end
    rescue
      error ->
        step_end = System.monotonic_time(:millisecond)
        Logger.error("❌ Project generation error: #{inspect(error)}")
        
        results
        |> add_step("project_generation", :failed, step_end - step_start, %{
          error: inspect(error)
        })
        |> Map.put(:status, :failed)
    end
  end
  
  defp test_file_generation(results) do
    if results.status == :failed, do: results, else: do_test_file_generation(results)
  end
  
  defp do_test_file_generation(results) do
    Logger.info("Step 4: Testing generated file validation")
    step_start = System.monotonic_time(:millisecond)
    
    # Get project path from previous step
    generation_step = Enum.find(results.steps, fn step -> step.name == "project_generation" end)
    
    if generation_step && generation_step.metadata[:project_path] do
      project_path = generation_step.metadata[:project_path]
      
      # Check for expected files
      expected_patterns = [
        "*_workflow.ex",
        "*_steps.ex", 
        "*_test.exs",
        "*_k8s.yaml"
      ]
      
      found_files = []
      
      Enum.each(expected_patterns, fn pattern ->
        matches = Path.wildcard(Path.join(project_path, pattern))
        found_files = found_files ++ matches
      end)
      
      step_end = System.monotonic_time(:millisecond)
      
      if length(found_files) >= 3 do  # At least workflow, steps, and test files
        Logger.info("✅ File generation validation successful: #{length(found_files)} files")
        
        # Validate file contents
        workflow_files = Enum.filter(found_files, &String.ends_with?(&1, "_workflow.ex"))
        workflow_valid = if length(workflow_files) > 0 do
          validate_workflow_file(hd(workflow_files))
        else
          false
        end
        
        add_step(results, "file_validation", :passed, step_end - step_start, %{
          found_files: length(found_files),
          workflow_syntax_valid: workflow_valid,
          file_types: get_file_types(found_files)
        })
      else
        Logger.error("❌ File generation validation failed: only #{length(found_files)} files found")
        
        results
        |> add_step("file_validation", :failed, step_end - step_start, %{
          found_files: length(found_files),
          expected_patterns: expected_patterns
        })
        |> Map.put(:status, :failed)
      end
    else
      step_end = System.monotonic_time(:millisecond)
      Logger.error("❌ No project path available from previous step")
      
      results
      |> add_step("file_validation", :failed, step_end - step_start, %{
        error: "No project path from generation step"
      })
      |> Map.put(:status, :failed)
    end
  end
  
  defp finalize_validation(results) do
    end_time = System.monotonic_time(:millisecond)
    total_duration = end_time - results.start_time
    
    passed_steps = Enum.count(results.steps, & &1.status == :passed)
    failed_steps = Enum.count(results.steps, & &1.status == :failed)
    success_rate = if (passed_steps + failed_steps) > 0 do
      passed_steps / (passed_steps + failed_steps)
    else
      0.0
    end
    
    final_status = if results.status == :failed, do: :failed, else: :passed
    
    final_results = results
    |> Map.put(:end_time, end_time)
    |> Map.put(:total_duration_ms, total_duration)
    |> Map.put(:status, final_status)
    |> Map.put(:summary, %{
      passed_steps: passed_steps,
      failed_steps: failed_steps,
      success_rate: success_rate,
      total_duration_ms: total_duration
    })
    
    # Generate simple report
    generate_simple_report(final_results)
    
    Logger.info("🎯 Validation completed with status: #{final_status}")
    Logger.info("📊 Success Rate: #{Float.round(success_rate * 100, 1)}%")
    Logger.info("⏱️  Total Duration: #{total_duration}ms")
    
    final_results
  end
  
  # Helper functions
  
  defp generate_validation_id() do
    :crypto.strong_rand_bytes(4) |> Base.encode16() |> String.downcase()
  end
  
  defp add_step(results, name, status, duration_ms, metadata) do
    step = %{
      name: name,
      status: status,
      duration_ms: duration_ms,
      metadata: metadata
    }
    
    Map.put(results, :steps, results.steps ++ [step])
  end
  
  defp extract_concept_count(output) do
    case Regex.run(~r/Extracted (\d+) semantic concepts/, output) do
      [_, count_str] -> String.to_integer(count_str)
      _ -> 0
    end
  end
  
  defp count_generated_files(project_path) do
    if File.dir?(project_path) do
      Path.wildcard(Path.join(project_path, "*")) |> length()
    else
      0
    end
  end
  
  defp validate_workflow_file(file_path) do
    try do
      content = File.read!(file_path)
      
      # Basic syntax validation
      has_defmodule = String.contains?(content, "defmodule")
      has_workflow = String.contains?(content, "Workflow")
      has_step = String.contains?(content, "step")
      
      has_defmodule and has_workflow and has_step
    rescue
      _ -> false
    end
  end
  
  defp get_file_types(files) do
    files
    |> Enum.map(&Path.extname/1)
    |> Enum.frequencies()
  end
  
  defp generate_simple_report(results) do
    File.mkdir_p!(@test_output_path)
    
    # Generate Mermaid report
    mermaid_content = generate_mermaid_report(results)
    mermaid_path = Path.join(@test_output_path, "validation_results_#{results.validation_id}.md")
    
    File.write!(mermaid_path, mermaid_content)
    
    Logger.info("📄 Mermaid report saved to: #{mermaid_path}")
  end
  
  defp generate_mermaid_report(results) do
    steps_diagram = results.steps
    |> Enum.with_index()
    |> Enum.map(fn {step, index} ->
      status_icon = if step.status == :passed, do: "✅", else: "❌"
      step_name = String.replace(step.name, "_", " ") |> String.capitalize()
      "  #{index + 1}[\"#{status_icon} #{step_name}<br/>#{step.duration_ms}ms\"]"
    end)
    |> Enum.join("\n")
    
    connections = results.steps
    |> Enum.with_index()
    |> Enum.map(fn {_step, index} ->
      if index < length(results.steps) - 1 do
        "  #{index + 1} --> #{index + 2}"
      else
        nil
      end
    end)
    |> Enum.filter(& &1)
    |> Enum.join("\n")
    
    """
    # CNS Forge TTL → Ash.Reactor Validation Results
    
    **Validation ID:** `#{results.validation_id}`  
    **Status:** #{if results.status == :passed, do: "✅ PASSED", else: "❌ FAILED"}  
    **Duration:** #{results.total_duration_ms}ms  
    **Success Rate:** #{Float.round(results.summary.success_rate * 100, 1)}%
    
    ## Validation Pipeline
    
    ```mermaid
    graph TD
      Start([\"🚀 Start Validation<br/>#{results.validation_id}\"]) --> 1
    #{steps_diagram}
      #{length(results.steps)}[\"📊 Complete<br/>#{results.status}\"] --> End([\"🏁 End\"])
      
    #{connections}
      
      classDef passed fill:#d4edda,stroke:#28a745,stroke-width:2px
      classDef failed fill:#f8d7da,stroke:#dc3545,stroke-width:2px
      classDef process fill:#cce5ff,stroke:#007bff,stroke-width:2px
      
      class Start,End process
    #{results.steps
    |> Enum.with_index()
    |> Enum.map(fn {step, index} ->
      class_name = if step.status == :passed, do: "passed", else: "failed"
      "  class #{index + 1} #{class_name}"
    end)
    |> Enum.join("\n")}
    ```
    
    ## Performance Metrics
    
    ```mermaid
    pie title Validation Step Performance (ms)
    #{results.steps
    |> Enum.map(fn step ->
      step_name = String.replace(step.name, "_", " ") |> String.capitalize()
      "    \"#{step_name}\" : #{step.duration_ms}"
    end)
    |> Enum.join("\n")}
    ```
    
    ## Validation Results
    
    | Step | Status | Duration | Key Metrics |
    |------|--------|----------|-------------|
    #{results.steps
    |> Enum.map(fn step ->
      status_icon = if step.status == :passed, do: "✅", else: "❌"
      step_name = String.replace(step.name, "_", " ") |> String.capitalize()
      key_metrics = get_key_metrics(step.metadata)
      "| #{step_name} | #{status_icon} #{step.status} | #{step.duration_ms}ms | #{key_metrics} |"
    end)
    |> Enum.join("\n")}
    
    ## System Status
    
    #{if results.status == :passed do
      "✅ **SYSTEM WORKING**: The TTL → Project Generation pipeline is functional"
    else
      "❌ **SYSTEM ISSUES**: The TTL → Project Generation pipeline has problems"
    end}
    
    ### Critical Path Analysis
    
    - **TTL File Access**: #{get_step_status_icon(results, "ttl_file_check")}
    - **Semantic Parsing**: #{get_step_status_icon(results, "ttl_parsing")}
    - **Project Generation**: #{get_step_status_icon(results, "project_generation")}
    - **File Validation**: #{get_step_status_icon(results, "file_validation")}
    
    ## Recommendations
    
    #{generate_recommendations(results)}
    """
  end
  
  defp get_key_metrics(metadata) do
    cond do
      Map.has_key?(metadata, :file_size) -> "#{metadata[:file_size]} bytes"
      Map.has_key?(metadata, :concepts_extracted) -> "#{metadata[:concepts_extracted]} concepts"
      Map.has_key?(metadata, :generated_files) -> "#{metadata[:generated_files]} files"
      Map.has_key?(metadata, :found_files) -> "#{metadata[:found_files]} files"
      true -> "N/A"
    end
  end
  
  defp get_step_status_icon(results, step_name) do
    case Enum.find(results.steps, & &1.name == step_name) do
      %{status: :passed} -> "✅"
      %{status: :failed} -> "❌"
      nil -> "⚠️"
    end
  end
  
  defp generate_recommendations(results) do
    if results.status == :passed do
      """
      - ✅ All validation steps passed successfully
      - 🚀 System is ready for production use
      - 📈 Consider adding more comprehensive test cases
      """
    else
      failed_steps = Enum.filter(results.steps, & &1.status == :failed)
      
      recommendations = Enum.map(failed_steps, fn step ->
        case step.name do
          "ttl_file_check" -> "- 📁 Verify TTL file path and permissions"
          "ttl_parsing" -> "- 🔍 Check TTL syntax and Python dependencies"
          "project_generation" -> "- ⚙️  Verify Python script and output permissions"
          "file_validation" -> "- 📝 Check generated file structure and content"
          _ -> "- ❓ Review #{step.name} configuration"
        end
      end)
      
      Enum.join(recommendations, "\n")
    end
  end
end

# Execute the validation
case CNSForge.SimpleValidation.run_validation() do
  %{status: :passed} = results ->
    IO.puts("\n🎉 VALIDATION SUCCESS!")
    IO.puts("The TTL → Project Generation pipeline works correctly.")
    System.halt(0)
    
  %{status: :failed} = results ->
    IO.puts("\n💥 VALIDATION FAILED!")
    failed_steps = Enum.filter(results.steps, & &1.status == :failed)
    IO.puts("Failed steps: #{Enum.map(failed_steps, & &1.name) |> Enum.join(", ")}")
    System.halt(1)
end