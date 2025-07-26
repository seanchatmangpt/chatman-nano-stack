#!/usr/bin/env elixir

# TTL TO ASH GENERATOR TEST
# =========================
# Tests the generated Ash resources from TTL ontology

defmodule TTLToAshGeneratorTest do
  @moduledoc """
  Validates that the TTL to Ash generator creates proper resources
  Tests both the Python generator and resulting Elixir code
  """

  def run_generator_tests do
    IO.puts("\n🔬 TTL TO ASH GENERATOR TEST\n")

    test_results = []

    # Test 1: Python generator execution
    {result, test_results} = test_python_generator(test_results)

    # Test 2: Generated file structure
    {result, test_results} = test_generated_structure(test_results)

    # Test 3: Generated Ash domains
    {result, test_results} = test_generated_domains(test_results)

    # Test 4: Generated Ash resources
    {result, test_results} = test_generated_resources(test_results)

    # Test 5: Generated mix task script
    {result, test_results} = test_mix_task_script(test_results)

    # Test 6: TTL parsing accuracy
    {result, test_results} = test_ttl_parsing_accuracy(test_results)

    # Summary
    summarize_results(test_results)
  end

  defp test_python_generator(results) do
    IO.write("1. Python TTL to Ash generator: ")
    
    # Check if generator exists
    if File.exists?("ttl_to_ash_generator.py") do
      # Test running the generator
      {output, exit_code} = System.cmd("python", [
        "ttl_to_ash_generator.py",
        "test_cybersecurity_ontology.ttl",
        "--app-name", "test_app",
        "--output-dir", "test_generated"
      ], stderr_to_stdout: true)
      
      success = exit_code == 0 and String.contains?(output, "Generation complete!")
      
      # Cleanup test directory
      File.rm_rf!("test_generated")
      
      if success do
        IO.puts("✅ PASSED")
        {true, [{:python_generator, true} | results]}
      else
        IO.puts("❌ FAILED - Generator error: #{inspect(output)}")
        {false, [{:python_generator, false} | results]}
      end
    else
      IO.puts("❌ FAILED - Generator not found")
      {false, [{:python_generator, false} | results]}
    end
  end

  defp test_generated_structure(results) do
    IO.write("2. Generated file structure: ")
    
    expected_files = [
      "generated_ash_resources/lib/cybersec/assets/domain.ex",
      "generated_ash_resources/lib/cybersec/threats/domain.ex", 
      "generated_ash_resources/lib/cybersec/controls/domain.ex",
      "generated_ash_resources/lib/cybersec/assets/asset.ex",
      "generated_ash_resources/lib/cybersec/assets/network_asset.ex",
      "generated_ash_resources/lib/cybersec/threats/threat.ex",
      "generated_ash_resources/lib/cybersec/threats/vulnerability.ex",
      "generated_ash_resources/lib/cybersec/controls/security_control.ex",
      "generated_ash_resources/lib/cybersec/controls/security_incident.ex",
      "generated_ash_resources/generate_ash_resources.sh"
    ]
    
    all_exist = Enum.all?(expected_files, &File.exists?/1)
    
    if all_exist do
      IO.puts("✅ PASSED")
      {true, [{:file_structure, true} | results]}
    else
      missing = Enum.filter(expected_files, &(!File.exists?(&1)))
      IO.puts("❌ FAILED - Missing files: #{inspect(missing)}")
      {false, [{:file_structure, false} | results]}
    end
  end

  defp test_generated_domains(results) do
    IO.write("3. Generated Ash domains: ")
    
    # Check domain content
    assets_domain = File.read!("generated_ash_resources/lib/cybersec/assets/domain.ex")
    
    checks = [
      String.contains?(assets_domain, "defmodule Cybersec.Assets do"),
      String.contains?(assets_domain, "use Ash.Domain"),
      String.contains?(assets_domain, "otp_app: :cybersec"),
      String.contains?(assets_domain, "resources do"),
      String.contains?(assets_domain, "resource Cybersec.Assets.Asset"),
      String.contains?(assets_domain, "resource Cybersec.Assets.NetworkAsset")
    ]
    
    if Enum.all?(checks) do
      IO.puts("✅ PASSED")
      {true, [{:domains, true} | results]}
    else
      IO.puts("❌ FAILED - Domain structure incorrect")
      {false, [{:domains, false} | results]}
    end
  end

  defp test_generated_resources(results) do
    IO.write("4. Generated Ash resources: ")
    
    # Check a resource with relationships
    incident_resource = File.read!("generated_ash_resources/lib/cybersec/controls/security_incident.ex")
    
    checks = [
      String.contains?(incident_resource, "defmodule Cybersec.Controls.SecurityIncident do"),
      String.contains?(incident_resource, "use Ash.Resource"),
      String.contains?(incident_resource, "domain: Cybersec.Controls"),
      String.contains?(incident_resource, "uuid_primary_key :id"),
      String.contains?(incident_resource, "timestamps()"),
      String.contains?(incident_resource, "actions do"),
      String.contains?(incident_resource, "defaults [:read, :destroy]")
    ]
    
    if Enum.all?(checks) do
      IO.puts("✅ PASSED")
      {true, [{:resources, true} | results]}
    else
      IO.puts("❌ FAILED - Resource structure incorrect")
      {false, [{:resources, false} | results]}
    end
  end

  defp test_mix_task_script(results) do
    IO.write("5. Generated mix task script: ")
    
    script = File.read!("generated_ash_resources/generate_ash_resources.sh")
    
    checks = [
      String.contains?(script, "#!/bin/bash"),
      String.contains?(script, "mix ash.gen.domain Cybersec.Assets"),
      String.contains?(script, "mix ash.gen.domain Cybersec.Threats"),
      String.contains?(script, "mix ash.gen.domain Cybersec.Controls"),
      String.contains?(script, "mix ash.gen.resource Cybersec.Assets.Asset"),
      String.contains?(script, "--domain Cybersec.Assets"),
      String.contains?(script, "--default-actions read,create,update,destroy"),
      String.contains?(script, "--timestamps")
    ]
    
    # Check if executable
    stat = File.stat!("generated_ash_resources/generate_ash_resources.sh")
    is_executable = rem(stat.mode, 8) >= 1  # Check if executable
    
    if Enum.all?(checks) and is_executable do
      IO.puts("✅ PASSED")
      {true, [{:mix_script, true} | results]}
    else
      IO.puts("❌ FAILED - Script incorrect or not executable")
      {false, [{:mix_script, false} | results]}
    end
  end

  defp test_ttl_parsing_accuracy(results) do
    IO.write("6. TTL parsing accuracy: ")
    
    # Test that the parser correctly identified classes and properties
    ttl_content = File.read!("test_cybersecurity_ontology.ttl")
    
    # Count expected elements
    class_count = length(Regex.scan(~r/\w+:\w+\s+a\s+owl:Class/, ttl_content))
    property_count = length(Regex.scan(~r/\w+:\w+\s+a\s+owl:(?:Object|Datatype)Property/, ttl_content))
    
    # The generator output should mention these counts
    {output, _} = System.cmd("python", [
      "ttl_to_ash_generator.py",
      "test_cybersecurity_ontology.ttl"
    ], stderr_to_stdout: true)
    
    parsed_correctly = String.contains?(output, "Parsed 6 classes and 11 properties")
    
    if parsed_correctly do
      IO.puts("✅ PASSED")
      {true, [{:ttl_parsing, true} | results]}
    else
      IO.puts("❌ FAILED - Incorrect parsing counts")
      {false, [{:ttl_parsing, false} | results]}
    end
  end

  defp summarize_results(results) do
    IO.puts("\n📊 TTL TO ASH GENERATOR TEST SUMMARY:")
    
    total = length(results)
    passed = Enum.count(results, fn {_, success} -> success end)
    
    IO.puts("Total Tests: #{total}")
    IO.puts("Passed: #{passed}")
    IO.puts("Failed: #{total - passed}")
    IO.puts("Success Rate: #{Float.round(passed / total * 100, 1)}%")
    
    if passed == total do
      IO.puts("\n✅ TTL TO ASH GENERATOR WORKING CORRECTLY!")
      :success
    else
      IO.puts("\n💥 SOME TESTS FAILED - GENERATOR NEEDS FIXES!")
      :failure
    end
  end
end

# Run the tests
case TTLToAshGeneratorTest.run_generator_tests() do
  :success -> System.halt(0)
  :failure -> System.halt(1)
end