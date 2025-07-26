#!/usr/bin/env elixir

# Test complete pipeline end-to-end
Mix.install([
  {:ash, "~> 3.0"},
  {:ash_postgres, "~> 2.0"}
])

Code.require_file("lib/cns_forge/pipeline_connector.ex")

defmodule PipelineTest do
  def run_pipeline_test do
    IO.puts("🧪 Testing complete pipeline...")
    
    try do
      # Test pipeline execution
      result = CnsForge.PipelineConnector.execute_full_pipeline()
      
      case result do
        {:ok, _} -> 
          IO.puts("✅ Pipeline executed successfully")
          :ok
        {:error, reason} -> 
          IO.puts("❌ Pipeline failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      e -> 
        IO.puts("❌ Pipeline crashed: #{inspect(e)}")
        {:error, e}
    end
  end
  
  def test_generated_files do
    files = [
      "generated_pipeline_ttl.ttl",
      "generated_ontology_dspy.py", 
      "generated_bitactor_system.md",
      "generated_pipeline_erlang.erl",
      "generated_pipeline_k8s.yaml"
    ]
    
    Enum.each(files, fn file ->
      if File.exists?(file) do
        IO.puts("✅ #{file} exists")
      else
        IO.puts("❌ #{file} missing")
      end
    end)
  end
end

# Run tests
PipelineTest.test_generated_files()
PipelineTest.run_pipeline_test()