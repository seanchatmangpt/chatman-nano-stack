#!/usr/bin/env elixir

# Simple pipeline validation without dependencies
defmodule SimplePipelineTest do
  def test_files_exist do
    files = [
      "generated_pipeline_ttl.ttl",
      "generated_ontology_dspy.py", 
      "generated_bitactor_system.md",
      "generated_pipeline_erlang.erl",
      "generated_pipeline_k8s.yaml"
    ]
    
    results = Enum.map(files, fn file ->
      {file, File.exists?(file)}
    end)
    
    failures = Enum.filter(results, fn {_, exists} -> not exists end)
    
    if length(failures) > 0 do
      IO.puts("❌ Missing files:")
      Enum.each(failures, fn {file, _} -> IO.puts("  - #{file}") end)
      {:error, :missing_files}
    else
      IO.puts("✅ All generated files exist")
      :ok
    end
  end
  
  def test_ttl_syntax do
    content = File.read!("generated_pipeline_ttl.ttl")
    
    # Basic TTL syntax validation
    has_prefixes = String.contains?(content, "@prefix")
    has_classes = String.contains?(content, "owl:Class")
    has_properties = String.contains?(content, "owl:ObjectProperty")
    
    if has_prefixes and has_classes and has_properties do
      IO.puts("✅ TTL file has valid basic structure")
      :ok
    else
      IO.puts("❌ TTL file missing required elements")
      {:error, :invalid_ttl}
    end
  end
  
  def test_erlang_syntax do
    content = File.read!("generated_pipeline_erlang.erl")
    
    # Check for basic Erlang GenServer structure
    has_module = String.contains?(content, "-module(")
    has_behaviour = String.contains?(content, "-behaviour(gen_server)")
    has_init = String.contains?(content, "init([]) ->")
    has_handle_call = String.contains?(content, "handle_call(")
    
    if has_module and has_behaviour and has_init and has_handle_call do
      IO.puts("✅ Erlang file has valid GenServer structure")
      :ok
    else
      IO.puts("❌ Erlang file missing GenServer components")
      {:error, :invalid_erlang}
    end
  end
  
  def test_k8s_yaml do
    content = File.read!("generated_pipeline_k8s.yaml")
    
    # Check for basic k8s structure
    has_deployment = String.contains?(content, "kind: Deployment")
    has_service = String.contains?(content, "kind: Service") 
    has_configmap = String.contains?(content, "kind: ConfigMap")
    
    if has_deployment and has_service and has_configmap do
      IO.puts("✅ Kubernetes YAML has required resources")
      :ok
    else
      IO.puts("❌ Kubernetes YAML missing required resources")
      {:error, :invalid_k8s}
    end
  end
  
  def run_all_tests do
    tests = [
      &test_files_exist/0,
      &test_ttl_syntax/0,
      &test_erlang_syntax/0,
      &test_k8s_yaml/0
    ]
    
    results = Enum.map(tests, fn test -> test.() end)
    failures = Enum.filter(results, fn result -> result != :ok end)
    
    if length(failures) == 0 do
      IO.puts("\n🎉 All pipeline tests passed!")
      :ok
    else
      IO.puts("\n❌ #{length(failures)} test(s) failed")
      {:error, failures}
    end
  end
end

# Run all tests
SimplePipelineTest.run_all_tests()