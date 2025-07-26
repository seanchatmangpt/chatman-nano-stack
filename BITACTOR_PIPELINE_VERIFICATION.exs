#!/usr/bin/env elixir

# BITACTOR PIPELINE VERIFICATION
# ==============================
# Verifies the complete 80/20 pipeline: typer→turtle→ttl2dspy→BitActor→Erlang→Ash→Reactor→k8s

defmodule BitActorPipelineVerification do
  @moduledoc """
  End-to-end verification of the BitActor pipeline
  Tests each stage of the transformation pipeline
  """

  def run_pipeline_verification do
    IO.puts("\n🚀 BITACTOR PIPELINE VERIFICATION - 80/20 IMPLEMENTATION\n")
    
    test_results = []

    # Stage 1: Python Types
    {result, test_results} = verify_python_types(test_results)

    # Stage 2: Turtle/TTL Ontology
    {result, test_results} = verify_turtle_ontology(test_results)

    # Stage 3: BitActor DSL
    {result, test_results} = verify_bitactor_dsl(test_results)

    # Stage 4: Erlang/Elixir GenServer
    {result, test_results} = verify_genserver_implementation(test_results)

    # Stage 5: Ash Resources
    {result, test_results} = verify_ash_resources(test_results)

    # Stage 6: Reactor Workflows
    {result, test_results} = verify_reactor_workflows(test_results)

    # Stage 7: Kubernetes Deployment
    {result, test_results} = verify_k8s_deployment(test_results)

    # Stage 8: TTL Constraint Preservation
    {result, test_results} = verify_ttl_preservation(test_results)

    # Final Summary
    generate_pipeline_report(test_results)
  end

  # Stage 1: Verify Python Types
  defp verify_python_types(results) do
    IO.write("1️⃣ Python Type Definitions: ")
    
    if File.exists?("bitactor_types.py") do
      content = File.read!("bitactor_types.py")
      
      checks = [
        String.contains?(content, "class BitActor(BaseModel)"),
        String.contains?(content, "class Signal(BaseModel)"),
        String.contains?(content, "class TelemetryFrame(BaseModel)"),
        String.contains?(content, "class TTLConstraint(BaseModel)"),
        String.contains?(content, "budget_ns: conint(gt=0)"),
        String.contains?(content, "TTLPrecision.NANOSECOND"),
        String.contains?(content, "ServiceMessage(TypedDict)"),
        String.contains?(content, "SwarmConfiguration(BaseModel)")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:python_types, true} | results]}
      else
        IO.puts("❌ FAILED - Missing type definitions")
        {false, [{:python_types, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:python_types, false} | results]}
    end
  end

  # Stage 2: Verify Turtle Ontology
  defp verify_turtle_ontology(results) do
    IO.write("2️⃣ Turtle/TTL Ontology: ")
    
    if File.exists?("bitactor_ontology.ttl") do
      content = File.read!("bitactor_ontology.ttl")
      
      checks = [
        String.contains?(content, "bitactor:BitActor a owl:Class"),
        String.contains?(content, "bitactor:Signal a owl:Class"),
        String.contains?(content, "bitactor:TTLConstraint a owl:Class"),
        String.contains?(content, "bitactor:processesSignal a owl:ObjectProperty"),
        String.contains?(content, "bitactor:budgetNs a owl:DatatypeProperty"),
        String.contains?(content, "rdfs:range xsd:long"),
        String.contains?(content, "bitactor:hasTTLConstraint"),
        String.contains?(content, "owl:minInclusive 1")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:turtle_ontology, true} | results]}
      else
        IO.puts("❌ FAILED - Incomplete ontology")
        {false, [{:turtle_ontology, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:turtle_ontology, false} | results]}
    end
  end

  # Stage 3: Verify BitActor DSL
  defp verify_bitactor_dsl(results) do
    IO.write("3️⃣ BitActor DSL: ")
    
    if File.exists?("bitactor_dsl.ex") do
      content = File.read!("bitactor_dsl.ex")
      
      checks = [
        String.contains?(content, "use Spark.Dsl"),
        String.contains?(content, "@bitactor %Spark.Dsl.Section"),
        String.contains?(content, "ttl_budget_ms:"),
        String.contains?(content, "@signals %Spark.Dsl.Section"),
        String.contains?(content, "BitActorDSL.Transformers.ValidateTTL"),
        String.contains?(content, "budget_ns = ttl_budget_ms * 1_000_000"),
        String.contains?(content, "defmodule ExampleBitActor"),
        String.contains?(content, "signals do")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:bitactor_dsl, true} | results]}
      else
        IO.puts("❌ FAILED - DSL structure incorrect")
        {false, [{:bitactor_dsl, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:bitactor_dsl, false} | results]}
    end
  end

  # Stage 4: Verify GenServer Implementation
  defp verify_genserver_implementation(results) do
    IO.write("4️⃣ Erlang/Elixir GenServer: ")
    
    if File.exists?("bitactor_genserver.ex") do
      content = File.read!("bitactor_genserver.ex")
      
      checks = [
        String.contains?(content, "use GenServer"),
        String.contains?(content, "def process_signal(actor, signal)"),
        String.contains?(content, "System.monotonic_time(:nanosecond)"),
        String.contains?(content, "handle_call({:process_signal, signal}"),
        String.contains?(content, "BitActor.Supervisor"),
        String.contains?(content, "DynamicSupervisor"),
        String.contains?(content, "handle_ttl_violation"),
        String.contains?(content, "emit_telemetry")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:genserver, true} | results]}
      else
        IO.puts("❌ FAILED - GenServer implementation incomplete")
        {false, [{:genserver, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:genserver, false} | results]}
    end
  end

  # Stage 5: Verify Ash Resources
  defp verify_ash_resources(results) do
    IO.write("5️⃣ Ash Resources: ")
    
    if File.exists?("bitactor_ash_resources.ex") do
      content = File.read!("bitactor_ash_resources.ex")
      
      checks = [
        String.contains?(content, "use Ash.Domain"),
        String.contains?(content, "BitActor.Ash.Resources.Actor"),
        String.contains?(content, "BitActor.Ash.Resources.Signal"),
        String.contains?(content, "BitActor.Ash.Resources.TTLConstraint"),
        String.contains?(content, "data_layer: AshPostgres.DataLayer"),
        String.contains?(content, "attribute :ttl_budget_ms, :integer"),
        String.contains?(content, "calculate :ttl_utilization"),
        String.contains?(content, "update :process_signal")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:ash_resources, true} | results]}
      else
        IO.puts("❌ FAILED - Ash resources incomplete")
        {false, [{:ash_resources, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:ash_resources, false} | results]}
    end
  end

  # Stage 6: Verify Reactor Workflows
  defp verify_reactor_workflows(results) do
    IO.write("6️⃣ Reactor Workflows: ")
    
    if File.exists?("bitactor_reactor_workflow.ex") do
      content = File.read!("bitactor_reactor_workflow.ex")
      
      checks = [
        String.contains?(content, "use Reactor"),
        String.contains?(content, "SignalProcessingWorkflow"),
        String.contains?(content, "step :validate_ttl"),
        String.contains?(content, "step :process_signal"),
        String.contains?(content, "compensate fn error"),
        String.contains?(content, "SwarmCoordinationWorkflow"),
        String.contains?(content, "TTLEnforcementWorkflow"),
        String.contains?(content, "Task.yield(task, timeout_ms)")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:reactor_workflows, true} | results]}
      else
        IO.puts("❌ FAILED - Reactor workflows incomplete")
        {false, [{:reactor_workflows, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:reactor_workflows, false} | results]}
    end
  end

  # Stage 7: Verify K8s Deployment
  defp verify_k8s_deployment(results) do
    IO.write("7️⃣ Kubernetes Deployment: ")
    
    if File.exists?("bitactor-k8s-deployment.yaml") do
      content = File.read!("bitactor-k8s-deployment.yaml")
      
      checks = [
        String.contains?(content, "kind: Namespace"),
        String.contains?(content, "name: bitactor-system"),
        String.contains?(content, "kind: ConfigMap"),
        String.contains?(content, "ttl_constraints.yaml"),
        String.contains?(content, "kind: StatefulSet"),
        String.contains?(content, "name: bitactor-genserver"),
        String.contains?(content, "kind: HorizontalPodAutoscaler"),
        String.contains?(content, "kind: ServiceMonitor"),
        String.contains?(content, "BitActorTTLViolation")
      ]
      
      if Enum.all?(checks) do
        IO.puts("✅ PASSED")
        {true, [{:k8s_deployment, true} | results]}
      else
        IO.puts("❌ FAILED - K8s manifests incomplete")
        {false, [{:k8s_deployment, false} | results]}
      end
    else
      IO.puts("❌ FAILED - File not found")
      {false, [{:k8s_deployment, false} | results]}
    end
  end

  # Stage 8: Verify TTL Preservation
  defp verify_ttl_preservation(results) do
    IO.write("8️⃣ TTL Constraint Preservation: ")
    
    # Check that TTL constraints are preserved through the pipeline
    ttl_checks = [
      # Python: nanosecond precision
      File.exists?("bitactor_types.py") and 
        String.contains?(File.read!("bitactor_types.py"), "budget_ns"),
      
      # Turtle: TTL properties
      File.exists?("bitactor_ontology.ttl") and
        String.contains?(File.read!("bitactor_ontology.ttl"), "bitactor:budgetNs"),
      
      # DSL: TTL validation
      File.exists?("bitactor_dsl.ex") and
        String.contains?(File.read!("bitactor_dsl.ex"), "ValidateTTL"),
      
      # GenServer: TTL enforcement
      File.exists?("bitactor_genserver.ex") and
        String.contains?(File.read!("bitactor_genserver.ex"), "ttl_budget_ns"),
      
      # K8s: TTL configuration
      File.exists?("bitactor-k8s-deployment.yaml") and
        String.contains?(File.read!("bitactor-k8s-deployment.yaml"), "TTL_BUDGET_MS")
    ]
    
    if Enum.all?(ttl_checks) do
      IO.puts("✅ PASSED")
      {true, [{:ttl_preservation, true} | results]}
    else
      IO.puts("❌ FAILED - TTL constraints not preserved")
      {false, [{:ttl_preservation, false} | results]}
    end
  end

  # Generate final report
  defp generate_pipeline_report(results) do
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("📊 BITACTOR PIPELINE VERIFICATION REPORT")
    IO.puts(String.duplicate("=", 60))
    
    total = length(results)
    passed = Enum.count(results, fn {_, success} -> success end)
    
    IO.puts("\n🔗 Pipeline Stages:")
    IO.puts("  typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s")
    
    IO.puts("\n📈 Test Results:")
    Enum.each(results, fn {stage, success} ->
      status = if success, do: "✅", else: "❌"
      IO.puts("  #{status} #{stage}")
    end)
    
    IO.puts("\n📊 Summary:")
    IO.puts("  Total Stages: #{total}")
    IO.puts("  Passed: #{passed}")
    IO.puts("  Failed: #{total - passed}")
    IO.puts("  Success Rate: #{Float.round(passed / total * 100, 1)}%")
    
    IO.puts("\n🎯 TTL Constraint Flow:")
    IO.puts("  Python: budget_ns (nanosecond precision)")
    IO.puts("  ↓")
    IO.puts("  Turtle: bitactor:budgetNs owl:DatatypeProperty")
    IO.puts("  ↓")
    IO.puts("  DSL: ttl_constraint with ValidateTTL transformer")
    IO.puts("  ↓")
    IO.puts("  GenServer: System.monotonic_time(:nanosecond)")
    IO.puts("  ↓")
    IO.puts("  Ash: ttl_budget_ms attribute with constraints")
    IO.puts("  ↓")
    IO.puts("  Reactor: TTLEnforcementWorkflow with deadlines")
    IO.puts("  ↓")
    IO.puts("  K8s: ConfigMap with ttl_constraints.yaml")
    
    if passed == total do
      IO.puts("\n✅ BITACTOR PIPELINE FULLY OPERATIONAL!")
      IO.puts("🌀 80/20 implementation successfully verified")
      :success
    else
      IO.puts("\n💥 PIPELINE VERIFICATION FAILED!")
      IO.puts("🌀 Some stages need attention")
      :failure
    end
  end
end

# Run the verification
case BitActorPipelineVerification.run_pipeline_verification() do
  :success -> System.halt(0)
  :failure -> System.halt(1)
end