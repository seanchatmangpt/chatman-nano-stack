defmodule AshReactor80_20.ValidationTest do
  @moduledoc """
  Validation tests for Ash.Reactor 80/20 implementation
  Tests core functionality and OTEL integration
  """
  
  use ExUnit.Case
  require Logger
  
  setup do
    # Ensure clean state
    :ets.delete_all_objects(:ontology_classes)
    :ets.delete_all_objects(:reactor_workflows)
    :ok
  end
  
  describe "TTL Transformation" do
    test "parses simple TTL content" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix test: <http://test.org/> .
      
      test:TestClass a owl:Class .
      """
      
      assert {:ok, result} = Reactor.run(
        AshReactor80_20.TTLReactor, 
        %{ttl_content: ttl}
      )
      
      assert result.workflows_executed == 1
      assert is_list(result.results)
    end
    
    test "creates ontology classes from TTL" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:Person a owl:Class .
      ex:Organization a owl:Class .
      """
      
      {:ok, _} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ttl}
      )
      
      {:ok, classes} = AshReactor80_20.Resources.OntologyClass.list_classes()
      assert length(classes) == 2
      assert Enum.any?(classes, &(&1.name == "Person"))
      assert Enum.any?(classes, &(&1.name == "Organization"))
    end
    
    test "generates reactor workflows for each class" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:TestEntity a owl:Class .
      """
      
      {:ok, _} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ttl}
      )
      
      {:ok, workflows} = AshReactor80_20.Resources.ReactorWorkflow.list_workflows()
      assert length(workflows) == 1
      assert hd(workflows).name == "TestEntityWorkflow"
    end
  end
  
  describe "OTEL Integration" do
    test "emits telemetry events during transformation" do
      self = self()
      
      :telemetry.attach(
        "test-handler",
        [:ash_reactor, :ttl, :create_classes],
        fn event, measurements, metadata, _ ->
          send(self, {:telemetry_event, event, measurements, metadata})
        end,
        nil
      )
      
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:TelemetryTest a owl:Class .
      """
      
      {:ok, _} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ttl}
      )
      
      assert_receive {:telemetry_event, [:ash_reactor, :ttl, :create_classes], measurements, metadata}
      assert measurements.class_count == 1
      
      :telemetry.detach("test-handler")
    end
    
    test "measures execution time" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      ex:TimingTest a owl:Class .
      """
      
      start_time = System.monotonic_time(:millisecond)
      
      {:ok, result} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ttl}
      )
      
      end_time = System.monotonic_time(:millisecond)
      execution_time = end_time - start_time
      
      assert execution_time < 1000 # Should complete within 1 second
      assert result.execution_time
    end
  end
  
  describe "Error Handling" do
    test "handles invalid TTL gracefully" do
      invalid_ttl = "This is not valid TTL content"
      
      assert {:ok, result} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: invalid_ttl}
      )
      
      # Should still succeed but with no classes found
      assert result.workflows_executed == 0
    end
    
    test "handles empty TTL content" do
      assert {:ok, result} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ""}
      )
      
      assert result.workflows_executed == 0
    end
  end
  
  describe "Performance" do
    test "processes multiple classes efficiently" do
      ttl = """
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      @prefix ex: <http://example.org/> .
      
      #{Enum.map(1..10, fn i -> "ex:Class#{i} a owl:Class ." end) |> Enum.join("\n")}
      """
      
      {:ok, result} = Reactor.run(
        AshReactor80_20.TTLReactor,
        %{ttl_content: ttl}
      )
      
      assert result.workflows_executed == 10
    end
  end
end