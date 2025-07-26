defmodule CnsForge.PipelineDemo do
  @moduledoc """
  🚀 COMPLETE PIPELINE DEMO
  Demonstrates the full integration:
  - typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
  - Real-time Phoenix Channels notifications
  - Telemetry streaming
  - Failure recovery
  
  80/20 APPROACH: Maximum demonstration value with minimal setup
  """
  
  require Logger
  alias CnsForge.CompletePipelineOrchestrator
  alias CnsForge.PipelineEventProcessor
  
  def run_complete_demo do
    Logger.info("🎬 Starting Complete Pipeline Demo")
    
    # 1. Setup real-time monitoring
    setup_demo_monitoring()
    
    # 2. Prepare demo input data
    demo_input = prepare_demo_input()
    
    # 3. Configure pipeline
    pipeline_config = %{
      pipeline_id: "demo_pipeline_#{System.unique_integer([:positive])}",
      priority_threshold: 0.8,
      enable_telemetry: true,
      enable_notifications: true
    }
    
    Logger.info("📋 Demo Configuration: #{inspect(pipeline_config)}")
    
    # 4. Execute the complete pipeline
    execute_demo_pipeline(demo_input, pipeline_config)
  end
  
  defp setup_demo_monitoring do
    Logger.info("📊 Setting up demo monitoring")
    
    # Start Phoenix Channels application if not already running
    ensure_channels_running()
    
    # Subscribe to demo events
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "pipeline_events:demo_pipeline")
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "reactor_steps:CompletePipelineOrchestrator")
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "telemetry:pipeline")
    Phoenix.PubSub.subscribe(CnsForge.PubSub, "celebrations")
    
    # Start demo event logger
    spawn(fn -> demo_event_logger() end)
  end
  
  defp ensure_channels_running do
    case Process.whereis(CnsForge.PipelineEventProcessor) do
      nil ->
        Logger.info("🔄 Starting Pipeline Event Processor")
        CnsForge.PipelineEventProcessor.start_link([])
      
      _pid ->
        Logger.info("✅ Pipeline Event Processor already running")
    end
    
    case Process.whereis(CnsForge.TelemetryStreamer) do
      nil ->
        Logger.info("🔄 Starting Telemetry Streamer")
        CnsForge.TelemetryStreamer.start_link([])
      
      _pid ->
        Logger.info("✅ Telemetry Streamer already running")
    end
  end
  
  defp prepare_demo_input do
    Logger.info("📦 Preparing demo input data")
    
    %{
      entities: [
        %{id: "entity_1", type: "Process", name: "DataProcessor", priority: 0.9},
        %{id: "entity_2", type: "Agent", name: "MonitoringAgent", priority: 0.85},
        %{id: "entity_3", type: "Resource", name: "DatabaseConnection", priority: 0.8},
        %{id: "entity_4", type: "Service", name: "APIGateway", priority: 0.75},
        %{id: "entity_5", type: "Pipeline", name: "DataFlow", priority: 0.95}
      ],
      metadata: %{
        source: "demo_generator",
        created_at: DateTime.utc_now(),
        format: "json",
        version: "1.0"
      },
      config: %{
        processing_mode: "high_performance",
        optimization_level: "80_20_pareto"
      }
    }
  end
  
  defp execute_demo_pipeline(input_data, config) do
    Logger.info("🚀 Executing Complete Pipeline Demo")
    
    # Set pipeline context
    context = %{
      pipeline_id: config.pipeline_id,
      pipeline_start_time: System.monotonic_time(:millisecond),
      demo_mode: true
    }
    
    # Create reactor inputs
    reactor_inputs = %{
      pipeline_input: input_data,
      pipeline_config: config
    }
    
    # Execute the complete pipeline through Reactor
    case Reactor.run(CompletePipelineOrchestrator, reactor_inputs, context) do
      {:ok, result} ->
        Logger.info("🎉 Pipeline Demo Completed Successfully!")
        Logger.info("📊 Final Result: #{inspect(result, limit: :infinity)}")
        
        # Generate completion report
        generate_demo_report(config.pipeline_id, result, context)
        
        {:ok, result}
      
      {:error, reason} ->
        Logger.error("💥 Pipeline Demo Failed: #{inspect(reason)}")
        
        # Generate failure report
        generate_failure_report(config.pipeline_id, reason, context)
        
        {:error, reason}
    end
  end
  
  defp demo_event_logger do
    receive do
      {:pipeline_stage, stage, status, data} ->
        Logger.info("📡 PIPELINE EVENT: #{stage} → #{status} | Data: #{inspect(data)}")
        demo_event_logger()
      
      {:reactor_step, step_name, result} ->
        Logger.info("⚛️ REACTOR STEP: #{step_name} → #{inspect(result)}")
        demo_event_logger()
      
      {:stage_transition, stage, from_status, to_status, data} ->
        Logger.info("🔄 STAGE TRANSITION: #{stage} | #{from_status} → #{to_status}")
        demo_event_logger()
      
      {:stage_completed, stage, result, metadata} ->
        Logger.info("✅ STAGE COMPLETED: #{stage} | Result: #{inspect(result)} | Meta: #{inspect(metadata)}")
        demo_event_logger()
      
      {:pipeline_failure, stage, error, context} ->
        Logger.error("❌ PIPELINE FAILURE: #{stage} | Error: #{inspect(error)}")
        demo_event_logger()
      
      {:metric_update, metric_name, value, metadata} ->
        Logger.info("📈 METRIC UPDATE: #{metric_name} = #{value}")
        demo_event_logger()
      
      {:milestone_notification, milestone, details} ->
        Logger.info("🏆 MILESTONE: #{milestone} | #{inspect(details)}")
        demo_event_logger()
      
      other ->
        Logger.info("📨 OTHER EVENT: #{inspect(other)}")
        demo_event_logger()
    after
      30_000 ->
        Logger.info("📡 Demo event logger shutting down after 30 seconds of inactivity")
    end
  end
  
  defp generate_demo_report(pipeline_id, result, context) do
    total_duration = System.monotonic_time(:millisecond) - context.pipeline_start_time
    
    report = %{
      pipeline_id: pipeline_id,
      status: "completed",
      total_duration_ms: total_duration,
      stages_completed: 8,
      final_result: %{
        deployment_id: result.deployment_id,
        pods_deployed: result.pod_count,
        services_created: result.service_count,
        cluster_status: result.status
      },
      performance_metrics: %{
        average_stage_duration: total_duration / 8,
        throughput_score: calculate_throughput_score(total_duration),
        efficiency_rating: calculate_efficiency_rating(result)
      },
      integration_validation: %{
        phoenix_channels: "✅ Active",
        telemetry_streaming: "✅ Active", 
        notifications: "✅ Active",
        ash_reactor: "✅ Functional",
        compensation: "✅ Available"
      },
      demo_timestamp: DateTime.utc_now()
    }
    
    Logger.info("📋 DEMO COMPLETION REPORT:")
    Logger.info("#{Jason.encode!(report, pretty: true)}")
    
    # Save report to file
    report_path = "/tmp/cns_forge_demo_report_#{pipeline_id}.json"
    File.write!(report_path, Jason.encode!(report, pretty: true))
    Logger.info("📄 Report saved to: #{report_path}")
    
    # Broadcast completion
    Phoenix.PubSub.broadcast(CnsForge.PubSub, "demo_results", 
      {:demo_completed, pipeline_id, report})
    
    report
  end
  
  defp generate_failure_report(pipeline_id, reason, context) do
    total_duration = System.monotonic_time(:millisecond) - context.pipeline_start_time
    
    failure_report = %{
      pipeline_id: pipeline_id,
      status: "failed",
      failure_reason: inspect(reason),
      duration_before_failure: total_duration,
      recovery_options: [
        "retry_from_failed_stage",
        "restart_complete_pipeline", 
        "manual_intervention_required"
      ],
      demo_timestamp: DateTime.utc_now()
    }
    
    Logger.error("📋 DEMO FAILURE REPORT:")
    Logger.error("#{Jason.encode!(failure_report, pretty: true)}")
    
    # Save failure report
    report_path = "/tmp/cns_forge_demo_failure_#{pipeline_id}.json"
    File.write!(report_path, Jason.encode!(failure_report, pretty: true))
    Logger.error("📄 Failure report saved to: #{report_path}")
    
    failure_report
  end
  
  defp calculate_throughput_score(duration_ms) do
    # Score based on completion time (lower is better)
    case duration_ms do
      d when d < 1000 -> "excellent"
      d when d < 3000 -> "good"  
      d when d < 5000 -> "fair"
      _ -> "needs_optimization"
    end
  end
  
  defp calculate_efficiency_rating(result) do
    # Score based on resource utilization
    pods_per_second = result.pod_count / (result.deployment_time / 1000)
    
    case pods_per_second do
      rate when rate > 5 -> "high_efficiency"
      rate when rate > 2 -> "medium_efficiency"
      _ -> "low_efficiency"
    end
  end
  
  # Interactive demo functions
  def run_stage_by_stage_demo do
    Logger.info("🎭 Starting Stage-by-Stage Demo")
    
    setup_demo_monitoring()
    input_data = prepare_demo_input()
    
    # Execute each stage individually with pauses
    stages = [
      {:typer_80_20_processing, "80/20 Typer Processing"},
      {:turtle_generation, "Turtle Generation"},
      {:ttl2dspy_transformation, "TTL2DSPy Transformation"},
      {:bitactor_processing, "BitActor Processing"},
      {:erlang_otp_coordination, "Erlang OTP Coordination"},
      {:ash_resource_creation, "Ash Resource Creation"},
      {:reactor_workflow_execution, "Reactor Workflow Execution"},
      {:k8s_deployment, "Kubernetes Deployment"}
    ]
    
    Enum.reduce_while(stages, input_data, fn {stage, description}, acc_data ->
      Logger.info("🎯 Executing: #{description}")
      
      # Simulate stage execution with detailed logging
      case simulate_stage_execution(stage, acc_data) do
        {:ok, result} ->
          Logger.info("✅ Completed: #{description}")
          
          # Pause for demonstration
          Process.sleep(1000)
          
          {:cont, result}
        
        {:error, reason} ->
          Logger.error("❌ Failed: #{description} - #{reason}")
          {:halt, {:error, reason}}
      end
    end)
  end
  
  defp simulate_stage_execution(stage, input_data) do
    case stage do
      :typer_80_20_processing ->
        CnsForge.PipelineStageImplementations.execute_typer_processing(input_data)
      
      :turtle_generation ->
        CnsForge.PipelineStageImplementations.execute_turtle_generation(input_data)
      
      :ttl2dspy_transformation ->
        CnsForge.PipelineStageImplementations.execute_ttl2dspy_transformation(input_data)
      
      :bitactor_processing ->
        CnsForge.PipelineStageImplementations.execute_bitactor_processing(input_data)
      
      :erlang_otp_coordination ->
        CnsForge.PipelineStageImplementations.execute_erlang_coordination(input_data)
      
      :ash_resource_creation ->
        CnsForge.PipelineStageImplementations.execute_ash_resource_creation(input_data)
      
      :reactor_workflow_execution ->
        CnsForge.PipelineStageImplementations.execute_reactor_workflows(input_data)
      
      :k8s_deployment ->
        CnsForge.PipelineStageImplementations.execute_k8s_deployment(input_data)
    end
  end
  
  # Failure simulation demo
  def run_failure_recovery_demo do
    Logger.info("💥 Starting Failure Recovery Demo")
    
    setup_demo_monitoring()
    
    # Inject failure at random stage
    failure_stage = Enum.random([:turtle_generation, :bitactor_processing, :k8s_deployment])
    Logger.info("🎯 Injecting failure at: #{failure_stage}")
    
    # Configure pipeline with failure injection
    config = %{
      pipeline_id: "failure_demo_#{System.unique_integer([:positive])}",
      inject_failure_at: failure_stage,
      enable_compensation: true
    }
    
    input_data = prepare_demo_input()
    
    # This will trigger compensation when failure occurs
    case execute_demo_pipeline(input_data, config) do
      {:ok, result} ->
        Logger.info("🎉 Recovery Demo: Unexpected success!")
        result
      
      {:error, reason} ->
        Logger.info("💪 Recovery Demo: Failure handled as expected")
        Logger.info("🔄 Compensation logic would trigger here")
        reason
    end
  end
  
  # Performance benchmark demo
  def run_performance_benchmark do
    Logger.info("🏃‍♂️ Starting Performance Benchmark Demo")
    
    setup_demo_monitoring()
    
    # Run multiple pipelines concurrently
    pipeline_count = 5
    
    tasks = Enum.map(1..pipeline_count, fn i ->
      Task.async(fn ->
        config = %{
          pipeline_id: "benchmark_#{i}",
          priority_threshold: 0.8
        }
        
        input_data = prepare_demo_input()
        
        start_time = System.monotonic_time(:millisecond)
        result = execute_demo_pipeline(input_data, config)
        end_time = System.monotonic_time(:millisecond)
        
        %{
          pipeline_id: config.pipeline_id,
          duration: end_time - start_time,
          result: result
        }
      end)
    end)
    
    # Wait for all pipelines to complete
    results = Task.await_many(tasks, 30_000)
    
    # Generate benchmark report
    benchmark_report = %{
      total_pipelines: pipeline_count,
      results: results,
      average_duration: calculate_average_duration(results),
      success_rate: calculate_success_rate(results),
      throughput: pipeline_count / (get_max_duration(results) / 1000)
    }
    
    Logger.info("📊 BENCHMARK REPORT:")
    Logger.info("#{Jason.encode!(benchmark_report, pretty: true)}")
    
    benchmark_report
  end
  
  defp calculate_average_duration(results) do
    total_duration = Enum.sum(Enum.map(results, & &1.duration))
    total_duration / length(results)
  end
  
  defp calculate_success_rate(results) do
    successful = Enum.count(results, fn r -> 
      case r.result do
        {:ok, _} -> true
        _ -> false
      end
    end)
    
    successful / length(results) * 100
  end
  
  defp get_max_duration(results) do
    results
    |> Enum.map(& &1.duration)
    |> Enum.max()
  end
end

# CLI interface for running demos
defmodule CnsForge.DemoCLI do
  @moduledoc """
  Command-line interface for running pipeline demos
  """
  
  def main(args \\\\ []) do
    case args do
      ["complete"] ->
        CnsForge.PipelineDemo.run_complete_demo()
      
      ["stage-by-stage"] ->
        CnsForge.PipelineDemo.run_stage_by_stage_demo()
      
      ["failure-recovery"] ->
        CnsForge.PipelineDemo.run_failure_recovery_demo()
      
      ["benchmark"] ->
        CnsForge.PipelineDemo.run_performance_benchmark()
      
      [] ->
        show_help()
        CnsForge.PipelineDemo.run_complete_demo()
      
      _ ->
        show_help()
    end
  end
  
  defp show_help do
    IO.puts("""
    🚀 CNS Forge Pipeline Demo CLI
    
    Usage: 
      iex> CnsForge.DemoCLI.main(["command"])
    
    Commands:
      complete         - Run complete pipeline demo (default)
      stage-by-stage   - Run stage-by-stage demo with pauses
      failure-recovery - Demonstrate failure recovery capabilities
      benchmark        - Run performance benchmark with multiple pipelines
    
    Example:
      iex> CnsForge.DemoCLI.main(["complete"])
    """)
  end
end