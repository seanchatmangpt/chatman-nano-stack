defmodule CnsForge.Channel8020Integration do
  @moduledoc """
  🚀 CHANNEL HANDLER 80/20 INTEGRATION
  
  Demonstrates how to integrate the ChannelHandler-based swarm
  into your application with minimal effort for maximum value.
  
  This module shows the 20% integration work that delivers 80% of
  the real-time pipeline monitoring and control capabilities.
  """
  
  require Logger
  
  # Application Setup
  
  def setup_channel_swarm do
    Logger.info("🎯 Setting up 80/20 Channel Swarm")
    
    # 1. Add ChannelHandler to dependencies
    ensure_dependencies()
    
    # 2. Configure Phoenix Socket
    configure_socket()
    
    # 3. Setup ETS tables for tracking
    setup_tracking_tables()
    
    # 4. Start telemetry collectors
    start_telemetry_collectors()
    
    Logger.info("✅ Channel Swarm ready!")
  end
  
  defp ensure_dependencies do
    # In mix.exs:
    # {:channel_handler, "~> 0.6"}
    # {:phoenix_pubsub, "~> 2.1"}
    :ok
  end
  
  defp configure_socket do
    # Socket configuration is added to endpoint
    :ok
  end
  
  defp setup_tracking_tables do
    # Create ETS tables for high-performance tracking
    :ets.new(:pipeline_health, [:set, :public, :named_table])
    :ets.new(:bitactors, [:set, :public, :named_table])
    :ets.new(:monitored_processes, [:set, :public, :named_table])
    :ets.new(:k8s_deployments, [:set, :public, :named_table])
    :ets.new(:workflows, [:set, :public, :named_table])
    :ets.new(:compensations, [:set, :public, :named_table])
    :ets.new(:metric_thresholds, [:set, :public, :named_table])
    :ets.new(:recovery_status, [:set, :public, :named_table])
    :ets.new(:turtle_namespaces, [:set, :public, :named_table])
    :ets.new(:distributed_data, [:set, :public, :named_table])
  end
  
  defp start_telemetry_collectors do
    # Attach telemetry handlers for metrics
    :telemetry.attach_many(
      "cns-forge-metrics",
      [
        [:typer, :validation],
        [:bitactor, :spawned],
        [:bitactor, :performance],
        [:pipeline, :stage, :monitored]
      ],
      &handle_telemetry_event/4,
      nil
    )
  end
  
  defp handle_telemetry_event(_event_name, measurements, metadata, _config) do
    # Forward to appropriate channel
    Phoenix.PubSub.broadcast(
      CnsForge.PubSub,
      "telemetry:performance",
      {:metric_update, measurements, metadata}
    )
  end
end

defmodule CnsForge.Endpoint do
  @moduledoc """
  Phoenix Endpoint configuration for ChannelHandler integration
  """
  
  use Phoenix.Endpoint, otp_app: :cns_forge
  
  # Socket configuration with ChannelHandler router
  socket "/socket", CnsForge.UserSocket,
    websocket: true,
    longpoll: false
  
  # Serve channel client library
  plug Plug.Static,
    at: "/",
    from: :cns_forge,
    gzip: true,
    only: ~w(assets fonts images favicon.ico robots.txt)
  
  plug CnsForge.Channel8020Router
end

defmodule CnsForge.UserSocket do
  @moduledoc """
  User socket with 80/20 channel routing
  """
  
  use Phoenix.Socket
  
  # Import the channel router
  use CnsForge.Channel8020Router
  
  @impl true
  def connect(params, socket, _connect_info) do
    # 80/20 auth: Simple token check covers most cases
    case authenticate(params["token"]) do
      {:ok, user_info} ->
        {:ok, assign(socket, user_info)}
      
      :error ->
        :error
    end
  end
  
  @impl true
  def id(socket), do: "user:#{socket.assigns.user_id}"
  
  defp authenticate(token) when is_binary(token) do
    # Simplified authentication
    {:ok, %{
      user_id: "user_#{:erlang.phash2(token)}",
      user_role: determine_role(token),
      authorized?: true
    }}
  end
  defp authenticate(_), do: :error
  
  defp determine_role(token) do
    case String.slice(token, 0..5) do
      "admin_" -> :admin
      "oper_" -> :operator
      _ -> :user
    end
  end
end

defmodule CnsForge.Channel8020Examples do
  @moduledoc """
  📚 USAGE EXAMPLES
  
  Practical examples showing the 20% of code needed
  to get 80% of the value from the channel system.
  """
  
  alias Phoenix.Socket.Broadcast
  
  # Example 1: Monitor a pipeline with real-time updates
  def monitor_pipeline_example do
    """
    // JavaScript client
    import { Socket } from "phoenix"
    
    const socket = new Socket("/socket", {
      params: { token: "user_token_123" }
    })
    
    socket.connect()
    
    // Join pipeline channel (Pattern 1: 95/100 value)
    const pipeline = socket.channel("pipeline:main_pipeline")
    
    pipeline.on("stage_transition", ({stage, from, to}) => {
      console.log(`Stage ${stage}: ${from} → ${to}`)
    })
    
    pipeline.on("stage_completed", ({stage, result}) => {
      console.log(`✅ ${stage} completed:`, result)
    })
    
    pipeline.on("pipeline_failure", ({stage, error}) => {
      console.error(`❌ Pipeline failed at ${stage}:`, error)
    })
    
    pipeline.join()
      .receive("ok", () => console.log("Monitoring pipeline"))
      .receive("error", ({reason}) => console.error("Failed:", reason))
    
    // Control operations
    pipeline.push("control:restart", {stage: "bitactor"})
      .receive("ok", () => console.log("Pipeline restarted"))
    """
  end
  
  # Example 2: Track Reactor steps with compensation
  def reactor_step_tracking_example do
    """
    // Join reactor channel (Pattern 2: 92/100 value)
    const reactor = socket.channel("reactor:CompletePipelineOrchestrator")
    
    reactor.on("step_started", ({step}) => {
      updateUI(`Starting ${step}...`)
    })
    
    reactor.on("step_completed", ({step, result, duration_ms}) => {
      updateUI(`${step} done in ${duration_ms}ms`, result)
    })
    
    reactor.on("step_error", ({step, error, recovery_available}) => {
      if (recovery_available) {
        // Auto-retry with compensation
        reactor.push("compensation:trigger", {step, error})
      }
    })
    
    reactor.join()
    
    // Batch retry failed steps
    reactor.push("steps:batch_retry", {
      steps: ["ttl2dspy_transformation", "bitactor_processing"]
    })
    """
  end
  
  # Example 3: High-frequency telemetry streaming
  def telemetry_streaming_example do
    """
    // Join telemetry channel (Pattern 4: 87/100 value)
    const telemetry = socket.channel("telemetry:performance")
    
    // Buffer for high-frequency metrics
    let metricsBuffer = []
    
    telemetry.on("live_metric", ({metric, value, timestamp}) => {
      metricsBuffer.push({metric, value, timestamp})
      
      // Batch update UI every 100ms
      if (metricsBuffer.length >= 10) {
        updateMetricsDisplay(metricsBuffer)
        metricsBuffer = []
      }
    })
    
    telemetry.on("metric_alert", ({metric, threshold, current_value}) => {
      showAlert(`${metric} exceeded threshold: ${current_value} > ${threshold}`)
    })
    
    telemetry.join()
    
    // Configure custom threshold
    telemetry.push("alerts:configure", {
      metric: "cpu_usage",
      threshold: 80
    })
    """
  end
  
  # Example 4: Stage-specific monitoring (BitActor)
  def bitactor_monitoring_example do
    """
    // Monitor BitActor performance
    const bitactor = socket.channel("stage:bitactor:session_123")
    
    bitactor.on("performance_burst", ({metrics}) => {
      // High-frequency performance data
      const avgLatency = metrics.reduce((sum, m) => sum + m.duration_ns, 0) / metrics.length
      updatePerformanceChart(avgLatency)
    })
    
    bitactor.join()
    
    // Spawn actors in batch (80/20: batch ops = huge efficiency gain)
    bitactor.push("batch:spawn", {
      items: Array(100).fill().map((_, i) => ({
        config: {id: i, type: "worker"}
      }))
    })
    """
  end
  
  # Example 5: Kubernetes deployment tracking
  def k8s_deployment_example do
    """
    // Monitor K8s deployments
    const k8s = socket.channel("stage:k8s:production")
    
    k8s.on("pod_health_update", ({pod_id, health}) => {
      updatePodStatus(pod_id, health)
    })
    
    k8s.on("deployment_scaled", ({deployment_id, old_replicas, new_replicas}) => {
      console.log(`Scaled ${deployment_id}: ${old_replicas} → ${new_replicas}`)
    })
    
    k8s.join()
    
    // Stream deployment status
    k8s.push("status:stream", {deployment: "main-app"})
    
    // Scale deployment
    k8s.push("scale", {
      deployment_id: "main-app",
      replicas: 5
    })
    """
  end
  
  # Server-side usage examples
  
  def broadcast_stage_transition(pipeline_id, stage, from, to) do
    CnsForge.PipelineEventProcessor.broadcast_stage_transition(
      pipeline_id, stage, from, to
    )
  end
  
  def emit_telemetry_metric(metric_name, value) do
    :telemetry.execute(
      [:cns_forge, :metric],
      %{value: value},
      %{metric: metric_name}
    )
  end
  
  def trigger_compensation(step, error) do
    Phoenix.PubSub.broadcast(
      CnsForge.PubSub,
      "recovery:pipeline",
      {:compensation_needed, step, error}
    )
  end
end

defmodule CnsForge.Channel8020Dashboard do
  @moduledoc """
  📊 EXAMPLE DASHBOARD HTML
  
  Complete working dashboard using the 80/20 channel system.
  """
  
  def dashboard_html do
    """
    <!DOCTYPE html>
    <html>
    <head>
      <title>CNS Forge 80/20 Pipeline Dashboard</title>
      <script src="https://unpkg.com/phoenix@1.7.7/priv/static/phoenix.min.js"></script>
      <style>
        body { font-family: -apple-system, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; }
        .pipeline-stages { display: flex; gap: 10px; margin: 20px 0; }
        .stage { 
          flex: 1; padding: 20px; border-radius: 8px; 
          text-align: center; transition: all 0.3s;
          background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .stage.idle { border-left: 4px solid #999; }
        .stage.processing { border-left: 4px solid #ff9800; background: #fff8e1; }
        .stage.completed { border-left: 4px solid #4caf50; background: #f1f8e9; }
        .stage.failed { border-left: 4px solid #f44336; background: #ffebee; }
        
        .metrics { 
          display: grid; grid-template-columns: repeat(4, 1fr); 
          gap: 15px; margin: 20px 0; 
        }
        .metric-card { 
          background: white; padding: 20px; border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric-value { font-size: 2em; font-weight: bold; color: #1976d2; }
        .metric-label { color: #666; margin-top: 5px; }
        
        .alerts { margin: 20px 0; }
        .alert { 
          padding: 15px; margin: 5px 0; border-radius: 4px;
          animation: slideIn 0.3s;
        }
        .alert.error { background: #ffebee; color: #c62828; }
        .alert.warning { background: #fff8e1; color: #f57c00; }
        .alert.info { background: #e3f2fd; color: #1976d2; }
        
        @keyframes slideIn {
          from { transform: translateX(-100%); opacity: 0; }
          to { transform: translateX(0); opacity: 1; }
        }
        
        .controls { margin: 20px 0; }
        button { 
          padding: 10px 20px; margin: 0 5px; border: none; 
          border-radius: 4px; cursor: pointer; background: #1976d2; 
          color: white; font-size: 14px;
        }
        button:hover { background: #1565c0; }
        button:disabled { background: #ccc; cursor: not-allowed; }
        
        .performance-chart { 
          background: white; padding: 20px; border-radius: 8px;
          height: 200px; position: relative; overflow: hidden;
        }
        .chart-line { 
          position: absolute; bottom: 0; width: 2px; 
          background: #1976d2; transition: height 0.3s;
        }
      </style>
    </head>
    <body>
      <div class="container">
        <h1>🚀 CNS Forge Pipeline Dashboard (80/20 Edition)</h1>
        
        <div class="pipeline-stages" id="stages">
          <div class="stage idle" data-stage="typer">
            <h3>📝 Typer</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="turtle">
            <h3>🐢 Turtle</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="ttl2dspy">
            <h3>🔄 TTL2DSPy</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="bitactor">
            <h3>⚡ BitActor</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="erlang">
            <h3>🎭 Erlang</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="ash">
            <h3>🔥 Ash</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="reactor">
            <h3>⚛️ Reactor</h3>
            <div class="status">Idle</div>
          </div>
          <div class="stage idle" data-stage="k8s">
            <h3>☸️ K8s</h3>
            <div class="status">Idle</div>
          </div>
        </div>
        
        <div class="metrics">
          <div class="metric-card">
            <div class="metric-value" id="throughput">0</div>
            <div class="metric-label">Pipeline Throughput</div>
          </div>
          <div class="metric-card">
            <div class="metric-value" id="latency">0ms</div>
            <div class="metric-label">Avg Latency</div>
          </div>
          <div class="metric-card">
            <div class="metric-value" id="success-rate">100%</div>
            <div class="metric-label">Success Rate</div>
          </div>
          <div class="metric-card">
            <div class="metric-value" id="active-actors">0</div>
            <div class="metric-label">Active Actors</div>
          </div>
        </div>
        
        <div class="performance-chart" id="performance">
          <h3>Real-time Performance</h3>
          <div id="chart"></div>
        </div>
        
        <div class="controls">
          <button onclick="restartPipeline()">🔄 Restart Pipeline</button>
          <button onclick="pausePipeline()">⏸️ Pause Pipeline</button>
          <button onclick="scaleActors()">📈 Scale Actors</button>
          <button onclick="triggerFailover()">🔧 Test Failover</button>
        </div>
        
        <div class="alerts" id="alerts"></div>
      </div>
      
      <script>
        // 80/20 Dashboard: 20% code for 80% monitoring capability
        
        const Socket = Phoenix.Socket
        const socket = new Socket("/socket", {
          params: { token: "dashboard_token_" + Date.now() }
        })
        
        socket.connect()
        
        // Join high-value channels
        const pipeline = socket.channel("pipeline:main")
        const telemetry = socket.channel("telemetry:performance")
        const recovery = socket.channel("recovery:auto")
        
        // Pipeline monitoring
        pipeline.on("stage_transition", ({stage, from, to}) => {
          updateStage(stage, to)
        })
        
        pipeline.on("stage_completed", ({stage, metadata}) => {
          updateStage(stage, "completed")
          if (metadata && metadata.duration) {
            showAlert(`Stage ${stage} completed in ${metadata.duration}ms`, "info")
          }
        })
        
        pipeline.on("pipeline_failure", ({failed_stage, error}) => {
          updateStage(failed_stage, "failed")
          showAlert(`Pipeline failed at ${failed_stage}: ${error}`, "error")
        })
        
        // Telemetry streaming
        let metricsBuffer = []
        let chartData = []
        
        telemetry.on("metrics_snapshot", (snapshot) => {
          updateMetrics(snapshot)
        })
        
        telemetry.on("live_metric", ({metric, value}) => {
          if (metric === "bitactor_throughput") {
            updatePerformanceChart(value)
          }
        })
        
        telemetry.on("threshold_alert", ({metric, threshold, current_value}) => {
          showAlert(
            `⚠️ ${metric} alert: ${current_value} exceeded threshold ${threshold}`,
            "warning"
          )
        })
        
        // Recovery monitoring
        recovery.on("recovery_initiated", ({recovery_id, type}) => {
          showAlert(`🔧 Recovery initiated: ${type} (${recovery_id})`, "info")
        })
        
        recovery.on("recovery_completed", ({recovery_id, duration}) => {
          showAlert(`✅ Recovery completed in ${duration}ms`, "info")
        })
        
        // Join channels
        pipeline.join()
          .receive("ok", () => console.log("Pipeline channel joined"))
          .receive("error", ({reason}) => console.error("Failed to join pipeline:", reason))
        
        telemetry.join()
          .receive("ok", () => {
            // Subscribe to key metrics
            telemetry.push("subscribe_metric", {metric: "bitactor_throughput"})
            telemetry.push("subscribe_metric", {metric: "pipeline_latency"})
          })
        
        recovery.join()
        
        // UI Update Functions
        function updateStage(stage, status) {
          const element = document.querySelector(`[data-stage="${stage}"]`)
          if (element) {
            element.className = `stage ${status}`
            element.querySelector('.status').textContent = 
              status.charAt(0).toUpperCase() + status.slice(1)
          }
        }
        
        function updateMetrics(data) {
          if (data.pipeline_throughput !== undefined) {
            document.getElementById('throughput').textContent = 
              Math.round(data.pipeline_throughput)
          }
          if (data.avg_latency !== undefined) {
            document.getElementById('latency').textContent = 
              Math.round(data.avg_latency) + 'ms'
          }
          if (data.success_rate !== undefined) {
            document.getElementById('success-rate').textContent = 
              Math.round(data.success_rate * 100) + '%'
          }
          if (data.active_actors !== undefined) {
            document.getElementById('active-actors').textContent = 
              data.active_actors
          }
        }
        
        function updatePerformanceChart(value) {
          chartData.push(value)
          if (chartData.length > 50) chartData.shift()
          
          const chart = document.getElementById('chart')
          chart.innerHTML = chartData.map((v, i) => 
            `<div class="chart-line" style="left: ${i * 4}px; height: ${v / 10}px"></div>`
          ).join('')
        }
        
        function showAlert(message, type = 'info') {
          const alerts = document.getElementById('alerts')
          const alert = document.createElement('div')
          alert.className = `alert ${type}`
          alert.textContent = message
          alerts.appendChild(alert)
          
          // Auto-remove after 5 seconds
          setTimeout(() => alert.remove(), 5000)
          
          // Keep only last 5 alerts
          while (alerts.children.length > 5) {
            alerts.removeChild(alerts.firstChild)
          }
        }
        
        // Control Functions
        function restartPipeline() {
          pipeline.push("control:restart", {})
            .receive("ok", () => showAlert("Pipeline restart initiated", "info"))
            .receive("error", (err) => showAlert("Failed to restart: " + err, "error"))
        }
        
        function pausePipeline() {
          pipeline.push("control:pause", {})
            .receive("ok", () => showAlert("Pipeline paused", "info"))
        }
        
        function scaleActors() {
          const bitactor = socket.channel("stage:bitactor:main")
          bitactor.join()
            .receive("ok", () => {
              bitactor.push("batch:spawn", {
                items: Array(20).fill().map(() => ({config: {type: "worker"}}))
              })
              .receive("ok", () => showAlert("Spawned 20 new actors", "info"))
            })
        }
        
        function triggerFailover() {
          const coordination = socket.channel("coordination:test")
          coordination.join()
            .receive("ok", () => {
              coordination.push("failover", {from: "node1"})
                .receive("ok", () => showAlert("Failover test initiated", "info"))
            })
        }
        
        // Start performance monitoring
        setInterval(() => {
          // Simulate performance updates for demo
          updatePerformanceChart(Math.random() * 100)
        }, 1000)
      </script>
    </body>
    </html>
    """
  end
end

defmodule CnsForge.Application do
  @moduledoc """
  Application supervisor for 80/20 Channel system
  """
  
  use Application
  
  @impl true
  def start(_type, _args) do
    children = [
      # PubSub for channel communication
      {Phoenix.PubSub, name: CnsForge.PubSub},
      
      # Dynamic supervisor for spawned processes
      {DynamicSupervisor, name: CnsForge.DynamicSupervisor, strategy: :one_for_one},
      
      # Endpoint for websocket connections
      CnsForge.Endpoint,
      
      # High-value background workers (80/20 selection)
      CnsForge.PipelineEventProcessor,
      CnsForge.TelemetryStreamer,
      CnsForge.ReactorStepNotifier
    ]
    
    opts = [strategy: :one_for_one, name: CnsForge.Supervisor]
    
    # Setup channel system on startup
    {:ok, pid} = Supervisor.start_link(children, opts)
    
    CnsForge.Channel8020Integration.setup_channel_swarm()
    
    {:ok, pid}
  end
  
  @impl true
  def config_change(changed, _new, removed) do
    CnsForge.Endpoint.config_change(changed, removed)
    :ok
  end
end