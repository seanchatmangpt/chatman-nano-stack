/**
 * 🚀 CNS FORGE PHOENIX CHANNELS CLIENT
 * JavaScript client for real-time pipeline monitoring and notifications
 * 
 * 80/20 APPROACH: Simple API for maximum real-time connectivity
 */

import { Socket } from "phoenix";

class CNSForgeClient {
  constructor(options = {}) {
    this.options = {
      socketUrl: options.socketUrl || "/socket",
      token: options.token || "pipeline_monitor_token",
      reconnectAfterMs: options.reconnectAfterMs || 1000,
      ...options
    };
    
    this.socket = null;
    this.channels = new Map();
    this.eventHandlers = new Map();
    this.isConnected = false;
  }

  /**
   * Connect to the CNS Forge WebSocket server
   */
  connect() {
    this.socket = new Socket(this.options.socketUrl, {
      params: { token: this.options.token },
      reconnectAfterMs: this.options.reconnectAfterMs
    });

    this.socket.onOpen(() => {
      this.isConnected = true;
      this.emit("connected");
      console.log("🔗 Connected to CNS Forge");
    });

    this.socket.onClose(() => {
      this.isConnected = false;
      this.emit("disconnected");
      console.log("❌ Disconnected from CNS Forge");
    });

    this.socket.onError((error) => {
      this.emit("error", error);
      console.error("💥 CNS Forge connection error:", error);
    });

    this.socket.connect();
    return this;
  }

  /**
   * Disconnect from the server
   */
  disconnect() {
    if (this.socket) {
      this.socket.disconnect();
      this.channels.clear();
      this.isConnected = false;
    }
    return this;
  }

  /**
   * Subscribe to pipeline monitoring for a specific pipeline
   */
  subscribeToPipeline(pipelineId) {
    const channelName = `pipeline:${pipelineId}`;
    
    if (this.channels.has(channelName)) {
      return this.channels.get(channelName);
    }

    const channel = this.socket.channel(channelName);
    
    // Pipeline-specific event handlers
    channel.on("pipeline_status", (data) => {
      this.emit("pipeline:status", { pipelineId, ...data });
    });

    channel.on("stage_transition", (data) => {
      this.emit("pipeline:stage_transition", { pipelineId, ...data });
    });

    channel.on("stage_completed", (data) => {
      this.emit("pipeline:stage_completed", { pipelineId, ...data });
    });

    channel.on("pipeline_failure", (data) => {
      this.emit("pipeline:failure", { pipelineId, ...data });
    });

    channel.on("telemetry", (data) => {
      this.emit("pipeline:telemetry", { pipelineId, ...data });
    });

    channel.on("pipeline_restarted", (data) => {
      this.emit("pipeline:restarted", { pipelineId, ...data });
    });

    channel.on("pipeline_paused", (data) => {
      this.emit("pipeline:paused", { pipelineId, ...data });
    });

    channel.join()
      .receive("ok", () => {
        console.log(`📊 Subscribed to pipeline: ${pipelineId}`);
        this.emit("pipeline:subscribed", { pipelineId });
      })
      .receive("error", (error) => {
        console.error(`❌ Failed to subscribe to pipeline ${pipelineId}:`, error);
        this.emit("pipeline:subscription_failed", { pipelineId, error });
      });

    this.channels.set(channelName, channel);
    return channel;
  }

  /**
   * Subscribe to Ash Reactor step notifications
   */
  subscribeToReactor(reactorName) {
    const channelName = `reactor:${reactorName}`;
    
    if (this.channels.has(channelName)) {
      return this.channels.get(channelName);
    }

    const channel = this.socket.channel(channelName);
    
    // Reactor-specific event handlers
    channel.on("reactor_status", (data) => {
      this.emit("reactor:status", { reactorName, ...data });
    });

    channel.on("step_started", (data) => {
      this.emit("reactor:step_started", { reactorName, ...data });
    });

    channel.on("step_completed", (data) => {
      this.emit("reactor:step_completed", { reactorName, ...data });
    });

    channel.on("step_error", (data) => {
      this.emit("reactor:step_error", { reactorName, ...data });
    });

    channel.on("step_compensation", (data) => {
      this.emit("reactor:step_compensation", { reactorName, ...data });
    });

    channel.on("step_retried", (data) => {
      this.emit("reactor:step_retried", { reactorName, ...data });
    });

    channel.join()
      .receive("ok", () => {
        console.log(`⚡ Subscribed to reactor: ${reactorName}`);
        this.emit("reactor:subscribed", { reactorName });
      })
      .receive("error", (error) => {
        console.error(`❌ Failed to subscribe to reactor ${reactorName}:`, error);
        this.emit("reactor:subscription_failed", { reactorName, error });
      });

    this.channels.set(channelName, channel);
    return channel;
  }

  /**
   * Subscribe to telemetry stream
   */
  subscribeToTelemetry(streamType = "performance") {
    const channelName = `telemetry:${streamType}`;
    
    if (this.channels.has(channelName)) {
      return this.channels.get(channelName);
    }

    const channel = this.socket.channel(channelName);
    
    // Telemetry-specific event handlers
    channel.on("metrics_snapshot", (data) => {
      this.emit("telemetry:snapshot", { streamType, ...data });
    });

    channel.on("metric", (data) => {
      this.emit("telemetry:metric", { streamType, ...data });
    });

    channel.on("aggregated_metrics", (data) => {
      this.emit("telemetry:aggregated", { streamType, ...data });
    });

    channel.on("threshold_alert", (data) => {
      this.emit("telemetry:threshold_alert", { streamType, ...data });
    });

    channel.on("metrics_batch", (data) => {
      this.emit("telemetry:batch", { streamType, ...data });
    });

    channel.join()
      .receive("ok", () => {
        console.log(`📈 Subscribed to telemetry: ${streamType}`);
        this.emit("telemetry:subscribed", { streamType });
      })
      .receive("error", (error) => {
        console.error(`❌ Failed to subscribe to telemetry ${streamType}:`, error);
        this.emit("telemetry:subscription_failed", { streamType, error });
      });

    this.channels.set(channelName, channel);
    return channel;
  }

  /**
   * Subscribe to notifications
   */
  subscribeToNotifications(topic = "alerts") {
    const channelName = `notifications:${topic}`;
    
    if (this.channels.has(channelName)) {
      return this.channels.get(channelName);
    }

    const channel = this.socket.channel(channelName);
    
    // Notification-specific event handlers
    channel.on("alert", (data) => {
      this.emit("notification:alert", { topic, ...data });
    });

    channel.on("notification", (data) => {
      this.emit("notification:message", { topic, ...data });
    });

    channel.on("announcement", (data) => {
      this.emit("notification:announcement", { topic, ...data });
    });

    channel.on("milestone", (data) => {
      this.emit("notification:milestone", { topic, ...data });
    });

    channel.join()
      .receive("ok", () => {
        console.log(`🔔 Subscribed to notifications: ${topic}`);
        this.emit("notification:subscribed", { topic });
      })
      .receive("error", (error) => {
        console.error(`❌ Failed to subscribe to notifications ${topic}:`, error);
        this.emit("notification:subscription_failed", { topic, error });
      });

    this.channels.set(channelName, channel);
    return channel;
  }

  /**
   * Pipeline control methods
   */
  restartPipeline(pipelineId, stage = null) {
    const channel = this.channels.get(`pipeline:${pipelineId}`);
    if (!channel) {
      throw new Error(`Not subscribed to pipeline: ${pipelineId}`);
    }

    const payload = stage ? { stage } : {};
    
    return new Promise((resolve, reject) => {
      channel.push("restart_pipeline", payload)
        .receive("ok", resolve)
        .receive("error", reject);
    });
  }

  pausePipeline(pipelineId) {
    const channel = this.channels.get(`pipeline:${pipelineId}`);
    if (!channel) {
      throw new Error(`Not subscribed to pipeline: ${pipelineId}`);
    }

    return new Promise((resolve, reject) => {
      channel.push("pause_pipeline", {})
        .receive("ok", resolve)
        .receive("error", reject);
    });
  }

  /**
   * Reactor control methods
   */
  retryReactorStep(reactorName, stepName) {
    const channel = this.channels.get(`reactor:${reactorName}`);
    if (!channel) {
      throw new Error(`Not subscribed to reactor: ${reactorName}`);
    }

    return new Promise((resolve, reject) => {
      channel.push("retry_step", { step: stepName })
        .receive("ok", resolve)
        .receive("error", reject);
    });
  }

  /**
   * Telemetry control methods
   */
  subscribeToMetric(streamType, metricName) {
    const channel = this.channels.get(`telemetry:${streamType}`);
    if (!channel) {
      throw new Error(`Not subscribed to telemetry: ${streamType}`);
    }

    return new Promise((resolve, reject) => {
      channel.push("subscribe_metric", { metric: metricName })
        .receive("ok", resolve)
        .receive("error", reject);
    });
  }

  setMetricThreshold(streamType, metric, threshold) {
    const channel = this.channels.get(`telemetry:${streamType}`);
    if (!channel) {
      throw new Error(`Not subscribed to telemetry: ${streamType}`);
    }

    return new Promise((resolve, reject) => {
      channel.push("set_threshold", { metric, threshold })
        .receive("ok", resolve)
        .receive("error", reject);
    });
  }

  /**
   * Event handling
   */
  on(event, handler) {
    if (!this.eventHandlers.has(event)) {
      this.eventHandlers.set(event, []);
    }
    this.eventHandlers.get(event).push(handler);
    return this;
  }

  off(event, handler) {
    if (this.eventHandlers.has(event)) {
      const handlers = this.eventHandlers.get(event);
      const index = handlers.indexOf(handler);
      if (index !== -1) {
        handlers.splice(index, 1);
      }
    }
    return this;
  }

  emit(event, data = {}) {
    if (this.eventHandlers.has(event)) {
      this.eventHandlers.get(event).forEach(handler => {
        try {
          handler(data);
        } catch (error) {
          console.error(`Error in event handler for ${event}:`, error);
        }
      });
    }
  }

  /**
   * Utility methods
   */
  getConnectionStatus() {
    return {
      connected: this.isConnected,
      channels: Array.from(this.channels.keys()),
      socket: this.socket ? this.socket.connectionState() : "disconnected"
    };
  }

  unsubscribeFromChannel(channelName) {
    if (this.channels.has(channelName)) {
      const channel = this.channels.get(channelName);
      channel.leave();
      this.channels.delete(channelName);
      console.log(`🔌 Unsubscribed from: ${channelName}`);
    }
  }

  unsubscribeFromAll() {
    this.channels.forEach((channel, channelName) => {
      channel.leave();
      console.log(`🔌 Unsubscribed from: ${channelName}`);
    });
    this.channels.clear();
  }
}

/**
 * Usage Examples and Helper Functions
 */

// Pipeline monitoring dashboard
class PipelineDashboard {
  constructor(containerElement) {
    this.container = containerElement;
    this.client = new CNSForgeClient();
    this.setupEventHandlers();
  }

  async init() {
    this.client.connect();
    
    // Subscribe to main pipeline
    this.client.subscribeToPipeline("main_pipeline");
    
    // Subscribe to reactor notifications
    this.client.subscribeToReactor("TTLMainReactor");
    
    // Subscribe to performance telemetry
    this.client.subscribeToTelemetry("performance");
    
    // Subscribe to alerts
    this.client.subscribeToNotifications("alerts");
  }

  setupEventHandlers() {
    // Pipeline events
    this.client.on("pipeline:stage_transition", (data) => {
      this.updateStageStatus(data.stage, data.to);
    });

    this.client.on("pipeline:failure", (data) => {
      this.showAlert("Pipeline failure in stage: " + data.failed_stage, "error");
    });

    // Reactor events
    this.client.on("reactor:step_completed", (data) => {
      this.updateStepProgress(data.step, "completed", data.duration_ms);
    });

    this.client.on("reactor:step_error", (data) => {
      this.showAlert(`Reactor step error: ${data.step}`, "error");
    });

    // Telemetry events
    this.client.on("telemetry:metric", (data) => {
      this.updateMetricDisplay(data.name, data.value);
    });

    this.client.on("telemetry:threshold_alert", (data) => {
      this.showAlert(`Metric alert: ${data.metric} exceeded threshold`, "warning");
    });

    // Notification events
    this.client.on("notification:alert", (data) => {
      this.showAlert(data.message, data.level);
    });

    this.client.on("notification:milestone", (data) => {
      this.showCelebration(data.milestone, data.details);
    });
  }

  updateStageStatus(stage, status) {
    const stageElement = this.container.querySelector(`[data-stage="${stage}"]`);
    if (stageElement) {
      stageElement.className = `stage stage-${status}`;
      stageElement.textContent = `${stage}: ${status}`;
    }
  }

  updateStepProgress(step, status, duration) {
    const stepElement = this.container.querySelector(`[data-step="${step}"]`);
    if (stepElement) {
      stepElement.innerHTML = `
        <strong>${step}</strong>: ${status}
        ${duration ? `<span class="duration">(${duration}ms)</span>` : ''}
      `;
    }
  }

  updateMetricDisplay(metricName, value) {
    const metricElement = this.container.querySelector(`[data-metric="${metricName}"]`);
    if (metricElement) {
      metricElement.textContent = value;
    }
  }

  showAlert(message, level) {
    const alertElement = document.createElement('div');
    alertElement.className = `alert alert-${level}`;
    alertElement.textContent = message;
    
    this.container.appendChild(alertElement);
    
    // Auto-remove after 5 seconds
    setTimeout(() => {
      alertElement.remove();
    }, 5000);
  }

  showCelebration(milestone, details) {
    const celebrationElement = document.createElement('div');
    celebrationElement.className = 'celebration';
    celebrationElement.innerHTML = `
      <h3>🎉 ${milestone}</h3>
      <p>${details}</p>
    `;
    
    this.container.appendChild(celebrationElement);
    
    // Auto-remove after 10 seconds
    setTimeout(() => {
      celebrationElement.remove();
    }, 10000);
  }

  async restartPipeline() {
    try {
      await this.client.restartPipeline("main_pipeline");
      this.showAlert("Pipeline restart initiated", "info");
    } catch (error) {
      this.showAlert("Failed to restart pipeline", "error");
    }
  }

  async retryFailedStep(stepName) {
    try {
      await this.client.retryReactorStep("TTLMainReactor", stepName);
      this.showAlert(`Step ${stepName} retry initiated`, "info");
    } catch (error) {
      this.showAlert(`Failed to retry step ${stepName}`, "error");
    }
  }
}

// Export for use in other modules
export { CNSForgeClient, PipelineDashboard };

// Example HTML integration
/*
<!DOCTYPE html>
<html>
<head>
    <title>CNS Forge Pipeline Dashboard</title>
    <style>
        .stage { padding: 10px; margin: 5px; border-radius: 5px; }
        .stage-ready { background-color: #e3f2fd; }
        .stage-running { background-color: #fff3e0; }
        .stage-completed { background-color: #e8f5e8; }
        .stage-failed { background-color: #ffebee; }
        .alert { padding: 10px; margin: 5px; border-radius: 5px; }
        .alert-info { background-color: #e3f2fd; }
        .alert-warning { background-color: #fff3e0; }
        .alert-error { background-color: #ffebee; }
        .celebration { padding: 20px; background-color: #e8f5e8; text-align: center; }
    </style>
</head>
<body>
    <div id="dashboard">
        <h1>CNS Forge Pipeline Dashboard</h1>
        
        <div class="pipeline-stages">
            <div data-stage="typer" class="stage">Typer: Ready</div>
            <div data-stage="turtle" class="stage">Turtle: Ready</div>
            <div data-stage="ttl2dspy" class="stage">TTL2DSPy: Ready</div>
            <div data-stage="bitactor" class="stage">BitActor: Ready</div>
            <div data-stage="ash" class="stage">Ash: Ready</div>
            <div data-stage="reactor" class="stage">Reactor: Ready</div>
            <div data-stage="k8s" class="stage">K8s: Ready</div>
        </div>
        
        <div class="metrics">
            <h3>Metrics</h3>
            <div>CPU: <span data-metric="cpu_usage">0</span>%</div>
            <div>Memory: <span data-metric="memory_usage">0</span>%</div>
            <div>Throughput: <span data-metric="pipeline_throughput">0</span> req/s</div>
        </div>
        
        <div class="controls">
            <button onclick="dashboard.restartPipeline()">Restart Pipeline</button>
        </div>
    </div>

    <script type="module">
        import { PipelineDashboard } from './cns_forge_client.js';
        
        const dashboardElement = document.getElementById('dashboard');
        const dashboard = new PipelineDashboard(dashboardElement);
        dashboard.init();
        
        // Make dashboard available globally for button clicks
        window.dashboard = dashboard;
    </script>
</body>
</html>
*/