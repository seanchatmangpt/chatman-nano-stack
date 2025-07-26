<!--
BitActor Nuxt UI Dashboard Variant
Complete UI dashboard for the 8-stage pipeline: typer > turtle > ttl2dspy > BitActor > Erlang > Ash > Reactor > k8s
No TypeScript - Pure JavaScript and Vue composition API
-->

<template>
  <div class="bitactor-ui-dashboard">
    <!-- Main Dashboard Header -->
    <header class="dashboard-header">
      <div class="header-content">
        <h1 class="dashboard-title">
          <span class="title-icon">🔬</span>
          BitActor Pipeline Dashboard
        </h1>
        <div class="header-controls">
          <div class="ttl-status" :class="globalTTLStatus">
            <span class="ttl-label">Global TTL:</span>
            <span class="ttl-value">{{ globalTTLBudget }}ms</span>
            <div class="ttl-progress">
              <div class="ttl-bar" :style="{ width: `${ttlUsagePercent}%` }"></div>
            </div>
          </div>
          <button @click="toggleRealTimeMode" class="mode-toggle" :class="{ active: realTimeMode }">
            {{ realTimeMode ? '⏸️ Pause' : '▶️ Live' }}
          </button>
          <button @click="refreshDashboard" class="refresh-btn">🔄</button>
        </div>
      </div>
    </header>

    <!-- Pipeline Status Overview -->
    <section class="pipeline-overview">
      <div class="overview-grid">
        <div v-for="stage in pipelineStages" :key="stage.name" 
             class="stage-card" :class="stage.status">
          <div class="stage-header">
            <span class="stage-icon">{{ stage.icon }}</span>
            <h3 class="stage-name">{{ stage.name }}</h3>
          </div>
          <div class="stage-metrics">
            <div class="metric">
              <span class="metric-label">Executions</span>
              <span class="metric-value">{{ stage.executions }}</span>
            </div>
            <div class="metric">
              <span class="metric-label">Avg Time</span>
              <span class="metric-value">{{ stage.avgTime }}ms</span>
            </div>
            <div class="metric">
              <span class="metric-label">Success Rate</span>
              <span class="metric-value">{{ stage.successRate }}%</span>
            </div>
          </div>
          <div class="stage-ttl">
            <div class="ttl-meter">
              <div class="ttl-fill" :style="{ width: `${stage.ttlUsage}%` }"></div>
            </div>
            <span class="ttl-text">{{ stage.ttlBudget }}ms budget</span>
          </div>
        </div>
      </div>
    </section>

    <!-- Live Pipeline Flow Visualization -->
    <section class="pipeline-flow">
      <h2 class="section-title">Live Pipeline Flow</h2>
      <div class="flow-container">
        <svg class="flow-svg" viewBox="0 0 1200 300">
          <!-- Pipeline stages as connected nodes -->
          <g v-for="(stage, index) in pipelineStages" :key="stage.name" 
             :transform="`translate(${100 + index * 140}, 150)`">
            
            <!-- Stage node -->
            <circle :r="30" :fill="stage.color" :stroke="stage.borderColor" 
                   stroke-width="3" class="stage-node" :class="stage.status"/>
            
            <!-- Stage icon -->
            <text x="0" y="8" text-anchor="middle" class="stage-node-icon">
              {{ stage.icon }}
            </text>
            
            <!-- Stage name -->
            <text x="0" y="50" text-anchor="middle" class="stage-label">
              {{ stage.name }}
            </text>
            
            <!-- TTL indicator -->
            <text x="0" y="65" text-anchor="middle" class="stage-ttl-label">
              {{ stage.ttlBudget }}ms
            </text>
            
            <!-- Connection arrow to next stage -->
            <g v-if="index < pipelineStages.length - 1">
              <path d="M 30 0 L 110 0" stroke="#4a90e2" stroke-width="2" 
                   marker-end="url(#arrowhead)" class="connection-line"/>
              
              <!-- Data flow indicator -->
              <circle v-if="stage.hasDataFlow" r="3" fill="#00ff00" class="data-flow">
                <animateTransform attributeName="transform" type="translate"
                                values="30,0; 110,0" dur="2s" repeatCount="indefinite"/>
              </circle>
            </g>
          </g>
          
          <!-- Arrow marker definition -->
          <defs>
            <marker id="arrowhead" markerWidth="10" markerHeight="7" 
                   refX="9" refY="3.5" orient="auto">
              <polygon points="0 0, 10 3.5, 0 7" fill="#4a90e2"/>
            </marker>
          </defs>
        </svg>
      </div>
    </section>

    <!-- Active Executions Monitor -->
    <section class="active-executions">
      <h2 class="section-title">Active Pipeline Executions</h2>
      <div class="executions-grid">
        <div v-for="execution in activeExecutions" :key="execution.id" 
             class="execution-card" :class="execution.status">
          <div class="execution-header">
            <span class="execution-id">{{ execution.id }}</span>
            <span class="execution-status" :class="execution.status">
              {{ execution.status }}
            </span>
          </div>
          <div class="execution-progress">
            <div class="progress-bar">
              <div class="progress-fill" 
                   :style="{ width: `${execution.progress}%` }"></div>
            </div>
            <span class="progress-text">
              {{ execution.currentStage }} ({{ execution.progress }}%)
            </span>
          </div>
          <div class="execution-timing">
            <span class="execution-elapsed">{{ execution.elapsed }}ms</span>
            <span class="execution-remaining">{{ execution.remaining }}ms left</span>
          </div>
        </div>
      </div>
    </section>

    <!-- Real-time Metrics Grid -->
    <section class="metrics-grid">
      <div class="metrics-row">
        <!-- Throughput Chart -->
        <div class="metric-panel">
          <h3 class="panel-title">Pipeline Throughput</h3>
          <div class="chart-container">
            <canvas ref="throughputChart" class="metric-chart"></canvas>
          </div>
          <div class="chart-legend">
            <span class="legend-item">
              <span class="legend-color" style="background: #4a90e2"></span>
              Executions/min
            </span>
          </div>
        </div>

        <!-- TTL Violations Chart -->
        <div class="metric-panel">
          <h3 class="panel-title">TTL Constraint Violations</h3>
          <div class="chart-container">
            <canvas ref="ttlChart" class="metric-chart"></canvas>
          </div>
          <div class="violation-summary">
            <div class="violation-stat">
              <span class="stat-value">{{ ttlViolations.total }}</span>
              <span class="stat-label">Total Violations</span>
            </div>
            <div class="violation-stat">
              <span class="stat-value">{{ ttlViolations.rate }}%</span>
              <span class="stat-label">Violation Rate</span>
            </div>
          </div>
        </div>

        <!-- Swarm Activity Monitor -->
        <div class="metric-panel">
          <h3 class="panel-title">Swarm Activity</h3>
          <div class="swarm-grid">
            <div v-for="swarm in activeSwarms" :key="swarm.id" 
                 class="swarm-card" :class="swarm.status">
              <div class="swarm-header">
                <span class="swarm-name">{{ swarm.name }}</span>
                <span class="swarm-agents">{{ swarm.agents }} agents</span>
              </div>
              <div class="swarm-metrics">
                <div class="swarm-metric">
                  <span class="metric-label">Tasks</span>
                  <span class="metric-value">{{ swarm.activeTasks }}</span>
                </div>
                <div class="swarm-metric">
                  <span class="metric-label">Load</span>
                  <span class="metric-value">{{ swarm.load }}%</span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- System Health Indicators -->
    <section class="system-health">
      <h2 class="section-title">System Health</h2>
      <div class="health-grid">
        <div class="health-card">
          <div class="health-icon">🔧</div>
          <h4 class="health-title">BitActor Engine</h4>
          <div class="health-status" :class="systemHealth.bitactor.status">
            {{ systemHealth.bitactor.status }}
          </div>
          <div class="health-details">
            <span>CPU: {{ systemHealth.bitactor.cpu }}%</span>
            <span>Memory: {{ systemHealth.bitactor.memory }}MB</span>
          </div>
        </div>

        <div class="health-card">
          <div class="health-icon">⚡</div>
          <h4 class="health-title">Elixir/OTP</h4>
          <div class="health-status" :class="systemHealth.elixir.status">
            {{ systemHealth.elixir.status }}
          </div>
          <div class="health-details">
            <span>Processes: {{ systemHealth.elixir.processes }}</span>
            <span>Uptime: {{ systemHealth.elixir.uptime }}h</span>
          </div>
        </div>

        <div class="health-card">
          <div class="health-icon">🌊</div>
          <h4 class="health-title">Ash Framework</h4>
          <div class="health-status" :class="systemHealth.ash.status">
            {{ systemHealth.ash.status }}
          </div>
          <div class="health-details">
            <span>Resources: {{ systemHealth.ash.resources }}</span>
            <span>Actions: {{ systemHealth.ash.actions }}/min</span>
          </div>
        </div>

        <div class="health-card">
          <div class="health-icon">🔄</div>
          <h4 class="health-title">Reactor Engine</h4>
          <div class="health-status" :class="systemHealth.reactor.status">
            {{ systemHealth.reactor.status }}
          </div>
          <div class="health-details">
            <span>Workflows: {{ systemHealth.reactor.workflows }}</span>
            <span>Queue: {{ systemHealth.reactor.queue }}</span>
          </div>
        </div>

        <div class="health-card">
          <div class="health-icon">☸️</div>
          <h4 class="health-title">Kubernetes</h4>
          <div class="health-status" :class="systemHealth.kubernetes.status">
            {{ systemHealth.kubernetes.status }}
          </div>
          <div class="health-details">
            <span>Pods: {{ systemHealth.kubernetes.pods }}</span>
            <span>Nodes: {{ systemHealth.kubernetes.nodes }}</span>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
export default {
  name: 'NuxtUIDashboardVariant',
  
  data() {
    return {
      realTimeMode: true,
      globalTTLBudget: 8,
      ttlUsagePercent: 65,
      globalTTLStatus: 'normal',
      
      pipelineStages: [
        {
          name: 'Typer',
          icon: '📝',
          status: 'active',
          color: '#4a90e2',
          borderColor: '#2171b5',
          executions: 1247,
          avgTime: 0.8,
          successRate: 99.2,
          ttlBudget: 1,
          ttlUsage: 80,
          hasDataFlow: true
        },
        {
          name: 'Turtle',
          icon: '🐢',
          status: 'active',
          color: '#5cb85c',
          borderColor: '#449d44',
          executions: 1247,
          avgTime: 1.2,
          successRate: 98.8,
          ttlBudget: 1,
          ttlUsage: 60,
          hasDataFlow: true
        },
        {
          name: 'TTL2DSPy',
          icon: '🔍',
          status: 'active',
          color: '#f0ad4e',
          borderColor: '#ec971f',
          executions: 1247,
          avgTime: 0.9,
          successRate: 99.5,
          ttlBudget: 1,
          ttlUsage: 45,
          hasDataFlow: true
        },
        {
          name: 'BitActor',
          icon: '⚡',
          status: 'active',
          color: '#d9534f',
          borderColor: '#c9302c',
          executions: 1247,
          avgTime: 1.5,
          successRate: 97.3,
          ttlBudget: 1,
          ttlUsage: 75,
          hasDataFlow: true
        },
        {
          name: 'Erlang',
          icon: '🔧',
          status: 'active',
          color: '#5bc0de',
          borderColor: '#31b0d5',
          executions: 1247,
          avgTime: 1.1,
          successRate: 99.1,
          ttlBudget: 1,
          ttlUsage: 55,
          hasDataFlow: true
        },
        {
          name: 'Ash',
          icon: '🌊',
          status: 'active',
          color: '#9b59b6',
          borderColor: '#8e44ad',
          executions: 1247,
          avgTime: 1.3,
          successRate: 98.9,
          ttlBudget: 1,
          ttlUsage: 65,
          hasDataFlow: true
        },
        {
          name: 'Reactor',
          icon: '🔄',
          status: 'active',
          color: '#e67e22',
          borderColor: '#d35400',
          executions: 1247,
          avgTime: 0.7,
          successRate: 99.7,
          ttlBudget: 1,
          ttlUsage: 35,
          hasDataFlow: true
        },
        {
          name: 'K8s',
          icon: '☸️',
          status: 'active',
          color: '#34495e',
          borderColor: '#2c3e50',
          executions: 1247,
          avgTime: 0.5,
          successRate: 99.9,
          ttlBudget: 1,
          ttlUsage: 25,
          hasDataFlow: false
        }
      ],
      
      activeExecutions: [
        {
          id: 'exec_1753475425_abc123',
          status: 'running',
          progress: 62,
          currentStage: 'Ash',
          elapsed: 4.2,
          remaining: 3.8
        },
        {
          id: 'exec_1753475426_def456',
          status: 'running',
          progress: 25,
          currentStage: 'BitActor',
          elapsed: 2.1,
          remaining: 5.9
        },
        {
          id: 'exec_1753475427_ghi789',
          status: 'running',
          progress: 87,
          currentStage: 'K8s',
          elapsed: 6.8,
          remaining: 1.2
        }
      ],
      
      ttlViolations: {
        total: 23,
        rate: 1.8
      },
      
      activeSwarms: [
        {
          id: 'swarm_coordinator_001',
          name: 'Main Coordinator',
          agents: 8,
          status: 'active',
          activeTasks: 15,
          load: 67
        },
        {
          id: 'swarm_processor_002',
          name: 'Data Processor',
          agents: 12,
          status: 'active',
          activeTasks: 28,
          load: 84
        },
        {
          id: 'swarm_monitor_003',
          name: 'Health Monitor',
          agents: 4,
          status: 'active',
          activeTasks: 6,
          load: 23
        }
      ],
      
      systemHealth: {
        bitactor: {
          status: 'healthy',
          cpu: 45,
          memory: 128
        },
        elixir: {
          status: 'healthy',
          processes: 1247,
          uptime: 72
        },
        ash: {
          status: 'healthy',
          resources: 24,
          actions: 156
        },
        reactor: {
          status: 'healthy',
          workflows: 8,
          queue: 3
        },
        kubernetes: {
          status: 'healthy',
          pods: 16,
          nodes: 3
        }
      },
      
      refreshInterval: null,
      throughputChart: null,
      ttlChart: null
    }
  },
  
  mounted() {
    this.initializeCharts()
    this.startRealTimeUpdates()
  },
  
  beforeUnmount() {
    this.stopRealTimeUpdates()
  },
  
  methods: {
    toggleRealTimeMode() {
      this.realTimeMode = !this.realTimeMode
      if (this.realTimeMode) {
        this.startRealTimeUpdates()
      } else {
        this.stopRealTimeUpdates()
      }
    },
    
    refreshDashboard() {
      this.updatePipelineMetrics()
      this.updateActiveExecutions()
      this.updateSystemHealth()
    },
    
    startRealTimeUpdates() {
      if (this.refreshInterval) return
      
      this.refreshInterval = setInterval(() => {
        this.updatePipelineMetrics()
        this.updateActiveExecutions()
        this.updateCharts()
      }, 1000)
    },
    
    stopRealTimeUpdates() {
      if (this.refreshInterval) {
        clearInterval(this.refreshInterval)
        this.refreshInterval = null
      }
    },
    
    updatePipelineMetrics() {
      // Simulate real-time metric updates
      this.pipelineStages.forEach(stage => {
        stage.ttlUsage = Math.max(10, Math.min(95, 
          stage.ttlUsage + (Math.random() - 0.5) * 10))
        stage.hasDataFlow = Math.random() > 0.3
      })
      
      this.ttlUsagePercent = Math.max(20, Math.min(95,
        this.ttlUsagePercent + (Math.random() - 0.5) * 5))
      
      this.globalTTLStatus = this.ttlUsagePercent > 80 ? 'critical' :
                           this.ttlUsagePercent > 60 ? 'warning' : 'normal'
    },
    
    updateActiveExecutions() {
      // Simulate execution progress
      this.activeExecutions.forEach(execution => {
        if (execution.status === 'running') {
          execution.progress = Math.min(100, execution.progress + Math.random() * 5)
          execution.elapsed += 0.1
          execution.remaining = Math.max(0, execution.remaining - 0.1)
          
          if (execution.progress >= 100) {
            execution.status = 'completed'
          }
        }
      })
    },
    
    updateSystemHealth() {
      // Simulate health metric fluctuations
      this.systemHealth.bitactor.cpu = Math.max(20, Math.min(80,
        this.systemHealth.bitactor.cpu + (Math.random() - 0.5) * 10))
      
      this.systemHealth.elixir.processes = Math.max(1000, Math.min(2000,
        this.systemHealth.elixir.processes + Math.floor((Math.random() - 0.5) * 50)))
    },
    
    initializeCharts() {
      // Initialize throughput chart
      const throughputCtx = this.$refs.throughputChart?.getContext('2d')
      if (throughputCtx) {
        this.throughputChart = this.createSimpleChart(throughputCtx, 'line', '#4a90e2')
      }
      
      // Initialize TTL violations chart
      const ttlCtx = this.$refs.ttlChart?.getContext('2d')
      if (ttlCtx) {
        this.ttlChart = this.createSimpleChart(ttlCtx, 'bar', '#d9534f')
      }
    },
    
    createSimpleChart(ctx, type, color) {
      // Simple chart implementation without external dependencies
      const data = Array.from({ length: 20 }, () => Math.random() * 100)
      return {
        ctx,
        data,
        type,
        color,
        render: this.renderChart
      }
    },
    
    renderChart(chart) {
      const { ctx, data, color } = chart
      const canvas = ctx.canvas
      const width = canvas.width
      const height = canvas.height
      
      ctx.clearRect(0, 0, width, height)
      ctx.strokeStyle = color
      ctx.fillStyle = color + '33'
      ctx.lineWidth = 2
      
      const stepX = width / (data.length - 1)
      const maxValue = Math.max(...data)
      
      ctx.beginPath()
      data.forEach((value, index) => {
        const x = index * stepX
        const y = height - (value / maxValue) * height
        
        if (index === 0) {
          ctx.moveTo(x, y)
        } else {
          ctx.lineTo(x, y)
        }
      })
      ctx.stroke()
    },
    
    updateCharts() {
      if (this.throughputChart) {
        this.throughputChart.data.push(Math.random() * 100)
        this.throughputChart.data.shift()
        this.renderChart(this.throughputChart)
      }
      
      if (this.ttlChart) {
        this.ttlChart.data.push(Math.random() * 20)
        this.ttlChart.data.shift()
        this.renderChart(this.ttlChart)
      }
    }
  }
}
</script>

<style scoped>
.bitactor-ui-dashboard {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  padding: 20px;
}

/* Dashboard Header */
.dashboard-header {
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 20px;
  margin-bottom: 30px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.header-content {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.dashboard-title {
  font-size: 2.5rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 15px;
}

.title-icon {
  font-size: 3rem;
}

.header-controls {
  display: flex;
  align-items: center;
  gap: 20px;
}

.ttl-status {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 15px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
}

.ttl-status.normal { border-left: 4px solid #5cb85c; }
.ttl-status.warning { border-left: 4px solid #f0ad4e; }
.ttl-status.critical { border-left: 4px solid #d9534f; }

.ttl-progress {
  width: 100px;
  height: 4px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 2px;
  overflow: hidden;
}

.ttl-bar {
  height: 100%;
  background: #4a90e2;
  transition: width 0.3s ease;
}

.mode-toggle, .refresh-btn {
  padding: 10px 20px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.mode-toggle:hover, .refresh-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

.mode-toggle.active {
  background: #5cb85c;
}

/* Pipeline Overview */
.overview-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
  gap: 20px;
  margin-bottom: 30px;
}

.stage-card {
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 20px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: transform 0.3s ease;
}

.stage-card:hover {
  transform: translateY(-5px);
}

.stage-card.active {
  border-color: #5cb85c;
  box-shadow: 0 0 20px rgba(92, 184, 92, 0.3);
}

.stage-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 15px;
}

.stage-icon {
  font-size: 2rem;
}

.stage-name {
  font-size: 1.2rem;
  font-weight: 600;
  margin: 0;
}

.stage-metrics {
  display: flex;
  justify-content: space-between;
  margin-bottom: 15px;
}

.metric {
  text-align: center;
}

.metric-label {
  display: block;
  font-size: 0.8rem;
  opacity: 0.7;
}

.metric-value {
  display: block;
  font-size: 1.1rem;
  font-weight: 600;
  margin-top: 2px;
}

.stage-ttl {
  display: flex;
  align-items: center;
  gap: 10px;
}

.ttl-meter {
  flex: 1;
  height: 6px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 3px;
  overflow: hidden;
}

.ttl-fill {
  height: 100%;
  background: linear-gradient(90deg, #5cb85c, #f0ad4e, #d9534f);
  transition: width 0.3s ease;
}

.ttl-text {
  font-size: 0.8rem;
  opacity: 0.8;
}

/* Pipeline Flow Visualization */
.pipeline-flow {
  margin-bottom: 30px;
}

.section-title {
  font-size: 1.8rem;
  font-weight: 600;
  margin-bottom: 20px;
  color: #ffffff;
}

.flow-container {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
  overflow-x: auto;
}

.flow-svg {
  width: 100%;
  height: 300px;
}

.stage-node {
  filter: drop-shadow(0 4px 8px rgba(0, 0, 0, 0.3));
  transition: all 0.3s ease;
}

.stage-node:hover {
  filter: drop-shadow(0 6px 12px rgba(0, 0, 0, 0.5));
}

.stage-node.active {
  filter: drop-shadow(0 0 15px rgba(92, 184, 92, 0.8));
}

.stage-node-icon {
  font-size: 16px;
  fill: white;
}

.stage-label {
  font-size: 12px;
  fill: white;
  font-weight: 600;
}

.stage-ttl-label {
  font-size: 10px;
  fill: rgba(255, 255, 255, 0.7);
}

.connection-line {
  stroke-dasharray: 5,5;
  animation: flow 2s linear infinite;
}

@keyframes flow {
  0% { stroke-dashoffset: 0; }
  100% { stroke-dashoffset: 10; }
}

.data-flow {
  filter: drop-shadow(0 0 4px #00ff00);
}

/* Active Executions */
.executions-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
  gap: 20px;
  margin-bottom: 30px;
}

.execution-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.execution-card.running {
  border-color: #5cb85c;
  box-shadow: 0 0 15px rgba(92, 184, 92, 0.2);
}

.execution-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
}

.execution-id {
  font-family: monospace;
  font-size: 0.9rem;
  opacity: 0.8;
}

.execution-status {
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.execution-status.running {
  background: #5cb85c;
  color: white;
}

.progress-bar {
  height: 8px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 10px;
}

.progress-fill {
  height: 100%;
  background: linear-gradient(90deg, #4a90e2, #5cb85c);
  transition: width 0.3s ease;
}

.progress-text {
  font-size: 0.9rem;
  opacity: 0.9;
}

.execution-timing {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
  opacity: 0.7;
  margin-top: 10px;
}

/* Metrics Grid */
.metrics-grid {
  margin-bottom: 30px;
}

.metrics-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 20px;
}

.metric-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 25px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.panel-title {
  font-size: 1.3rem;
  font-weight: 600;
  margin-bottom: 20px;
  color: #ffffff;
}

.chart-container {
  height: 200px;
  margin-bottom: 15px;
}

.metric-chart {
  width: 100%;
  height: 100%;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

.chart-legend {
  display: flex;
  gap: 15px;
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 0.9rem;
}

.legend-color {
  width: 12px;
  height: 12px;
  border-radius: 2px;
}

.violation-summary {
  display: flex;
  gap: 30px;
}

.violation-stat {
  text-align: center;
}

.stat-value {
  display: block;
  font-size: 1.8rem;
  font-weight: 700;
  color: #d9534f;
}

.stat-label {
  display: block;
  font-size: 0.9rem;
  opacity: 0.7;
  margin-top: 5px;
}

.swarm-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 15px;
}

.swarm-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 15px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.swarm-card.active {
  border-color: #5cb85c;
}

.swarm-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
}

.swarm-name {
  font-weight: 600;
  font-size: 0.9rem;
}

.swarm-agents {
  font-size: 0.8rem;
  opacity: 0.7;
}

.swarm-metrics {
  display: flex;
  justify-content: space-between;
}

.swarm-metric {
  text-align: center;
}

/* System Health */
.health-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 20px;
}

.health-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 25px;
  text-align: center;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: transform 0.3s ease;
}

.health-card:hover {
  transform: translateY(-3px);
}

.health-icon {
  font-size: 3rem;
  margin-bottom: 15px;
}

.health-title {
  font-size: 1.2rem;
  font-weight: 600;
  margin-bottom: 10px;
  color: #ffffff;
}

.health-status {
  padding: 6px 16px;
  border-radius: 20px;
  font-size: 0.9rem;
  font-weight: 600;
  text-transform: uppercase;
  margin-bottom: 15px;
  display: inline-block;
}

.health-status.healthy {
  background: #5cb85c;
  color: white;
}

.health-status.warning {
  background: #f0ad4e;
  color: white;
}

.health-status.critical {
  background: #d9534f;
  color: white;
}

.health-details {
  display: flex;
  flex-direction: column;
  gap: 5px;
  font-size: 0.9rem;
  opacity: 0.8;
}

/* Responsive Design */
@media (max-width: 768px) {
  .dashboard-title {
    font-size: 1.8rem;
  }
  
  .header-content {
    flex-direction: column;
    gap: 20px;
  }
  
  .overview-grid {
    grid-template-columns: 1fr;
  }
  
  .metrics-row {
    grid-template-columns: 1fr;
  }
  
  .health-grid {
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  }
}
</style>