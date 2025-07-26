<!--
BitActor Nuxt UI Pipeline Visualizer Variant
Advanced 3D pipeline visualization for typer > turtle > ttl2dspy > BitActor > Erlang > Ash > Reactor > k8s
Real-time data flow animation with TTL constraint visualization
No TypeScript - Pure JavaScript with advanced SVG/Canvas rendering
-->

<template>
  <div class="pipeline-visualizer">
    <!-- Visualizer Header Controls -->
    <div class="visualizer-header">
      <h2 class="visualizer-title">
        <span class="title-icon">🌊</span>
        Pipeline Flow Visualizer
      </h2>
      <div class="visualizer-controls">
        <div class="view-selector">
          <button v-for="view in viewModes" :key="view.id"
                  @click="currentView = view.id"
                  class="view-btn" :class="{ active: currentView === view.id }">
            {{ view.icon }} {{ view.name }}
          </button>
        </div>
        <div class="flow-controls">
          <button @click="toggleDataFlow" class="control-btn" :class="{ active: dataFlowActive }">
            {{ dataFlowActive ? '⏸️' : '▶️' }} Flow
          </button>
          <button @click="resetVisualization" class="control-btn">
            🔄 Reset
          </button>
          <select v-model="animationSpeed" class="speed-selector">
            <option value="0.5">Slow</option>
            <option value="1">Normal</option>
            <option value="2">Fast</option>
          </select>
        </div>
      </div>
    </div>

    <!-- Main Visualization Container -->
    <div class="visualization-container" :class="currentView">
      <!-- 3D Pipeline View -->
      <div v-if="currentView === '3d'" class="pipeline-3d">
        <svg class="pipeline-svg" viewBox="0 0 1400 600" @click="handleCanvasClick">
          <!-- 3D Pipeline Stages -->
          <defs>
            <!-- Gradients for 3D effect -->
            <linearGradient id="stageGradient" x1="0%" y1="0%" x2="100%" y2="100%">
              <stop offset="0%" style="stop-color:#ffffff;stop-opacity:0.3" />
              <stop offset="100%" style="stop-color:#000000;stop-opacity:0.1" />
            </linearGradient>
            
            <!-- Shadow filters -->
            <filter id="dropShadow" x="-50%" y="-50%" width="200%" height="200%">
              <feDropShadow dx="3" dy="6" stdDeviation="4" flood-opacity="0.3"/>
            </filter>
            
            <!-- Glow effect -->
            <filter id="glow" x="-50%" y="-50%" width="200%" height="200%">
              <feGaussianBlur stdDeviation="4" result="coloredBlur"/>
              <feMerge> 
                <feMergeNode in="coloredBlur"/>
                <feMergeNode in="SourceGraphic"/>
              </feMerge>
            </filter>
          </defs>

          <!-- Background grid -->
          <g class="background-grid" opacity="0.1">
            <defs>
              <pattern id="grid" width="40" height="40" patternUnits="userSpaceOnUse">
                <path d="M 40 0 L 0 0 0 40" fill="none" stroke="#ffffff" stroke-width="1"/>
              </pattern>
            </defs>
            <rect width="100%" height="100%" fill="url(#grid)" />
          </g>

          <!-- Pipeline stages with 3D effect -->
          <g v-for="(stage, index) in pipelineStages" :key="stage.name" 
             class="stage-group" :transform="getStageTransform(index)">
            
            <!-- Stage base (3D bottom) -->
            <ellipse :rx="50" ry="15" :fill="stage.color" 
                    opacity="0.6" transform="translate(0, 45)"/>
            
            <!-- Stage cylinder side -->
            <rect x="-50" y="-25" width="100" height="70" 
                  :fill="stage.color" opacity="0.8"/>
            
            <!-- Stage top -->
            <ellipse :rx="50" ry="15" :fill="stage.color" 
                    fill="url(#stageGradient)" filter="url(#dropShadow)"/>
            
            <!-- Stage status indicator -->
            <circle :r="8" :fill="getStatusColor(stage.status)" 
                   transform="translate(40, -20)" filter="url(#glow)">
              <animate v-if="stage.status === 'active'" 
                      attributeName="opacity" values="0.5;1;0.5" dur="2s" repeatCount="indefinite"/>
            </circle>
            
            <!-- Stage icon -->
            <text x="0" y="8" text-anchor="middle" class="stage-icon-3d" 
                  :fill="stage.textColor || '#ffffff'">
              {{ stage.icon }}
            </text>
            
            <!-- Stage name -->
            <text x="0" y="75" text-anchor="middle" class="stage-name-3d">
              {{ stage.name }}
            </text>
            
            <!-- TTL progress ring -->
            <circle :r="55" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="3"/>
            <circle :r="55" fill="none" :stroke="getTTLColor(stage.ttlUsage)" 
                   stroke-width="3" stroke-linecap="round"
                   :stroke-dasharray="getTTLDashArray(stage.ttlUsage)"
                   stroke-dashoffset="0" transform="rotate(-90)">
              <animateTransform v-if="dataFlowActive" 
                              attributeName="transform" type="rotate"
                              values="-90;270;-90" dur="4s" repeatCount="indefinite"/>
            </circle>
            
            <!-- Performance metrics display -->
            <g class="metrics-display" transform="translate(0, -55)">
              <rect x="-30" y="-10" width="60" height="20" 
                   fill="rgba(0,0,0,0.7)" rx="10" opacity="0"/>
              <text x="0" y="2" text-anchor="middle" class="metric-text">
                {{ stage.avgTime }}ms
              </text>
              <animate attributeName="opacity" values="0;1;0" dur="3s" repeatCount="indefinite"/>
            </g>
            
            <!-- Data flow particles -->
            <g v-if="dataFlowActive && index < pipelineStages.length - 1" class="data-particles">
              <circle v-for="particle in getStageParticles(index)" :key="particle.id"
                     :r="particle.size" :fill="particle.color" 
                     :transform="particle.transform" opacity="0.8">
                <animateTransform attributeName="transform" type="translate"
                                :values="particle.path" :dur="particle.duration"
                                repeatCount="indefinite"/>
                <animate attributeName="opacity" values="0;1;0" 
                        :dur="particle.duration" repeatCount="indefinite"/>
              </circle>
            </g>
          </g>

          <!-- Inter-stage connections -->
          <g class="connections">
            <g v-for="(connection, index) in stageConnections" :key="index" 
               class="connection-group">
              
              <!-- Connection tube -->
              <path :d="connection.path" fill="none" 
                   stroke="url(#connectionGradient)" stroke-width="8" opacity="0.6"/>
              
              <!-- Data flow animation -->
              <path v-if="dataFlowActive" :d="connection.path" fill="none" 
                   stroke="#00ff88" stroke-width="3" opacity="0.8"
                   stroke-dasharray="20,10">
                <animate attributeName="stroke-dashoffset" 
                        values="0;-30" dur="1s" repeatCount="indefinite"/>
              </path>
              
              <!-- Connection label -->
              <text :x="connection.labelX" :y="connection.labelY" 
                   text-anchor="middle" class="connection-label">
                {{ connection.throughput }}/s
              </text>
            </g>
          </g>

          <!-- Gradient definitions for connections -->
          <defs>
            <linearGradient id="connectionGradient" x1="0%" y1="0%" x2="100%" y2="0%">
              <stop offset="0%" style="stop-color:#4a90e2;stop-opacity:0.8" />
              <stop offset="50%" style="stop-color:#5cb85c;stop-opacity:0.9" />
              <stop offset="100%" style="stop-color:#4a90e2;stop-opacity:0.8" />
            </linearGradient>
          </defs>
        </svg>

        <!-- 3D Controls -->
        <div class="pipeline-controls-3d">
          <div class="control-group">
            <label>Rotation X:</label>
            <input type="range" v-model="rotation.x" min="-45" max="45" class="rotation-slider">
          </div>
          <div class="control-group">
            <label>Rotation Y:</label>
            <input type="range" v-model="rotation.y" min="-45" max="45" class="rotation-slider">
          </div>
          <div class="control-group">
            <label>Zoom:</label>
            <input type="range" v-model="zoom" min="0.5" max="2" step="0.1" class="zoom-slider">
          </div>
        </div>
      </div>

      <!-- Flow Diagram View -->
      <div v-if="currentView === 'flow'" class="pipeline-flow">
        <div class="flow-canvas" ref="flowCanvas">
          <canvas ref="flowCanvasElement" class="flow-canvas-element"></canvas>
        </div>
        
        <!-- Flow Legend -->
        <div class="flow-legend">
          <div class="legend-item">
            <div class="legend-color" style="background: #00ff88"></div>
            <span>Active Data Flow</span>
          </div>
          <div class="legend-item">
            <div class="legend-color" style="background: #ff4444"></div>
            <span>TTL Violation</span>
          </div>
          <div class="legend-item">
            <div class="legend-color" style="background: #ffaa00"></div>
            <span>Processing Delay</span>
          </div>
        </div>
      </div>

      <!-- Network View -->
      <div v-if="currentView === 'network'" class="pipeline-network">
        <div class="network-container" ref="networkContainer">
          <!-- Network nodes will be dynamically created -->
        </div>
      </div>
    </div>

    <!-- Pipeline Statistics Panel -->
    <div class="pipeline-stats">
      <div class="stats-grid">
        <div class="stat-card">
          <div class="stat-icon">⚡</div>
          <div class="stat-content">
            <div class="stat-value">{{ totalThroughput }}</div>
            <div class="stat-label">Operations/sec</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">⏱️</div>
          <div class="stat-content">
            <div class="stat-value">{{ averageLatency }}ms</div>
            <div class="stat-label">Avg Latency</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">🎯</div>
          <div class="stat-content">
            <div class="stat-value">{{ ttlCompliance }}%</div>
            <div class="stat-label">TTL Compliance</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">🚀</div>
          <div class="stat-content">
            <div class="stat-value">{{ systemLoad }}%</div>
            <div class="stat-label">System Load</div>
          </div>
        </div>
      </div>
    </div>

    <!-- Interactive Pipeline Inspector -->
    <div v-if="selectedStage" class="stage-inspector">
      <div class="inspector-header">
        <h3>{{ selectedStage.icon }} {{ selectedStage.name }} Inspector</h3>
        <button @click="selectedStage = null" class="close-btn">✕</button>
      </div>
      
      <div class="inspector-content">
        <div class="inspector-metrics">
          <div class="metric-row">
            <span class="metric-name">Status:</span>
            <span class="metric-value" :class="selectedStage.status">
              {{ selectedStage.status }}
            </span>
          </div>
          <div class="metric-row">
            <span class="metric-name">Executions:</span>
            <span class="metric-value">{{ selectedStage.executions }}</span>
          </div>
          <div class="metric-row">
            <span class="metric-name">Average Time:</span>
            <span class="metric-value">{{ selectedStage.avgTime }}ms</span>
          </div>
          <div class="metric-row">
            <span class="metric-name">Success Rate:</span>
            <span class="metric-value">{{ selectedStage.successRate }}%</span>
          </div>
          <div class="metric-row">
            <span class="metric-name">TTL Usage:</span>
            <span class="metric-value">{{ selectedStage.ttlUsage }}%</span>
          </div>
        </div>
        
        <div class="inspector-chart">
          <canvas ref="inspectorChart" class="inspector-chart-canvas"></canvas>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'NuxtUIPipelineVisualizerVariant',
  
  data() {
    return {
      currentView: '3d',
      dataFlowActive: true,
      animationSpeed: 1,
      selectedStage: null,
      
      rotation: {
        x: 0,
        y: 0
      },
      zoom: 1,
      
      viewModes: [
        { id: '3d', name: '3D Pipeline', icon: '🎲' },
        { id: 'flow', name: 'Flow Diagram', icon: '🌊' },
        { id: 'network', name: 'Network Graph', icon: '🕸️' }
      ],
      
      pipelineStages: [
        {
          name: 'Typer',
          icon: '📝',
          status: 'active',
          color: '#4a90e2',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 0.8,
          successRate: 99.2,
          ttlUsage: 80,
          ttlBudget: 1
        },
        {
          name: 'Turtle',
          icon: '🐢',
          status: 'active',
          color: '#5cb85c',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 1.2,
          successRate: 98.8,
          ttlUsage: 60,
          ttlBudget: 1
        },
        {
          name: 'TTL2DSPy',
          icon: '🔍',
          status: 'active',
          color: '#f0ad4e',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 0.9,
          successRate: 99.5,
          ttlUsage: 45,
          ttlBudget: 1
        },
        {
          name: 'BitActor',
          icon: '⚡',
          status: 'active',
          color: '#d9534f',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 1.5,
          successRate: 97.3,
          ttlUsage: 75,
          ttlBudget: 1
        },
        {
          name: 'Erlang',
          icon: '🔧',
          status: 'active',
          color: '#5bc0de',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 1.1,
          successRate: 99.1,
          ttlUsage: 55,
          ttlBudget: 1
        },
        {
          name: 'Ash',
          icon: '🌊',
          status: 'active',
          color: '#9b59b6',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 1.3,
          successRate: 98.9,
          ttlUsage: 65,
          ttlBudget: 1
        },
        {
          name: 'Reactor',
          icon: '🔄',
          status: 'active',
          color: '#e67e22',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 0.7,
          successRate: 99.7,
          ttlUsage: 35,
          ttlBudget: 1
        },
        {
          name: 'K8s',
          icon: '☸️',
          status: 'active',
          color: '#34495e',
          textColor: '#ffffff',
          executions: 1247,
          avgTime: 0.5,
          successRate: 99.9,
          ttlUsage: 25,
          ttlBudget: 1
        }
      ],
      
      flowCanvas: null,
      networkNodes: [],
      animationFrame: null,
      particles: []
    }
  },
  
  computed: {
    stageConnections() {
      const connections = []
      for (let i = 0; i < this.pipelineStages.length - 1; i++) {
        const startX = 150 + i * 150
        const endX = 150 + (i + 1) * 150
        const y = 300
        
        connections.push({
          path: `M ${startX + 50} ${y} Q ${startX + 100} ${y - 30} ${endX - 50} ${y}`,
          labelX: startX + 75,
          labelY: y - 20,
          throughput: Math.floor(Math.random() * 500 + 100)
        })
      }
      return connections
    },
    
    totalThroughput() {
      return this.pipelineStages.reduce((sum, stage) => sum + stage.executions, 0)
    },
    
    averageLatency() {
      const total = this.pipelineStages.reduce((sum, stage) => sum + stage.avgTime, 0)
      return (total / this.pipelineStages.length).toFixed(1)
    },
    
    ttlCompliance() {
      const compliantStages = this.pipelineStages.filter(stage => stage.ttlUsage < 90).length
      return Math.round((compliantStages / this.pipelineStages.length) * 100)
    },
    
    systemLoad() {
      const avgLoad = this.pipelineStages.reduce((sum, stage) => sum + stage.ttlUsage, 0) / this.pipelineStages.length
      return Math.round(avgLoad)
    }
  },
  
  mounted() {
    this.initializeVisualizations()
    this.startAnimationLoop()
  },
  
  beforeUnmount() {
    this.stopAnimationLoop()
  },
  
  methods: {
    getStageTransform(index) {
      const baseX = 150 + index * 150
      const baseY = 300
      const rotX = this.rotation.x
      const rotY = this.rotation.y
      const scale = this.zoom
      
      return `translate(${baseX}, ${baseY}) scale(${scale}) rotateX(${rotX}) rotateY(${rotY})`
    },
    
    getStatusColor(status) {
      const colors = {
        active: '#00ff88',
        warning: '#ffaa00',
        error: '#ff4444',
        idle: '#888888'
      }
      return colors[status] || '#888888'
    },
    
    getTTLColor(usage) {
      if (usage > 80) return '#ff4444'
      if (usage > 60) return '#ffaa00'
      return '#00ff88'
    },
    
    getTTLDashArray(usage) {
      const circumference = 2 * Math.PI * 55
      const dashLength = (usage / 100) * circumference
      return `${dashLength} ${circumference - dashLength}`
    },
    
    getStageParticles(stageIndex) {
      return Array.from({ length: 3 }, (_, i) => ({
        id: `particle_${stageIndex}_${i}`,
        size: Math.random() * 3 + 2,
        color: this.pipelineStages[stageIndex].color,
        transform: `translate(${55 + i * 10}, 0)`,
        path: `55,0; 95,0; 135,0`,
        duration: `${2 + Math.random()}s`
      }))
    },
    
    toggleDataFlow() {
      this.dataFlowActive = !this.dataFlowActive
      if (this.dataFlowActive) {
        this.startAnimationLoop()
      } else {
        this.stopAnimationLoop()
      }
    },
    
    resetVisualization() {
      this.rotation.x = 0
      this.rotation.y = 0
      this.zoom = 1
      this.selectedStage = null
    },
    
    handleCanvasClick(event) {
      // Detect which stage was clicked
      const rect = event.currentTarget.getBoundingClientRect()
      const x = event.clientX - rect.left
      const y = event.clientY - rect.top
      
      // Simple collision detection for stage selection
      this.pipelineStages.forEach((stage, index) => {
        const stageX = 150 + index * 150
        const stageY = 300
        const distance = Math.sqrt((x - stageX) ** 2 + (y - stageY) ** 2)
        
        if (distance < 60) {
          this.selectedStage = stage
        }
      })
    },
    
    initializeVisualizations() {
      this.initializeFlowCanvas()
      this.initializeNetworkView()
    },
    
    initializeFlowCanvas() {
      const canvas = this.$refs.flowCanvasElement
      if (!canvas) return
      
      canvas.width = 1200
      canvas.height = 400
      this.flowCanvas = canvas.getContext('2d')
      this.drawFlowDiagram()
    },
    
    drawFlowDiagram() {
      if (!this.flowCanvas) return
      
      const ctx = this.flowCanvas
      const canvas = ctx.canvas
      
      // Clear canvas
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      
      // Draw background
      ctx.fillStyle = 'rgba(26, 26, 46, 0.1)'
      ctx.fillRect(0, 0, canvas.width, canvas.height)
      
      // Draw pipeline stages
      this.pipelineStages.forEach((stage, index) => {
        const x = 100 + index * 140
        const y = 200
        
        // Stage background
        ctx.fillStyle = stage.color
        ctx.beginPath()
        ctx.roundRect(x - 50, y - 30, 100, 60, 10)
        ctx.fill()
        
        // Stage border
        ctx.strokeStyle = this.getTTLColor(stage.ttlUsage)
        ctx.lineWidth = 3
        ctx.stroke()
        
        // Stage text
        ctx.fillStyle = '#ffffff'
        ctx.font = '14px Inter'
        ctx.textAlign = 'center'
        ctx.fillText(stage.name, x, y + 5)
        
        // TTL indicator
        ctx.fillStyle = this.getTTLColor(stage.ttlUsage)
        ctx.beginPath()
        ctx.arc(x + 35, y - 20, 5, 0, 2 * Math.PI)
        ctx.fill()
        
        // Data flow connections
        if (index < this.pipelineStages.length - 1) {
          this.drawDataFlow(ctx, x + 50, y, x + 90, y)
        }
      })
    },
    
    drawDataFlow(ctx, startX, startY, endX, endY) {
      if (!this.dataFlowActive) return
      
      // Flow line
      ctx.strokeStyle = '#00ff88'
      ctx.lineWidth = 2
      ctx.setLineDash([5, 5])
      ctx.beginPath()
      ctx.moveTo(startX, startY)
      ctx.lineTo(endX, endY)
      ctx.stroke()
      ctx.setLineDash([])
      
      // Flow particles
      const time = Date.now() / 1000
      const progress = (time * this.animationSpeed) % 1
      const particleX = startX + (endX - startX) * progress
      
      ctx.fillStyle = '#00ff88'
      ctx.beginPath()
      ctx.arc(particleX, startY, 3, 0, 2 * Math.PI)
      ctx.fill()
    },
    
    initializeNetworkView() {
      // Create network nodes for network view
      this.networkNodes = this.pipelineStages.map((stage, index) => ({
        id: stage.name,
        x: Math.random() * 800 + 100,
        y: Math.random() * 400 + 100,
        vx: 0,
        vy: 0,
        stage: stage,
        connections: index < this.pipelineStages.length - 1 ? [index + 1] : []
      }))
    },
    
    startAnimationLoop() {
      if (this.animationFrame) return
      
      const animate = () => {
        if (this.currentView === 'flow') {
          this.drawFlowDiagram()
        }
        this.animationFrame = requestAnimationFrame(animate)
      }
      
      animate()
    },
    
    stopAnimationLoop() {
      if (this.animationFrame) {
        cancelAnimationFrame(this.animationFrame)
        this.animationFrame = null
      }
    }
  }
}
</script>

<style scoped>
.pipeline-visualizer {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

.visualizer-header {
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 20px;
  margin-bottom: 20px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.visualizer-title {
  font-size: 2rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 12px;
}

.title-icon {
  font-size: 2.5rem;
}

.visualizer-controls {
  display: flex;
  gap: 20px;
  align-items: center;
}

.view-selector {
  display: flex;
  gap: 10px;
}

.view-btn {
  padding: 10px 16px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
  font-weight: 500;
}

.view-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

.view-btn.active {
  background: #4a90e2;
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(74, 144, 226, 0.3);
}

.flow-controls {
  display: flex;
  gap: 15px;
  align-items: center;
}

.control-btn {
  padding: 10px 16px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.control-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

.control-btn.active {
  background: #5cb85c;
}

.speed-selector {
  padding: 8px 12px;
  border: none;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
}

.visualization-container {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
  margin-bottom: 20px;
  min-height: 500px;
  position: relative;
}

/* 3D Pipeline View */
.pipeline-3d {
  position: relative;
}

.pipeline-svg {
  width: 100%;
  height: 500px;
  background: radial-gradient(ellipse at center, rgba(74, 144, 226, 0.1) 0%, transparent 70%);
  border-radius: 12px;
}

.stage-group {
  cursor: pointer;
  transition: all 0.3s ease;
}

.stage-group:hover {
  transform: scale(1.05);
}

.stage-icon-3d {
  font-size: 20px;
  font-weight: bold;
  filter: drop-shadow(0 2px 4px rgba(0, 0, 0, 0.5));
}

.stage-name-3d {
  font-size: 14px;
  font-weight: 600;
  fill: #ffffff;
  filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.5));
}

.metric-text {
  font-size: 10px;
  fill: #ffffff;
  font-weight: 600;
}

.connection-label {
  font-size: 11px;
  fill: #ffffff;
  font-weight: 500;
  opacity: 0.8;
}

.pipeline-controls-3d {
  position: absolute;
  top: 20px;
  right: 20px;
  background: rgba(0, 0, 0, 0.7);
  padding: 20px;
  border-radius: 12px;
  backdrop-filter: blur(10px);
}

.control-group {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 15px;
}

.control-group label {
  min-width: 80px;
  font-size: 0.9rem;
  color: #ffffff;
}

.rotation-slider, .zoom-slider {
  width: 100px;
}

/* Flow Canvas */
.flow-canvas {
  position: relative;
  width: 100%;
  height: 400px;
}

.flow-canvas-element {
  width: 100%;
  height: 100%;
  border-radius: 12px;
  background: rgba(0, 0, 0, 0.2);
}

.flow-legend {
  position: absolute;
  top: 20px;
  left: 20px;
  background: rgba(0, 0, 0, 0.7);
  padding: 15px;
  border-radius: 8px;
  backdrop-filter: blur(10px);
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 8px;
  font-size: 0.9rem;
}

.legend-color {
  width: 12px;
  height: 12px;
  border-radius: 2px;
}

/* Network View */
.network-container {
  width: 100%;
  height: 400px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 12px;
  position: relative;
}

/* Pipeline Statistics */
.pipeline-stats {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 20px;
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 20px;
}

.stat-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  display: flex;
  align-items: center;
  gap: 15px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: transform 0.3s ease;
}

.stat-card:hover {
  transform: translateY(-3px);
}

.stat-icon {
  font-size: 2.5rem;
}

.stat-content {
  flex: 1;
}

.stat-value {
  font-size: 1.8rem;
  font-weight: 700;
  color: #4a90e2;
  display: block;
}

.stat-label {
  font-size: 0.9rem;
  opacity: 0.8;
  margin-top: 4px;
}

/* Stage Inspector */
.stage-inspector {
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 500px;
  background: rgba(26, 26, 46, 0.95);
  backdrop-filter: blur(20px);
  border-radius: 16px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 1000;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.5);
}

.inspector-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 20px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.2);
}

.inspector-header h3 {
  margin: 0;
  font-size: 1.3rem;
  font-weight: 600;
}

.close-btn {
  width: 30px;
  height: 30px;
  border: none;
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.2);
  color: white;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
}

.inspector-content {
  padding: 20px;
}

.inspector-metrics {
  margin-bottom: 20px;
}

.metric-row {
  display: flex;
  justify-content: space-between;
  padding: 8px 0;
  border-bottom: 1px solid rgba(255, 255, 255, 0.1);
}

.metric-name {
  font-weight: 500;
  opacity: 0.8;
}

.metric-value {
  font-weight: 600;
}

.metric-value.active {
  color: #5cb85c;
}

.metric-value.warning {
  color: #f0ad4e;
}

.metric-value.error {
  color: #d9534f;
}

.inspector-chart {
  height: 200px;
}

.inspector-chart-canvas {
  width: 100%;
  height: 100%;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

/* Responsive Design */
@media (max-width: 768px) {
  .visualizer-header {
    flex-direction: column;
    gap: 20px;
  }
  
  .visualizer-controls {
    flex-direction: column;
    gap: 15px;
  }
  
  .view-selector {
    flex-wrap: wrap;
  }
  
  .stats-grid {
    grid-template-columns: 1fr;
  }
  
  .stage-inspector {
    width: 90vw;
    max-width: 450px;
  }
}

/* Animation Classes */
@keyframes pulse {
  0%, 100% { opacity: 0.8; }
  50% { opacity: 1; }
}

@keyframes flow {
  0% { transform: translateX(-100%); opacity: 0; }
  50% { opacity: 1; }
  100% { transform: translateX(100%); opacity: 0; }
}

.data-particles circle {
  animation: pulse 2s infinite;
}

.connection-line {
  animation: flow 3s linear infinite;
}
</style>