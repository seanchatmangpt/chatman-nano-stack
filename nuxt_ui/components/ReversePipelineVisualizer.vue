<template>
  <div class="reverse-pipeline-visualizer">
    <h2>🔄 Reverse Pipeline Flow Visualizer</h2>
    <p class="flow-description">
      Output Flow: k8s → Reactor → Ash → Erlang → BitActor → ttl2dspy → turtle → typer
    </p>
    
    <!-- Reverse Flow Control Panel -->
    <div class="reverse-control-panel">
      <h3>🎛️ Reverse Flow Controls</h3>
      <div class="control-grid">
        <div class="control-section">
          <label>Output Source</label>
          <select v-model="outputSource" @change="updateReverseFlow">
            <option value="k8s_deployment">K8s Deployment Results</option>
            <option value="k8s_logs">K8s System Logs</option>
            <option value="k8s_metrics">K8s Performance Metrics</option>
            <option value="k8s_events">K8s Cluster Events</option>
          </select>
        </div>
        
        <div class="control-section">
          <label>Feedback Intensity</label>
          <input 
            type="range" 
            v-model="feedbackIntensity" 
            min="0.1" 
            max="1.0" 
            step="0.1"
            @input="updateReverseFlow"
          />
          <span class="intensity-value">{{ feedbackIntensity }}</span>
        </div>
        
        <div class="control-section">
          <label>80/20 Reverse Optimization</label>
          <button 
            @click="applyReverseOptimization" 
            class="optimize-btn"
            :class="{ active: reverseOptimizationActive }"
          >
            ⚡ Apply 80/20 Reverse
          </button>
        </div>
        
        <div class="control-section">
          <label>Flow Direction</label>
          <button 
            @click="toggleFlowDirection" 
            class="direction-btn"
            :class="{ reverse: isReverseFlow }"
          >
            {{ isReverseFlow ? '⬅️ Reverse' : '➡️ Forward' }}
          </button>
        </div>
      </div>
    </div>
    
    <!-- Reverse Pipeline Visualization -->
    <div class="reverse-pipeline-flow">
      <svg :width="canvasWidth" :height="canvasHeight" class="reverse-flow-svg">
        <!-- Background Grid -->
        <defs>
          <pattern id="reverse-grid" width="30" height="30" patternUnits="userSpaceOnUse">
            <path d="M 30 0 L 0 0 0 30" fill="none" stroke="#2a2a2a" stroke-width="1"/>
          </pattern>
          
          <!-- Reverse Flow Gradient -->
          <linearGradient id="reverse-gradient" x1="100%" y1="0%" x2="0%" y2="0%">
            <stop offset="0%" style="stop-color:#0084ff;stop-opacity:1" />
            <stop offset="50%" style="stop-color:#00ff84;stop-opacity:0.8" />
            <stop offset="100%" style="stop-color:#ff8400;stop-opacity:0.6" />
          </linearGradient>
          
          <!-- Pulse Animation -->
          <filter id="pulse">
            <feGaussianBlur stdDeviation="3" result="coloredBlur"/>
            <feMerge> 
              <feMergeNode in="coloredBlur"/>
              <feMergeNode in="SourceGraphic"/> 
            </feMerge>
          </filter>
        </defs>
        
        <rect width="100%" height="100%" fill="url(#reverse-grid)" opacity="0.3"/>
        
        <!-- Reverse Pipeline Stages -->
        <g class="reverse-stages">
          <g
            v-for="(stage, index) in reverseStages"
            :key="stage.name"
            class="reverse-stage"
            :class="{ 
              active: stage.active, 
              processing: stage.processing,
              optimized: stage.optimized,
              critical: stage.critical
            }"
            :transform="`translate(${getStageX(index)}, ${stageY})`"
          >
            <!-- Stage Background -->
            <rect
              :width="stageWidth"
              :height="stageHeight"
              :fill="getStageColor(stage)"
              :stroke="stage.active ? '#00ff84' : '#444'"
              :stroke-width="stage.active ? 3 : 1"
              rx="12"
              class="stage-bg"
              :filter="stage.processing ? 'url(#pulse)' : 'none'"
            />
            
            <!-- Stage Icon -->
            <text
              :x="stageWidth / 2"
              :y="35"
              text-anchor="middle"
              class="stage-icon"
              :fill="stage.active ? '#fff' : '#aaa'"
            >
              {{ getStageIcon(stage.name) }}
            </text>
            
            <!-- Stage Name -->
            <text
              :x="stageWidth / 2"
              :y="55"
              text-anchor="middle"
              class="stage-name"
              :fill="stage.active ? '#fff' : '#ccc'"
            >
              {{ stage.name }}
            </text>
            
            <!-- Feedback Data -->
            <text
              :x="stageWidth / 2"
              :y="70"
              text-anchor="middle"
              class="feedback-data"
              :fill="stage.active ? '#00ff84' : '#666'"
            >
              {{ stage.feedbackData }}
            </text>
            
            <!-- 80/20 Optimization Indicator -->
            <circle
              v-if="stage.optimized"
              :cx="stageWidth - 15"
              :cy="15"
              r="8"
              fill="#ff8400"
              class="optimization-indicator"
            />
            <text
              v-if="stage.optimized"
              :x="stageWidth - 15"
              :y="19"
              text-anchor="middle"
              class="optimization-text"
              fill="#fff"
              font-size="10"
            >
              80
            </text>
            
            <!-- Reverse Flow Arrow -->
            <path
              v-if="index < reverseStages.length - 1"
              :d="getReverseFlowPath(index)"
              stroke="url(#reverse-gradient)"
              stroke-width="4"
              fill="none"
              marker-end="url(#reverse-arrow)"
              class="reverse-flow-line"
              :class="{ animated: isFlowAnimated }"
            />
            
            <!-- Data Volume Indicator -->
            <rect
              :x="5"
              :y="stageHeight - 15"
              :width="(stageWidth - 10) * stage.dataVolume"
              height="5"
              :fill="stage.active ? '#00ff84' : '#444'"
              rx="2"
              class="data-volume"
            />
          </g>
        </g>
        
        <!-- Reverse Arrow Marker -->
        <defs>
          <marker
            id="reverse-arrow"
            markerWidth="12"
            markerHeight="8"
            refX="11"
            refY="4"
            orient="auto"
            markerUnits="strokeWidth"
          >
            <path
              d="M0,0 L0,8 L12,4 z"
              fill="url(#reverse-gradient)"
            />
          </marker>
        </defs>
        
        <!-- Flow Animation Particles -->
        <g class="flow-particles" v-if="showParticles">
          <circle
            v-for="particle in flowParticles"
            :key="particle.id"
            :cx="particle.x"
            :cy="particle.y"
            :r="particle.size"
            :fill="particle.color"
            class="flow-particle"
            :opacity="particle.opacity"
          />
        </g>
      </svg>
    </div>
    
    <!-- Reverse Flow Metrics -->
    <div class="reverse-metrics">
      <h3>📊 Reverse Flow Analytics</h3>
      <div class="metrics-grid">
        <div class="metric-card">
          <div class="metric-icon">🔄</div>
          <div class="metric-content">
            <div class="metric-label">Feedback Cycles</div>
            <div class="metric-value">{{ feedbackCycles }}</div>
            <div class="metric-detail">per minute</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">⚡</div>
          <div class="metric-content">
            <div class="metric-label">80/20 Efficiency</div>
            <div class="metric-value">{{ reverseEfficiency }}%</div>
            <div class="metric-detail">optimization gain</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">📈</div>
          <div class="metric-content">
            <div class="metric-label">Data Throughput</div>
            <div class="metric-value">{{ dataThroughput }}</div>
            <div class="metric-detail">MB/s reverse</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">🎯</div>
          <div class="metric-content">
            <div class="metric-label">Critical Feedback</div>
            <div class="metric-value">{{ criticalFeedbackCount }}</div>
            <div class="metric-detail">active paths</div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Reverse Optimization Suggestions -->
    <div class="reverse-optimization" v-if="reverseOptimizationActive">
      <h3>🧠 Reverse 80/20 Optimization</h3>
      <div class="optimization-list">
        <div
          v-for="optimization in reverseOptimizations"
          :key="optimization.id"
          class="optimization-item"
          :class="optimization.priority"
        >
          <div class="optimization-icon">{{ optimization.icon }}</div>
          <div class="optimization-content">
            <div class="optimization-title">{{ optimization.title }}</div>
            <div class="optimization-description">{{ optimization.description }}</div>
            <div class="optimization-impact">Impact: {{ optimization.impact }}%</div>
          </div>
          <button @click="applyOptimization(optimization)" class="apply-optimization">
            Apply
          </button>
        </div>
      </div>
    </div>
    
    <!-- Reverse Flow Insights -->
    <div class="reverse-insights">
      <h3>💡 Swarm Intelligence Insights</h3>
      <div class="insights-container">
        <div
          v-for="insight in reverseInsights"
          :key="insight.id"
          class="insight-card"
          :class="insight.type"
        >
          <div class="insight-header">
            <span class="insight-icon">{{ insight.icon }}</span>
            <span class="insight-title">{{ insight.title }}</span>
          </div>
          <div class="insight-content">{{ insight.content }}</div>
          <div class="insight-confidence">Confidence: {{ insight.confidence }}%</div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'ReversePipelineVisualizer',
  
  data() {
    return {
      canvasWidth: 1200,
      canvasHeight: 400,
      stageWidth: 120,
      stageHeight: 80,
      stageY: 150,
      
      outputSource: 'k8s_deployment',
      feedbackIntensity: 0.7,
      isReverseFlow: true,
      reverseOptimizationActive: false,
      isFlowAnimated: true,
      showParticles: true,
      
      reverseStages: [
        {
          name: 'k8s',
          active: true,
          processing: false,
          optimized: false,
          critical: true,
          feedbackData: '94.5MB',
          dataVolume: 0.95
        },
        {
          name: 'reactor',
          active: false,
          processing: false,
          optimized: false,
          critical: true,
          feedbackData: '67.2MB',
          dataVolume: 0.72
        },
        {
          name: 'ash',
          active: false,
          processing: false,
          optimized: false,
          critical: true,
          feedbackData: '45.8MB',
          dataVolume: 0.58
        },
        {
          name: 'erlang',
          active: false,
          processing: false,
          optimized: false,
          critical: false,
          feedbackData: '23.1MB',
          dataVolume: 0.31
        },
        {
          name: 'bitactor',
          active: false,
          processing: false,
          optimized: false,
          critical: false,
          feedbackData: '12.7MB',
          dataVolume: 0.27
        },
        {
          name: 'ttl2dspy',
          active: false,
          processing: false,
          optimized: false,
          critical: false,
          feedbackData: '8.9MB',
          dataVolume: 0.19
        },
        {
          name: 'turtle',
          active: false,
          processing: false,
          optimized: false,
          critical: true,
          feedbackData: '5.2MB',
          dataVolume: 0.12
        },
        {
          name: 'typer',
          active: false,
          processing: false,
          optimized: false,
          critical: true,
          feedbackData: '2.1MB',
          dataVolume: 0.05
        }
      ],
      
      flowParticles: [],
      
      // Metrics
      feedbackCycles: 24,
      reverseEfficiency: 85,
      dataThroughput: 45.7,
      criticalFeedbackCount: 4,
      
      reverseOptimizations: [
        {
          id: 'skip-non-critical',
          icon: '✂️',
          title: 'Skip Non-Critical Reverse Stages',
          description: 'Bypass erlang, bitactor, ttl2dspy for 80/20 efficiency',
          impact: 60,
          priority: 'high'
        },
        {
          id: 'batch-feedback',
          icon: '📦',
          title: 'Batch Feedback Processing',
          description: 'Group reverse data flows for better throughput',
          impact: 35,
          priority: 'medium'
        },
        {
          id: 'smart-filtering',
          icon: '🎯',
          title: 'Smart Data Filtering',
          description: 'Filter feedback data at source (k8s) level',
          impact: 45,
          priority: 'high'
        }
      ],
      
      reverseInsights: [
        {
          id: 'bottleneck',
          icon: '🚧',
          title: 'Reverse Bottleneck Detected',
          content: 'Reactor stage processing 80% of reverse data volume',
          confidence: 92,
          type: 'warning'
        },
        {
          id: 'optimization',
          icon: '⚡',
          title: '80/20 Opportunity',
          content: 'Skip 3 stages to achieve 60% efficiency improvement',
          confidence: 88,
          type: 'optimization'
        },
        {
          id: 'pattern',
          icon: '🔍',
          title: 'Feedback Pattern',
          content: 'K8s metrics show cyclical 5-minute feedback loops',
          confidence: 85,
          type: 'pattern'
        }
      ]
    }
  },
  
  mounted() {
    this.startReverseFlow()
    this.generateFlowParticles()
    setInterval(this.updateMetrics, 2000)
    setInterval(this.animateParticles, 100)
  },
  
  methods: {
    startReverseFlow() {
      this.reverseStages[0].active = true
      this.simulateReverseFlow()
    },
    
    simulateReverseFlow() {
      let currentStage = 0
      
      const flowInterval = setInterval(() => {
        if (currentStage < this.reverseStages.length) {
          // Activate current stage
          this.reverseStages[currentStage].active = true
          this.reverseStages[currentStage].processing = true
          
          // Deactivate previous stage
          if (currentStage > 0) {
            this.reverseStages[currentStage - 1].processing = false
          }
          
          currentStage++
        } else {
          // Reset flow
          this.reverseStages.forEach(stage => {
            stage.active = false
            stage.processing = false
          })
          currentStage = 0
          
          // Restart flow
          setTimeout(() => this.simulateReverseFlow(), 1000)
          clearInterval(flowInterval)
        }
      }, 800)
    },
    
    updateReverseFlow() {
      // Update feedback data based on source and intensity
      this.reverseStages.forEach((stage, index) => {
        const baseData = [94.5, 67.2, 45.8, 23.1, 12.7, 8.9, 5.2, 2.1][index]
        const adjustedData = baseData * this.feedbackIntensity
        stage.feedbackData = `${adjustedData.toFixed(1)}MB`
        stage.dataVolume = adjustedData / 100
      })
      
      this.updateInsights()
    },
    
    applyReverseOptimization() {
      this.reverseOptimizationActive = !this.reverseOptimizationActive
      
      if (this.reverseOptimizationActive) {
        // Apply 80/20 optimization to reverse flow
        this.reverseStages.forEach(stage => {
          if (stage.critical) {
            stage.optimized = true
          } else {
            stage.optimized = false
            // Reduce data volume for non-critical stages
            stage.dataVolume *= 0.3
          }
        })
        
        this.reverseEfficiency = 92
      } else {
        // Remove optimization
        this.reverseStages.forEach(stage => {
          stage.optimized = false
        })
        this.updateReverseFlow()
        this.reverseEfficiency = 85
      }
    },
    
    toggleFlowDirection() {
      this.isReverseFlow = !this.isReverseFlow
      
      if (!this.isReverseFlow) {
        // Switch to forward flow
        this.reverseStages.reverse()
      } else {
        // Switch back to reverse flow
        this.reverseStages.reverse()
      }
    },
    
    getStageX(index) {
      const spacing = (this.canvasWidth - 100) / (this.reverseStages.length - 1)
      return 50 + (index * spacing)
    },
    
    getStageColor(stage) {
      if (stage.processing) return '#00ff84'
      if (stage.active) return '#0084ff'
      if (stage.optimized) return '#ff8400'
      if (stage.critical) return '#ff4444'
      return '#444'
    },
    
    getStageIcon(stageName) {
      const icons = {
        k8s: '☸️',
        reactor: '⚡',
        ash: '🔥',
        erlang: '🔧',
        bitactor: '⚛️',
        ttl2dspy: '🧠',
        turtle: '🐢',
        typer: '🎯'
      }
      return icons[stageName] || '📦'
    },
    
    getReverseFlowPath(index) {
      const x1 = this.getStageX(index) + this.stageWidth
      const y1 = this.stageY + this.stageHeight / 2
      const x2 = this.getStageX(index + 1)
      const y2 = this.stageY + this.stageHeight / 2
      
      const midX = (x1 + x2) / 2
      return `M ${x1} ${y1} Q ${midX} ${y1 - 20} ${x2} ${y2}`
    },
    
    generateFlowParticles() {
      this.flowParticles = []
      for (let i = 0; i < 20; i++) {
        this.flowParticles.push({
          id: i,
          x: Math.random() * this.canvasWidth,
          y: this.stageY + this.stageHeight / 2,
          size: Math.random() * 4 + 2,
          color: ['#00ff84', '#0084ff', '#ff8400'][Math.floor(Math.random() * 3)],
          speed: Math.random() * 2 + 1,
          opacity: Math.random() * 0.8 + 0.2
        })
      }
    },
    
    animateParticles() {
      if (!this.showParticles) return
      
      this.flowParticles.forEach(particle => {
        particle.x -= particle.speed
        
        if (particle.x < 0) {
          particle.x = this.canvasWidth
          particle.y = this.stageY + this.stageHeight / 2 + (Math.random() - 0.5) * 40
        }
        
        // Pulse opacity
        particle.opacity = 0.5 + Math.sin(Date.now() * 0.01 + particle.id) * 0.3
      })
    },
    
    updateMetrics() {
      // Simulate real-time metrics updates
      this.feedbackCycles = 20 + Math.floor(Math.random() * 10)
      this.dataThroughput = 40 + Math.random() * 20
      
      if (this.reverseOptimizationActive) {
        this.reverseEfficiency = 88 + Math.floor(Math.random() * 8)
        this.criticalFeedbackCount = 4
      } else {
        this.reverseEfficiency = 82 + Math.floor(Math.random() * 6)
        this.criticalFeedbackCount = 6
      }
    },
    
    applyOptimization(optimization) {
      console.log(`Applying optimization: ${optimization.title}`)
      
      if (optimization.id === 'skip-non-critical') {
        this.reverseStages.forEach(stage => {
          if (!stage.critical) {
            stage.optimized = true
            stage.dataVolume *= 0.2 // Drastically reduce non-critical data
          }
        })
      }
      
      // Remove applied optimization from list
      this.reverseOptimizations = this.reverseOptimizations.filter(opt => opt.id !== optimization.id)
      
      // Update efficiency
      this.reverseEfficiency = Math.min(95, this.reverseEfficiency + optimization.impact * 0.3)
    },
    
    updateInsights() {
      // Dynamic insight generation based on current state
      const newInsights = []
      
      if (this.feedbackIntensity > 0.8) {
        newInsights.push({
          id: 'high-intensity',
          icon: '🔥',
          title: 'High Feedback Intensity',
          content: 'Current intensity may overwhelm downstream stages',
          confidence: 90,
          type: 'warning'
        })
      }
      
      if (this.reverseOptimizationActive) {
        newInsights.push({
          id: 'optimization-active',
          icon: '✅',
          title: 'Optimization Active',
          content: '80/20 reverse optimization showing positive results',
          confidence: 95,
          type: 'success'
        })
      }
      
      this.reverseInsights = [...this.reverseInsights.slice(0, 2), ...newInsights]
    }
  }
}
</script>

<style scoped>
.reverse-pipeline-visualizer {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.reverse-pipeline-visualizer h2 {
  margin-bottom: 1rem;
  font-size: 2rem;
}

.flow-description {
  margin-bottom: 2rem;
  font-size: 1.1rem;
  color: #00ff84;
  font-weight: 600;
}

.reverse-control-panel {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.control-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1.5rem;
  margin-top: 1rem;
}

.control-section label {
  display: block;
  margin-bottom: 0.5rem;
  color: #888;
  font-weight: 600;
}

.control-section select,
.control-section input[type="range"] {
  width: 100%;
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.intensity-value {
  margin-left: 0.5rem;
  color: #00ff84;
  font-weight: 600;
}

.optimize-btn, .direction-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  border: none;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.optimize-btn {
  background: #ff8400;
  color: #fff;
}

.optimize-btn.active {
  background: #00ff84;
  color: #000;
}

.direction-btn {
  background: #0084ff;
  color: #fff;
}

.direction-btn.reverse {
  background: #ff4444;
}

.reverse-pipeline-flow {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1rem;
  margin-bottom: 2rem;
  overflow-x: auto;
}

.reverse-flow-svg {
  display: block;
}

.reverse-stage {
  cursor: pointer;
  transition: all 0.3s ease;
}

.reverse-stage:hover .stage-bg {
  filter: brightness(1.2);
}

.stage-bg {
  transition: all 0.3s ease;
}

.stage-icon {
  font-size: 1.5rem;
}

.stage-name {
  font-size: 0.9rem;
  font-weight: 600;
}

.feedback-data {
  font-size: 0.8rem;
  font-weight: 700;
}

.optimization-indicator {
  animation: pulse 2s infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.6; }
}

.reverse-flow-line.animated {
  stroke-dasharray: 10;
  animation: flow 2s linear infinite;
}

@keyframes flow {
  0% { stroke-dashoffset: 0; }
  100% { stroke-dashoffset: -20; }
}

.data-volume {
  transition: all 0.3s ease;
}

.flow-particle {
  transition: all 0.1s ease;
}

.reverse-metrics {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.metrics-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1.5rem;
  margin-top: 1rem;
}

.metric-card {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
  display: flex;
  align-items: center;
  gap: 1rem;
}

.metric-icon {
  font-size: 2rem;
}

.metric-content {
  flex: 1;
}

.metric-label {
  color: #888;
  font-size: 0.9rem;
  margin-bottom: 0.25rem;
}

.metric-value {
  font-size: 1.8rem;
  font-weight: 700;
  color: #00ff84;
  margin-bottom: 0.25rem;
}

.metric-detail {
  color: #666;
  font-size: 0.8rem;
}

.reverse-optimization {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.optimization-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin-top: 1rem;
}

.optimization-item {
  background: #2a2a2a;
  border-radius: 8px;
  padding: 1rem;
  display: flex;
  align-items: center;
  gap: 1rem;
  border-left: 4px solid #666;
}

.optimization-item.high {
  border-left-color: #ff4444;
}

.optimization-item.medium {
  border-left-color: #ff8400;
}

.optimization-icon {
  font-size: 1.5rem;
}

.optimization-content {
  flex: 1;
}

.optimization-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.optimization-description {
  color: #aaa;
  font-size: 0.9rem;
  margin-bottom: 0.25rem;
}

.optimization-impact {
  color: #00ff84;
  font-size: 0.8rem;
  font-weight: 600;
}

.apply-optimization {
  padding: 0.5rem 1rem;
  background: #0084ff;
  color: #fff;
  border: none;
  border-radius: 6px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-optimization:hover {
  background: #0066cc;
}

.reverse-insights {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.insights-container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1rem;
  margin-top: 1rem;
}

.insight-card {
  background: #2a2a2a;
  border-radius: 8px;
  padding: 1rem;
  border-left: 4px solid #666;
}

.insight-card.warning {
  border-left-color: #ff8400;
}

.insight-card.optimization {
  border-left-color: #00ff84;
}

.insight-card.pattern {
  border-left-color: #0084ff;
}

.insight-card.success {
  border-left-color: #00ff84;
}

.insight-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.insight-icon {
  font-size: 1.2rem;
}

.insight-title {
  font-weight: 600;
}

.insight-content {
  color: #aaa;
  font-size: 0.9rem;
  margin-bottom: 0.5rem;
}

.insight-confidence {
  color: #00ff84;
  font-size: 0.8rem;
  font-weight: 600;
}
</style>