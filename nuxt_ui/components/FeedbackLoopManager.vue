<template>
  <div class="feedback-loop-manager">
    <h2>🔄 Feedback Loop Management System</h2>
    <p class="system-description">
      Intelligent management of reverse feedback loops with 80/20 optimization
    </p>
    
    <!-- Feedback Loop Control Center -->
    <div class="loop-control-center">
      <h3>🎛️ Feedback Loop Control Center</h3>
      <div class="control-panels">
        <div class="control-panel">
          <h4>Loop Configuration</h4>
          <div class="config-options">
            <div class="config-item">
              <label>Loop Type</label>
              <select v-model="activeLoopType" @change="updateLoopConfiguration">
                <option value="continuous">Continuous Feedback</option>
                <option value="batch">Batch Processing</option>
                <option value="adaptive">Adaptive Loops</option>
                <option value="critical_only">Critical Only (80/20)</option>
              </select>
            </div>
            
            <div class="config-item">
              <label>Loop Frequency</label>
              <input 
                type="range" 
                v-model="loopFrequency" 
                min="1" 
                max="100" 
                @input="updateLoopConfiguration"
              />
              <span class="frequency-value">{{ loopFrequency }} Hz</span>
            </div>
            
            <div class="config-item">
              <label>Buffer Size</label>
              <input 
                type="number" 
                v-model="bufferSize" 
                min="10" 
                max="1000" 
                @change="updateLoopConfiguration"
              />
              <span class="buffer-unit">MB</span>
            </div>
            
            <div class="config-item">
              <label>80/20 Threshold</label>
              <input 
                type="range" 
                v-model="paretoThreshold" 
                min="0.5" 
                max="0.95" 
                step="0.05"
                @input="updateLoopConfiguration"
              />
              <span class="threshold-value">{{ (paretoThreshold * 100).toFixed(0) }}%</span>
            </div>
          </div>
        </div>
        
        <div class="control-panel">
          <h4>Loop Status</h4>
          <div class="status-indicators">
            <div class="status-item" :class="{ active: loopStatus.running }">
              <div class="status-icon">{{ loopStatus.running ? '🟢' : '🔴' }}</div>
              <div class="status-text">
                <div class="status-label">System Status</div>
                <div class="status-value">{{ loopStatus.running ? 'Running' : 'Stopped' }}</div>
              </div>
            </div>
            
            <div class="status-item">
              <div class="status-icon">🔄</div>
              <div class="status-text">
                <div class="status-label">Active Loops</div>
                <div class="status-value">{{ activeLoops.length }}</div>
              </div>
            </div>
            
            <div class="status-item">
              <div class="status-icon">⚡</div>
              <div class="status-text">
                <div class="status-label">Optimization</div>
                <div class="status-value">{{ currentOptimization }}%</div>
              </div>
            </div>
            
            <div class="status-item">
              <div class="status-icon">📊</div>
              <div class="status-text">
                <div class="status-label">Throughput</div>
                <div class="status-value">{{ currentThroughput }} MB/s</div>
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <div class="loop-actions">
        <button @click="startLoopSystem" :disabled="loopStatus.running" class="action-btn start">
          ▶️ Start Loops
        </button>
        <button @click="stopLoopSystem" :disabled="!loopStatus.running" class="action-btn stop">
          ⏹️ Stop Loops
        </button>
        <button @click="optimizeLoops" class="action-btn optimize">
          ⚡ 80/20 Optimize
        </button>
        <button @click="resetLoops" class="action-btn reset">
          🔄 Reset
        </button>
      </div>
    </div>
    
    <!-- Active Feedback Loops Visualization -->
    <div class="active-loops-visualization">
      <h3>🌊 Active Feedback Loops</h3>
      <div class="loops-container">
        <div
          v-for="loop in activeLoops"
          :key="loop.id"
          class="feedback-loop"
          :class="{ 
            optimized: loop.optimized, 
            critical: loop.critical,
            active: loop.active,
            bottleneck: loop.bottleneck
          }"
        >
          <div class="loop-header">
            <div class="loop-title">
              <span class="loop-icon">{{ loop.icon }}</span>
              <span class="loop-name">{{ loop.name }}</span>
            </div>
            <div class="loop-metrics">
              <span class="efficiency">{{ loop.efficiency }}%</span>
              <span class="latency">{{ loop.latency }}ms</span>
            </div>
          </div>
          
          <div class="loop-flow">
            <div class="flow-stages">
              <div
                v-for="stage in loop.stages"
                :key="stage"
                class="flow-stage"
                :class="{ active: loop.currentStage === stage }"
              >
                {{ getStageIcon(stage) }}
              </div>
            </div>
            <div class="flow-direction">
              <div class="direction-arrow" :class="{ reverse: loop.isReverse }">
                {{ loop.isReverse ? '⬅️' : '➡️' }}
              </div>
            </div>
          </div>
          
          <div class="loop-stats">
            <div class="stat-item">
              <span class="stat-label">Data Volume</span>
              <div class="stat-bar">
                <div 
                  class="stat-fill" 
                  :style="{ width: loop.dataVolume + '%' }"
                ></div>
              </div>
              <span class="stat-value">{{ loop.dataVolume }}%</span>
            </div>
            
            <div class="stat-item">
              <span class="stat-label">Processing Speed</span>
              <div class="stat-bar">
                <div 
                  class="stat-fill speed" 
                  :style="{ width: loop.processingSpeed + '%' }"
                ></div>
              </div>
              <span class="stat-value">{{ loop.processingSpeed }}%</span>
            </div>
          </div>
          
          <div class="loop-controls">
            <button @click="toggleLoop(loop)" class="loop-btn toggle">
              {{ loop.active ? '⏸️' : '▶️' }}
            </button>
            <button @click="optimizeLoop(loop)" class="loop-btn optimize">
              ⚡
            </button>
            <button @click="analyzeLoop(loop)" class="loop-btn analyze">
              🔍
            </button>
            <button @click="removeLoop(loop)" class="loop-btn remove">
              🗑️
            </button>
          </div>
        </div>
      </div>
      
      <div class="add-loop-section">
        <button @click="createNewLoop" class="create-loop-btn">
          ➕ Create New Feedback Loop
        </button>
      </div>
    </div>
    
    <!-- Loop Performance Analytics -->
    <div class="loop-analytics">
      <h3>📈 Loop Performance Analytics</h3>
      <div class="analytics-grid">
        <div class="analytics-card">
          <h4>🎯 80/20 Analysis</h4>
          <div class="pareto-chart">
            <div class="chart-container">
              <canvas ref="paretoChart" width="300" height="200"></canvas>
            </div>
            <div class="pareto-summary">
              <div class="summary-item">
                <span class="summary-label">Critical Loops (20%)</span>
                <span class="summary-value">{{ criticalLoopsCount }}</span>
              </div>
              <div class="summary-item">
                <span class="summary-label">Performance Impact (80%)</span>
                <span class="summary-value">{{ paretoImpact }}%</span>
              </div>
            </div>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>🔄 Loop Efficiency Trends</h4>
          <div class="efficiency-chart">
            <canvas ref="efficiencyChart" width="300" height="200"></canvas>
          </div>
          <div class="trend-summary">
            <div class="trend-item">
              <span class="trend-icon">📈</span>
              <span class="trend-text">+{{ efficiencyTrend }}% this hour</span>
            </div>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>🚧 Bottleneck Detection</h4>
          <div class="bottleneck-list">
            <div
              v-for="bottleneck in detectedBottlenecks"
              :key="bottleneck.id"
              class="bottleneck-item"
              :class="bottleneck.severity"
            >
              <div class="bottleneck-icon">{{ bottleneck.icon }}</div>
              <div class="bottleneck-content">
                <div class="bottleneck-stage">{{ bottleneck.stage }}</div>
                <div class="bottleneck-description">{{ bottleneck.description }}</div>
                <div class="bottleneck-impact">Impact: {{ bottleneck.impact }}%</div>
              </div>
              <button @click="resolveBottleneck(bottleneck)" class="resolve-btn">
                🔧 Fix
              </button>
            </div>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>🧠 AI Recommendations</h4>
          <div class="recommendations-list">
            <div
              v-for="recommendation in aiRecommendations"
              :key="recommendation.id"
              class="recommendation-item"
              :class="recommendation.priority"
            >
              <div class="recommendation-icon">{{ recommendation.icon }}</div>
              <div class="recommendation-content">
                <div class="recommendation-title">{{ recommendation.title }}</div>
                <div class="recommendation-description">{{ recommendation.description }}</div>
                <div class="recommendation-benefit">Benefit: +{{ recommendation.benefit }}%</div>
              </div>
              <button @click="applyRecommendation(recommendation)" class="apply-btn">
                ✅ Apply
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Loop Creation Modal -->
    <div v-if="showCreateModal" class="create-modal-overlay" @click="closeCreateModal">
      <div class="create-modal" @click.stop>
        <h3>➕ Create New Feedback Loop</h3>
        <div class="modal-content">
          <div class="form-group">
            <label>Loop Name</label>
            <input v-model="newLoop.name" placeholder="Enter loop name" />
          </div>
          
          <div class="form-group">
            <label>Loop Type</label>
            <select v-model="newLoop.type">
              <option value="continuous">Continuous</option>
              <option value="batch">Batch</option>
              <option value="adaptive">Adaptive</option>
              <option value="critical">Critical Only</option>
            </select>
          </div>
          
          <div class="form-group">
            <label>Stages (select stages for this loop)</label>
            <div class="stage-selector">
              <div
                v-for="stage in availableStages"
                :key="stage"
                class="stage-option"
                :class="{ selected: newLoop.stages.includes(stage) }"
                @click="toggleStageSelection(stage)"
              >
                {{ getStageIcon(stage) }} {{ stage }}
              </div>
            </div>
          </div>
          
          <div class="form-group">
            <label>Direction</label>
            <div class="direction-selector">
              <button 
                @click="newLoop.direction = 'forward'" 
                :class="{ active: newLoop.direction === 'forward' }"
                class="direction-option"
              >
                ➡️ Forward
              </button>
              <button 
                @click="newLoop.direction = 'reverse'" 
                :class="{ active: newLoop.direction === 'reverse' }"
                class="direction-option"
              >
                ⬅️ Reverse
              </button>
            </div>
          </div>
          
          <div class="form-actions">
            <button @click="closeCreateModal" class="modal-btn cancel">
              Cancel
            </button>
            <button @click="createLoop" class="modal-btn create">
              Create Loop
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'FeedbackLoopManager',
  
  data() {
    return {
      activeLoopType: 'adaptive',
      loopFrequency: 25,
      bufferSize: 256,
      paretoThreshold: 0.8,
      
      loopStatus: {
        running: false,
        totalLoops: 0,
        activeLoops: 0
      },
      
      currentOptimization: 85,
      currentThroughput: 34.7,
      
      activeLoops: [
        {
          id: 'loop-1',
          name: 'K8s → Reactor Critical',
          icon: '🔥',
          stages: ['k8s', 'reactor', 'ash'],
          currentStage: 'reactor',
          isReverse: true,
          active: true,
          optimized: true,
          critical: true,
          bottleneck: false,
          efficiency: 92,
          latency: 45,
          dataVolume: 85,
          processingSpeed: 78
        },
        {
          id: 'loop-2',
          name: 'Full Reverse Pipeline',
          icon: '🌊',
          stages: ['k8s', 'reactor', 'ash', 'erlang', 'bitactor', 'ttl2dspy', 'turtle', 'typer'],
          currentStage: 'ash',
          isReverse: true,
          active: true,
          optimized: false,
          critical: false,
          bottleneck: true,
          efficiency: 67,
          latency: 120,
          dataVolume: 95,
          processingSpeed: 45
        },
        {
          id: 'loop-3',
          name: 'Adaptive Learning Loop',
          icon: '🧠',
          stages: ['ash', 'reactor', 'k8s'],
          currentStage: 'k8s',
          isReverse: false,
          active: true,
          optimized: true,
          critical: true,
          bottleneck: false,
          efficiency: 88,
          latency: 30,
          dataVolume: 42,
          processingSpeed: 92
        }
      ],
      
      availableStages: ['k8s', 'reactor', 'ash', 'erlang', 'bitactor', 'ttl2dspy', 'turtle', 'typer'],
      
      criticalLoopsCount: 2,
      paretoImpact: 84,
      efficiencyTrend: 12,
      
      detectedBottlenecks: [
        {
          id: 'bottleneck-1',
          stage: 'erlang',
          icon: '🚧',
          description: 'High memory usage causing delays',
          impact: 35,
          severity: 'high'
        },
        {
          id: 'bottleneck-2',
          stage: 'bitactor',
          icon: '⚠️',
          description: 'CPU utilization at 95%',
          impact: 22,
          severity: 'medium'
        }
      ],
      
      aiRecommendations: [
        {
          id: 'rec-1',
          icon: '✂️',
          title: 'Skip Non-Critical Stages',
          description: 'Remove erlang and bitactor from reverse loops for 60% improvement',
          benefit: 60,
          priority: 'high'
        },
        {
          id: 'rec-2',
          icon: '📦',
          title: 'Implement Batch Processing',
          description: 'Group feedback data to reduce loop overhead',
          benefit: 25,
          priority: 'medium'
        },
        {
          id: 'rec-3',
          icon: '🎯',
          title: 'Optimize Buffer Size',
          description: 'Increase buffer to 512MB for better throughput',
          benefit: 18,
          priority: 'low'
        }
      ],
      
      showCreateModal: false,
      newLoop: {
        name: '',
        type: 'adaptive',
        stages: [],
        direction: 'reverse'
      }
    }
  },
  
  mounted() {
    this.drawCharts()
    this.startMetricsUpdate()
  },
  
  methods: {
    updateLoopConfiguration() {
      console.log('Updating loop configuration:', {
        type: this.activeLoopType,
        frequency: this.loopFrequency,
        buffer: this.bufferSize,
        threshold: this.paretoThreshold
      })
      
      // Apply 80/20 optimization if threshold is high
      if (this.paretoThreshold > 0.8) {
        this.optimizeLoops()
      }
    },
    
    startLoopSystem() {
      this.loopStatus.running = true
      this.loopStatus.activeLoops = this.activeLoops.length
      
      // Start loop simulation
      this.simulateLoopExecution()
    },
    
    stopLoopSystem() {
      this.loopStatus.running = false
      this.activeLoops.forEach(loop => {
        loop.active = false
      })
    },
    
    optimizeLoops() {
      // Apply 80/20 optimization to all loops
      this.activeLoops.forEach(loop => {
        if (loop.critical || loop.efficiency < 70) {
          loop.optimized = true
          loop.efficiency = Math.min(95, loop.efficiency + 15)
          loop.latency = Math.max(20, loop.latency * 0.7)
          
          // Remove non-critical stages if 80/20 optimization
          if (this.paretoThreshold > 0.8) {
            const criticalStages = ['k8s', 'reactor', 'ash', 'typer']
            loop.stages = loop.stages.filter(stage => criticalStages.includes(stage))
          }
        }
      })
      
      this.currentOptimization = Math.min(95, this.currentOptimization + 10)
    },
    
    resetLoops() {
      this.stopLoopSystem()
      this.activeLoops.forEach(loop => {
        loop.optimized = false
        loop.efficiency = 60 + Math.random() * 30
        loop.latency = 50 + Math.random() * 100
      })
      this.currentOptimization = 75
    },
    
    simulateLoopExecution() {
      if (!this.loopStatus.running) return
      
      this.activeLoops.forEach(loop => {
        if (loop.active) {
          // Advance current stage
          const currentIndex = loop.stages.indexOf(loop.currentStage)
          const nextIndex = (currentIndex + 1) % loop.stages.length
          loop.currentStage = loop.stages[nextIndex]
          
          // Update metrics
          loop.dataVolume = Math.max(20, loop.dataVolume + (Math.random() - 0.5) * 10)
          loop.processingSpeed = Math.max(30, loop.processingSpeed + (Math.random() - 0.5) * 5)
          
          if (loop.optimized) {
            loop.efficiency = Math.min(95, loop.efficiency + Math.random() * 2)
          }
        }
      })
      
      setTimeout(() => this.simulateLoopExecution(), 2000)
    },
    
    toggleLoop(loop) {
      loop.active = !loop.active
      this.loopStatus.activeLoops = this.activeLoops.filter(l => l.active).length
    },
    
    optimizeLoop(loop) {
      loop.optimized = !loop.optimized
      
      if (loop.optimized) {
        loop.efficiency = Math.min(95, loop.efficiency + 20)
        loop.latency = Math.max(20, loop.latency * 0.6)
      } else {
        loop.efficiency = Math.max(40, loop.efficiency - 15)
        loop.latency = loop.latency * 1.5
      }
    },
    
    analyzeLoop(loop) {
      console.log('Analyzing loop:', loop.name)
      // In real implementation, would open detailed analysis view
    },
    
    removeLoop(loop) {
      this.activeLoops = this.activeLoops.filter(l => l.id !== loop.id)
      this.loopStatus.activeLoops = this.activeLoops.length
    },
    
    createNewLoop() {
      this.showCreateModal = true
      this.newLoop = {
        name: '',
        type: 'adaptive',
        stages: [],
        direction: 'reverse'
      }
    },
    
    closeCreateModal() {
      this.showCreateModal = false
    },
    
    toggleStageSelection(stage) {
      if (this.newLoop.stages.includes(stage)) {
        this.newLoop.stages = this.newLoop.stages.filter(s => s !== stage)
      } else {
        this.newLoop.stages.push(stage)
      }
    },
    
    createLoop() {
      if (this.newLoop.name && this.newLoop.stages.length > 0) {
        const newLoop = {
          id: `loop-${Date.now()}`,
          name: this.newLoop.name,
          icon: this.getLoopIcon(this.newLoop.type),
          stages: [...this.newLoop.stages],
          currentStage: this.newLoop.stages[0],
          isReverse: this.newLoop.direction === 'reverse',
          active: false,
          optimized: false,
          critical: this.newLoop.type === 'critical',
          bottleneck: false,
          efficiency: 60 + Math.random() * 20,
          latency: 40 + Math.random() * 60,
          dataVolume: 30 + Math.random() * 50,
          processingSpeed: 50 + Math.random() * 30
        }
        
        this.activeLoops.push(newLoop)
        this.closeCreateModal()
      }
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
    
    getLoopIcon(type) {
      const icons = {
        continuous: '🔄',
        batch: '📦',
        adaptive: '🧠',
        critical: '🔥'
      }
      return icons[type] || '🔄'
    },
    
    resolveBottleneck(bottleneck) {
      console.log('Resolving bottleneck:', bottleneck.stage)
      this.detectedBottlenecks = this.detectedBottlenecks.filter(b => b.id !== bottleneck.id)
      
      // Apply optimization to affected loops
      this.activeLoops.forEach(loop => {
        if (loop.stages.includes(bottleneck.stage)) {
          loop.bottleneck = false
          loop.efficiency += bottleneck.impact * 0.5
          loop.latency = Math.max(20, loop.latency - bottleneck.impact)
        }
      })
    },
    
    applyRecommendation(recommendation) {
      console.log('Applying recommendation:', recommendation.title)
      
      if (recommendation.id === 'rec-1') {
        // Skip non-critical stages
        this.activeLoops.forEach(loop => {
          if (!loop.critical) {
            const criticalStages = ['k8s', 'reactor', 'ash', 'typer']
            loop.stages = loop.stages.filter(stage => criticalStages.includes(stage))
            loop.optimized = true
            loop.efficiency += recommendation.benefit * 0.5
          }
        })
      }
      
      this.aiRecommendations = this.aiRecommendations.filter(r => r.id !== recommendation.id)
      this.currentOptimization += recommendation.benefit * 0.3
    },
    
    drawCharts() {
      this.$nextTick(() => {
        this.drawParetoChart()
        this.drawEfficiencyChart()
      })
    },
    
    drawParetoChart() {
      const canvas = this.$refs.paretoChart
      if (!canvas) return
      
      const ctx = canvas.getContext('2d')
      const width = canvas.width
      const height = canvas.height
      
      // Clear canvas
      ctx.fillStyle = '#2a2a2a'
      ctx.fillRect(0, 0, width, height)
      
      // Draw Pareto bars
      const data = [85, 67, 42, 28, 15, 8, 5, 3]
      const colors = ['#ff4444', '#ff4444', '#ff8800', '#ff8800', '#666', '#666', '#666', '#666']
      
      const barWidth = width / data.length
      
      data.forEach((value, index) => {
        const barHeight = (value / 100) * height * 0.8
        const x = index * barWidth
        const y = height - barHeight
        
        ctx.fillStyle = colors[index]
        ctx.fillRect(x + 5, y, barWidth - 10, barHeight)
      })
      
      // Draw 80% line
      ctx.strokeStyle = '#ff8800'
      ctx.lineWidth = 2
      ctx.setLineDash([5, 5])
      const eightyPercentY = height * 0.2
      ctx.beginPath()
      ctx.moveTo(0, eightyPercentY)
      ctx.lineTo(width, eightyPercentY)
      ctx.stroke()
    },
    
    drawEfficiencyChart() {
      const canvas = this.$refs.efficiencyChart
      if (!canvas) return
      
      const ctx = canvas.getContext('2d')
      const width = canvas.width
      const height = canvas.height
      
      // Clear canvas
      ctx.fillStyle = '#2a2a2a'
      ctx.fillRect(0, 0, width, height)
      
      // Draw efficiency trend line
      const data = [65, 68, 72, 75, 78, 82, 85, 88, 85, 87]
      const pointSpacing = width / (data.length - 1)
      
      ctx.strokeStyle = '#00ff84'
      ctx.lineWidth = 3
      ctx.setLineDash([])
      
      ctx.beginPath()
      data.forEach((value, index) => {
        const x = index * pointSpacing
        const y = height - (value / 100) * height
        
        if (index === 0) {
          ctx.moveTo(x, y)
        } else {
          ctx.lineTo(x, y)
        }
      })
      ctx.stroke()
      
      // Draw data points
      ctx.fillStyle = '#00ff84'
      data.forEach((value, index) => {
        const x = index * pointSpacing
        const y = height - (value / 100) * height
        
        ctx.beginPath()
        ctx.arc(x, y, 4, 0, 2 * Math.PI)
        ctx.fill()
      })
    },
    
    startMetricsUpdate() {
      setInterval(() => {
        this.currentThroughput = 30 + Math.random() * 20
        this.efficiencyTrend = 8 + Math.random() * 8
        
        if (this.loopStatus.running) {
          this.currentOptimization = Math.min(95, this.currentOptimization + (Math.random() - 0.5) * 2)
        }
      }, 3000)
    }
  }
}
</script>

<style scoped>
.feedback-loop-manager {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.feedback-loop-manager h2 {
  margin-bottom: 1rem;
  font-size: 2rem;
}

.system-description {
  margin-bottom: 2rem;
  font-size: 1.1rem;
  color: #00ff84;
  font-weight: 600;
}

.loop-control-center {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.control-panels {
  display: grid;
  grid-template-columns: 2fr 1fr;
  gap: 2rem;
  margin-bottom: 2rem;
}

.control-panel {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.control-panel h4 {
  margin-bottom: 1rem;
  color: #888;
}

.config-options {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
}

.config-item {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.config-item label {
  font-size: 0.9rem;
  color: #aaa;
  font-weight: 600;
}

.config-item select,
.config-item input {
  padding: 0.5rem;
  background: #3a3a3a;
  border: 1px solid #555;
  border-radius: 6px;
  color: #e0e0e0;
}

.frequency-value,
.threshold-value,
.buffer-unit {
  font-size: 0.8rem;
  color: #00ff84;
  font-weight: 600;
}

.status-indicators {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.status-item {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.status-icon {
  font-size: 1.5rem;
}

.status-text {
  flex: 1;
}

.status-label {
  font-size: 0.8rem;
  color: #888;
}

.status-value {
  font-weight: 600;
  color: #00ff84;
}

.status-item.active .status-value {
  color: #00ff84;
}

.loop-actions {
  display: flex;
  gap: 1rem;
  flex-wrap: wrap;
}

.action-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  border: none;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.action-btn.start {
  background: #00ff84;
  color: #000;
}

.action-btn.stop {
  background: #ff4444;
  color: #fff;
}

.action-btn.optimize {
  background: #ff8800;
  color: #fff;
}

.action-btn.reset {
  background: #666;
  color: #fff;
}

.action-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.active-loops-visualization {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.loops-container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 1.5rem;
  margin-bottom: 1.5rem;
}

.feedback-loop {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
  border-left: 4px solid #666;
  transition: all 0.3s ease;
}

.feedback-loop.critical {
  border-left-color: #ff4444;
}

.feedback-loop.optimized {
  border-left-color: #00ff84;
}

.feedback-loop.bottleneck {
  border-left-color: #ff8800;
}

.loop-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.loop-title {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.loop-icon {
  font-size: 1.5rem;
}

.loop-name {
  font-weight: 600;
  font-size: 1.1rem;
}

.loop-metrics {
  display: flex;
  gap: 1rem;
}

.efficiency,
.latency {
  font-size: 0.9rem;
  font-weight: 600;
}

.efficiency {
  color: #00ff84;
}

.latency {
  color: #ff8800;
}

.loop-flow {
  margin-bottom: 1rem;
}

.flow-stages {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.flow-stage {
  padding: 0.25rem 0.5rem;
  background: #3a3a3a;
  border-radius: 4px;
  font-size: 0.8rem;
  transition: all 0.3s ease;
}

.flow-stage.active {
  background: #00ff84;
  color: #000;
}

.flow-direction {
  display: flex;
  justify-content: center;
}

.direction-arrow {
  font-size: 1.2rem;
  padding: 0.25rem;
  border-radius: 4px;
  background: #3a3a3a;
}

.direction-arrow.reverse {
  background: #ff4444;
}

.loop-stats {
  margin-bottom: 1rem;
}

.stat-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.stat-label {
  font-size: 0.8rem;
  color: #888;
  min-width: 100px;
}

.stat-bar {
  flex: 1;
  height: 8px;
  background: #3a3a3a;
  border-radius: 4px;
  overflow: hidden;
}

.stat-fill {
  height: 100%;
  background: #00ff84;
  transition: all 0.3s ease;
}

.stat-fill.speed {
  background: #0084ff;
}

.stat-value {
  font-size: 0.8rem;
  color: #e0e0e0;
  min-width: 40px;
  text-align: right;
}

.loop-controls {
  display: flex;
  gap: 0.5rem;
}

.loop-btn {
  padding: 0.5rem;
  border-radius: 6px;
  border: none;
  background: #3a3a3a;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.loop-btn:hover {
  background: #4a4a4a;
}

.loop-btn.remove {
  background: #ff4444;
}

.create-loop-btn {
  padding: 1rem 2rem;
  background: #0084ff;
  color: #fff;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.create-loop-btn:hover {
  background: #0066cc;
  transform: translateY(-2px);
}

.loop-analytics {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.analytics-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
  margin-top: 1rem;
}

.analytics-card {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.analytics-card h4 {
  margin-bottom: 1rem;
  color: #888;
}

.chart-container {
  margin-bottom: 1rem;
}

.pareto-summary,
.trend-summary {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.summary-item,
.trend-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.summary-label {
  color: #aaa;
  font-size: 0.9rem;
}

.summary-value {
  color: #00ff84;
  font-weight: 600;
}

.trend-icon {
  font-size: 1.2rem;
}

.trend-text {
  color: #00ff84;
  font-weight: 600;
}

.bottleneck-list,
.recommendations-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.bottleneck-item,
.recommendation-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #3a3a3a;
  border-radius: 8px;
  border-left: 4px solid #666;
}

.bottleneck-item.high {
  border-left-color: #ff4444;
}

.bottleneck-item.medium {
  border-left-color: #ff8800;
}

.recommendation-item.high {
  border-left-color: #ff4444;
}

.recommendation-item.medium {
  border-left-color: #ff8800;
}

.recommendation-item.low {
  border-left-color: #666;
}

.bottleneck-icon,
.recommendation-icon {
  font-size: 1.5rem;
}

.bottleneck-content,
.recommendation-content {
  flex: 1;
}

.bottleneck-stage,
.recommendation-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.bottleneck-description,
.recommendation-description {
  color: #aaa;
  font-size: 0.9rem;
  margin-bottom: 0.25rem;
}

.bottleneck-impact,
.recommendation-benefit {
  color: #ff8800;
  font-size: 0.8rem;
  font-weight: 600;
}

.resolve-btn,
.apply-btn {
  padding: 0.5rem 1rem;
  border-radius: 6px;
  border: none;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.resolve-btn {
  background: #ff8800;
  color: #fff;
}

.apply-btn {
  background: #00ff84;
  color: #000;
}

.create-modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.8);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.create-modal {
  background: #2a2a2a;
  border-radius: 16px;
  padding: 2rem;
  max-width: 500px;
  width: 90%;
  max-height: 80vh;
  overflow-y: auto;
}

.create-modal h3 {
  margin-bottom: 1.5rem;
  color: #e0e0e0;
}

.modal-content {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.form-group label {
  color: #888;
  font-weight: 600;
}

.form-group input,
.form-group select {
  padding: 0.75rem;
  background: #3a3a3a;
  border: 1px solid #555;
  border-radius: 6px;
  color: #e0e0e0;
}

.stage-selector {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
  gap: 0.5rem;
}

.stage-option {
  padding: 0.5rem;
  background: #3a3a3a;
  border-radius: 6px;
  text-align: center;
  cursor: pointer;
  transition: all 0.3s ease;
  border: 2px solid transparent;
}

.stage-option:hover {
  background: #4a4a4a;
}

.stage-option.selected {
  background: #00ff84;
  color: #000;
  border-color: #00ff84;
}

.direction-selector {
  display: flex;
  gap: 0.5rem;
}

.direction-option {
  flex: 1;
  padding: 0.75rem;
  background: #3a3a3a;
  border: none;
  border-radius: 6px;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.direction-option.active {
  background: #0084ff;
  color: #fff;
}

.form-actions {
  display: flex;
  gap: 1rem;
  justify-content: flex-end;
}

.modal-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 6px;
  border: none;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.modal-btn.cancel {
  background: #666;
  color: #fff;
}

.modal-btn.create {
  background: #00ff84;
  color: #000;
}

.modal-btn:hover {
  transform: translateY(-1px);
}
</style>