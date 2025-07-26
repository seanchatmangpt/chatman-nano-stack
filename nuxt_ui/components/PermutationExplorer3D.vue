<template>
  <div class="permutation-explorer-3d">
    <h2>🌌 3D Permutation Explorer</h2>
    
    <!-- Controls -->
    <div class="controls-panel">
      <div class="control-group">
        <label>View Mode</label>
        <select v-model="viewMode" @change="updateView">
          <option value="galaxy">Galaxy View</option>
          <option value="flow">Flow Network</option>
          <option value="performance">Performance Space</option>
          <option value="optimization">80/20 Optimization</option>
        </select>
      </div>
      
      <div class="control-group">
        <label>Complexity</label>
        <input 
          type="range" 
          v-model.number="complexity" 
          min="0" 
          max="1" 
          step="0.1"
          @input="updatePermutations"
        >
        <span>{{ complexity }}</span>
      </div>
      
      <div class="control-group">
        <label>Animation Speed</label>
        <input 
          type="range" 
          v-model.number="animationSpeed" 
          min="0.1" 
          max="2" 
          step="0.1"
        >
        <span>{{ animationSpeed }}x</span>
      </div>
      
      <div class="control-group">
        <button @click="rotateView" class="control-btn">
          🔄 Auto Rotate
        </button>
        <button @click="resetCamera" class="control-btn">
          📷 Reset Camera
        </button>
        <button @click="exploreSwarm" class="control-btn">
          🧠 Swarm Explore
        </button>
      </div>
    </div>
    
    <!-- 3D Canvas -->
    <div class="canvas-3d-container">
      <canvas 
        ref="canvas3d" 
        :width="canvasWidth" 
        :height="canvasHeight"
        @mousedown="startMouseDrag"
        @mousemove="handleMouseMove"
        @mouseup="endMouseDrag"
        @wheel="handleWheel"
        @click="handleCanvasClick"
      ></canvas>
      
      <!-- Overlay Info -->
      <div class="canvas-overlay">
        <div class="view-info">
          <span>{{ viewMode.toUpperCase() }} VIEW</span>
          <span>Permutations: {{ visiblePermutations.length }}</span>
          <span>Camera: {{ Math.round(camera.distance) }}°</span>
        </div>
        
        <div class="selected-info" v-if="selectedPermutation">
          <h4>{{ selectedPermutation.name }}</h4>
          <p>Efficiency: {{ (selectedPermutation.efficiency * 100).toFixed(1) }}%</p>
          <p>Stages: {{ selectedPermutation.stages.length }}</p>
          <p>Duration: {{ selectedPermutation.duration }}ms</p>
        </div>
      </div>
    </div>
    
    <!-- Permutation Details -->
    <div class="permutation-details">
      <div class="detail-tabs">
        <button 
          v-for="tab in detailTabs" 
          :key="tab.id"
          @click="activeDetailTab = tab.id"
          :class="['tab-btn', { active: activeDetailTab === tab.id }]"
        >
          {{ tab.icon }} {{ tab.name }}
        </button>
      </div>
      
      <div class="detail-content">
        <!-- Strategy Details -->
        <div v-if="activeDetailTab === 'strategies'" class="detail-panel">
          <h3>Available Strategies</h3>
          <div class="strategy-grid-3d">
            <div
              v-for="strategy in strategies3D"
              :key="strategy.id"
              class="strategy-card-3d"
              :class="{ active: strategy.id === selectedStrategy }"
              @click="selectStrategy3D(strategy)"
            >
              <div class="strategy-header">
                <span class="strategy-icon">{{ strategy.icon }}</span>
                <span class="strategy-name">{{ strategy.name }}</span>
              </div>
              <div class="strategy-metrics">
                <div class="metric">
                  <span class="label">Efficiency</span>
                  <span class="value">{{ (strategy.efficiency * 100).toFixed(0) }}%</span>
                </div>
                <div class="metric">
                  <span class="label">Complexity</span>
                  <span class="value">{{ strategy.complexity }}</span>
                </div>
                <div class="metric">
                  <span class="label">Position</span>
                  <span class="value">{{ formatPosition(strategy.position) }}</span>
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Performance Analysis -->
        <div v-if="activeDetailTab === 'performance'" class="detail-panel">
          <h3>3D Performance Analysis</h3>
          <div class="performance-3d">
            <div class="axis-info">
              <div class="axis">
                <span class="axis-label">X-Axis</span>
                <span class="axis-desc">Efficiency (0-100%)</span>
              </div>
              <div class="axis">
                <span class="axis-label">Y-Axis</span>
                <span class="axis-desc">Complexity (0-1)</span>
              </div>
              <div class="axis">
                <span class="axis-label">Z-Axis</span>
                <span class="axis-desc">Duration (ms)</span>
              </div>
            </div>
            
            <div class="performance-stats">
              <div class="stat">
                <span class="stat-label">Best Efficiency</span>
                <span class="stat-value">{{ bestEfficiency }}%</span>
              </div>
              <div class="stat">
                <span class="stat-label">Optimal Zone</span>
                <span class="stat-value">{{ optimalZoneCount }} strategies</span>
              </div>
              <div class="stat">
                <span class="stat-label">80/20 Candidates</span>
                <span class="stat-value">{{ eightyTwentyCandidates }}</span>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Path Exploration -->
        <div v-if="activeDetailTab === 'paths'" class="detail-panel">
          <h3>Execution Paths</h3>
          <div class="path-explorer">
            <div
              v-for="path in explorationPaths"
              :key="path.id"
              class="path-item"
              @click="animateToPath(path)"
            >
              <div class="path-header">
                <span class="path-name">{{ path.name }}</span>
                <span class="path-type">{{ path.type }}</span>
              </div>
              <div class="path-stages">
                <span
                  v-for="stage in path.stages"
                  :key="stage"
                  class="stage-badge"
                >
                  {{ getStageIcon(stage) }}
                </span>
              </div>
              <div class="path-metrics">
                <span>{{ path.duration }}ms</span>
                <span>{{ path.efficiency }}% efficient</span>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Swarm Intelligence -->
        <div v-if="activeDetailTab === 'swarm'" class="detail-panel">
          <h3>Swarm Intelligence Insights</h3>
          <div class="swarm-insights">
            <div class="insight-card">
              <h4>🎯 Optimal Strategy</h4>
              <p>{{ swarmInsights.optimalStrategy }}</p>
              <p class="insight-reason">{{ swarmInsights.optimalReason }}</p>
            </div>
            
            <div class="insight-card">
              <h4>⚡ 80/20 Opportunity</h4>
              <p>{{ swarmInsights.optimization }}</p>
              <p class="insight-reason">{{ swarmInsights.optimizationReason }}</p>
            </div>
            
            <div class="insight-card">
              <h4>🔮 Prediction</h4>
              <p>{{ swarmInsights.prediction }}</p>
              <p class="insight-reason">{{ swarmInsights.predictionReason }}</p>
            </div>
            
            <div class="swarm-actions">
              <button @click="applySwarmRecommendation" class="swarm-btn">
                Apply Swarm Recommendation
              </button>
              <button @click="exploreEmergentPaths" class="swarm-btn">
                Explore Emergent Paths
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'PermutationExplorer3D',
  
  data() {
    return {
      canvasWidth: 800,
      canvasHeight: 600,
      viewMode: 'galaxy',
      complexity: 0.5,
      animationSpeed: 1,
      
      // Camera controls
      camera: {
        x: 0,
        y: 0,
        distance: 500,
        rotationX: 0,
        rotationY: 0,
        rotationZ: 0
      },
      
      // Mouse interaction
      mouseDown: false,
      lastMouseX: 0,
      lastMouseY: 0,
      
      // Animation
      animationFrame: null,
      autoRotate: false,
      
      // 3D strategies
      strategies3D: [],
      selectedStrategy: null,
      selectedPermutation: null,
      
      // UI state
      activeDetailTab: 'strategies',
      detailTabs: [
        { id: 'strategies', name: 'Strategies', icon: '🎯' },
        { id: 'performance', name: 'Performance', icon: '📊' },
        { id: 'paths', name: 'Paths', icon: '🛤️' },
        { id: 'swarm', name: 'Swarm AI', icon: '🧠' }
      ],
      
      // 3D rendering context
      ctx: null,
      
      // Exploration data
      explorationPaths: [],
      swarmInsights: {}
    }
  },
  
  computed: {
    visiblePermutations() {
      return this.strategies3D.filter(s => this.isInView(s))
    },
    
    bestEfficiency() {
      if (this.strategies3D.length === 0) return 0
      return Math.max(...this.strategies3D.map(s => s.efficiency * 100)).toFixed(1)
    },
    
    optimalZoneCount() {
      return this.strategies3D.filter(s => s.efficiency > 0.8).length
    },
    
    eightyTwentyCandidates() {
      return this.strategies3D.filter(s => s.stages && s.stages.length <= 4).length
    }
  },
  
  mounted() {
    this.initializeCanvas()
    this.initializeStrategies3D()
    this.generateExplorationPaths()
    this.generateSwarmInsights()
    this.startAnimation()
  },
  
  beforeDestroy() {
    if (this.animationFrame) {
      cancelAnimationFrame(this.animationFrame)
    }
  },
  
  methods: {
    initializeCanvas() {
      const canvas = this.$refs.canvas3d
      this.ctx = canvas.getContext('2d')
      
      // Set canvas size
      const container = canvas.parentElement
      this.canvasWidth = container.clientWidth
      this.canvasHeight = container.clientHeight - 100
      
      canvas.width = this.canvasWidth
      canvas.height = this.canvasHeight
    },
    
    initializeStrategies3D() {
      const strategies = [
        { 
          id: 'linear', 
          name: 'Linear', 
          icon: '📏', 
          efficiency: 0.6, 
          complexity: 0.8,
          duration: 800,
          stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
        },
        { 
          id: 'skip', 
          name: 'Skip (80/20)', 
          icon: '⚡', 
          efficiency: 0.9, 
          complexity: 0.3,
          duration: 300,
          stages: ['typer', 'turtle', 'ash', 'k8s']
        },
        { 
          id: 'parallel', 
          name: 'Parallel', 
          icon: '🔀', 
          efficiency: 0.8, 
          complexity: 0.6,
          duration: 450,
          stages: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
        },
        { 
          id: 'emergence', 
          name: 'Emergence', 
          icon: '🧠', 
          efficiency: 0.85, 
          complexity: 0.7,
          duration: 400,
          stages: ['typer', 'turtle', 'ash', 'ttl2dspy', 'reactor', 'k8s']
        },
        { 
          id: 'domain', 
          name: 'Domain', 
          icon: '🎯', 
          efficiency: 0.7, 
          complexity: 0.9,
          duration: 600,
          stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'ash', 'reactor', 'erlang', 'k8s']
        },
        { 
          id: 'adaptive', 
          name: 'Adaptive', 
          icon: '🌳', 
          efficiency: 0.75, 
          complexity: 0.5,
          duration: 500,
          stages: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
        }
      ]
      
      // Position strategies in 3D space
      this.strategies3D = strategies.map((strategy, index) => {
        const angle = (index / strategies.length) * Math.PI * 2
        const radius = 200
        
        return {
          ...strategy,
          position: {
            x: Math.cos(angle) * radius,
            y: (strategy.efficiency - 0.5) * 300,
            z: Math.sin(angle) * radius
          },
          screenPosition: { x: 0, y: 0 },
          color: this.getStrategyColor(strategy.efficiency)
        }
      })
    },
    
    generateExplorationPaths() {
      this.explorationPaths = [
        {
          id: 'critical',
          name: 'Critical Path',
          type: 'optimized',
          stages: ['typer', 'turtle', 'ash', 'k8s'],
          duration: 300,
          efficiency: 90
        },
        {
          id: 'comprehensive',
          name: 'Comprehensive',
          type: 'complete',
          stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
          duration: 800,
          efficiency: 60
        },
        {
          id: 'balanced',
          name: 'Balanced',
          type: 'hybrid',
          stages: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s'],
          duration: 500,
          efficiency: 75
        },
        {
          id: 'security',
          name: 'Security Focus',
          type: 'domain',
          stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'ash', 'reactor', 'k8s'],
          duration: 650,
          efficiency: 68
        }
      ]
    },
    
    generateSwarmInsights() {
      this.swarmInsights = {
        optimalStrategy: 'Skip Optimization (80/20)',
        optimalReason: 'Based on current complexity level, skip optimization provides maximum efficiency with minimal resource usage.',
        optimization: 'Reduce stages by 50%',
        optimizationReason: 'Focus on critical stages (typer, turtle, ash, k8s) for 80% impact with 20% effort.',
        prediction: 'Performance will improve by 3x',
        predictionReason: 'Swarm intelligence predicts significant speedup through parallel execution and stage reduction.'
      }
    },
    
    getStrategyColor(efficiency) {
      // Color based on efficiency
      if (efficiency > 0.8) return '#00ff88'
      if (efficiency > 0.6) return '#ffff00'
      if (efficiency > 0.4) return '#ff8800'
      return '#ff4444'
    },
    
    project3D(x, y, z) {
      // Simple 3D to 2D projection
      const distance = this.camera.distance
      const perspective = distance / (distance + z)
      
      return {
        x: this.canvasWidth / 2 + (x * perspective),
        y: this.canvasHeight / 2 + (y * perspective),
        scale: perspective
      }
    },
    
    rotatePoint(x, y, z) {
      // Apply camera rotation
      const cos = Math.cos
      const sin = Math.sin
      
      // Rotate around Y axis
      const cosY = cos(this.camera.rotationY)
      const sinY = sin(this.camera.rotationY)
      const xRotY = x * cosY - z * sinY
      const zRotY = x * sinY + z * cosY
      
      // Rotate around X axis
      const cosX = cos(this.camera.rotationX)
      const sinX = sin(this.camera.rotationX)
      const yRotX = y * cosX - zRotY * sinX
      const zRotX = y * sinX + zRotY * cosX
      
      return { x: xRotY, y: yRotX, z: zRotX }
    },
    
    render() {
      const ctx = this.ctx
      ctx.clearRect(0, 0, this.canvasWidth, this.canvasHeight)
      
      // Set background
      ctx.fillStyle = '#0a0a0a'
      ctx.fillRect(0, 0, this.canvasWidth, this.canvasHeight)
      
      if (this.viewMode === 'galaxy') {
        this.renderGalaxyView(ctx)
      } else if (this.viewMode === 'flow') {
        this.renderFlowView(ctx)
      } else if (this.viewMode === 'performance') {
        this.renderPerformanceView(ctx)
      } else if (this.viewMode === 'optimization') {
        this.renderOptimizationView(ctx)
      }
      
      // Render UI overlays
      this.renderOverlays(ctx)
    },
    
    renderGalaxyView(ctx) {
      // Render strategies as stars in a galaxy
      this.strategies3D.forEach(strategy => {
        const rotated = this.rotatePoint(
          strategy.position.x,
          strategy.position.y,
          strategy.position.z
        )
        
        const projected = this.project3D(rotated.x, rotated.y, rotated.z)
        strategy.screenPosition = projected
        
        // Draw strategy as glowing orb
        const radius = 15 * projected.scale
        ctx.beginPath()
        ctx.arc(projected.x, projected.y, radius, 0, Math.PI * 2)
        
        // Glow effect
        const gradient = ctx.createRadialGradient(
          projected.x, projected.y, 0,
          projected.x, projected.y, radius * 2
        )
        gradient.addColorStop(0, strategy.color + 'ff')
        gradient.addColorStop(0.5, strategy.color + '80')
        gradient.addColorStop(1, strategy.color + '00')
        
        ctx.fillStyle = gradient
        ctx.fill()
        
        // Draw strategy icon
        ctx.fillStyle = '#fff'
        ctx.font = `${12 * projected.scale}px Arial`
        ctx.textAlign = 'center'
        ctx.fillText(strategy.icon, projected.x, projected.y + 4)
        
        // Draw strategy name
        ctx.fillStyle = strategy.color
        ctx.font = `${10 * projected.scale}px Arial`
        ctx.fillText(strategy.name, projected.x, projected.y + radius + 15)
      })
      
      // Draw connections between strategies
      this.drawGalaxyConnections(ctx)
    },
    
    renderFlowView(ctx) {
      // Render as connected flow network
      this.strategies3D.forEach(strategy => {
        const rotated = this.rotatePoint(
          strategy.position.x,
          strategy.position.y,
          strategy.position.z
        )
        
        const projected = this.project3D(rotated.x, rotated.y, rotated.z)
        strategy.screenPosition = projected
        
        // Draw node
        ctx.beginPath()
        ctx.arc(projected.x, projected.y, 20, 0, Math.PI * 2)
        ctx.fillStyle = strategy.color
        ctx.fill()
        ctx.strokeStyle = '#666'
        ctx.lineWidth = 2
        ctx.stroke()
        
        // Draw efficiency percentage
        ctx.fillStyle = '#fff'
        ctx.font = '12px Arial'
        ctx.textAlign = 'center'
        ctx.fillText(
          `${(strategy.efficiency * 100).toFixed(0)}%`,
          projected.x,
          projected.y + 4
        )
      })
      
      this.drawFlowConnections(ctx)
    },
    
    renderPerformanceView(ctx) {
      // Render in 3D performance space
      this.drawAxes(ctx)
      
      this.strategies3D.forEach(strategy => {
        // Map performance metrics to 3D space
        const x = (strategy.efficiency - 0.5) * 400
        const y = (strategy.complexity - 0.5) * 400
        const z = (strategy.duration - 500) * 0.5
        
        const rotated = this.rotatePoint(x, y, z)
        const projected = this.project3D(rotated.x, rotated.y, rotated.z)
        strategy.screenPosition = projected
        
        // Draw performance point
        const size = 10 + (strategy.efficiency * 10)
        ctx.beginPath()
        ctx.arc(projected.x, projected.y, size, 0, Math.PI * 2)
        ctx.fillStyle = strategy.color
        ctx.fill()
        
        // Draw label
        ctx.fillStyle = '#fff'
        ctx.font = '10px Arial'
        ctx.textAlign = 'center'
        ctx.fillText(strategy.name, projected.x, projected.y - size - 5)
      })
    },
    
    renderOptimizationView(ctx) {
      // Highlight 80/20 optimization opportunities
      this.strategies3D.forEach(strategy => {
        const rotated = this.rotatePoint(
          strategy.position.x,
          strategy.position.y,
          strategy.position.z
        )
        
        const projected = this.project3D(rotated.x, rotated.y, rotated.z)
        strategy.screenPosition = projected
        
        // Highlight based on 80/20 potential
        const is80_20 = strategy.stages && strategy.stages.length <= 4
        const radius = is80_20 ? 25 : 15
        
        ctx.beginPath()
        ctx.arc(projected.x, projected.y, radius, 0, Math.PI * 2)
        
        if (is80_20) {
          // Pulsing effect for 80/20 candidates
          const pulse = Math.sin(Date.now() * 0.005) * 0.3 + 0.7
          ctx.fillStyle = `rgba(255, 255, 0, ${pulse})`
        } else {
          ctx.fillStyle = strategy.color + '80'
        }
        
        ctx.fill()
        
        if (is80_20) {
          ctx.strokeStyle = '#ffff00'
          ctx.lineWidth = 3
          ctx.stroke()
        }
        
        // Draw icon
        ctx.fillStyle = '#fff'
        ctx.font = '16px Arial'
        ctx.textAlign = 'center'
        ctx.fillText(strategy.icon, projected.x, projected.y + 5)
      })
    },
    
    drawGalaxyConnections(ctx) {
      // Draw constellation-like connections
      for (let i = 0; i < this.strategies3D.length; i++) {
        for (let j = i + 1; j < this.strategies3D.length; j++) {
          const strategy1 = this.strategies3D[i]
          const strategy2 = this.strategies3D[j]
          
          // Draw connection if strategies are similar
          const similarity = this.calculateSimilarity(strategy1, strategy2)
          if (similarity > 0.3) {
            ctx.beginPath()
            ctx.moveTo(strategy1.screenPosition.x, strategy1.screenPosition.y)
            ctx.lineTo(strategy2.screenPosition.x, strategy2.screenPosition.y)
            ctx.strokeStyle = `rgba(0, 136, 255, ${similarity * 0.5})`
            ctx.lineWidth = similarity * 3
            ctx.stroke()
          }
        }
      }
    },
    
    drawFlowConnections(ctx) {
      // Draw directed flow connections
      this.strategies3D.forEach((strategy, index) => {
        if (index < this.strategies3D.length - 1) {
          const next = this.strategies3D[index + 1]
          
          ctx.beginPath()
          ctx.moveTo(strategy.screenPosition.x, strategy.screenPosition.y)
          ctx.lineTo(next.screenPosition.x, next.screenPosition.y)
          ctx.strokeStyle = '#0088ff'
          ctx.lineWidth = 2
          ctx.stroke()
          
          // Draw arrow
          this.drawArrow(ctx, strategy.screenPosition, next.screenPosition)
        }
      })
    },
    
    drawAxes(ctx) {
      // Draw 3D axes for performance view
      const origin = this.project3D(0, 0, 0)
      const xAxis = this.project3D(200, 0, 0)
      const yAxis = this.project3D(0, 200, 0)
      const zAxis = this.project3D(0, 0, 200)
      
      // X axis (red) - Efficiency
      ctx.beginPath()
      ctx.moveTo(origin.x, origin.y)
      ctx.lineTo(xAxis.x, xAxis.y)
      ctx.strokeStyle = '#ff0000'
      ctx.lineWidth = 2
      ctx.stroke()
      
      // Y axis (green) - Complexity
      ctx.beginPath()
      ctx.moveTo(origin.x, origin.y)
      ctx.lineTo(yAxis.x, yAxis.y)
      ctx.strokeStyle = '#00ff00'
      ctx.lineWidth = 2
      ctx.stroke()
      
      // Z axis (blue) - Duration
      ctx.beginPath()
      ctx.moveTo(origin.x, origin.y)
      ctx.lineTo(zAxis.x, zAxis.y)
      ctx.strokeStyle = '#0000ff'
      ctx.lineWidth = 2
      ctx.stroke()
      
      // Axis labels
      ctx.fillStyle = '#fff'
      ctx.font = '12px Arial'
      ctx.fillText('Efficiency', xAxis.x + 10, xAxis.y)
      ctx.fillText('Complexity', yAxis.x, yAxis.y - 10)
      ctx.fillText('Duration', zAxis.x + 10, zAxis.y)
    },
    
    drawArrow(ctx, from, to) {
      const angle = Math.atan2(to.y - from.y, to.x - from.x)
      const arrowLength = 10
      
      ctx.beginPath()
      ctx.moveTo(to.x, to.y)
      ctx.lineTo(
        to.x - arrowLength * Math.cos(angle - Math.PI / 6),
        to.y - arrowLength * Math.sin(angle - Math.PI / 6)
      )
      ctx.moveTo(to.x, to.y)
      ctx.lineTo(
        to.x - arrowLength * Math.cos(angle + Math.PI / 6),
        to.y - arrowLength * Math.sin(angle + Math.PI / 6)
      )
      ctx.strokeStyle = '#0088ff'
      ctx.lineWidth = 2
      ctx.stroke()
    },
    
    calculateSimilarity(strategy1, strategy2) {
      // Calculate similarity based on efficiency and complexity
      const effDiff = Math.abs(strategy1.efficiency - strategy2.efficiency)
      const compDiff = Math.abs(strategy1.complexity - strategy2.complexity)
      return 1 - (effDiff + compDiff) / 2
    },
    
    renderOverlays(ctx) {
      // Render selection indicators and other overlays
      if (this.selectedPermutation) {
        const pos = this.selectedPermutation.screenPosition
        ctx.beginPath()
        ctx.arc(pos.x, pos.y, 35, 0, Math.PI * 2)
        ctx.strokeStyle = '#00ff88'
        ctx.lineWidth = 3
        ctx.setLineDash([5, 5])
        ctx.stroke()
        ctx.setLineDash([])
      }
    },
    
    startAnimation() {
      const animate = () => {
        if (this.autoRotate) {
          this.camera.rotationY += 0.01 * this.animationSpeed
        }
        
        this.render()
        this.animationFrame = requestAnimationFrame(animate)
      }
      
      animate()
    },
    
    // Event handlers
    startMouseDrag(event) {
      this.mouseDown = true
      this.lastMouseX = event.clientX
      this.lastMouseY = event.clientY
    },
    
    handleMouseMove(event) {
      if (this.mouseDown) {
        const deltaX = event.clientX - this.lastMouseX
        const deltaY = event.clientY - this.lastMouseY
        
        this.camera.rotationY += deltaX * 0.01
        this.camera.rotationX += deltaY * 0.01
        
        this.lastMouseX = event.clientX
        this.lastMouseY = event.clientY
      }
    },
    
    endMouseDrag() {
      this.mouseDown = false
    },
    
    handleWheel(event) {
      event.preventDefault()
      this.camera.distance += event.deltaY
      this.camera.distance = Math.max(100, Math.min(1000, this.camera.distance))
    },
    
    handleCanvasClick(event) {
      const rect = this.$refs.canvas3d.getBoundingClientRect()
      const clickX = event.clientX - rect.left
      const clickY = event.clientY - rect.top
      
      // Find clicked strategy
      for (const strategy of this.strategies3D) {
        const pos = strategy.screenPosition
        const distance = Math.sqrt(
          Math.pow(clickX - pos.x, 2) + Math.pow(clickY - pos.y, 2)
        )
        
        if (distance < 30) {
          this.selectedPermutation = strategy
          this.selectedStrategy = strategy.id
          break
        }
      }
    },
    
    // Control methods
    updateView() {
      this.render()
    },
    
    updatePermutations() {
      // Update strategies based on complexity
      this.strategies3D.forEach(strategy => {
        // Adjust position based on complexity
        strategy.position.y = (strategy.efficiency - this.complexity) * 300
      })
    },
    
    rotateView() {
      this.autoRotate = !this.autoRotate
    },
    
    resetCamera() {
      this.camera = {
        x: 0,
        y: 0,
        distance: 500,
        rotationX: 0,
        rotationY: 0,
        rotationZ: 0
      }
    },
    
    exploreSwarm() {
      // Animate through different strategies
      let index = 0
      const interval = setInterval(() => {
        if (index < this.strategies3D.length) {
          this.selectedPermutation = this.strategies3D[index]
          this.selectedStrategy = this.strategies3D[index].id
          index++
        } else {
          clearInterval(interval)
        }
      }, 1000)
    },
    
    selectStrategy3D(strategy) {
      this.selectedStrategy = strategy.id
      this.selectedPermutation = strategy
    },
    
    animateToPath(path) {
      console.log('Animating to path:', path.name)
      // Could implement camera animation to focus on specific path
    },
    
    applySwarmRecommendation() {
      // Apply the swarm's recommended strategy
      const optimal = this.strategies3D.find(s => s.id === 'skip')
      if (optimal) {
        this.selectedPermutation = optimal
        this.selectedStrategy = optimal.id
      }
    },
    
    exploreEmergentPaths() {
      // Highlight emergent patterns
      this.viewMode = 'optimization'
      this.autoRotate = true
    },
    
    isInView(strategy) {
      // Simple frustum culling
      return strategy.screenPosition.x > 0 && 
             strategy.screenPosition.x < this.canvasWidth &&
             strategy.screenPosition.y > 0 && 
             strategy.screenPosition.y < this.canvasHeight
    },
    
    formatPosition(position) {
      return `(${Math.round(position.x)}, ${Math.round(position.y)}, ${Math.round(position.z)})`
    },
    
    getStageIcon(stage) {
      const icons = {
        typer: '🎯',
        turtle: '🐢',
        ttl2dspy: '🧠',
        bitactor: '⚛️',
        erlang: '🔧',
        ash: '🔥',
        reactor: '⚡',
        k8s: '☸️'
      }
      return icons[stage] || '📦'
    }
  }
}
</script>

<style scoped>
.permutation-explorer-3d {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
  height: 100vh;
  display: grid;
  grid-template-areas: 
    "title title"
    "controls canvas"
    "details details";
  grid-template-columns: 300px 1fr;
  grid-template-rows: auto auto 1fr;
  gap: 1rem;
}

.permutation-explorer-3d h2 {
  grid-area: title;
  font-size: 2rem;
  margin-bottom: 1rem;
}

.controls-panel {
  grid-area: controls;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.control-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.control-group label {
  font-weight: 600;
  color: #888;
}

.control-group select,
.control-group input[type="range"] {
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.control-btn {
  padding: 0.75rem;
  background: #0088ff;
  color: #fff;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.control-btn:hover {
  background: #0066cc;
  transform: translateY(-2px);
}

.canvas-3d-container {
  grid-area: canvas;
  background: #1a1a1a;
  border-radius: 16px;
  position: relative;
  overflow: hidden;
}

.canvas-overlay {
  position: absolute;
  top: 1rem;
  left: 1rem;
  right: 1rem;
  display: flex;
  justify-content: space-between;
  pointer-events: none;
}

.view-info {
  background: rgba(26, 26, 26, 0.9);
  padding: 1rem;
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  font-size: 0.9rem;
}

.selected-info {
  background: rgba(26, 26, 26, 0.9);
  padding: 1rem;
  border-radius: 8px;
  min-width: 200px;
}

.selected-info h4 {
  margin: 0 0 0.5rem 0;
  color: #00ff88;
}

.permutation-details {
  grid-area: details;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.detail-tabs {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 1.5rem;
}

.tab-btn {
  padding: 0.75rem 1.5rem;
  background: transparent;
  border: 2px solid #333;
  border-radius: 8px;
  color: #888;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.tab-btn:hover {
  border-color: #0088ff;
  color: #e0e0e0;
}

.tab-btn.active {
  background: #0088ff;
  border-color: #0088ff;
  color: #fff;
}

.strategy-grid-3d {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.strategy-card-3d {
  background: #2a2a2a;
  border: 2px solid #444;
  border-radius: 12px;
  padding: 1rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.strategy-card-3d:hover {
  border-color: #0088ff;
  transform: translateY(-2px);
}

.strategy-card-3d.active {
  border-color: #00ff88;
  background: #1a2a1a;
}

.strategy-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.strategy-icon {
  font-size: 1.5rem;
}

.strategy-name {
  font-weight: 600;
}

.strategy-metrics {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 0.5rem;
}

.metric {
  text-align: center;
}

.metric .label {
  display: block;
  font-size: 0.8rem;
  color: #888;
}

.metric .value {
  display: block;
  font-weight: 600;
  color: #00ff88;
}

.axis-info {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.axis {
  text-align: center;
}

.axis-label {
  display: block;
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.axis-desc {
  font-size: 0.9rem;
  color: #888;
}

.performance-stats {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
}

.stat {
  text-align: center;
}

.stat-label {
  display: block;
  font-size: 0.9rem;
  color: #888;
  margin-bottom: 0.25rem;
}

.stat-value {
  display: block;
  font-size: 1.5rem;
  font-weight: 700;
  color: #00ff88;
}

.path-explorer {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.path-item {
  background: #2a2a2a;
  border-radius: 8px;
  padding: 1rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.path-item:hover {
  background: #333;
  transform: translateX(5px);
}

.path-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.5rem;
}

.path-name {
  font-weight: 600;
}

.path-type {
  color: #888;
  font-size: 0.9rem;
}

.path-stages {
  display: flex;
  gap: 0.25rem;
  margin-bottom: 0.5rem;
}

.stage-badge {
  font-size: 1.2rem;
}

.path-metrics {
  display: flex;
  justify-content: space-between;
  font-size: 0.9rem;
  color: #888;
}

.swarm-insights {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.insight-card {
  background: #2a2a2a;
  border-radius: 8px;
  padding: 1rem;
}

.insight-card h4 {
  margin: 0 0 0.5rem 0;
  color: #00ff88;
}

.insight-reason {
  font-size: 0.9rem;
  color: #888;
  font-style: italic;
}

.swarm-actions {
  display: flex;
  gap: 1rem;
}

.swarm-btn {
  padding: 0.75rem 1.5rem;
  background: #0088ff;
  color: #fff;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.swarm-btn:hover {
  background: #0066cc;
  transform: translateY(-2px);
}
</style>