<template>
  <div class="parallel-execution-visualizer">
    <h2>🔀 Parallel Execution Visualizer</h2>
    
    <!-- Parallel Branches Configuration -->
    <div class="branch-configurator">
      <h3>Configure Parallel Branches</h3>
      <div class="branch-controls">
        <button @click="addBranch" class="add-branch-btn">
          ➕ Add Branch
        </button>
        <button @click="optimizeBranches" class="optimize-btn">
          ⚡ 80/20 Optimize
        </button>
        <button @click="balanceLoad" class="balance-btn">
          ⚖️ Balance Load
        </button>
      </div>
    </div>
    
    <!-- Parallel Execution Visualization -->
    <div class="parallel-viz">
      <svg ref="parallelSvg" :width="svgWidth" :height="svgHeight">
        <!-- Branch Lanes -->
        <g v-for="(branch, index) in branches" :key="branch.id">
          <rect
            :x="10"
            :y="getBranchY(index)"
            :width="svgWidth - 20"
            :height="branchHeight"
            :fill="getBranchColor(index)"
            :opacity="0.1"
            rx="8"
          />
          
          <!-- Branch Label -->
          <text
            :x="20"
            :y="getBranchY(index) + 20"
            class="branch-label"
          >
            Branch {{ index + 1 }} ({{ branch.stages.length }} stages)
          </text>
          
          <!-- Stages in Branch -->
          <g
            v-for="(stage, stageIndex) in branch.stages"
            :key="`${branch.id}-${stage.id}`"
          >
            <!-- Stage Box -->
            <rect
              :x="getStageX(stageIndex, branch.stages.length)"
              :y="getBranchY(index) + 30"
              :width="stageWidth"
              :height="stageHeight"
              :fill="getStageColor(stage)"
              :stroke="stage.active ? '#00ff88' : '#444'"
              :stroke-width="stage.active ? 3 : 1"
              rx="6"
              @click="toggleStage(branch.id, stage.id)"
              @dragstart="startDrag($event, stage, branch.id)"
              @dragover.prevent
              @drop="handleDrop($event, branch.id, stageIndex)"
              draggable="true"
              class="stage-box"
            />
            
            <!-- Stage Icon -->
            <text
              :x="getStageX(stageIndex, branch.stages.length) + stageWidth/2"
              :y="getBranchY(index) + 55"
              text-anchor="middle"
              class="stage-icon"
            >
              {{ getStageIcon(stage.name) }}
            </text>
            
            <!-- Stage Name -->
            <text
              :x="getStageX(stageIndex, branch.stages.length) + stageWidth/2"
              :y="getBranchY(index) + 75"
              text-anchor="middle"
              class="stage-name"
            >
              {{ stage.name }}
            </text>
            
            <!-- Duration -->
            <text
              :x="getStageX(stageIndex, branch.stages.length) + stageWidth/2"
              :y="getBranchY(index) + 90"
              text-anchor="middle"
              class="stage-duration"
            >
              {{ stage.duration }}ms
            </text>
            
            <!-- Connection Line to Next Stage -->
            <line
              v-if="stageIndex < branch.stages.length - 1"
              :x1="getStageX(stageIndex, branch.stages.length) + stageWidth"
              :y1="getBranchY(index) + 60"
              :x2="getStageX(stageIndex + 1, branch.stages.length)"
              :y2="getBranchY(index) + 60"
              stroke="#666"
              stroke-width="2"
              marker-end="url(#arrowhead)"
            />
          </g>
        </g>
        
        <!-- Merge Point -->
        <g v-if="branches.length > 1">
          <circle
            :cx="svgWidth - 100"
            :cy="svgHeight / 2"
            r="30"
            fill="#0088ff"
            stroke="#00ccff"
            stroke-width="3"
          />
          <text
            :x="svgWidth - 100"
            :y="svgHeight / 2 + 5"
            text-anchor="middle"
            class="merge-text"
          >
            MERGE
          </text>
          
          <!-- Lines from branches to merge -->
          <line
            v-for="(branch, index) in branches"
            :key="`merge-${branch.id}`"
            :x1="getLastStageX(branch) + stageWidth"
            :y1="getBranchY(index) + 60"
            :x2="svgWidth - 130"
            :y2="svgHeight / 2"
            stroke="#0088ff"
            stroke-width="2"
            stroke-dasharray="5,5"
          />
        </g>
        
        <!-- Arrow marker definition -->
        <defs>
          <marker
            id="arrowhead"
            markerWidth="10"
            markerHeight="7"
            refX="9"
            refY="3.5"
            orient="auto"
          >
            <polygon
              points="0 0, 10 3.5, 0 7"
              fill="#666"
            />
          </marker>
        </defs>
      </svg>
    </div>
    
    <!-- Execution Timeline -->
    <div class="execution-timeline">
      <h3>⏱️ Execution Timeline</h3>
      <div class="timeline-container">
        <div class="timeline-header">
          <span>0ms</span>
          <span>{{ maxDuration / 2 }}ms</span>
          <span>{{ maxDuration }}ms</span>
        </div>
        <div class="timeline-tracks">
          <div
            v-for="(branch, index) in branches"
            :key="`timeline-${branch.id}`"
            class="timeline-track"
          >
            <div class="track-label">B{{ index + 1 }}</div>
            <div class="track-bar">
              <div
                v-for="stage in branch.stages"
                :key="`timeline-stage-${stage.id}`"
                class="stage-bar"
                :style="{
                  left: getTimelinePosition(stage.startTime) + '%',
                  width: getTimelineWidth(stage.duration) + '%',
                  backgroundColor: getStageColor(stage)
                }"
                :title="`${stage.name}: ${stage.duration}ms`"
              ></div>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Performance Metrics -->
    <div class="performance-metrics">
      <h3>📊 Parallel Performance Analysis</h3>
      <div class="metrics-grid">
        <div class="metric-card">
          <span class="metric-label">Total Duration</span>
          <span class="metric-value">{{ totalDuration }}ms</span>
          <span class="metric-detail">vs {{ sequentialDuration }}ms sequential</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">Speedup Factor</span>
          <span class="metric-value">{{ speedupFactor }}x</span>
          <span class="metric-detail">{{ speedupPercentage }}% faster</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">Branch Utilization</span>
          <span class="metric-value">{{ avgUtilization }}%</span>
          <span class="metric-detail">{{ loadBalance }}</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">80/20 Score</span>
          <span class="metric-value">{{ eightyTwentyScore }}%</span>
          <span class="metric-detail">{{ criticalPathStages }} critical stages</span>
        </div>
      </div>
    </div>
    
    <!-- Swarm Optimization Suggestions -->
    <div class="swarm-suggestions">
      <h3>🧠 Swarm Intelligence Suggestions</h3>
      <div class="suggestion-list">
        <div
          v-for="suggestion in swarmSuggestions"
          :key="suggestion.id"
          class="suggestion-item"
          :class="suggestion.type"
        >
          <span class="suggestion-icon">{{ getSuggestionIcon(suggestion.type) }}</span>
          <span class="suggestion-text">{{ suggestion.text }}</span>
          <button
            @click="applySuggestion(suggestion)"
            class="apply-suggestion-btn"
          >
            Apply
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'ParallelExecutionVisualizer',
  
  data() {
    return {
      svgWidth: 1000,
      svgHeight: 400,
      branchHeight: 120,
      stageWidth: 100,
      stageHeight: 70,
      
      branches: [
        {
          id: 'branch-1',
          stages: [
            { id: 's1', name: 'typer', duration: 50, startTime: 0, active: false },
            { id: 's2', name: 'turtle', duration: 30, startTime: 50, active: false },
            { id: 's3', name: 'ash', duration: 150, startTime: 80, active: false },
            { id: 's4', name: 'reactor', duration: 100, startTime: 230, active: false }
          ]
        },
        {
          id: 'branch-2',
          stages: [
            { id: 's5', name: 'typer', duration: 50, startTime: 0, active: false },
            { id: 's6', name: 'turtle', duration: 30, startTime: 50, active: false },
            { id: 's7', name: 'ttl2dspy', duration: 100, startTime: 80, active: false },
            { id: 's8', name: 'bitactor', duration: 200, startTime: 180, active: false },
            { id: 's9', name: 'erlang', duration: 100, startTime: 380, active: false }
          ]
        }
      ],
      
      draggedStage: null,
      draggedFromBranch: null,
      
      swarmSuggestions: []
    }
  },
  
  computed: {
    totalDuration() {
      // Calculate total parallel execution time
      const branchDurations = this.branches.map(branch => {
        if (branch.stages.length === 0) return 0
        const lastStage = branch.stages[branch.stages.length - 1]
        return lastStage.startTime + lastStage.duration
      })
      return Math.max(...branchDurations)
    },
    
    sequentialDuration() {
      // Calculate sequential execution time for comparison
      const allStages = this.branches.flatMap(b => b.stages)
      return allStages.reduce((sum, stage) => sum + stage.duration, 0)
    },
    
    speedupFactor() {
      if (this.totalDuration === 0) return 1
      return (this.sequentialDuration / this.totalDuration).toFixed(2)
    },
    
    speedupPercentage() {
      return Math.round((1 - this.totalDuration / this.sequentialDuration) * 100)
    },
    
    avgUtilization() {
      // Calculate average branch utilization
      const utilizations = this.branches.map(branch => {
        if (branch.stages.length === 0) return 0
        const branchActive = branch.stages.reduce((sum, s) => sum + s.duration, 0)
        return (branchActive / this.totalDuration) * 100
      })
      return Math.round(utilizations.reduce((sum, u) => sum + u, 0) / utilizations.length)
    },
    
    loadBalance() {
      const utilizations = this.branches.map(branch => {
        if (branch.stages.length === 0) return 0
        return branch.stages.reduce((sum, s) => sum + s.duration, 0)
      })
      const max = Math.max(...utilizations)
      const min = Math.min(...utilizations)
      const balance = min / max
      
      if (balance > 0.9) return 'Excellent'
      if (balance > 0.7) return 'Good'
      if (balance > 0.5) return 'Fair'
      return 'Poor'
    },
    
    maxDuration() {
      return Math.max(this.totalDuration, 500)
    },
    
    eightyTwentyScore() {
      // Calculate how well the configuration follows 80/20 principle
      const allStages = this.branches.flatMap(b => b.stages)
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      const criticalCount = allStages.filter(s => criticalStages.includes(s.name)).length
      const totalCount = allStages.length
      
      if (totalCount === 0) return 0
      
      // Perfect 80/20 would be 4 critical stages out of 8 total (50%)
      const ratio = criticalCount / totalCount
      const score = ratio > 0.5 ? 100 : ratio * 200
      return Math.round(score)
    },
    
    criticalPathStages() {
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      return this.branches.flatMap(b => b.stages).filter(s => criticalStages.includes(s.name)).length
    }
  },
  
  mounted() {
    this.generateSwarmSuggestions()
    this.updateStageTimings()
  },
  
  methods: {
    getBranchY(index) {
      return 50 + (index * (this.branchHeight + 20))
    },
    
    getBranchColor(index) {
      const colors = ['#00ff88', '#0088ff', '#ff00ff', '#ff8800']
      return colors[index % colors.length]
    },
    
    getStageX(stageIndex, totalStages) {
      const availableWidth = this.svgWidth - 300
      const spacing = availableWidth / (totalStages + 1)
      return 100 + (stageIndex * spacing)
    },
    
    getLastStageX(branch) {
      if (branch.stages.length === 0) return 100
      return this.getStageX(branch.stages.length - 1, branch.stages.length)
    },
    
    getStageColor(stage) {
      const colors = {
        typer: '#ff6b6b',
        turtle: '#4ecdc4',
        ttl2dspy: '#45b7d1',
        bitactor: '#96ceb4',
        erlang: '#dda0dd',
        ash: '#f7b731',
        reactor: '#5f27cd',
        k8s: '#0984e3'
      }
      return colors[stage.name] || '#666'
    },
    
    getStageIcon(stageName) {
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
      return icons[stageName] || '📦'
    },
    
    addBranch() {
      const newBranch = {
        id: `branch-${Date.now()}`,
        stages: []
      }
      this.branches.push(newBranch)
      this.updateStageTimings()
    },
    
    optimizeBranches() {
      // Apply 80/20 optimization
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      
      // Clear all branches
      this.branches = [
        {
          id: 'optimized-1',
          stages: [
            { id: 'opt-1', name: 'typer', duration: 40, startTime: 0, active: false },
            { id: 'opt-2', name: 'turtle', duration: 25, startTime: 40, active: false },
            { id: 'opt-3', name: 'ash', duration: 120, startTime: 65, active: false }
          ]
        },
        {
          id: 'optimized-2',
          stages: [
            { id: 'opt-4', name: 'k8s', duration: 60, startTime: 0, active: false }
          ]
        }
      ]
      
      this.generateSwarmSuggestions()
    },
    
    balanceLoad() {
      // Redistribute stages for better load balance
      const allStages = this.branches.flatMap(b => b.stages)
      const targetBranches = Math.min(this.branches.length, 3)
      
      // Sort stages by duration (longest first)
      allStages.sort((a, b) => b.duration - a.duration)
      
      // Clear branches
      this.branches = Array(targetBranches).fill(null).map((_, i) => ({
        id: `balanced-${i}`,
        stages: []
      }))
      
      // Distribute stages using greedy algorithm
      allStages.forEach(stage => {
        // Find branch with minimum total duration
        const branchDurations = this.branches.map(b => 
          b.stages.reduce((sum, s) => sum + s.duration, 0)
        )
        const minIndex = branchDurations.indexOf(Math.min(...branchDurations))
        
        this.branches[minIndex].stages.push({
          ...stage,
          id: `balanced-${stage.id}`
        })
      })
      
      this.updateStageTimings()
    },
    
    toggleStage(branchId, stageId) {
      const branch = this.branches.find(b => b.id === branchId)
      if (branch) {
        const stage = branch.stages.find(s => s.id === stageId)
        if (stage) {
          stage.active = !stage.active
        }
      }
    },
    
    startDrag(event, stage, branchId) {
      this.draggedStage = stage
      this.draggedFromBranch = branchId
      event.dataTransfer.effectAllowed = 'move'
    },
    
    handleDrop(event, targetBranchId, targetIndex) {
      event.preventDefault()
      
      if (!this.draggedStage || !this.draggedFromBranch) return
      
      // Remove from source branch
      const sourceBranch = this.branches.find(b => b.id === this.draggedFromBranch)
      if (sourceBranch) {
        const sourceIndex = sourceBranch.stages.findIndex(s => s.id === this.draggedStage.id)
        if (sourceIndex !== -1) {
          sourceBranch.stages.splice(sourceIndex, 1)
        }
      }
      
      // Add to target branch
      const targetBranch = this.branches.find(b => b.id === targetBranchId)
      if (targetBranch) {
        targetBranch.stages.splice(targetIndex, 0, this.draggedStage)
      }
      
      // Reset drag state
      this.draggedStage = null
      this.draggedFromBranch = null
      
      // Update timings
      this.updateStageTimings()
    },
    
    updateStageTimings() {
      // Recalculate start times for all stages
      this.branches.forEach(branch => {
        let currentTime = 0
        branch.stages.forEach(stage => {
          stage.startTime = currentTime
          currentTime += stage.duration
        })
      })
    },
    
    getTimelinePosition(startTime) {
      return (startTime / this.maxDuration) * 100
    },
    
    getTimelineWidth(duration) {
      return (duration / this.maxDuration) * 100
    },
    
    generateSwarmSuggestions() {
      this.swarmSuggestions = []
      
      // Check for optimization opportunities
      const allStages = this.branches.flatMap(b => b.stages)
      const stageNames = allStages.map(s => s.name)
      
      // Check for missing critical stages
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      const missingCritical = criticalStages.filter(cs => !stageNames.includes(cs))
      
      if (missingCritical.length > 0) {
        this.swarmSuggestions.push({
          id: 'missing-critical',
          type: 'warning',
          text: `Missing critical stages: ${missingCritical.join(', ')}`,
          action: () => this.addCriticalStages(missingCritical)
        })
      }
      
      // Check for imbalanced branches
      if (this.loadBalance === 'Poor' || this.loadBalance === 'Fair') {
        this.swarmSuggestions.push({
          id: 'imbalance',
          type: 'optimization',
          text: 'Branches are imbalanced. Redistribute stages for better performance.',
          action: () => this.balanceLoad()
        })
      }
      
      // Check for parallel opportunities
      const longStages = allStages.filter(s => s.duration > 150)
      if (longStages.length > 1 && this.branches.length < 3) {
        this.swarmSuggestions.push({
          id: 'parallel-opp',
          type: 'performance',
          text: 'Long-running stages detected. Add more branches for parallelism.',
          action: () => this.addBranch()
        })
      }
      
      // 80/20 optimization opportunity
      if (this.eightyTwentyScore < 80) {
        this.swarmSuggestions.push({
          id: '80-20',
          type: 'optimization',
          text: 'Apply 80/20 optimization to focus on critical stages only.',
          action: () => this.optimizeBranches()
        })
      }
    },
    
    getSuggestionIcon(type) {
      const icons = {
        warning: '⚠️',
        optimization: '⚡',
        performance: '🚀',
        info: 'ℹ️'
      }
      return icons[type] || '💡'
    },
    
    applySuggestion(suggestion) {
      if (suggestion.action) {
        suggestion.action()
      }
      
      // Remove applied suggestion
      this.swarmSuggestions = this.swarmSuggestions.filter(s => s.id !== suggestion.id)
      
      // Regenerate suggestions after changes
      setTimeout(() => this.generateSwarmSuggestions(), 500)
    },
    
    addCriticalStages(stages) {
      // Add missing critical stages to branches
      stages.forEach((stageName, index) => {
        const targetBranch = this.branches[index % this.branches.length]
        targetBranch.stages.push({
          id: `critical-${Date.now()}-${index}`,
          name: stageName,
          duration: this.getDefaultDuration(stageName),
          startTime: 0,
          active: false
        })
      })
      
      this.updateStageTimings()
    },
    
    getDefaultDuration(stageName) {
      const durations = {
        typer: 50,
        turtle: 30,
        ttl2dspy: 100,
        bitactor: 200,
        erlang: 100,
        ash: 150,
        reactor: 100,
        k8s: 70
      }
      return durations[stageName] || 50
    }
  }
}
</script>

<style scoped>
.parallel-execution-visualizer {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.parallel-execution-visualizer h2 {
  margin-bottom: 2rem;
  font-size: 2rem;
}

.branch-configurator {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.branch-controls {
  display: flex;
  gap: 1rem;
  margin-top: 1rem;
}

.add-branch-btn, .optimize-btn, .balance-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  border: none;
}

.add-branch-btn {
  background: #00ff88;
  color: #000;
}

.optimize-btn {
  background: #ff8800;
  color: #fff;
}

.balance-btn {
  background: #0088ff;
  color: #fff;
}

.add-branch-btn:hover, .optimize-btn:hover, .balance-btn:hover {
  transform: translateY(-2px);
  filter: brightness(1.1);
}

.parallel-viz {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1rem;
  margin-bottom: 2rem;
  overflow-x: auto;
}

.branch-label {
  fill: #888;
  font-size: 0.9rem;
}

.stage-box {
  cursor: move;
  transition: all 0.3s ease;
}

.stage-box:hover {
  filter: brightness(1.2);
}

.stage-icon {
  font-size: 1.5rem;
  fill: #fff;
}

.stage-name {
  font-size: 0.8rem;
  fill: #fff;
  font-weight: 600;
}

.stage-duration {
  font-size: 0.7rem;
  fill: #aaa;
}

.merge-text {
  fill: #fff;
  font-weight: 700;
}

.execution-timeline {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.timeline-header {
  display: flex;
  justify-content: space-between;
  color: #888;
  font-size: 0.8rem;
  margin-bottom: 1rem;
}

.timeline-tracks {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.timeline-track {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.track-label {
  width: 30px;
  font-weight: 600;
  color: #888;
}

.track-bar {
  flex: 1;
  height: 30px;
  background: #2a2a2a;
  border-radius: 4px;
  position: relative;
  overflow: hidden;
}

.stage-bar {
  position: absolute;
  height: 100%;
  border-radius: 4px;
  transition: all 0.3s ease;
}

.stage-bar:hover {
  filter: brightness(1.3);
  z-index: 10;
}

.performance-metrics {
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
  text-align: center;
}

.metric-label {
  display: block;
  color: #888;
  font-size: 0.9rem;
  margin-bottom: 0.5rem;
}

.metric-value {
  display: block;
  font-size: 2rem;
  font-weight: 700;
  color: #00ff88;
  margin-bottom: 0.5rem;
}

.metric-detail {
  display: block;
  font-size: 0.8rem;
  color: #666;
}

.swarm-suggestions {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.suggestion-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin-top: 1rem;
}

.suggestion-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #2a2a2a;
  border-radius: 8px;
  border-left: 4px solid #666;
}

.suggestion-item.warning {
  border-left-color: #ff8800;
}

.suggestion-item.optimization {
  border-left-color: #00ff88;
}

.suggestion-item.performance {
  border-left-color: #0088ff;
}

.suggestion-icon {
  font-size: 1.5rem;
}

.suggestion-text {
  flex: 1;
}

.apply-suggestion-btn {
  padding: 0.5rem 1rem;
  background: #0088ff;
  color: #fff;
  border: none;
  border-radius: 6px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-suggestion-btn:hover {
  background: #0066cc;
}
</style>