<template>
  <div class="matrix-visualizer">
    <h3 class="visualizer-title">🔄 Advanced Permutation Matrix</h3>
    
    <!-- Matrix Controls -->
    <div class="matrix-controls">
      <div class="view-controls">
        <button 
          @click="setViewMode('grid')" 
          :class="['view-btn', { active: viewMode === 'grid' }]"
        >
          📊 Grid View
        </button>
        <button 
          @click="setViewMode('network')" 
          :class="['view-btn', { active: viewMode === 'network' }]"
        >
          🕸️ Network View
        </button>
        <button 
          @click="setViewMode('flow')" 
          :class="['view-btn', { active: viewMode === 'flow' }]"
        >
          🌊 Flow View
        </button>
        <button 
          @click="setViewMode('3d')" 
          :class="['view-btn', { active: viewMode === '3d' }]"
        >
          🎯 3D View
        </button>
      </div>
      
      <div class="filter-controls">
        <label class="filter-label">
          <input type="checkbox" v-model="showBypass"> Show Bypass Routes
        </label>
        <label class="filter-label">
          <input type="checkbox" v-model="showParallel"> Show Parallel Paths
        </label>
        <label class="filter-label">
          <input type="checkbox" v-model="showOptimized"> Show Optimized Only
        </label>
      </div>
      
      <div class="analysis-controls">
        <button @click="analyzeComplexity" class="analysis-btn">🧮 Analyze Complexity</button>
        <button @click="generateRecommendations" class="analysis-btn">💡 Get Recommendations</button>
        <button @click="exportMatrix" class="analysis-btn">📄 Export Matrix</button>
      </div>
    </div>
    
    <!-- Grid View -->
    <div v-if="viewMode === 'grid'" class="grid-view">
      <h4 class="view-title">📊 Permutation Combination Grid</h4>
      
      <!-- Stage Headers -->
      <div class="grid-container">
        <div class="grid-header">
          <div class="corner-cell">Stages</div>
          <div 
            v-for="stage in stages" 
            :key="stage.id"
            class="header-cell"
          >
            <div class="stage-icon">{{ stage.icon }}</div>
            <div class="stage-name">{{ stage.name }}</div>
          </div>
        </div>
        
        <!-- Permutation Rows -->
        <div 
          v-for="permutation in filteredPermutations" 
          :key="permutation.id"
          class="grid-row"
        >
          <div class="row-header">
            <span class="perm-icon">{{ permutation.icon }}</span>
            <span class="perm-name">{{ permutation.name }}</span>
            <span class="perm-type">{{ permutation.type }}</span>
          </div>
          
          <div 
            v-for="stage in stages" 
            :key="`${permutation.id}-${stage.id}`"
            :class="['grid-cell', getCellStatus(permutation, stage)]"
            @click="selectCell(permutation, stage)"
          >
            <div class="cell-content">
              <span class="cell-status">{{ getCellIcon(permutation, stage) }}</span>
              <span v-if="getCellTime(permutation, stage)" class="cell-time">
                {{ getCellTime(permutation, stage) }}ms
              </span>
            </div>
          </div>
        </div>
      </div>
      
      <!-- Grid Legend -->
      <div class="grid-legend">
        <div class="legend-item">
          <span class="legend-icon included">✅</span>
          <span>Included</span>
        </div>
        <div class="legend-item">
          <span class="legend-icon skipped">⏭️</span>
          <span>Skipped</span>
        </div>
        <div class="legend-item">
          <span class="legend-icon parallel">🔄</span>
          <span>Parallel</span>
        </div>
        <div class="legend-item">
          <span class="legend-icon optimized">⚡</span>
          <span>Optimized</span>
        </div>
      </div>
    </div>
    
    <!-- Network View -->
    <div v-if="viewMode === 'network'" class="network-view">
      <h4 class="view-title">🕸️ Permutation Network Graph</h4>
      
      <div class="network-container">
        <svg ref="networkSvg" class="network-svg" viewBox="0 0 800 600">
          <!-- Stage Nodes -->
          <g class="stage-nodes">
            <circle
              v-for="(stage, index) in stages"
              :key="`node-${stage.id}`"
              :cx="getNodeX(index, stages.length)"
              :cy="getNodeY(index, stages.length)"
              :r="nodeRadius"
              :class="['stage-node', getNodeClass(stage)]"
              @click="selectNode(stage)"
            />
            <text
              v-for="(stage, index) in stages"
              :key="`label-${stage.id}`"
              :x="getNodeX(index, stages.length)"
              :y="getNodeY(index, stages.length) + 5"
              class="node-label"
              text-anchor="middle"
            >
              {{ stage.icon }}
            </text>
          </g>
          
          <!-- Permutation Paths -->
          <g class="permutation-paths">
            <path
              v-for="permutation in filteredPermutations"
              :key="`path-${permutation.id}`"
              :d="getPermutationPath(permutation)"
              :class="['perm-path', permutation.type]"
              :stroke-width="getPathWidth(permutation)"
              @click="selectPath(permutation)"
            />
          </g>
          
          <!-- Connection Weights -->
          <g class="connection-weights">
            <text
              v-for="connection in visibleConnections"
              :key="`weight-${connection.id}`"
              :x="connection.x"
              :y="connection.y"
              class="weight-label"
              text-anchor="middle"
            >
              {{ connection.weight }}
            </text>
          </g>
        </svg>
        
        <!-- Network Controls -->
        <div class="network-controls">
          <button @click="resetZoom" class="network-btn">🔍 Reset Zoom</button>
          <button @click="autoLayout" class="network-btn">🎯 Auto Layout</button>
          <button @click="highlightCriticalPath" class="network-btn">⚡ Critical Path</button>
        </div>
      </div>
    </div>
    
    <!-- Flow View -->
    <div v-if="viewMode === 'flow'" class="flow-view">
      <h4 class="view-title">🌊 Permutation Flow Diagram</h4>
      
      <div class="flow-container">
        <div class="flow-swimlanes">
          <div 
            v-for="swimlane in flowSwimlanes" 
            :key="swimlane.id"
            class="swimlane"
          >
            <div class="swimlane-header">
              <span class="swimlane-icon">{{ swimlane.icon }}</span>
              <span class="swimlane-name">{{ swimlane.name }}</span>
              <span class="swimlane-count">{{ swimlane.permutations.length }}</span>
            </div>
            
            <div class="swimlane-content">
              <div 
                v-for="(perm, index) in swimlane.permutations" 
                :key="`flow-${perm.id}`"
                class="flow-item"
                :style="{ left: `${index * 120 + 20}px` }"
                @click="selectFlowItem(perm)"
              >
                <div class="flow-icon">{{ perm.icon }}</div>
                <div class="flow-name">{{ perm.name }}</div>
                <div class="flow-stages">
                  <div 
                    v-for="stage in perm.stages" 
                    :key="`${perm.id}-${stage}`"
                    class="flow-stage"
                  >
                    {{ getStageIcon(stage) }}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <!-- Flow Connections -->
        <svg class="flow-connections" viewBox="0 0 1000 400">
          <path
            v-for="connection in flowConnections"
            :key="`flow-conn-${connection.id}`"
            :d="connection.path"
            class="flow-connection"
            :stroke-dasharray="connection.dashed ? '5,5' : ''"
          />
        </svg>
      </div>
    </div>
    
    <!-- 3D View -->
    <div v-if="viewMode === '3d'" class="3d-view">
      <h4 class="view-title">🎯 3D Permutation Space</h4>
      
      <div class="3d-container" ref="3dContainer">
        <div class="3d-scene">
          <!-- 3D Visualization would be implemented with Three.js -->
          <div class="3d-placeholder">
            <div class="3d-axis x-axis">
              <span class="axis-label">Speed →</span>
            </div>
            <div class="3d-axis y-axis">
              <span class="axis-label">↑ Complexity</span>
            </div>
            <div class="3d-axis z-axis">
              <span class="axis-label">Quality ↗</span>
            </div>
            
            <div 
              v-for="permutation in filteredPermutations" 
              :key="`3d-${permutation.id}`"
              class="3d-point"
              :style="get3DPosition(permutation)"
              @click="select3DPoint(permutation)"
            >
              <span class="3d-icon">{{ permutation.icon }}</span>
              <span class="3d-label">{{ permutation.name }}</span>
            </div>
          </div>
        </div>
        
        <div class="3d-controls">
          <button @click="rotate3D('x')" class="3d-btn">🔄 Rotate X</button>
          <button @click="rotate3D('y')" class="3d-btn">🔄 Rotate Y</button>
          <button @click="rotate3D('z')" class="3d-btn">🔄 Rotate Z</button>
          <button @click="reset3D" class="3d-btn">🎯 Reset</button>
        </div>
      </div>
    </div>
    
    <!-- Analysis Panel -->
    <div v-if="showAnalysis" class="analysis-panel">
      <h4 class="analysis-title">🧮 Complexity Analysis</h4>
      
      <div class="analysis-content">
        <div class="complexity-metrics">
          <div class="metric-card">
            <div class="metric-label">Total Permutations</div>
            <div class="metric-value">{{ totalPermutations }}</div>
          </div>
          <div class="metric-card">
            <div class="metric-label">Unique Paths</div>
            <div class="metric-value">{{ uniquePaths }}</div>
          </div>
          <div class="metric-card">
            <div class="metric-label">Optimization Potential</div>
            <div class="metric-value">{{ optimizationPotential }}%</div>
          </div>
          <div class="metric-card">
            <div class="metric-label">Complexity Score</div>
            <div class="metric-value">{{ complexityScore }}</div>
          </div>
        </div>
        
        <div class="recommendations">
          <h5 class="rec-title">💡 Recommendations</h5>
          <div 
            v-for="(rec, index) in recommendations" 
            :key="`rec-${index}`"
            class="recommendation"
          >
            <span class="rec-icon">{{ rec.icon }}</span>
            <span class="rec-text">{{ rec.text }}</span>
            <button v-if="rec.action" @click="applyRecommendation(rec)" class="rec-action">
              Apply
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Selected Item Details -->
    <div v-if="selectedItem" class="item-details">
      <h4 class="details-title">🔍 {{ selectedItem.name }} Details</h4>
      
      <div class="details-content">
        <div class="detail-grid">
          <div class="detail-item">
            <span class="detail-label">Type:</span>
            <span class="detail-value">{{ selectedItem.type }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Speed:</span>
            <span class="detail-value">{{ selectedItem.speed }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Stages:</span>
            <span class="detail-value">{{ selectedItem.stages?.length || 0 }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Parallel:</span>
            <span class="detail-value">{{ selectedItem.parallel ? 'Yes' : 'No' }}</span>
          </div>
        </div>
        
        <div v-if="selectedItem.stages" class="stage-breakdown">
          <h6>Stage Breakdown:</h6>
          <div class="breakdown-items">
            <div 
              v-for="stage in selectedItem.stages" 
              :key="`breakdown-${stage}`"
              class="breakdown-item"
            >
              <span class="breakdown-icon">{{ getStageIcon(stage) }}</span>
              <span class="breakdown-name">{{ stage }}</span>
            </div>
          </div>
        </div>
        
        <button @click="closeDetails" class="close-details">✕ Close</button>
      </div>
    </div>
  </div>
</template>

<script setup>
// 🔄 ULTRATHINK SWARM 80/20: Advanced Permutation Matrix Visualizer
// JavaScript implementation for complex permutation visualization

import { ref, computed, onMounted } from 'vue'

// View state
const viewMode = ref('grid')
const showBypass = ref(true)
const showParallel = ref(true)
const showOptimized = ref(false)
const showAnalysis = ref(false)
const selectedItem = ref(null)

// 3D rotation state
const rotation3D = ref({ x: 0, y: 0, z: 0 })
const nodeRadius = 30

// Pipeline stages
const stages = ref([
  { id: 'typer', name: 'Typer', icon: '📝' },
  { id: 'turtle', name: 'Turtle', icon: '🐢' },
  { id: 'ttl2dspy', name: 'TTL→DSPy', icon: '🔄' },
  { id: 'bitactor', name: 'BitActor', icon: '🎭' },
  { id: 'erlang', name: 'Erlang', icon: '📡' },
  { id: 'ash', name: 'Ash', icon: '🔥' },
  { id: 'reactor', name: 'Reactor', icon: '⚛️' },
  { id: 'k8s', name: 'K8s', icon: '☸️' }
])

// All possible permutations
const allPermutations = ref([
  {
    id: 'ultra_bypass',
    name: 'Ultra Bypass',
    icon: '⚡',
    type: 'bypass',
    speed: 'ultra-fast',
    stages: ['typer', 'k8s'],
    skippedStages: ['turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor'],
    parallel: false,
    estimatedTime: 150,
    complexity: 1
  },
  {
    id: 'speed_bypass',
    name: 'Speed Bypass',
    icon: '🚀',
    type: 'bypass',
    speed: 'very-fast',
    stages: ['typer', 'ash'],
    skippedStages: ['turtle', 'ttl2dspy', 'bitactor', 'erlang', 'reactor', 'k8s'],
    parallel: false,
    estimatedTime: 300,
    complexity: 2
  },
  {
    id: 'smart_bypass',
    name: 'Smart Bypass',
    icon: '🧠',
    type: 'bypass',
    speed: 'fast',
    stages: ['typer', 'reactor'],
    skippedStages: ['turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'k8s'],
    parallel: false,
    estimatedTime: 400,
    complexity: 3
  },
  {
    id: 'parallel_full',
    name: 'Full Parallel',
    icon: '🔄',
    type: 'parallel',
    speed: 'optimized',
    stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
    parallel: true,
    parallelGroups: [['turtle', 'ttl2dspy'], ['bitactor', 'erlang'], ['ash', 'reactor']],
    estimatedTime: 800,
    complexity: 8
  },
  {
    id: 'parallel_optimized',
    name: 'Optimized Parallel',
    icon: '⚡🔄',
    type: 'parallel',
    speed: 'fast',
    stages: ['typer', 'turtle', 'ash', 'reactor', 'k8s'],
    skippedStages: ['ttl2dspy', 'bitactor', 'erlang'],
    parallel: true,
    parallelGroups: [['turtle', 'ash'], ['reactor']],
    estimatedTime: 500,
    complexity: 5
  },
  {
    id: 'adaptive_routing',
    name: 'Adaptive Routing',
    icon: '🎯',
    type: 'adaptive',
    speed: 'variable',
    stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
    adaptiveRouting: true,
    estimatedTime: 600,
    complexity: 6
  },
  {
    id: 'comprehensive',
    name: 'Comprehensive',
    icon: '📚',
    type: 'comprehensive',
    speed: 'thorough',
    stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
    parallel: false,
    estimatedTime: 1200,
    complexity: 8
  }
])

// Computed properties
const filteredPermutations = computed(() => {
  return allPermutations.value.filter(perm => {
    if (!showBypass.value && perm.type === 'bypass') return false
    if (!showParallel.value && perm.parallel) return false
    if (showOptimized.value && perm.speed !== 'ultra-fast' && perm.speed !== 'very-fast') return false
    return true
  })
})

const flowSwimlanes = computed(() => {
  const lanes = [
    { id: 'bypass', name: 'Bypass Routes', icon: '⚡', permutations: [] },
    { id: 'parallel', name: 'Parallel Execution', icon: '🔄', permutations: [] },
    { id: 'adaptive', name: 'Adaptive Routing', icon: '🎯', permutations: [] },
    { id: 'comprehensive', name: 'Comprehensive', icon: '📚', permutations: [] }
  ]
  
  filteredPermutations.value.forEach(perm => {
    const lane = lanes.find(l => l.id === perm.type)
    if (lane) {
      lane.permutations.push(perm)
    }
  })
  
  return lanes.filter(lane => lane.permutations.length > 0)
})

const flowConnections = computed(() => {
  // Generate connections between flow items
  return []
})

const visibleConnections = computed(() => {
  // Generate connection weights for network view
  return []
})

const totalPermutations = computed(() => allPermutations.value.length)
const uniquePaths = computed(() => new Set(allPermutations.value.map(p => p.stages.join('-'))).size)
const optimizationPotential = computed(() => Math.round((allPermutations.value.filter(p => p.type === 'bypass').length / totalPermutations.value) * 100))
const complexityScore = computed(() => {
  const avgComplexity = allPermutations.value.reduce((sum, p) => sum + p.complexity, 0) / totalPermutations.value
  return Math.round(avgComplexity * 10) / 10
})

const recommendations = ref([
  { icon: '⚡', text: 'Use bypass routes for simple ontologies', action: 'apply_bypass' },
  { icon: '🔄', text: 'Enable parallel execution for complex transforms', action: 'enable_parallel' },
  { icon: '🎯', text: 'Consider adaptive routing for dynamic workloads', action: 'use_adaptive' }
])

// Methods
const setViewMode = (mode) => {
  viewMode.value = mode
}

const getCellStatus = (permutation, stage) => {
  if (permutation.stages.includes(stage.id)) {
    if (permutation.parallel && permutation.parallelGroups?.some(group => group.includes(stage.id))) {
      return 'parallel'
    }
    return 'included'
  } else if (permutation.skippedStages?.includes(stage.id)) {
    return 'skipped'
  }
  return 'not-applicable'
}

const getCellIcon = (permutation, stage) => {
  const status = getCellStatus(permutation, stage)
  const icons = {
    included: '✅',
    skipped: '⏭️',
    parallel: '🔄',
    'not-applicable': '◯'
  }
  return icons[status] || '◯'
}

const getCellTime = (permutation, stage) => {
  if (permutation.stages.includes(stage.id)) {
    // Estimate stage time based on permutation
    const baseTime = 100
    const factor = permutation.speed === 'ultra-fast' ? 0.5 : permutation.speed === 'fast' ? 0.8 : 1.0
    return Math.round(baseTime * factor)
  }
  return null
}

const selectCell = (permutation, stage) => {
  selectedItem.value = {
    ...permutation,
    selectedStage: stage,
    cellStatus: getCellStatus(permutation, stage)
  }
}

const getNodeX = (index, total) => {
  const radius = 250
  const angle = (index / total) * 2 * Math.PI
  return 400 + radius * Math.cos(angle)
}

const getNodeY = (index, total) => {
  const radius = 250
  const angle = (index / total) * 2 * Math.PI
  return 300 + radius * Math.sin(angle)
}

const getNodeClass = (stage) => {
  return stage.id
}

const getPermutationPath = (permutation) => {
  // Generate SVG path for permutation
  const stageIndices = permutation.stages.map(stageId => 
    stages.value.findIndex(s => s.id === stageId)
  ).filter(i => i !== -1)
  
  if (stageIndices.length < 2) return ''
  
  let path = `M ${getNodeX(stageIndices[0], stages.value.length)} ${getNodeY(stageIndices[0], stages.value.length)}`
  
  for (let i = 1; i < stageIndices.length; i++) {
    const x = getNodeX(stageIndices[i], stages.value.length)
    const y = getNodeY(stageIndices[i], stages.value.length)
    path += ` L ${x} ${y}`
  }
  
  return path
}

const getPathWidth = (permutation) => {
  const widths = {
    bypass: 3,
    parallel: 5,
    adaptive: 4,
    comprehensive: 6
  }
  return widths[permutation.type] || 2
}

const selectNode = (stage) => {
  selectedItem.value = { ...stage, type: 'stage' }
}

const selectPath = (permutation) => {
  selectedItem.value = permutation
}

const selectFlowItem = (permutation) => {
  selectedItem.value = permutation
}

const select3DPoint = (permutation) => {
  selectedItem.value = permutation
}

const get3DPosition = (permutation) => {
  // Map permutation properties to 3D coordinates
  const speedMap = { 'ultra-fast': 80, 'very-fast': 60, 'fast': 40, 'optimized': 30, 'thorough': 10, 'variable': 50 }
  const x = speedMap[permutation.speed] || 50
  const y = (permutation.complexity / 8) * 80
  const z = permutation.stages.length * 10
  
  return {
    left: `${x}%`,
    top: `${100 - y}%`,
    transform: `translateZ(${z}px)`
  }
}

const getStageIcon = (stageId) => {
  const stage = stages.value.find(s => s.id === stageId)
  return stage ? stage.icon : '❓'
}

const analyzeComplexity = () => {
  showAnalysis.value = !showAnalysis.value
}

const generateRecommendations = () => {
  // Update recommendations based on current permutations
  const newRecs = []
  
  if (filteredPermutations.value.filter(p => p.type === 'bypass').length > 3) {
    newRecs.push({ icon: '⚡', text: 'High bypass potential detected - consider ultra-fast routes', action: 'optimize_bypass' })
  }
  
  if (filteredPermutations.value.some(p => p.parallel)) {
    newRecs.push({ icon: '🔄', text: 'Parallel execution available - can reduce total time', action: 'use_parallel' })
  }
  
  recommendations.value = newRecs.length > 0 ? newRecs : recommendations.value
}

const exportMatrix = () => {
  const data = {
    permutations: filteredPermutations.value,
    stages: stages.value,
    analysis: {
      totalPermutations: totalPermutations.value,
      uniquePaths: uniquePaths.value,
      optimizationPotential: optimizationPotential.value,
      complexityScore: complexityScore.value
    }
  }
  
  const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `permutation-matrix-${Date.now()}.json`
  a.click()
  URL.revokeObjectURL(url)
}

const resetZoom = () => {
  // Reset network view zoom
}

const autoLayout = () => {
  // Auto-arrange network layout
}

const highlightCriticalPath = () => {
  // Highlight critical path in network
}

const rotate3D = (axis) => {
  rotation3D.value[axis] += 15
}

const reset3D = () => {
  rotation3D.value = { x: 0, y: 0, z: 0 }
}

const applyRecommendation = (rec) => {
  switch (rec.action) {
    case 'apply_bypass':
      showBypass.value = true
      showOptimized.value = true
      break
    case 'enable_parallel':
      showParallel.value = true
      break
    case 'use_adaptive':
      // Focus on adaptive permutations
      break
  }
}

const closeDetails = () => {
  selectedItem.value = null
}

onMounted(() => {
  // Initialize visualizer
})
</script>

<style scoped>
.matrix-visualizer {
  @apply space-y-6;
}

.visualizer-title {
  @apply text-xl font-bold mb-4;
}

/* Matrix Controls */
.matrix-controls {
  @apply bg-gray-50 p-4 rounded-lg space-y-4;
}

.view-controls {
  @apply flex gap-2;
}

.view-btn {
  @apply px-4 py-2 rounded-lg border-2 border-gray-300 hover:border-blue-400 transition-all;
}

.view-btn.active {
  @apply border-blue-500 bg-blue-50;
}

.filter-controls, .analysis-controls {
  @apply flex gap-4 items-center;
}

.filter-label {
  @apply flex items-center gap-2 text-sm;
}

.analysis-btn {
  @apply px-3 py-1 bg-blue-600 text-white rounded hover:bg-blue-700;
}

/* Grid View */
.grid-view {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.view-title {
  @apply text-lg font-semibold mb-4;
}

.grid-container {
  @apply overflow-x-auto;
}

.grid-header {
  @apply flex border-b-2 border-gray-200;
}

.corner-cell {
  @apply w-48 p-3 font-semibold bg-gray-100;
}

.header-cell {
  @apply w-24 p-3 text-center bg-gray-100 border-l border-gray-200;
}

.stage-icon {
  @apply text-xl;
}

.stage-name {
  @apply text-xs font-medium;
}

.grid-row {
  @apply flex border-b border-gray-100;
}

.row-header {
  @apply w-48 p-3 bg-gray-50 flex flex-col;
}

.perm-icon {
  @apply text-xl;
}

.perm-name {
  @apply font-semibold;
}

.perm-type {
  @apply text-xs text-gray-600;
}

.grid-cell {
  @apply w-24 p-3 border-l border-gray-100 cursor-pointer hover:bg-gray-50;
}

.grid-cell.included {
  @apply bg-green-50;
}

.grid-cell.skipped {
  @apply bg-orange-50;
}

.grid-cell.parallel {
  @apply bg-blue-50;
}

.cell-content {
  @apply text-center;
}

.cell-status {
  @apply text-lg;
}

.cell-time {
  @apply text-xs text-gray-600 block;
}

.grid-legend {
  @apply flex gap-6 mt-4 text-sm;
}

.legend-item {
  @apply flex items-center gap-2;
}

.legend-icon {
  @apply text-lg;
}

/* Network View */
.network-view {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.network-container {
  @apply relative;
}

.network-svg {
  @apply w-full h-96 border border-gray-200 rounded;
}

.stage-node {
  @apply fill-blue-500 stroke-blue-700 stroke-2 cursor-pointer;
}

.stage-node:hover {
  @apply fill-blue-600;
}

.node-label {
  @apply fill-white font-bold cursor-pointer;
}

.perm-path {
  @apply fill-none stroke-purple-500 cursor-pointer opacity-70;
}

.perm-path:hover {
  @apply opacity-100 stroke-purple-700;
}

.perm-path.bypass {
  @apply stroke-orange-500;
}

.perm-path.parallel {
  @apply stroke-green-500;
}

.perm-path.adaptive {
  @apply stroke-blue-500;
}

.weight-label {
  @apply fill-gray-600 text-xs;
}

.network-controls {
  @apply absolute top-4 right-4 flex gap-2;
}

.network-btn {
  @apply px-3 py-1 bg-white border border-gray-300 rounded hover:bg-gray-50;
}

/* Flow View */
.flow-view {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.flow-container {
  @apply relative;
}

.flow-swimlanes {
  @apply space-y-4;
}

.swimlane {
  @apply border border-gray-200 rounded-lg;
}

.swimlane-header {
  @apply bg-gray-100 p-3 flex items-center gap-3;
}

.swimlane-icon {
  @apply text-xl;
}

.swimlane-name {
  @apply font-semibold flex-1;
}

.swimlane-count {
  @apply text-sm text-gray-600 bg-gray-200 px-2 py-1 rounded;
}

.swimlane-content {
  @apply relative h-24 overflow-x-auto;
}

.flow-item {
  @apply absolute top-2 w-24 p-2 bg-blue-100 rounded cursor-pointer hover:bg-blue-200;
}

.flow-icon {
  @apply text-lg text-center;
}

.flow-name {
  @apply text-xs font-medium text-center;
}

.flow-stages {
  @apply flex justify-center gap-1 mt-1;
}

.flow-stage {
  @apply text-xs;
}

.flow-connections {
  @apply absolute inset-0 pointer-events-none;
}

.flow-connection {
  @apply stroke-gray-400 stroke-2 fill-none;
}

/* 3D View */
.3d-view {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.3d-container {
  @apply relative;
}

.3d-scene {
  @apply h-96 bg-gradient-to-br from-blue-50 to-purple-50 rounded-lg overflow-hidden;
  perspective: 1000px;
}

.3d-placeholder {
  @apply relative w-full h-full;
  transform-style: preserve-3d;
}

.3d-axis {
  @apply absolute;
}

.x-axis {
  @apply bottom-4 left-4;
}

.y-axis {
  @apply top-4 left-4;
}

.z-axis {
  @apply top-4 right-4;
}

.axis-label {
  @apply text-sm font-semibold text-gray-600;
}

.3d-point {
  @apply absolute w-16 h-16 bg-white rounded-full shadow-lg cursor-pointer;
  @apply flex flex-col items-center justify-center hover:scale-110 transition-transform;
}

.3d-icon {
  @apply text-lg;
}

.3d-label {
  @apply text-xs font-medium;
}

.3d-controls {
  @apply absolute bottom-4 right-4 flex gap-2;
}

.3d-btn {
  @apply px-3 py-1 bg-white border border-gray-300 rounded hover:bg-gray-50;
}

/* Analysis Panel */
.analysis-panel {
  @apply bg-yellow-50 p-6 rounded-lg;
}

.analysis-title {
  @apply text-lg font-semibold mb-4;
}

.analysis-content {
  @apply space-y-4;
}

.complexity-metrics {
  @apply grid grid-cols-2 md:grid-cols-4 gap-4;
}

.metric-card {
  @apply bg-white p-4 rounded-lg text-center;
}

.metric-label {
  @apply text-sm text-gray-600;
}

.metric-value {
  @apply text-2xl font-bold;
}

.recommendations {
  @apply bg-white p-4 rounded-lg;
}

.rec-title {
  @apply font-semibold mb-3;
}

.recommendation {
  @apply flex items-center gap-3 mb-2;
}

.rec-icon {
  @apply text-xl;
}

.rec-text {
  @apply flex-1;
}

.rec-action {
  @apply px-3 py-1 bg-blue-600 text-white rounded hover:bg-blue-700;
}

/* Item Details */
.item-details {
  @apply bg-blue-50 p-6 rounded-lg;
}

.details-title {
  @apply text-lg font-semibold mb-4;
}

.details-content {
  @apply space-y-4;
}

.detail-grid {
  @apply grid grid-cols-2 gap-4;
}

.detail-item {
  @apply flex justify-between;
}

.detail-label {
  @apply text-gray-600;
}

.detail-value {
  @apply font-medium;
}

.stage-breakdown h6 {
  @apply font-semibold mb-2;
}

.breakdown-items {
  @apply flex flex-wrap gap-2;
}

.breakdown-item {
  @apply flex items-center gap-1 px-2 py-1 bg-white rounded;
}

.breakdown-icon {
  @apply text-sm;
}

.breakdown-name {
  @apply text-sm;
}

.close-details {
  @apply px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700;
}
</style>