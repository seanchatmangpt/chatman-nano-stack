<template>
  <div class="realtime-executor">
    <h3 class="executor-title">⚡ Real-Time Pipeline Executor</h3>
    
    <!-- Execution Controls -->
    <div class="execution-controls">
      <div class="control-panel">
        <button 
          @click="startExecution" 
          :disabled="isExecuting"
          class="control-btn start"
        >
          🚀 Start Pipeline
        </button>
        <button 
          @click="pauseExecution" 
          :disabled="!isExecuting"
          class="control-btn pause"
        >
          ⏸️ Pause
        </button>
        <button 
          @click="stopExecution" 
          :disabled="!isExecuting"
          class="control-btn stop"
        >
          ⏹️ Stop
        </button>
        <button 
          @click="resetExecution" 
          class="control-btn reset"
        >
          🔄 Reset
        </button>
      </div>
      
      <div class="execution-mode">
        <label class="mode-label">Execution Mode:</label>
        <select v-model="executionMode" class="mode-select" :disabled="isExecuting">
          <option value="sequential">Sequential</option>
          <option value="parallel">Parallel</option>
          <option value="adaptive">Adaptive</option>
          <option value="bypass">Bypass Chain</option>
        </select>
      </div>
      
      <div class="speed-control">
        <label class="speed-label">Speed: {{ executionSpeed }}x</label>
        <input 
          type="range" 
          min="0.1" 
          max="3" 
          step="0.1" 
          v-model="executionSpeed"
          class="speed-slider"
          :disabled="isExecuting"
        />
      </div>
    </div>
    
    <!-- Live Pipeline Status -->
    <div class="pipeline-status">
      <div class="status-header">
        <h4 class="status-title">📊 Live Pipeline Status</h4>
        <div class="status-indicators">
          <div :class="['indicator', executionState]">{{ getStateLabel(executionState) }}</div>
          <div class="throughput">{{ throughput }} stages/min</div>
          <div class="eta">ETA: {{ estimatedCompletion }}</div>
        </div>
      </div>
      
      <!-- Real-time Stage Grid -->
      <div class="stage-grid">
        <div 
          v-for="stage in pipelineStages" 
          :key="stage.id"
          :class="['stage-cell', getStageStatus(stage)]"
          @click="focusStage(stage)"
        >
          <div class="stage-icon">{{ stage.icon }}</div>
          <div class="stage-name">{{ stage.name }}</div>
          <div class="stage-progress">
            <div class="progress-bar">
              <div 
                class="progress-fill" 
                :style="{ width: `${stage.progress}%` }"
              ></div>
            </div>
            <span class="progress-text">{{ stage.progress }}%</span>
          </div>
          <div v-if="stage.duration" class="stage-timing">{{ stage.duration }}ms</div>
          <div v-if="stage.error" class="stage-error">⚠️ {{ stage.error }}</div>
        </div>
      </div>
    </div>
    
    <!-- Live Permutation Matrix -->
    <div class="permutation-matrix">
      <h4 class="matrix-title">🔄 Active Permutation Matrix</h4>
      <div class="matrix-grid">
        <div 
          v-for="permutation in activePermutations" 
          :key="permutation.id"
          :class="['permutation-cell', permutation.status]"
        >
          <div class="perm-header">
            <span class="perm-icon">{{ permutation.icon }}</span>
            <span class="perm-name">{{ permutation.name }}</span>
            <span class="perm-status">{{ permutation.status }}</span>
          </div>
          <div class="perm-progress">
            <div class="perm-bar" :style="{ width: `${permutation.progress}%` }"></div>
          </div>
          <div class="perm-metrics">
            <span class="metric">{{ permutation.processed }}/{{ permutation.total }}</span>
            <span class="metric">{{ permutation.speed }}</span>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Real-time Logs -->
    <div class="realtime-logs">
      <h4 class="logs-title">📝 Execution Logs</h4>
      <div class="log-controls">
        <button @click="clearLogs" class="log-btn clear">🗑️ Clear</button>
        <button @click="exportLogs" class="log-btn export">📄 Export</button>
        <label class="log-filter">
          <input type="checkbox" v-model="showDebugLogs"> Show Debug
        </label>
        <label class="log-filter">
          <input type="checkbox" v-model="autoScroll"> Auto Scroll
        </label>
      </div>
      <div ref="logsContainer" class="logs-container">
        <div 
          v-for="(log, index) in filteredLogs" 
          :key="`log-${index}`"
          :class="['log-entry', log.level]"
        >
          <span class="log-timestamp">{{ formatTimestamp(log.timestamp) }}</span>
          <span class="log-level">{{ log.level.toUpperCase() }}</span>
          <span class="log-message">{{ log.message }}</span>
          <span v-if="log.data" class="log-data">{{ JSON.stringify(log.data) }}</span>
        </div>
      </div>
    </div>
    
    <!-- Performance Metrics -->
    <div class="performance-metrics">
      <h4 class="metrics-title">📊 Real-Time Performance</h4>
      <div class="metrics-grid">
        <div class="metric-card cpu">
          <div class="metric-icon">💻</div>
          <div class="metric-content">
            <div class="metric-value">{{ cpuUsage }}%</div>
            <div class="metric-label">CPU Usage</div>
            <div class="metric-trend" :class="cpuTrend">{{ cpuTrend }}</div>
          </div>
        </div>
        
        <div class="metric-card memory">
          <div class="metric-icon">🧠</div>
          <div class="metric-content">
            <div class="metric-value">{{ memoryUsage }}MB</div>
            <div class="metric-label">Memory</div>
            <div class="metric-trend" :class="memoryTrend">{{ memoryTrend }}</div>
          </div>
        </div>
        
        <div class="metric-card bandwidth">
          <div class="metric-icon">📡</div>
          <div class="metric-content">
            <div class="metric-value">{{ bandwidth }}kb/s</div>
            <div class="metric-label">Bandwidth</div>
            <div class="metric-trend" :class="bandwidthTrend">{{ bandwidthTrend }}</div>
          </div>
        </div>
        
        <div class="metric-card errors">
          <div class="metric-icon">⚠️</div>
          <div class="metric-content">
            <div class="metric-value">{{ errorCount }}</div>
            <div class="metric-label">Errors</div>
            <div class="metric-trend" :class="errorTrend">{{ errorTrend }}</div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Stage Details Modal -->
    <div v-if="focusedStage" class="stage-modal-overlay" @click="closeFocus">
      <div class="stage-modal" @click.stop>
        <h5 class="modal-title">{{ focusedStage.icon }} {{ focusedStage.name }} Details</h5>
        <div class="modal-content">
          <div class="stage-info">
            <div class="info-row">
              <span class="info-label">Status:</span>
              <span class="info-value">{{ getStageStatus(focusedStage) }}</span>
            </div>
            <div class="info-row">
              <span class="info-label">Progress:</span>
              <span class="info-value">{{ focusedStage.progress }}%</span>
            </div>
            <div class="info-row">
              <span class="info-label">Duration:</span>
              <span class="info-value">{{ focusedStage.duration || 0 }}ms</span>
            </div>
            <div class="info-row">
              <span class="info-label">Input Size:</span>
              <span class="info-value">{{ focusedStage.inputSize || 'N/A' }}</span>
            </div>
            <div class="info-row">
              <span class="info-label">Output Size:</span>
              <span class="info-value">{{ focusedStage.outputSize || 'N/A' }}</span>
            </div>
          </div>
          
          <div v-if="focusedStage.logs" class="stage-logs">
            <h6>Recent Logs:</h6>
            <div class="modal-logs">
              <div v-for="log in focusedStage.logs" :key="log.id" class="modal-log">
                {{ log.message }}
              </div>
            </div>
          </div>
        </div>
        <button @click="closeFocus" class="modal-close">✕</button>
      </div>
    </div>
  </div>
</template>

<script setup>
// ⚡ ULTRATHINK SWARM 80/20: Real-Time Pipeline Executor
// JavaScript implementation for live pipeline execution visualization

import { ref, computed, onMounted, onUnmounted, nextTick } from 'vue'

// Use pipeline composable
const { transformationState, startVisualization } = usePipelineTransform()

// Execution state
const isExecuting = ref(false)
const executionState = ref('idle') // idle, running, paused, stopped, completed, error
const executionMode = ref('sequential')
const executionSpeed = ref(1.0)
const throughput = ref(0)
const estimatedCompletion = ref('--:--')

// Pipeline stages with real-time data
const pipelineStages = ref([
  { id: 'typer', name: 'Typer', icon: '📝', progress: 0, status: 'pending' },
  { id: 'turtle', name: 'Turtle', icon: '🐢', progress: 0, status: 'pending' },
  { id: 'ttl2dspy', name: 'TTL→DSPy', icon: '🔄', progress: 0, status: 'pending' },
  { id: 'bitactor', name: 'BitActor', icon: '🎭', progress: 0, status: 'pending' },
  { id: 'erlang', name: 'Erlang', icon: '📡', progress: 0, status: 'pending' },
  { id: 'ash', name: 'Ash', icon: '🔥', progress: 0, status: 'pending' },
  { id: 'reactor', name: 'Reactor', icon: '⚛️', progress: 0, status: 'pending' },
  { id: 'k8s', name: 'K8s', icon: '☸️', progress: 0, status: 'pending' }
])

// Active permutations
const activePermutations = ref([
  { 
    id: 'ultra_bypass', 
    name: 'Ultra Bypass', 
    icon: '⚡', 
    status: 'ready',
    progress: 0,
    processed: 0,
    total: 3,
    speed: 'ultra-fast'
  },
  { 
    id: 'parallel_exec', 
    name: 'Parallel Exec', 
    icon: '🔄', 
    status: 'ready',
    progress: 0,
    processed: 0,
    total: 8,
    speed: 'optimized'
  }
])

// Logs
const logs = ref([])
const showDebugLogs = ref(false)
const autoScroll = ref(true)
const logsContainer = ref(null)

// Performance metrics
const cpuUsage = ref(0)
const memoryUsage = ref(0)
const bandwidth = ref(0)
const errorCount = ref(0)
const cpuTrend = ref('stable')
const memoryTrend = ref('stable')
const bandwidthTrend = ref('stable')
const errorTrend = ref('stable')

// Modal
const focusedStage = ref(null)

// Execution intervals
let executionInterval = null
let metricsInterval = null

// Computed
const filteredLogs = computed(() => {
  if (showDebugLogs.value) return logs.value
  return logs.value.filter(log => log.level !== 'debug')
})

// Methods
const startExecution = async () => {
  if (isExecuting.value) return
  
  isExecuting.value = true
  executionState.value = 'running'
  
  addLog('info', 'Pipeline execution started', { mode: executionMode.value, speed: executionSpeed.value })
  
  // Reset all stages
  pipelineStages.value.forEach(stage => {
    stage.progress = 0
    stage.status = 'pending'
    stage.duration = null
    stage.error = null
  })
  
  // Start execution based on mode
  switch (executionMode.value) {
    case 'sequential':
      await executeSequential()
      break
    case 'parallel':
      await executeParallel()
      break
    case 'adaptive':
      await executeAdaptive()
      break
    case 'bypass':
      await executeBypass()
      break
  }
}

const pauseExecution = () => {
  if (executionInterval) {
    clearInterval(executionInterval)
    executionInterval = null
  }
  executionState.value = 'paused'
  addLog('warn', 'Pipeline execution paused')
}

const stopExecution = () => {
  if (executionInterval) {
    clearInterval(executionInterval)
    executionInterval = null
  }
  isExecuting.value = false
  executionState.value = 'stopped'
  addLog('warn', 'Pipeline execution stopped')
}

const resetExecution = () => {
  stopExecution()
  pipelineStages.value.forEach(stage => {
    stage.progress = 0
    stage.status = 'pending'
    stage.duration = null
    stage.error = null
  })
  activePermutations.value.forEach(perm => {
    perm.progress = 0
    perm.processed = 0
    perm.status = 'ready'
  })
  executionState.value = 'idle'
  addLog('info', 'Pipeline execution reset')
}

// Execution modes
const executeSequential = async () => {
  for (let i = 0; i < pipelineStages.value.length; i++) {
    if (!isExecuting.value) break
    
    const stage = pipelineStages.value[i]
    await executeStage(stage, i)
    
    if (stage.error) {
      executionState.value = 'error'
      break
    }
  }
  
  if (isExecuting.value && executionState.value !== 'error') {
    executionState.value = 'completed'
    addLog('success', 'Sequential pipeline execution completed')
  }
  
  isExecuting.value = false
}

const executeParallel = async () => {
  // Simulate parallel execution in batches
  const batches = [
    [0, 1], // typer, turtle
    [2, 3, 4], // ttl2dspy, bitactor, erlang  
    [5, 6], // ash, reactor
    [7] // k8s
  ]
  
  for (const batch of batches) {
    if (!isExecuting.value) break
    
    const promises = batch.map(index => executeStage(pipelineStages.value[index], index))
    await Promise.all(promises)
  }
  
  if (isExecuting.value) {
    executionState.value = 'completed'
    addLog('success', 'Parallel pipeline execution completed')
  }
  
  isExecuting.value = false
}

const executeAdaptive = async () => {
  // Simulate adaptive routing
  addLog('info', 'Adaptive execution: analyzing optimal path...')
  
  // Simple adaptive logic - skip some stages based on "analysis"
  const stages = [0, 1, 5, 6, 7] // Skip ttl2dspy, bitactor, erlang
  
  for (const index of stages) {
    if (!isExecuting.value) break
    await executeStage(pipelineStages.value[index], index)
  }
  
  if (isExecuting.value) {
    executionState.value = 'completed'
    addLog('success', 'Adaptive pipeline execution completed')
  }
  
  isExecuting.value = false
}

const executeBypass = async () => {
  // Simulate bypass execution (ultra-fast)
  addLog('info', 'Bypass execution: ultra-fast transformation...')
  
  const bypassStages = [0, 7] // typer -> k8s directly
  
  for (const index of bypassStages) {
    if (!isExecuting.value) break
    await executeStage(pipelineStages.value[index], index)
  }
  
  if (isExecuting.value) {
    executionState.value = 'completed'
    addLog('success', 'Bypass pipeline execution completed')
  }
  
  isExecuting.value = false
}

const executeStage = async (stage, index) => {
  stage.status = 'running'
  addLog('info', `Starting stage: ${stage.name}`)
  
  const startTime = Date.now()
  const duration = (Math.random() * 2000 + 500) / executionSpeed.value // 500-2500ms base
  
  // Simulate progress
  const progressInterval = setInterval(() => {
    if (stage.status === 'running') {
      stage.progress = Math.min(stage.progress + Math.random() * 15, 100)
      
      if (stage.progress >= 100) {
        clearInterval(progressInterval)
        stage.status = 'completed'
        stage.duration = Date.now() - startTime
        addLog('success', `Completed stage: ${stage.name}`, { duration: stage.duration })
      }
    }
  }, 100)
  
  // Wait for completion or error
  await new Promise(resolve => {
    setTimeout(() => {
      clearInterval(progressInterval)
      
      // Simulate occasional errors
      if (Math.random() < 0.1) {
        stage.status = 'error'
        stage.error = 'Simulated error'
        addLog('error', `Stage failed: ${stage.name}`, { error: stage.error })
      } else {
        stage.status = 'completed'
        stage.progress = 100
        stage.duration = Date.now() - startTime
      }
      
      resolve()
    }, duration)
  })
}

// Utility functions
const getStateLabel = (state) => {
  const labels = {
    idle: 'Idle',
    running: 'Running',
    paused: 'Paused',
    stopped: 'Stopped',
    completed: 'Completed',
    error: 'Error'
  }
  return labels[state] || state
}

const getStageStatus = (stage) => {
  return stage.status || 'pending'
}

const focusStage = (stage) => {
  focusedStage.value = stage
}

const closeFocus = () => {
  focusedStage.value = null
}

const addLog = (level, message, data = null) => {
  logs.value.push({
    id: Date.now() + Math.random(),
    timestamp: new Date(),
    level,
    message,
    data
  })
  
  // Auto-scroll to bottom
  if (autoScroll.value) {
    nextTick(() => {
      if (logsContainer.value) {
        logsContainer.value.scrollTop = logsContainer.value.scrollHeight
      }
    })
  }
  
  // Limit log entries
  if (logs.value.length > 1000) {
    logs.value = logs.value.slice(-1000)
  }
}

const clearLogs = () => {
  logs.value = []
}

const exportLogs = () => {
  const logText = logs.value.map(log => 
    `[${formatTimestamp(log.timestamp)}] ${log.level.toUpperCase()}: ${log.message}`
  ).join('\n')
  
  const blob = new Blob([logText], { type: 'text/plain' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `pipeline-logs-${Date.now()}.txt`
  a.click()
  URL.revokeObjectURL(url)
}

const formatTimestamp = (timestamp) => {
  return timestamp.toLocaleTimeString()
}

// Start metrics simulation
const startMetricsSimulation = () => {
  metricsInterval = setInterval(() => {
    // Simulate realistic metrics
    cpuUsage.value = Math.max(0, Math.min(100, cpuUsage.value + (Math.random() - 0.5) * 10))
    memoryUsage.value = Math.max(0, memoryUsage.value + (Math.random() - 0.5) * 50)
    bandwidth.value = Math.max(0, bandwidth.value + (Math.random() - 0.5) * 100)
    
    if (Math.random() < 0.05) { // 5% chance of error
      errorCount.value++
    }
    
    // Update trends
    cpuTrend.value = Math.random() > 0.5 ? 'up' : 'down'
    memoryTrend.value = Math.random() > 0.5 ? 'up' : 'down'
    bandwidthTrend.value = Math.random() > 0.5 ? 'up' : 'down'
    
    // Calculate throughput
    const completedStages = pipelineStages.value.filter(s => s.status === 'completed').length
    throughput.value = completedStages * 6 // stages per minute approximation
    
    // Estimate completion
    if (isExecuting.value) {
      const totalStages = pipelineStages.value.length
      const remaining = totalStages - completedStages
      const avgTimePerStage = 1000 / executionSpeed.value
      const etaMs = remaining * avgTimePerStage
      const etaMinutes = Math.floor(etaMs / 60000)
      const etaSeconds = Math.floor((etaMs % 60000) / 1000)
      estimatedCompletion.value = `${etaMinutes}:${etaSeconds.toString().padStart(2, '0')}`
    }
  }, 1000)
}

// Lifecycle
onMounted(() => {
  addLog('info', 'Real-time pipeline executor initialized')
  startMetricsSimulation()
})

onUnmounted(() => {
  if (executionInterval) clearInterval(executionInterval)
  if (metricsInterval) clearInterval(metricsInterval)
})
</script>

<style scoped>
.realtime-executor {
  @apply space-y-6;
}

.executor-title {
  @apply text-xl font-bold mb-4;
}

/* Execution Controls */
.execution-controls {
  @apply bg-gray-50 p-4 rounded-lg flex flex-wrap gap-4 items-center;
}

.control-panel {
  @apply flex gap-2;
}

.control-btn {
  @apply px-4 py-2 rounded-lg font-medium transition-all duration-200;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.control-btn.start {
  @apply bg-green-600 text-white hover:bg-green-700;
}

.control-btn.pause {
  @apply bg-yellow-600 text-white hover:bg-yellow-700;
}

.control-btn.stop {
  @apply bg-red-600 text-white hover:bg-red-700;
}

.control-btn.reset {
  @apply bg-gray-600 text-white hover:bg-gray-700;
}

.execution-mode, .speed-control {
  @apply flex items-center gap-2;
}

.mode-label, .speed-label {
  @apply text-sm font-medium;
}

.mode-select {
  @apply px-3 py-1 border rounded;
}

.speed-slider {
  @apply w-20;
}

/* Pipeline Status */
.pipeline-status {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.status-header {
  @apply flex justify-between items-center mb-4;
}

.status-title {
  @apply text-lg font-semibold;
}

.status-indicators {
  @apply flex gap-4 text-sm;
}

.indicator {
  @apply px-3 py-1 rounded-full font-medium;
}

.indicator.idle {
  @apply bg-gray-100 text-gray-800;
}

.indicator.running {
  @apply bg-blue-100 text-blue-800;
}

.indicator.paused {
  @apply bg-yellow-100 text-yellow-800;
}

.indicator.completed {
  @apply bg-green-100 text-green-800;
}

.indicator.error {
  @apply bg-red-100 text-red-800;
}

.throughput, .eta {
  @apply text-gray-600;
}

/* Stage Grid */
.stage-grid {
  @apply grid grid-cols-2 md:grid-cols-4 gap-4;
}

.stage-cell {
  @apply p-4 rounded-lg border-2 cursor-pointer transition-all duration-200;
}

.stage-cell.pending {
  @apply border-gray-200 bg-gray-50;
}

.stage-cell.running {
  @apply border-blue-400 bg-blue-50 animate-pulse;
}

.stage-cell.completed {
  @apply border-green-400 bg-green-50;
}

.stage-cell.error {
  @apply border-red-400 bg-red-50;
}

.stage-icon {
  @apply text-2xl text-center mb-2;
}

.stage-name {
  @apply text-sm font-semibold text-center mb-2;
}

.stage-progress {
  @apply space-y-1;
}

.progress-bar {
  @apply w-full h-2 bg-gray-200 rounded-full overflow-hidden;
}

.progress-fill {
  @apply h-full bg-blue-600 transition-all duration-300;
}

.progress-text {
  @apply text-xs text-center;
}

.stage-timing {
  @apply text-xs text-gray-600 text-center mt-1;
}

.stage-error {
  @apply text-xs text-red-600 text-center mt-1;
}

/* Permutation Matrix */
.permutation-matrix {
  @apply bg-purple-50 p-6 rounded-lg;
}

.matrix-title {
  @apply text-lg font-semibold mb-4;
}

.matrix-grid {
  @apply grid grid-cols-1 md:grid-cols-2 gap-4;
}

.permutation-cell {
  @apply bg-white p-4 rounded-lg border-2;
}

.permutation-cell.ready {
  @apply border-gray-300;
}

.permutation-cell.running {
  @apply border-purple-400 bg-purple-50;
}

.permutation-cell.completed {
  @apply border-green-400 bg-green-50;
}

.perm-header {
  @apply flex justify-between items-center mb-2;
}

.perm-icon {
  @apply text-xl;
}

.perm-name {
  @apply font-semibold;
}

.perm-status {
  @apply text-sm text-gray-600;
}

.perm-progress {
  @apply w-full h-2 bg-gray-200 rounded-full overflow-hidden mb-2;
}

.perm-bar {
  @apply h-full bg-purple-600 transition-all duration-300;
}

.perm-metrics {
  @apply flex justify-between text-sm text-gray-600;
}

/* Real-time Logs */
.realtime-logs {
  @apply bg-gray-900 text-gray-100 p-4 rounded-lg;
}

.logs-title {
  @apply text-lg font-semibold mb-3 text-white;
}

.log-controls {
  @apply flex gap-4 items-center mb-3 text-sm;
}

.log-btn {
  @apply px-3 py-1 bg-gray-700 hover:bg-gray-600 rounded;
}

.log-filter {
  @apply flex items-center gap-1;
}

.logs-container {
  @apply h-64 overflow-y-auto bg-black p-3 rounded font-mono text-sm;
}

.log-entry {
  @apply mb-1 flex gap-2;
}

.log-entry.info {
  @apply text-blue-300;
}

.log-entry.success {
  @apply text-green-300;
}

.log-entry.warn {
  @apply text-yellow-300;
}

.log-entry.error {
  @apply text-red-300;
}

.log-entry.debug {
  @apply text-gray-500;
}

.log-timestamp {
  @apply text-gray-500 w-20;
}

.log-level {
  @apply w-16 font-semibold;
}

.log-message {
  @apply flex-1;
}

.log-data {
  @apply text-gray-400 text-xs;
}

/* Performance Metrics */
.performance-metrics {
  @apply bg-gray-50 p-6 rounded-lg;
}

.metrics-title {
  @apply text-lg font-semibold mb-4;
}

.metrics-grid {
  @apply grid grid-cols-2 md:grid-cols-4 gap-4;
}

.metric-card {
  @apply bg-white p-4 rounded-lg flex items-center gap-3;
}

.metric-icon {
  @apply text-2xl;
}

.metric-content {
  @apply flex-1;
}

.metric-value {
  @apply text-xl font-bold;
}

.metric-label {
  @apply text-sm text-gray-600;
}

.metric-trend {
  @apply text-xs font-medium;
}

.metric-trend.up {
  @apply text-green-600;
}

.metric-trend.down {
  @apply text-red-600;
}

.metric-trend.stable {
  @apply text-gray-600;
}

/* Stage Modal */
.stage-modal-overlay {
  @apply fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50;
}

.stage-modal {
  @apply bg-white p-6 rounded-lg max-w-md w-full mx-4 relative;
}

.modal-title {
  @apply text-lg font-semibold mb-4;
}

.modal-content {
  @apply space-y-4;
}

.stage-info {
  @apply space-y-2;
}

.info-row {
  @apply flex justify-between;
}

.info-label {
  @apply text-gray-600;
}

.info-value {
  @apply font-medium;
}

.stage-logs h6 {
  @apply font-semibold mb-2;
}

.modal-logs {
  @apply bg-gray-100 p-3 rounded max-h-32 overflow-y-auto;
}

.modal-log {
  @apply text-sm mb-1;
}

.modal-close {
  @apply absolute top-2 right-2 w-8 h-8 flex items-center justify-center;
  @apply bg-gray-200 hover:bg-gray-300 rounded-full;
}
</style>