<!-- BitActor Nuxt.js Cybersecurity Components Variant -->
<!-- This variant explores Vue component patterns with:
  - Real-time cybersecurity data visualization
  - TTL-aware component rendering
  - Interactive BitActor pipeline monitoring
  - Threat detection visualization with TTL constraints
  - Security metrics dashboard components
  - Responsive cybersecurity event displays
-->

<template>
  <div id="app" class="cybersecurity-dashboard">
    <!-- Main Dashboard Layout -->
    <CyberSecurityLayout>
      <template #header>
        <TTLMetricsHeader 
          :ttl-budget="globalTTLBudget"
          :current-utilization="currentTTLUtilization"
          :violation-count="ttlViolationCount"
          @ttl-budget-changed="updateTTLBudget"
        />
      </template>
      
      <template #sidebar>
        <PipelineStagesNavigation 
          :stages="pipelineStages"
          :active-stage="activeStage"
          :stage-statuses="stageStatuses"
          @stage-selected="selectStage"
        />
      </template>
      
      <template #main>
        <!-- Real-time Pipeline Visualization -->
        <div class="pipeline-visualization">
          <BitActorPipelineFlow
            :stages="pipelineStages"
            :execution-data="executionData"
            :ttl-constraints="ttlConstraints"
            :real-time="true"
            @stage-clicked="onStageClicked"
            @ttl-violation="onTTLViolation"
          />
        </div>
        
        <!-- Threat Detection Components -->
        <div class="threat-detection-grid">
          <ThreatDetectionComponent
            v-for="detector in threatDetectors"
            :key="detector.id"
            :detector="detector"
            :ttl-budget="detector.ttlBudget"
            :real-time-data="threatData[detector.id]"
            @threat-detected="onThreatDetected"
            @ttl-exceeded="onDetectorTTLExceeded"
          />
        </div>
        
        <!-- Security Metrics Dashboard -->
        <div class="security-metrics">
          <SecurityMetricsDashboard
            :metrics="securityMetrics"
            :ttl-performance="ttlPerformanceMetrics"
            :time-range="selectedTimeRange"
            @metric-drill-down="onMetricDrillDown"
          />
        </div>
        
        <!-- WebSocket Event Stream -->
        <div class="event-stream">
          <RealtimeEventStream
            :events="realtimeEvents"
            :max-events="100"
            :ttl-aware="true"
            @event-clicked="onEventClicked"
          />
        </div>
      </template>
      
      <template #footer>
        <SystemStatusFooter
          :connection-status="websocketStatus"
          :pipeline-health="pipelineHealth"
          :ttl-system-status="ttlSystemStatus"
        />
      </template>
    </CyberSecurityLayout>
  </div>
</template>

<script setup>
// =============================================================================
// Component Imports and Setup
// =============================================================================

// Import BitActor composables
const { executePipeline, ttlMetrics } = useBitActorSSR()
const { connectionStatus, executionResults } = useWebSocketPipeline()
const { realtimeMetrics, updateTTLMetrics } = useRealtimeTTL()

// Global reactive state
const globalTTLBudget = ref(8) // 8ms default
const currentTTLUtilization = ref(0)
const ttlViolationCount = ref(0)
const activeStage = ref('typer')
const selectedTimeRange = ref('1h')

// Pipeline configuration
const pipelineStages = ref([
  {
    id: 'typer',
    name: 'Python Types',
    description: 'Generate Pydantic types with TTL constraints',
    ttlBudget: 2,
    status: 'ready',
    color: '#3b82f6'
  },
  {
    id: 'turtle',
    name: 'TTL Ontology',
    description: 'RDF/Turtle ontology generation',
    ttlBudget: 1,
    status: 'ready',
    color: '#8b5cf6'
  },
  {
    id: 'ttl2dspy',
    name: 'TTL Parser',
    description: 'Parse TTL constraints with precision',
    ttlBudget: 3,
    status: 'ready',
    color: '#10b981'
  },
  {
    id: 'bitactor',
    name: 'BitActor DSL',
    description: 'Generate BitActor DSL modules',
    ttlBudget: 2,
    status: 'ready',
    color: '#f59e0b'
  },
  {
    id: 'erlang',
    name: 'Erlang GenServer',
    description: 'OTP GenServer with TTL enforcement',
    ttl Budget: 1,
    status: 'ready',
    color: '#ef4444'
  },
  {
    id: 'ash',
    name: 'Ash Resources',
    description: 'Ash framework resources with TTL attributes',
    ttlBudget: 2,
    status: 'ready',
    color: '#06b6d4'
  },
  {
    id: 'reactor',
    name: 'Reactor Workflows',
    description: 'Reactor workflow orchestration',
    ttlBudget: 3,
    status: 'ready',
    color: '#84cc16'
  },
  {
    id: 'k8s',
    name: 'K8s Deployment',
    description: 'Kubernetes manifests with TTL config',
    ttlBudget: 1,
    status: 'ready',
    color: '#6366f1'
  }
])

// Threat detection setup
const threatDetectors = ref([
  {
    id: 'malware-detector',
    name: 'Malware Detection',
    ttlBudget: 5,
    enabled: true,
    sensitivity: 'high'
  },
  {
    id: 'intrusion-detector',
    name: 'Intrusion Detection',
    ttlBudget: 8,
    enabled: true,
    sensitivity: 'medium'
  },
  {
    id: 'anomaly-detector',
    name: 'Anomaly Detection',
    ttlBudget: 12,
    enabled: true,
    sensitivity: 'low'
  }
])

// Reactive data
const executionData = ref([])
const ttlConstraints = ref({})
const stageStatuses = ref({})
const threatData = ref({})
const securityMetrics = ref({})
const ttlPerformanceMetrics = ref({})
const realtimeEvents = ref([])
const websocketStatus = ref('disconnected')
const pipelineHealth = ref('healthy')
const ttlSystemStatus = ref('operational')

// =============================================================================
// Event Handlers
// =============================================================================

const updateTTLBudget = (newBudget) => {
  globalTTLBudget.value = newBudget
  
  // Update all stage TTL budgets proportionally
  const totalOriginalBudget = pipelineStages.value.reduce((sum, stage) => sum + stage.ttlBudget, 0)
  const scaleFactor = newBudget / totalOriginalBudget
  
  pipelineStages.value.forEach(stage => {
    stage.ttlBudget = Math.max(1, Math.round(stage.ttlBudget * scaleFactor))
  })
}

const selectStage = (stageId) => {
  activeStage.value = stageId
  
  // Load stage-specific data
  loadStageData(stageId)
}

const onStageClicked = (stage) => {
  selectStage(stage.id)
  
  // Show stage details in modal or side panel
  showStageDetails(stage)
}

const onTTLViolation = (violation) => {
  ttlViolationCount.value++
  
  // Add violation to events
  realtimeEvents.value.unshift({
    id: Date.now(),
    type: 'ttl_violation',
    message: `TTL violation in ${violation.stage}: ${violation.actualTime}ms > ${violation.budgetTime}ms`,
    severity: 'warning',
    timestamp: new Date(),
    stage: violation.stage
  })
  
  // Update TTL system status
  if (ttlViolationCount.value > 10) {
    ttlSystemStatus.value = 'degraded'
  }
}

const onThreatDetected = (threat) => {
  // Add threat to realtime events
  realtimeEvents.value.unshift({
    id: Date.now(),
    type: 'threat_detected',
    message: `${threat.type} detected: ${threat.description}`,
    severity: threat.severity,
    timestamp: new Date(),
    detector: threat.detectorId,
    data: threat
  })
  
  // Update security metrics
  securityMetrics.value.threatsDetected = (securityMetrics.value.threatsDetected || 0) + 1
}

const onDetectorTTLExceeded = (detector) => {
  console.warn(`Detector ${detector.name} exceeded TTL budget`)
  
  // Temporarily disable detector if TTL violations are frequent
  const detectorRef = threatDetectors.value.find(d => d.id === detector.id)
  if (detectorRef && detector.violationCount > 5) {
    detectorRef.enabled = false
    
    realtimeEvents.value.unshift({
      id: Date.now(),
      type: 'detector_disabled',
      message: `${detector.name} disabled due to TTL violations`,
      severity: 'error',
      timestamp: new Date(),
      detector: detector.id
    })
  }
}

const onMetricDrillDown = (metric) => {
  // Navigate to detailed metric view
  navigateTo(`/metrics/${metric.id}`)
}

const onEventClicked = (event) => {
  // Show event details
  showEventDetails(event)
}

// =============================================================================
// Data Loading Functions
// =============================================================================

const loadStageData = async (stageId) => {
  const stage = pipelineStages.value.find(s => s.id === stageId)
  if (!stage) return
  
  stage.status = 'loading'
  
  try {
    // Simulate loading stage-specific data
    await new Promise(resolve => setTimeout(resolve, 100))
    
    // Update stage status and data
    stage.status = 'ready'
    
    // Load TTL constraints for this stage
    ttlConstraints.value[stageId] = {
      budgetMs: stage.ttlBudget,
      precisionLevel: 'nanosecond',
      enforcementMode: 'strict',
      lastViolation: null
    }
    
  } catch (error) {
    stage.status = 'error'
    console.error(`Failed to load data for stage ${stageId}:`, error)
  }
}

const loadSecurityMetrics = async () => {
  try {
    // Simulate loading security metrics
    securityMetrics.value = {
      threatsDetected: Math.floor(Math.random() * 50),
      threatsBlocked: Math.floor(Math.random() * 45),
      falsePositives: Math.floor(Math.random() * 5),
      systemUptime: '99.8%',
      averageResponseTime: '2.3ms',
      ttlCompliance: '98.5%'
    }
    
    // TTL-specific performance metrics
    ttlPerformanceMetrics.value = {
      averageExecutionTime: 6.2,
      ttlBudgetUtilization: 77.5,
      violationRate: 1.5,
      fastest Stage: 'turtle',
      slowestStage: 'reactor',
      totalPipelineExecutions: 1247
    }
    
  } catch (error) {
    console.error('Failed to load security metrics:', error)
  }
}

// =============================================================================
// Lifecycle and Watchers
// =============================================================================

// Initialize component data
onMounted(async () => {
  await loadSecurityMetrics()
  
  // Start periodic updates
  const metricsInterval = setInterval(loadSecurityMetrics, 5000)
  const ttlUpdateInterval = setInterval(updateTTLUtilization, 1000)
  
  onUnmounted(() => {
    clearInterval(metricsInterval)
    clearInterval(ttlUpdateInterval)
  })
})

// Watch for WebSocket connection changes
watch(connectionStatus, (newStatus) => {
  websocketStatus.value = newStatus
  
  if (newStatus === 'connected') {
    pipelineHealth.value = 'healthy'
  } else if (newStatus === 'error') {
    pipelineHealth.value = 'unhealthy'
  }
})

// Watch for TTL metrics updates
watch(ttlMetrics, (newMetrics) => {
  currentTTLUtilization.value = newMetrics.utilizationPercent || 0
}, { deep: true })

// Utility functions
const updateTTLUtilization = () => {
  // Calculate current TTL utilization across all active processes
  const activeExecutions = executionResults.value.filter(r => r.status === 'running')
  
  if (activeExecutions.length > 0) {
    const totalUsed = activeExecutions.reduce((sum, exec) => sum + (exec.timeUsed || 0), 0)
    const totalBudget = activeExecutions.reduce((sum, exec) => sum + (exec.timeBudget || 8), 0)
    
    currentTTLUtilization.value = (totalUsed / totalBudget) * 100
  }
}

const showStageDetails = (stage) => {
  // Implementation for showing stage details modal
  console.log('Showing details for stage:', stage)
}

const showEventDetails = (event) => {
  // Implementation for showing event details modal
  console.log('Showing details for event:', event)
}

// Provide global state to child components
provide('globalTTLBudget', globalTTLBudget)
provide('ttlViolationCount', ttlViolationCount)
provide('pipelineStages', pipelineStages)
</script>

<!-- =============================================================================
     Component Definitions
     ============================================================================= -->

<!-- TTL Metrics Header Component -->
<template name="TTLMetricsHeaderComponent">
  <div class="ttl-metrics-header">
    <div class="ttl-budget-control">
      <label for="ttl-budget">Global TTL Budget (ms):</label>
      <input
        id="ttl-budget"
        v-model.number="localTTLBudget"
        type="range"
        min="1"
        max="50"
        step="1"
        class="ttl-slider"
        @input="$emit('ttl-budget-changed', localTTLBudget)"
      >
      <span class="ttl-value">{{ localTTLBudget }}ms</span>
    </div>
    
    <div class="ttl-utilization">
      <div class="utilization-bar">
        <div 
          class="utilization-fill"
          :class="utilizationClass"
          :style="{ width: `${Math.min(currentUtilization, 100)}%` }"
        ></div>
      </div>
      <span class="utilization-text">{{ currentUtilization.toFixed(1) }}% utilized</span>
    </div>
    
    <div class="violation-counter">
      <span class="violation-count">{{ violationCount }}</span>
      <span class="violation-label">TTL Violations</span>
    </div>
  </div>
</template>

<script setup name="TTLMetricsHeader">
defineProps({
  ttlBudget: {
    type: Number,
    default: 8
  },
  currentUtilization: {
    type: Number,
    default: 0
  },
  violationCount: {
    type: Number,
    default: 0
  }
})

defineEmits(['ttl-budget-changed'])

const localTTLBudget = ref(8)

const utilizationClass = computed(() => {
  if (currentUtilization.value > 90) return 'critical'
  if (currentUtilization.value > 75) return 'warning'
  return 'normal'
})
</script>

<!-- BitActor Pipeline Flow Component -->
<template name="BitActorPipelineFlowComponent">
  <div class="pipeline-flow">
    <svg class="pipeline-svg" viewBox="0 0 1000 200">
      <!-- Pipeline stages as connected nodes -->
      <g
        v-for="(stage, index) in stages"
        :key="stage.id"
        class="pipeline-stage"
        :transform="`translate(${index * 120 + 50}, 100)`"
        @click="$emit('stage-clicked', stage)"
      >
        <!-- Stage circle -->
        <circle
          :r="20"
          :fill="stage.color"
          :class="[
            'stage-circle',
            `status-${stage.status}`,
            { 'ttl-violation': hasTTLViolation(stage) }
          ]"
        />
        
        <!-- Stage label -->
        <text
          y="35"
          text-anchor="middle"
          class="stage-label"
        >
          {{ stage.name }}
        </text>
        
        <!-- TTL budget indicator -->
        <text
          y="50"
          text-anchor="middle"
          class="ttl-budget"
        >
          {{ stage.ttlBudget }}ms
        </text>
        
        <!-- Connection line to next stage -->
        <line
          v-if="index < stages.length - 1"
          x1="20"
          y1="0"
          x2="100"
          y2="0"
          class="connection-line"
          :class="{ 'active': isConnectionActive(index) }"
        />
        
        <!-- Execution progress indicator -->
        <circle
          v-if="isStageExecuting(stage)"
          r="25"
          fill="none"
          stroke="#3b82f6"
          stroke-width="2"
          class="execution-indicator"
        >
          <animate
            attributeName="stroke-dasharray"
            values="0 157;78.5 78.5;157 0"
            dur="2s"
            repeatCount="indefinite"
          />
        </circle>
      </g>
      
      <!-- Real-time data flow visualization -->
      <g v-if="realTime && executionData.length > 0">
        <circle
          v-for="(data, index) in executionData"
          :key="data.id"
          :cx="data.x"
          :cy="100"
          r="3"
          fill="#10b981"
          class="data-flow-dot"
        >
          <animate
            attributeName="cx"
            :values="`${data.startX};${data.endX}`"
            :dur="`${data.duration}s`"
            repeatCount="indefinite"
          />
        </circle>
      </g>
    </svg>
    
    <!-- TTL constraint display -->
    <div class="ttl-constraints-display">
      <h4>TTL Constraints</h4>
      <div class="constraints-grid">
        <div
          v-for="(constraint, stageId) in ttlConstraints"
          :key="stageId"
          class="constraint-item"
        >
          <span class="stage-name">{{ getStageNameById(stageId) }}</span>
          <span class="constraint-budget">{{ constraint.budgetMs }}ms</span>
          <span 
            class="constraint-status"
            :class="constraint.lastViolation ? 'violated' : 'compliant'"
          >
            {{ constraint.lastViolation ? 'VIOLATED' : 'OK' }}
          </span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup name="BitActorPipelineFlow">
defineProps({
  stages: {
    type: Array,
    required: true
  },
  executionData: {
    type: Array,
    default: () => []
  },
  ttlConstraints: {
    type: Object,
    default: () => ({})
  },
  realTime: {
    type: Boolean,
    default: false
  }
})

defineEmits(['stage-clicked', 'ttl-violation'])

const hasTTLViolation = (stage) => {
  const constraint = ttlConstraints.value[stage.id]
  return constraint && constraint.lastViolation
}

const isConnectionActive = (index) => {
  // Check if data is flowing between stages
  return executionData.value.some(data => 
    data.currentStageIndex === index && data.status === 'flowing'
  )
}

const isStageExecuting = (stage) => {
  return executionData.value.some(data => 
    data.currentStage === stage.id && data.status === 'executing'
  )
}

const getStageNameById = (stageId) => {
  const stage = stages.value.find(s => s.id === stageId)
  return stage ? stage.name : stageId
}
</script>

<!-- Threat Detection Component -->
<template name="ThreatDetectionComponent">
  <div class="threat-detector" :class="{ disabled: !detector.enabled }">
    <div class="detector-header">
      <h3>{{ detector.name }}</h3>
      <div class="detector-controls">
        <button 
          @click="toggleDetector"
          :class="detector.enabled ? 'enabled' : 'disabled'"
        >
          {{ detector.enabled ? 'ON' : 'OFF' }}
        </button>
      </div>
    </div>
    
    <div class="detector-status">
      <div class="ttl-budget-indicator">
        <span class="label">TTL Budget:</span>
        <span class="value">{{ detector.ttlBudget }}ms</span>
        <div class="budget-bar">
          <div 
            class="budget-used"
            :style="{ width: `${ttlUtilization}%` }"
            :class="ttlStatusClass"
          ></div>
        </div>
      </div>
      
      <div class="sensitivity-control">
        <label>Sensitivity:</label>
        <select v-model="detector.sensitivity">
          <option value="low">Low</option>
          <option value="medium">Medium</option>
          <option value="high">High</option>
        </select>
      </div>
    </div>
    
    <div class="threat-display">
      <div v-if="currentThreat" class="threat-alert">
        <div class="threat-severity" :class="`severity-${currentThreat.severity}`">
          {{ currentThreat.severity.toUpperCase() }}
        </div>
        <div class="threat-details">
          <h4>{{ currentThreat.type }}</h4>
          <p>{{ currentThreat.description }}</p>
          <small>Detected: {{ formatTime(currentThreat.timestamp) }}</small>
        </div>
      </div>
      
      <div v-else class="no-threats">
        <span class="status-icon">✓</span>
        <span>No threats detected</span>
      </div>
    </div>
    
    <div class="detector-metrics">
      <div class="metric">
        <span class="metric-value">{{ detectorMetrics.threatsDetected }}</span>
        <span class="metric-label">Threats</span>
      </div>
      <div class="metric">
        <span class="metric-value">{{ detectorMetrics.avgResponseTime }}ms</span>
        <span class="metric-label">Avg Response</span>
      </div>
      <div class="metric">
        <span class="metric-value">{{ detectorMetrics.ttlViolations }}</span>
        <span class="metric-label">TTL Violations</span>
      </div>
    </div>
  </div>
</template>

<script setup name="ThreatDetectionComponent">
defineProps({
  detector: {
    type: Object,
    required: true
  },
  ttlBudget: {
    type: Number,
    required: true
  },
  realTimeData: {
    type: Object,
    default: () => ({})
  }
})

defineEmits(['threat-detected', 'ttl-exceeded'])

const currentThreat = ref(null)
const ttlUtilization = ref(0)
const detectorMetrics = ref({
  threatsDetected: 0,
  avgResponseTime: 0,
  ttlViolations: 0
})

const ttlStatusClass = computed(() => {
  if (ttlUtilization.value > 90) return 'critical'
  if (ttlUtilization.value > 75) return 'warning'
  return 'normal'
})

const toggleDetector = () => {
  detector.value.enabled = !detector.value.enabled
}

const formatTime = (timestamp) => {
  return new Date(timestamp).toLocaleTimeString()
}

// Simulate threat detection
onMounted(() => {
  const detectionInterval = setInterval(() => {
    if (detector.value.enabled && Math.random() < 0.1) {
      const threat = {
        id: Date.now(),
        type: ['Malware', 'Intrusion', 'Anomaly'][Math.floor(Math.random() * 3)],
        description: 'Suspicious activity detected in network traffic',
        severity: ['low', 'medium', 'high'][Math.floor(Math.random() * 3)],
        timestamp: Date.now()
      }
      
      currentThreat.value = threat
      detectorMetrics.value.threatsDetected++
      
      $emit('threat-detected', { ...threat, detectorId: detector.value.id })
      
      // Clear threat after 5 seconds
      setTimeout(() => {
        currentThreat.value = null
      }, 5000)
    }
  }, 2000)
  
  onUnmounted(() => {
    clearInterval(detectionInterval)
  })
})
</script>

<!-- Styles -->
<style scoped>
.cybersecurity-dashboard {
  font-family: 'Inter', -apple-system, sans-serif;
  background: #0f172a;
  color: #e2e8f0;
  min-height: 100vh;
}

.ttl-metrics-header {
  display: flex;
  align-items: center;
  gap: 2rem;
  padding: 1rem 2rem;
  background: #1e293b;
  border-bottom: 1px solid #334155;
}

.ttl-budget-control {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.ttl-slider {
  width: 120px;
}

.utilization-bar {
  width: 200px;
  height: 8px;
  background: #334155;
  border-radius: 4px;
  overflow: hidden;
}

.utilization-fill {
  height: 100%;
  transition: width 0.3s ease;
}

.utilization-fill.normal {
  background: #10b981;
}

.utilization-fill.warning {
  background: #f59e0b;
}

.utilization-fill.critical {
  background: #ef4444;
}

.violation-counter {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.violation-count {
  font-size: 1.5rem;
  font-weight: bold;
  color: #ef4444;
}

.pipeline-flow {
  background: #1e293b;
  border-radius: 8px;
  padding: 1rem;
  margin-bottom: 2rem;
}

.pipeline-svg {
  width: 100%;
  height: 200px;
}

.stage-circle {
  cursor: pointer;
  transition: all 0.3s ease;
}

.stage-circle:hover {
  r: 25;
}

.stage-circle.ttl-violation {
  stroke: #ef4444;
  stroke-width: 3;
  animation: pulse 1s infinite;
}

.connection-line {
  stroke: #475569;
  stroke-width: 2;
}

.connection-line.active {
  stroke: #3b82f6;
  stroke-width: 3;
}

.execution-indicator {
  stroke-dasharray: 0 157;
}

.threat-detector {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  padding: 1rem;
  margin-bottom: 1rem;
}

.threat-detector.disabled {
  opacity: 0.6;
}

.detector-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.detector-controls button {
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  border: none;
  font-weight: bold;
  cursor: pointer;
}

.detector-controls button.enabled {
  background: #10b981;
  color: white;
}

.detector-controls button.disabled {
  background: #6b7280;
  color: #d1d5db;
}

.budget-bar {
  width: 100px;
  height: 4px;
  background: #334155;
  border-radius: 2px;
  overflow: hidden;
}

.budget-used {
  height: 100%;
  transition: width 0.3s ease;
}

.budget-used.normal {
  background: #10b981;
}

.budget-used.warning {
  background: #f59e0b;
}

.budget-used.critical {
  background: #ef4444;
}

.threat-alert {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  background: rgba(239, 68, 68, 0.1);
  border: 1px solid #ef4444;
  border-radius: 6px;
  margin: 1rem 0;
}

.threat-severity {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: bold;
}

.severity-low {
  background: #10b981;
  color: white;
}

.severity-medium {
  background: #f59e0b;
  color: white;
}

.severity-high {
  background: #ef4444;
  color: white;
}

.detector-metrics {
  display: flex;
  gap: 1rem;
  margin-top: 1rem;
}

.metric {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 0.5rem;
  background: #334155;
  border-radius: 4px;
  flex: 1;
}

.metric-value {
  font-size: 1.25rem;
  font-weight: bold;
  color: #3b82f6;
}

.metric-label {
  font-size: 0.75rem;
  color: #94a3b8;
}

@keyframes pulse {
  0%, 100% {
    opacity: 1;
  }
  50% {
    opacity: 0.5;
  }
}</style>