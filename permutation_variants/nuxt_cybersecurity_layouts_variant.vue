<!-- BitActor Nuxt.js Cybersecurity Layouts Variant -->
<!-- This variant explores Nuxt layout patterns with:
     - Multi-layout cybersecurity dashboard structure
     - TTL-aware layout rendering and responsiveness
     - Real-time layout adaptation based on threat levels
     - Responsive grid systems for security monitoring
     - Layout-level TTL constraint management
     - Dynamic layout switching based on operational context
-->

<!-- =============================================================================
     Main Cybersecurity Dashboard Layout
     ============================================================================= -->
<template name="cybersecurity-dashboard">
  <div class="cybersecurity-dashboard-layout">
    <!-- Global TTL Status Bar -->
    <div class="ttl-status-bar" :class="ttlStatusClass">
      <div class="ttl-indicator">
        <span class="ttl-label">Global TTL Budget:</span>
        <span class="ttl-value">{{ globalTTLBudget }}ms</span>
        <div class="ttl-usage-bar">
          <div 
            class="ttl-usage-fill"
            :style="{ width: `${ttlUsagePercentage}%` }"
            :class="ttlUsageClass"
          ></div>
        </div>
      </div>
      <div class="threat-level-indicator">
        <span class="threat-label">Threat Level:</span>
        <span class="threat-value" :class="threatLevelClass">{{ currentThreatLevel }}</span>
      </div>
      <div class="swarm-status-indicator">
        <span class="swarm-label">Active Swarms:</span>
        <span class="swarm-count">{{ activeSwarmCount }}</span>
      </div>
    </div>

    <!-- Main Navigation Header -->
    <header class="dashboard-header">
      <nav class="main-navigation">
        <div class="nav-brand">
          <h1 class="brand-title">
            <span class="brand-icon">🛡️</span>
            BitActor CyberSec
          </h1>
        </div>
        
        <div class="nav-menu">
          <NuxtLink 
            v-for="item in navigationItems" 
            :key="item.path"
            :to="item.path"
            class="nav-item"
            :class="{ active: $route.path === item.path }"
          >
            <span class="nav-icon">{{ item.icon }}</span>
            <span class="nav-label">{{ item.label }}</span>
          </NuxtLink>
        </div>
        
        <div class="nav-actions">
          <button class="emergency-stop-btn" @click="emergencyStop">
            🚨 Emergency Stop
          </button>
          <div class="system-health" :class="systemHealthClass">
            {{ systemHealth }}
          </div>
        </div>
      </nav>
    </header>

    <!-- Sidebar for Pipeline and Swarm Controls -->
    <aside class="dashboard-sidebar">
      <div class="sidebar-section">
        <h3 class="section-title">Pipeline Control</h3>
        <div class="pipeline-controls">
          <div class="pipeline-stage" 
               v-for="stage in pipelineStages" 
               :key="stage.id"
               :class="{ active: stage.active, error: stage.hasError }">
            <div class="stage-indicator"></div>
            <span class="stage-name">{{ stage.name }}</span>
            <span class="stage-ttl">{{ stage.ttlBudget }}ms</span>
          </div>
        </div>
      </div>
      
      <div class="sidebar-section">
        <h3 class="section-title">Swarm Coordination</h3>
        <div class="swarm-controls">
          <div class="swarm-item"
               v-for="swarm in activeSwarms"
               :key="swarm.id"
               :class="{ healthy: swarm.healthy, warning: swarm.warning, critical: swarm.critical }">
            <div class="swarm-status-dot"></div>
            <span class="swarm-name">{{ swarm.name }}</span>
            <span class="swarm-agents">{{ swarm.agentCount }} agents</span>
          </div>
        </div>
      </div>
      
      <div class="sidebar-section">
        <h3 class="section-title">Quick Actions</h3>
        <div class="quick-actions">
          <button class="action-btn primary" @click="runFullPipeline">
            ⚡ Run Pipeline
          </button>
          <button class="action-btn secondary" @click="coordinateSwarms">
            🐝 Coordinate Swarms
          </button>
          <button class="action-btn tertiary" @click="viewLogs">
            📋 View Logs
          </button>
        </div>
      </div>
    </aside>

    <!-- Main Content Area -->
    <main class="dashboard-main">
      <!-- TTL-aware content rendering -->
      <div class="content-wrapper" :class="{ 'ttl-constrained': isTTLConstrained }">
        <slot />
      </div>
    </main>

    <!-- Real-time Status Footer -->
    <footer class="dashboard-footer">
      <div class="status-metrics">
        <div class="metric">
          <span class="metric-label">Pipeline Executions:</span>
          <span class="metric-value">{{ totalPipelineExecutions }}</span>
        </div>
        <div class="metric">
          <span class="metric-label">TTL Violations:</span>
          <span class="metric-value error">{{ totalTTLViolations }}</span>
        </div>
        <div class="metric">
          <span class="metric-label">Threats Detected:</span>
          <span class="metric-value warning">{{ threatsDetected }}</span>
        </div>
        <div class="metric">
          <span class="metric-label">System Uptime:</span>
          <span class="metric-value">{{ systemUptime }}</span>
        </div>
      </div>
      
      <div class="connection-status">
        <div class="ws-status" :class="websocketStatus">
          <span class="ws-indicator"></span>
          WebSocket: {{ websocketStatus }}
        </div>
      </div>
    </footer>
  </div>
</template>

<!-- =============================================================================
     Real-time Monitoring Layout
     ============================================================================= -->
<template name="monitoring-layout">
  <div class="monitoring-layout">
    <!-- Monitoring Header with TTL Controls -->
    <div class="monitoring-header">
      <div class="monitoring-title">
        <h2>🔍 Real-time Security Monitoring</h2>
        <div class="monitoring-controls">
          <label class="ttl-control">
            TTL Budget: 
            <input 
              v-model.number="monitoringTTLBudget" 
              type="range" 
              min="1" 
              max="20" 
              step="1"
              @change="updateMonitoringTTL"
            >
            <span>{{ monitoringTTLBudget }}ms</span>
          </label>
          <button class="pause-monitoring-btn" @click="toggleMonitoring">
            {{ isMonitoring ? '⏸️ Pause' : '▶️ Start' }} Monitoring
          </button>
        </div>
      </div>
    </div>

    <!-- Multi-panel Monitoring Grid -->
    <div class="monitoring-grid">
      <!-- Real-time Threat Map -->
      <div class="monitoring-panel threat-map-panel">
        <div class="panel-header">
          <h3>🗺️ Threat Map</h3>
          <div class="panel-ttl">TTL: {{ threatMapTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="threat-map" />
        </div>
      </div>

      <!-- Pipeline Flow Visualization -->
      <div class="monitoring-panel pipeline-flow-panel">
        <div class="panel-header">
          <h3>🔄 Pipeline Flow</h3>
          <div class="panel-ttl">TTL: {{ pipelineFlowTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="pipeline-flow" />
        </div>
      </div>

      <!-- Alert Stream -->
      <div class="monitoring-panel alert-stream-panel">
        <div class="panel-header">
          <h3>🚨 Alert Stream</h3>
          <div class="panel-ttl">TTL: {{ alertStreamTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="alert-stream" />
        </div>
      </div>

      <!-- System Metrics -->
      <div class="monitoring-panel metrics-panel">
        <div class="panel-header">
          <h3>📊 System Metrics</h3>
          <div class="panel-ttl">TTL: {{ metricsTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="system-metrics" />
        </div>
      </div>

      <!-- Network Traffic -->
      <div class="monitoring-panel network-panel">
        <div class="panel-header">
          <h3>🌐 Network Traffic</h3>
          <div class="panel-ttl">TTL: {{ networkTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="network-traffic" />
        </div>
      </div>

      <!-- Swarm Coordination Status -->
      <div class="monitoring-panel swarm-panel">
        <div class="panel-header">
          <h3>🐝 Swarm Status</h3>
          <div class="panel-ttl">TTL: {{ swarmTTL }}ms</div>
        </div>
        <div class="panel-content">
          <slot name="swarm-status" />
        </div>
      </div>
    </div>

    <!-- Timeline Navigation -->
    <div class="timeline-controls">
      <div class="timeline-nav">
        <button class="timeline-btn" @click="setTimeRange('1m')">1m</button>
        <button class="timeline-btn" @click="setTimeRange('5m')">5m</button>
        <button class="timeline-btn" @click="setTimeRange('15m')">15m</button>
        <button class="timeline-btn" @click="setTimeRange('1h')">1h</button>
        <button class="timeline-btn" @click="setTimeRange('6h')">6h</button>
        <button class="timeline-btn" @click="setTimeRange('24h')">24h</button>
      </div>
      <div class="timeline-indicator">
        Viewing: {{ selectedTimeRange }} | Auto-refresh: {{ autoRefreshInterval }}s
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     Detailed Analysis Layout
     ============================================================================= -->
<template name="analysis-layout">
  <div class="analysis-layout">
    <!-- Analysis Header -->
    <div class="analysis-header">
      <div class="breadcrumb-nav">
        <NuxtLink to="/dashboard" class="breadcrumb-item">Dashboard</NuxtLink>
        <span class="breadcrumb-separator">/</span>
        <NuxtLink to="/monitoring" class="breadcrumb-item">Monitoring</NuxtLink>
        <span class="breadcrumb-separator">/</span>
        <span class="breadcrumb-current">Analysis</span>
      </div>
      
      <div class="analysis-controls">
        <select v-model="analysisType" class="analysis-type-select">
          <option value="threat">Threat Analysis</option>
          <option value="performance">Performance Analysis</option>
          <option value="ttl">TTL Analysis</option>
          <option value="swarm">Swarm Analysis</option>
        </select>
        
        <div class="time-range-picker">
          <label>Time Range:</label>
          <input 
            v-model="analysisStartTime" 
            type="datetime-local" 
            class="time-input"
          >
          <span>to</span>
          <input 
            v-model="analysisEndTime" 
            type="datetime-local" 
            class="time-input"
          >
        </div>
        
        <button class="run-analysis-btn" @click="runAnalysis">
          🔬 Run Analysis
        </button>
      </div>
    </div>

    <!-- Analysis Content Areas -->
    <div class="analysis-content">
      <!-- Left Panel: Filters and Configuration -->
      <div class="analysis-sidebar">
        <div class="filter-section">
          <h3>📊 Analysis Filters</h3>
          <div class="filter-group">
            <label class="filter-label">Severity Level:</label>
            <div class="checkbox-group">
              <label><input type="checkbox" v-model="filters.severity" value="low"> Low</label>
              <label><input type="checkbox" v-model="filters.severity" value="medium"> Medium</label>
              <label><input type="checkbox" v-model="filters.severity" value="high"> High</label>
              <label><input type="checkbox" v-model="filters.severity" value="critical"> Critical</label>
            </div>
          </div>
          
          <div class="filter-group">
            <label class="filter-label">Event Types:</label>
            <div class="checkbox-group">
              <label><input type="checkbox" v-model="filters.eventTypes" value="malware"> Malware</label>
              <label><input type="checkbox" v-model="filters.eventTypes" value="intrusion"> Intrusion</label>
              <label><input type="checkbox" v-model="filters.eventTypes" value="anomaly"> Anomaly</label>
              <label><input type="checkbox" v-model="filters.eventTypes" value="ttl_violation"> TTL Violation</label>
            </div>
          </div>
          
          <div class="filter-group">
            <label class="filter-label">Pipeline Stages:</label>
            <div class="checkbox-group">
              <label><input type="checkbox" v-model="filters.stages" value="typer"> Typer</label>
              <label><input type="checkbox" v-model="filters.stages" value="turtle"> Turtle</label>
              <label><input type="checkbox" v-model="filters.stages" value="ttl2dspy"> TTL2DSPy</label>
              <label><input type="checkbox" v-model="filters.stages" value="bitactor"> BitActor</label>
              <label><input type="checkbox" v-model="filters.stages" value="erlang"> Erlang</label>
              <label><input type="checkbox" v-model="filters.stages" value="ash"> Ash</label>
              <label><input type="checkbox" v-model="filters.stages" value="reactor"> Reactor</label>
              <label><input type="checkbox" v-model="filters.stages" value="k8s"> K8s</label>
            </div>
          </div>
        </div>
        
        <div class="analysis-config">
          <h3>⚙️ Analysis Configuration</h3>
          <div class="config-group">
            <label>Analysis TTL Budget:</label>
            <input 
              v-model.number="analysisTTLBudget" 
              type="number" 
              min="10" 
              max="5000" 
              step="10"
              class="ttl-input"
            >
            <span>ms</span>
          </div>
          
          <div class="config-group">
            <label>Sample Size:</label>
            <select v-model="analysisSampleSize" class="sample-size-select">
              <option value="100">100 events</option>
              <option value="500">500 events</option>
              <option value="1000">1,000 events</option>
              <option value="5000">5,000 events</option>
              <option value="all">All events</option>
            </select>
          </div>
          
          <div class="config-group">
            <label>
              <input type="checkbox" v-model="includeRawData"> 
              Include raw data in export
            </label>
          </div>
        </div>
      </div>

      <!-- Main Analysis Area -->
      <div class="analysis-main">
        <div class="analysis-tabs">
          <button 
            v-for="tab in analysisTabs" 
            :key="tab.id"
            class="analysis-tab"
            :class="{ active: activeTab === tab.id }"
            @click="activeTab = tab.id"
          >
            {{ tab.icon }} {{ tab.label }}
          </button>
        </div>
        
        <div class="analysis-tab-content">
          <slot :name="activeTab" />
        </div>
      </div>

      <!-- Right Panel: Analysis Results Summary -->
      <div class="analysis-results">
        <div class="results-summary">
          <h3>📈 Analysis Summary</h3>
          <div class="summary-metrics">
            <div class="summary-metric">
              <span class="metric-label">Events Analyzed:</span>
              <span class="metric-value">{{ analysisResults.eventsAnalyzed }}</span>
            </div>
            <div class="summary-metric">
              <span class="metric-label">Analysis Time:</span>
              <span class="metric-value">{{ analysisResults.executionTime }}ms</span>
            </div>
            <div class="summary-metric">
              <span class="metric-label">TTL Compliance:</span>
              <span class="metric-value" :class="analysisResults.ttlCompliant ? 'success' : 'error'">
                {{ analysisResults.ttlCompliant ? '✅' : '❌' }}
              </span>
            </div>
          </div>
        </div>
        
        <div class="key-findings">
          <h4>🔍 Key Findings</h4>
          <ul class="findings-list">
            <li v-for="finding in analysisResults.keyFindings" :key="finding.id" 
                class="finding-item" :class="finding.severity">
              <span class="finding-icon">{{ finding.icon }}</span>
              <span class="finding-text">{{ finding.description }}</span>
            </li>
          </ul>
        </div>
        
        <div class="export-options">
          <h4>💾 Export Results</h4>
          <div class="export-buttons">
            <button class="export-btn" @click="exportResults('json')">
              📄 JSON
            </button>
            <button class="export-btn" @click="exportResults('csv')">
              📊 CSV
            </button>
            <button class="export-btn" @click="exportResults('pdf')">
              📋 PDF Report
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     Layout Script with TTL-Aware Logic
     ============================================================================= -->
<script setup>
// =============================================================================
// Layout State Management
// =============================================================================

// Global layout state
const globalTTLBudget = ref(8) // 8ms default
const ttlUsagePercentage = ref(0)
const currentThreatLevel = ref('LOW')
const activeSwarmCount = ref(3)
const systemHealth = ref('HEALTHY')

// Navigation configuration
const navigationItems = ref([
  { path: '/dashboard', icon: '🏠', label: 'Dashboard' },
  { path: '/monitoring', icon: '🔍', label: 'Monitoring' },
  { path: '/analysis', icon: '📊', label: 'Analysis' },
  { path: '/threats', icon: '🚨', label: 'Threats' },
  { path: '/swarms', icon: '🐝', label: 'Swarms' },
  { path: '/pipeline', icon: '⚡', label: 'Pipeline' },
  { path: '/settings', icon: '⚙️', label: 'Settings' }
])

// Pipeline stages configuration
const pipelineStages = ref([
  { id: 'typer', name: 'Typer', active: true, hasError: false, ttlBudget: 2 },
  { id: 'turtle', name: 'Turtle', active: true, hasError: false, ttlBudget: 1 },
  { id: 'ttl2dspy', name: 'TTL2DSPy', active: true, hasError: false, ttlBudget: 3 },
  { id: 'bitactor', name: 'BitActor', active: false, hasError: false, ttlBudget: 2 },
  { id: 'erlang', name: 'Erlang', active: false, hasError: false, ttlBudget: 1 },
  { id: 'ash', name: 'Ash', active: false, hasError: false, ttlBudget: 2 },
  { id: 'reactor', name: 'Reactor', active: false, hasError: true, ttlBudget: 3 },
  { id: 'k8s', name: 'K8s', active: false, hasError: false, ttlBudget: 1 }
])

// Active swarms
const activeSwarms = ref([
  { id: 'swarm1', name: 'Primary Swarm', agentCount: 8, healthy: true, warning: false, critical: false },
  { id: 'swarm2', name: 'Secondary Swarm', agentCount: 6, healthy: false, warning: true, critical: false },
  { id: 'swarm3', name: 'Analysis Swarm', agentCount: 4, healthy: true, warning: false, critical: false }
])

// System metrics
const totalPipelineExecutions = ref(1247)
const totalTTLViolations = ref(23)
const threatsDetected = ref(156)
const systemUptime = ref('99.8%')
const websocketStatus = ref('connected')

// Monitoring layout state
const monitoringTTLBudget = ref(8)
const isMonitoring = ref(true)
const selectedTimeRange = ref('15m')
const autoRefreshInterval = ref(5)

// Individual panel TTL budgets
const threatMapTTL = ref(3)
const pipelineFlowTTL = ref(2)
const alertStreamTTL = ref(1)
const metricsTTL = ref(2)
const networkTTL = ref(3)
const swarmTTL = ref(2)

// Analysis layout state
const analysisType = ref('threat')
const analysisStartTime = ref('')
const analysisEndTime = ref('')
const analysisTTLBudget = ref(100)
const analysisSampleSize = ref('1000')
const includeRawData = ref(false)
const activeTab = ref('overview')

// Analysis filters
const filters = ref({
  severity: ['medium', 'high', 'critical'],
  eventTypes: ['malware', 'intrusion', 'anomaly'],
  stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
})

// Analysis tabs
const analysisTabs = ref([
  { id: 'overview', icon: '📋', label: 'Overview' },
  { id: 'timeline', icon: '📅', label: 'Timeline' },
  { id: 'patterns', icon: '🔍', label: 'Patterns' },
  { id: 'correlations', icon: '🔗', label: 'Correlations' },
  { id: 'predictions', icon: '🔮', label: 'Predictions' }
])

// Analysis results
const analysisResults = ref({
  eventsAnalyzed: 0,
  executionTime: 0,
  ttlCompliant: true,
  keyFindings: []
})

// =============================================================================
// Computed Properties
// =============================================================================

const ttlStatusClass = computed(() => {
  if (ttlUsagePercentage.value > 90) return 'critical'
  if (ttlUsagePercentage.value > 75) return 'warning'
  return 'normal'
})

const ttlUsageClass = computed(() => {
  if (ttlUsagePercentage.value > 90) return 'critical'
  if (ttlUsagePercentage.value > 75) return 'warning'
  return 'normal'
})

const threatLevelClass = computed(() => {
  return `threat-${currentThreatLevel.value.toLowerCase()}`
})

const systemHealthClass = computed(() => {
  return `health-${systemHealth.value.toLowerCase()}`
})

const isTTLConstrained = computed(() => {
  return ttlUsagePercentage.value > 80
})

// =============================================================================
// Layout Actions
// =============================================================================

const emergencyStop = () => {
  console.log('🚨 Emergency stop activated')
  // Implement emergency stop logic
}

const runFullPipeline = () => {
  console.log('⚡ Running full pipeline')
  // Implement pipeline execution
}

const coordinateSwarms = () => {
  console.log('🐝 Coordinating swarms')
  // Implement swarm coordination
}

const viewLogs = () => {
  console.log('📋 Opening logs view')
  // Navigate to logs
}

const updateMonitoringTTL = () => {
  // Redistribute TTL budget across monitoring panels
  const totalPanels = 6
  const baseTTL = Math.floor(monitoringTTLBudget.value / totalPanels)
  
  threatMapTTL.value = baseTTL + 1
  pipelineFlowTTL.value = baseTTL
  alertStreamTTL.value = Math.max(1, baseTTL - 1)
  metricsTTL.value = baseTTL
  networkTTL.value = baseTTL + 1
  swarmTTL.value = baseTTL
}

const toggleMonitoring = () => {
  isMonitoring.value = !isMonitoring.value
  console.log(`Monitoring ${isMonitoring.value ? 'started' : 'paused'}`)
}

const setTimeRange = (range) => {
  selectedTimeRange.value = range
  console.log(`Time range set to: ${range}`)
}

const runAnalysis = () => {
  console.log(`Running ${analysisType.value} analysis`)
  console.log(`Time range: ${analysisStartTime.value} to ${analysisEndTime.value}`)
  console.log(`TTL budget: ${analysisTTLBudget.value}ms`)
  
  // Simulate analysis execution
  const startTime = performance.now()
  
  setTimeout(() => {
    const executionTime = performance.now() - startTime
    
    analysisResults.value = {
      eventsAnalyzed: parseInt(analysisSampleSize.value) || 1000,
      executionTime: Math.round(executionTime),
      ttlCompliant: executionTime <= analysisTTLBudget.value,
      keyFindings: [
        {
          id: 1,
          severity: 'high',
          icon: '🚨',
          description: 'Unusual spike in malware detection at 14:32'
        },
        {
          id: 2,
          severity: 'medium',
          icon: '⚠️',
          description: 'TTL violations increased by 15% in reactor stage'
        },
        {
          id: 3,
          severity: 'low',
          icon: 'ℹ️',
          description: 'Swarm coordination efficiency improved by 8%'
        }
      ]
    }
  }, Math.min(analysisTTLBudget.value - 10, 50)) // Simulate analysis time
}

const exportResults = (format) => {
  console.log(`Exporting analysis results as ${format}`)
  // Implement export functionality
}

// =============================================================================
// Lifecycle and Reactivity
// =============================================================================

// Initialize layout state
onMounted(() => {
  // Set default analysis time range (last hour)
  const now = new Date()
  const oneHourAgo = new Date(now.getTime() - 60 * 60 * 1000)
  
  analysisEndTime.value = now.toISOString().slice(0, 16)
  analysisStartTime.value = oneHourAgo.toISOString().slice(0, 16)
  
  // Start TTL usage simulation
  const ttlUpdateInterval = setInterval(() => {
    ttlUsagePercentage.value = Math.random() * 100
  }, 1000)
  
  onUnmounted(() => {
    clearInterval(ttlUpdateInterval)
  })
})

// Watch for TTL budget changes and update panels
watch(monitoringTTLBudget, updateMonitoringTTL)

// Provide layout state to child components
provide('layoutState', {
  globalTTLBudget,
  ttlUsagePercentage,
  currentThreatLevel,
  systemHealth,
  isMonitoring,
  analysisResults
})
</script>

<!-- =============================================================================
     Layout Styles with TTL-Aware Theming
     ============================================================================= -->
<style scoped>
/* Global Layout Variables */
:root {
  --ttl-normal: #10b981;
  --ttl-warning: #f59e0b;
  --ttl-critical: #ef4444;
  --threat-low: #10b981;
  --threat-medium: #f59e0b;
  --threat-high: #ef4444;
  --threat-critical: #dc2626;
  --panel-bg: #1e293b;
  --panel-border: #334155;
  --text-primary: #e2e8f0;
  --text-secondary: #94a3b8;
}

/* Cybersecurity Dashboard Layout */
.cybersecurity-dashboard-layout {
  display: grid;
  grid-template-areas: 
    "ttl-bar ttl-bar ttl-bar"
    "header header header"
    "sidebar main main"
    "footer footer footer";
  grid-template-rows: auto auto 1fr auto;
  grid-template-columns: 250px 1fr;
  min-height: 100vh;
  background: #0f172a;
  color: var(--text-primary);
  font-family: 'Inter', -apple-system, sans-serif;
}

.ttl-status-bar {
  grid-area: ttl-bar;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.5rem 2rem;
  background: var(--panel-bg);
  border-bottom: 1px solid var(--panel-border);
  font-size: 0.875rem;
}

.ttl-status-bar.normal { border-bottom-color: var(--ttl-normal); }
.ttl-status-bar.warning { border-bottom-color: var(--ttl-warning); }
.ttl-status-bar.critical { 
  border-bottom-color: var(--ttl-critical);
  animation: pulse 1s infinite;
}

.ttl-indicator {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.ttl-usage-bar {
  width: 100px;
  height: 4px;
  background: #334155;
  border-radius: 2px;
  overflow: hidden;
}

.ttl-usage-fill {
  height: 100%;
  transition: width 0.3s ease;
  border-radius: 2px;
}

.ttl-usage-fill.normal { background: var(--ttl-normal); }
.ttl-usage-fill.warning { background: var(--ttl-warning); }
.ttl-usage-fill.critical { background: var(--ttl-critical); }

.threat-level-indicator {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.threat-low { color: var(--threat-low); }
.threat-medium { color: var(--threat-medium); }
.threat-high { color: var(--threat-high); }
.threat-critical { color: var(--threat-critical); }

.dashboard-header {
  grid-area: header;
  background: var(--panel-bg);
  border-bottom: 1px solid var(--panel-border);
}

.main-navigation {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem 2rem;
}

.nav-brand {
  display: flex;
  align-items: center;
}

.brand-title {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 1.25rem;
  font-weight: bold;
  margin: 0;
}

.nav-menu {
  display: flex;
  gap: 1rem;
}

.nav-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 1rem;
  border-radius: 6px;
  text-decoration: none;
  color: var(--text-secondary);
  transition: all 0.2s;
}

.nav-item:hover, .nav-item.active {
  background: #334155;
  color: var(--text-primary);
}

.nav-actions {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.emergency-stop-btn {
  padding: 0.5rem 1rem;
  background: var(--ttl-critical);
  color: white;
  border: none;
  border-radius: 6px;
  font-weight: bold;
  cursor: pointer;
}

.system-health {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: bold;
}

.health-healthy { background: var(--ttl-normal); color: white; }
.health-warning { background: var(--ttl-warning); color: white; }
.health-critical { background: var(--ttl-critical); color: white; }

.dashboard-sidebar {
  grid-area: sidebar;
  background: var(--panel-bg);
  border-right: 1px solid var(--panel-border);
  padding: 1rem;
  overflow-y: auto;
}

.sidebar-section {
  margin-bottom: 2rem;
}

.section-title {
  font-size: 0.875rem;
  font-weight: bold;
  margin: 0 0 1rem 0;
  color: var(--text-secondary);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.pipeline-stage {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  margin-bottom: 0.25rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.pipeline-stage.active { background: rgba(16, 185, 129, 0.1); }
.pipeline-stage.error { background: rgba(239, 68, 68, 0.1); }

.stage-indicator {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #6b7280;
}

.pipeline-stage.active .stage-indicator { background: var(--ttl-normal); }
.pipeline-stage.error .stage-indicator { background: var(--ttl-critical); }

.swarm-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  margin-bottom: 0.25rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.swarm-status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #6b7280;
}

.swarm-item.healthy .swarm-status-dot { background: var(--ttl-normal); }
.swarm-item.warning .swarm-status-dot { background: var(--ttl-warning); }
.swarm-item.critical .swarm-status-dot { background: var(--ttl-critical); }

.quick-actions {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.action-btn {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 6px;
  font-weight: bold;
  cursor: pointer;
  text-align: left;
}

.action-btn.primary {
  background: var(--ttl-normal);
  color: white;
}

.action-btn.secondary {
  background: #334155;
  color: var(--text-primary);
}

.action-btn.tertiary {
  background: transparent;
  color: var(--text-secondary);
  border: 1px solid var(--panel-border);
}

.dashboard-main {
  grid-area: main;
  padding: 2rem;
  overflow-y: auto;
}

.content-wrapper.ttl-constrained {
  filter: saturate(0.7);
  opacity: 0.9;
}

.dashboard-footer {
  grid-area: footer;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.75rem 2rem;
  background: var(--panel-bg);
  border-top: 1px solid var(--panel-border);
  font-size: 0.875rem;
}

.status-metrics {
  display: flex;
  gap: 2rem;
}

.metric {
  display: flex;
  gap: 0.5rem;
}

.metric-value.error { color: var(--ttl-critical); }
.metric-value.warning { color: var(--ttl-warning); }

.ws-status {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.ws-indicator {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #6b7280;
}

.ws-status.connected .ws-indicator { background: var(--ttl-normal); }
.ws-status.disconnected .ws-indicator { background: var(--ttl-critical); }

/* Monitoring Layout */
.monitoring-layout {
  display: flex;
  flex-direction: column;
  height: 100vh;
  background: #0f172a;
  color: var(--text-primary);
}

.monitoring-header {
  padding: 1rem 2rem;
  background: var(--panel-bg);
  border-bottom: 1px solid var(--panel-border);
}

.monitoring-title {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.monitoring-controls {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.ttl-control {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.875rem;
}

.monitoring-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-template-rows: repeat(2, 1fr);
  gap: 1rem;
  padding: 1rem;
  flex: 1;
}

.monitoring-panel {
  background: var(--panel-bg);
  border: 1px solid var(--panel-border);
  border-radius: 8px;
  overflow: hidden;
}

.panel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.75rem 1rem;
  background: #334155;
  border-bottom: 1px solid var(--panel-border);
}

.panel-ttl {
  font-size: 0.75rem;
  color: var(--text-secondary);
}

.panel-content {
  padding: 1rem;
  height: calc(100% - 60px);
}

.timeline-controls {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.75rem 2rem;
  background: var(--panel-bg);
  border-top: 1px solid var(--panel-border);
}

.timeline-nav {
  display: flex;
  gap: 0.5rem;
}

.timeline-btn {
  padding: 0.25rem 0.75rem;
  background: #334155;
  color: var(--text-primary);
  border: none;
  border-radius: 4px;
  cursor: pointer;
}

.timeline-btn:hover {
  background: #475569;
}

/* Analysis Layout */
.analysis-layout {
  display: flex;
  flex-direction: column;
  height: 100vh;
  background: #0f172a;
  color: var(--text-primary);
}

.analysis-header {
  padding: 1rem 2rem;
  background: var(--panel-bg);
  border-bottom: 1px solid var(--panel-border);
}

.breadcrumb-nav {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 1rem;
  font-size: 0.875rem;
}

.breadcrumb-item {
  color: var(--text-secondary);
  text-decoration: none;
}

.breadcrumb-item:hover {
  color: var(--text-primary);
}

.breadcrumb-current {
  color: var(--text-primary);
  font-weight: bold;
}

.analysis-controls {
  display: flex;
  align-items: center;
  gap: 1rem;
  flex-wrap: wrap;
}

.analysis-content {
  display: grid;
  grid-template-columns: 250px 1fr 300px;
  gap: 1rem;
  padding: 1rem;
  flex: 1;
  overflow: hidden;
}

.analysis-sidebar {
  background: var(--panel-bg);
  border: 1px solid var(--panel-border);
  border-radius: 8px;
  padding: 1rem;
  overflow-y: auto;
}

.filter-section, .analysis-config {
  margin-bottom: 2rem;
}

.filter-group, .config-group {
  margin-bottom: 1rem;
}

.filter-label {
  display: block;
  font-weight: bold;
  margin-bottom: 0.5rem;
  font-size: 0.875rem;
}

.checkbox-group {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.checkbox-group label {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.875rem;
}

.analysis-main {
  background: var(--panel-bg);
  border: 1px solid var(--panel-border);
  border-radius: 8px;
  overflow: hidden;
}

.analysis-tabs {
  display: flex;
  background: #334155;
  border-bottom: 1px solid var(--panel-border);
}

.analysis-tab {
  padding: 0.75rem 1rem;
  background: transparent;
  color: var(--text-secondary);
  border: none;
  cursor: pointer;
  border-bottom: 2px solid transparent;
}

.analysis-tab.active {
  color: var(--text-primary);
  border-bottom-color: var(--ttl-normal);
}

.analysis-tab-content {
  padding: 1rem;
  height: calc(100% - 60px);
  overflow-y: auto;
}

.analysis-results {
  background: var(--panel-bg);
  border: 1px solid var(--panel-border);
  border-radius: 8px;
  padding: 1rem;
  overflow-y: auto;
}

.summary-metrics {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.summary-metric {
  display: flex;
  justify-content: space-between;
  font-size: 0.875rem;
}

.summary-metric .metric-value.success { color: var(--ttl-normal); }
.summary-metric .metric-value.error { color: var(--ttl-critical); }

.findings-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.finding-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  margin-bottom: 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.finding-item.high { background: rgba(239, 68, 68, 0.1); }
.finding-item.medium { background: rgba(245, 158, 11, 0.1); }
.finding-item.low { background: rgba(16, 185, 129, 0.1); }

.export-buttons {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.export-btn {
  padding: 0.5rem;
  background: #334155;
  color: var(--text-primary);
  border: none;
  border-radius: 4px;
  cursor: pointer;
  text-align: left;
}

.export-btn:hover {
  background: #475569;
}

/* Animations */
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

/* Responsive Design */
@media (max-width: 1024px) {
  .cybersecurity-dashboard-layout {
    grid-template-columns: 200px 1fr;
  }
  
  .monitoring-grid {
    grid-template-columns: repeat(2, 1fr);
    grid-template-rows: repeat(3, 1fr);
  }
  
  .analysis-content {
    grid-template-columns: 1fr;
    grid-template-rows: auto 1fr auto;
  }
}

@media (max-width: 768px) {
  .cybersecurity-dashboard-layout {
    grid-template-areas: 
      "ttl-bar"
      "header"
      "main"
      "footer";
    grid-template-columns: 1fr;
  }
  
  .dashboard-sidebar {
    display: none;
  }
  
  .monitoring-grid {
    grid-template-columns: 1fr;
  }
  
  .nav-menu {
    display: none;
  }
}
</style>