<template>
  <div class="performance-dashboard">
    <h3 class="dashboard-title">📊 Performance Dashboard</h3>
    
    <!-- Dashboard Controls -->
    <div class="dashboard-controls">
      <div class="time-range">
        <label class="range-label">Time Range:</label>
        <select v-model="selectedTimeRange" class="range-select">
          <option value="1h">Last Hour</option>
          <option value="6h">Last 6 Hours</option>
          <option value="24h">Last 24 Hours</option>
          <option value="7d">Last 7 Days</option>
          <option value="live">Live (Real-time)</option>
        </select>
      </div>
      
      <div class="dashboard-actions">
        <button @click="refreshData" class="action-btn refresh">🔄 Refresh</button>
        <button @click="exportReport" class="action-btn export">📄 Export</button>
        <button @click="toggleAutoRefresh" :class="['action-btn', { active: autoRefresh }]">
          {{ autoRefresh ? '⏸️' : '▶️' }} Auto Refresh
        </button>
      </div>
    </div>
    
    <!-- Key Performance Indicators -->
    <div class="kpi-section">
      <h4 class="section-title">🎯 Key Performance Indicators</h4>
      <div class="kpi-grid">
        <div class="kpi-card">
          <div class="kpi-icon">⚡</div>
          <div class="kpi-content">
            <div class="kpi-value">{{ avgExecutionTime }}ms</div>
            <div class="kpi-label">Avg Execution Time</div>
            <div :class="['kpi-trend', executionTimeTrend]">
              {{ formatTrend(executionTimeTrend, executionTimeChange) }}
            </div>
          </div>
        </div>
        
        <div class="kpi-card">
          <div class="kpi-icon">📈</div>
          <div class="kpi-content">
            <div class="kpi-value">{{ successRate }}%</div>
            <div class="kpi-label">Success Rate</div>
            <div :class="['kpi-trend', successRateTrend]">
              {{ formatTrend(successRateTrend, successRateChange) }}
            </div>
          </div>
        </div>
        
        <div class="kpi-card">
          <div class="kpi-icon">🔄</div>
          <div class="kpi-content">
            <div class="kpi-value">{{ throughput }}</div>
            <div class="kpi-label">Throughput/hour</div>
            <div :class="['kpi-trend', throughputTrend]">
              {{ formatTrend(throughputTrend, throughputChange) }}
            </div>
          </div>
        </div>
        
        <div class="kpi-card">
          <div class="kpi-icon">🧠</div>
          <div class="kpi-content">
            <div class="kpi-value">{{ memoryEfficiency }}%</div>
            <div class="kpi-label">Memory Efficiency</div>
            <div :class="['kpi-trend', memoryTrend]">
              {{ formatTrend(memoryTrend, memoryChange) }}
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Real-time Charts -->
    <div class="charts-section">
      <h4 class="section-title">📈 Performance Charts</h4>
      
      <div class="charts-grid">
        <!-- Execution Time Chart -->
        <div class="chart-container">
          <h5 class="chart-title">⚡ Execution Time Trends</h5>
          <div class="chart-area">
            <canvas ref="executionChart" class="chart-canvas"></canvas>
          </div>
          <div class="chart-legend">
            <div class="legend-item">
              <span class="legend-color ultra-bypass"></span>
              <span>Ultra Bypass</span>
            </div>
            <div class="legend-item">
              <span class="legend-color parallel"></span>
              <span>Parallel</span>
            </div>
            <div class="legend-item">
              <span class="legend-color traditional"></span>
              <span>Traditional</span>
            </div>
          </div>
        </div>
        
        <!-- Resource Usage Chart -->
        <div class="chart-container">
          <h5 class="chart-title">🔧 Resource Usage</h5>
          <div class="chart-area">
            <canvas ref="resourceChart" class="chart-canvas"></canvas>
          </div>
          <div class="resource-breakdown">
            <div class="resource-item">
              <span class="resource-label">CPU:</span>
              <div class="resource-bar">
                <div class="resource-fill cpu" :style="{ width: `${cpuUsage}%` }"></div>
              </div>
              <span class="resource-value">{{ cpuUsage }}%</span>
            </div>
            <div class="resource-item">
              <span class="resource-label">Memory:</span>
              <div class="resource-bar">
                <div class="resource-fill memory" :style="{ width: `${memoryUsage}%` }"></div>
              </div>
              <span class="resource-value">{{ memoryUsage }}%</span>
            </div>
            <div class="resource-item">
              <span class="resource-label">I/O:</span>
              <div class="resource-bar">
                <div class="resource-fill io" :style="{ width: `${ioUsage}%` }"></div>
              </div>
              <span class="resource-value">{{ ioUsage }}%</span>
            </div>
          </div>
        </div>
        
        <!-- Stage Performance Heatmap -->
        <div class="chart-container full-width">
          <h5 class="chart-title">🔥 Stage Performance Heatmap</h5>
          <div class="heatmap-container">
            <div class="heatmap-grid">
              <div class="heatmap-header">
                <div class="heatmap-corner">Stages</div>
                <div 
                  v-for="timeSlot in timeSlots" 
                  :key="timeSlot"
                  class="heatmap-time"
                >
                  {{ timeSlot }}
                </div>
              </div>
              <div 
                v-for="stage in pipelineStages" 
                :key="stage.id"
                class="heatmap-row"
              >
                <div class="heatmap-stage">
                  <span class="stage-icon">{{ stage.icon }}</span>
                  <span class="stage-name">{{ stage.name }}</span>
                </div>
                <div 
                  v-for="(value, index) in stage.performanceData" 
                  :key="`${stage.id}-${index}`"
                  :class="['heatmap-cell', getHeatmapIntensity(value)]"
                  :title="`${stage.name}: ${value}ms`"
                >
                  {{ value }}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Bottleneck Analysis -->
    <div class="bottleneck-section">
      <h4 class="section-title">🚧 Bottleneck Analysis</h4>
      <div class="bottleneck-content">
        <div class="bottleneck-overview">
          <div class="bottleneck-score">
            <span class="score-value">{{ bottleneckScore }}</span>
            <span class="score-label">Overall Health Score</span>
            <div :class="['score-indicator', getHealthStatus(bottleneckScore)]"></div>
          </div>
          
          <div class="bottleneck-summary">
            <div class="summary-item">
              <span class="summary-icon">⚠️</span>
              <span class="summary-text">{{ criticalBottlenecks }} Critical Issues</span>
            </div>
            <div class="summary-item">
              <span class="summary-icon">🔶</span>
              <span class="summary-text">{{ warningBottlenecks }} Warnings</span>
            </div>
            <div class="summary-item">
              <span class="summary-icon">💡</span>
              <span class="summary-text">{{ optimizationOpportunities }} Optimization Opportunities</span>
            </div>
          </div>
        </div>
        
        <div class="bottleneck-details">
          <div 
            v-for="bottleneck in detectedBottlenecks" 
            :key="bottleneck.id"
            :class="['bottleneck-item', bottleneck.severity]"
          >
            <div class="bottleneck-header">
              <span class="bottleneck-icon">{{ getBottleneckIcon(bottleneck.severity) }}</span>
              <span class="bottleneck-title">{{ bottleneck.title }}</span>
              <span class="bottleneck-impact">{{ bottleneck.impact }}ms impact</span>
            </div>
            <div class="bottleneck-description">{{ bottleneck.description }}</div>
            <div class="bottleneck-actions">
              <button 
                v-for="action in bottleneck.actions" 
                :key="action.id"
                @click="applyOptimization(action)"
                class="bottleneck-action"
              >
                {{ action.label }}
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Optimization Recommendations -->
    <div class="optimization-section">
      <h4 class="section-title">💡 Smart Optimization Recommendations</h4>
      <div class="optimization-grid">
        <div 
          v-for="recommendation in smartRecommendations" 
          :key="recommendation.id"
          class="recommendation-card"
        >
          <div class="rec-header">
            <span class="rec-icon">{{ recommendation.icon }}</span>
            <span class="rec-title">{{ recommendation.title }}</span>
            <span class="rec-impact">{{ recommendation.expectedGain }}% gain</span>
          </div>
          <div class="rec-description">{{ recommendation.description }}</div>
          <div class="rec-metrics">
            <div class="metric">
              <span class="metric-label">Effort:</span>
              <span class="metric-value">{{ recommendation.effort }}</span>
            </div>
            <div class="metric">
              <span class="metric-label">Risk:</span>
              <span class="metric-value">{{ recommendation.risk }}</span>
            </div>
            <div class="metric">
              <span class="metric-label">Priority:</span>
              <span class="metric-value">{{ recommendation.priority }}</span>
            </div>
          </div>
          <div class="rec-actions">
            <button @click="previewOptimization(recommendation)" class="rec-btn preview">
              👁️ Preview
            </button>
            <button @click="applyOptimization(recommendation)" class="rec-btn apply">
              ⚡ Apply
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Performance Comparison -->
    <div class="comparison-section">
      <h4 class="section-title">⚖️ Performance Comparison</h4>
      <div class="comparison-content">
        <div class="comparison-selector">
          <label class="selector-label">Compare:</label>
          <select v-model="comparisonBaseline" class="selector-dropdown">
            <option value="yesterday">Yesterday</option>
            <option value="last_week">Last Week</option>
            <option value="last_month">Last Month</option>
            <option value="custom">Custom Date</option>
          </select>
        </div>
        
        <div class="comparison-metrics">
          <div class="comparison-metric">
            <div class="metric-name">Execution Time</div>
            <div class="metric-comparison">
              <div class="metric-current">{{ avgExecutionTime }}ms</div>
              <div class="metric-arrow">{{ comparisonArrow.executionTime }}</div>
              <div class="metric-baseline">{{ baselineExecutionTime }}ms</div>
            </div>
            <div :class="['metric-change', getChangeDirection(executionTimeChange)]">
              {{ Math.abs(executionTimeChange) }}% {{ getChangeDirection(executionTimeChange) }}
            </div>
          </div>
          
          <div class="comparison-metric">
            <div class="metric-name">Success Rate</div>
            <div class="metric-comparison">
              <div class="metric-current">{{ successRate }}%</div>
              <div class="metric-arrow">{{ comparisonArrow.successRate }}</div>
              <div class="metric-baseline">{{ baselineSuccessRate }}%</div>
            </div>
            <div :class="['metric-change', getChangeDirection(successRateChange)]">
              {{ Math.abs(successRateChange) }}% {{ getChangeDirection(successRateChange) }}
            </div>
          </div>
          
          <div class="comparison-metric">
            <div class="metric-name">Throughput</div>
            <div class="metric-comparison">
              <div class="metric-current">{{ throughput }}</div>
              <div class="metric-arrow">{{ comparisonArrow.throughput }}</div>
              <div class="metric-baseline">{{ baselineThroughput }}</div>
            </div>
            <div :class="['metric-change', getChangeDirection(throughputChange)]">
              {{ Math.abs(throughputChange) }}% {{ getChangeDirection(throughputChange) }}
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Live Alerts -->
    <div v-if="liveAlerts.length > 0" class="alerts-section">
      <h4 class="section-title">🚨 Live Alerts</h4>
      <div class="alerts-container">
        <div 
          v-for="alert in liveAlerts" 
          :key="alert.id"
          :class="['alert-item', alert.severity]"
        >
          <div class="alert-icon">{{ getAlertIcon(alert.severity) }}</div>
          <div class="alert-content">
            <div class="alert-title">{{ alert.title }}</div>
            <div class="alert-message">{{ alert.message }}</div>
            <div class="alert-timestamp">{{ formatTimestamp(alert.timestamp) }}</div>
          </div>
          <div class="alert-actions">
            <button @click="acknowledgeAlert(alert)" class="alert-btn ack">✓</button>
            <button @click="dismissAlert(alert)" class="alert-btn dismiss">✕</button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
// 📊 ULTRATHINK SWARM 80/20: Performance Dashboard
// JavaScript implementation for advanced performance analytics

import { ref, computed, onMounted, onUnmounted } from 'vue'

// Dashboard state
const selectedTimeRange = ref('1h')
const autoRefresh = ref(true)
const comparisonBaseline = ref('yesterday')

// Performance metrics
const avgExecutionTime = ref(450)
const successRate = ref(95)
const throughput = ref(42)
const memoryEfficiency = ref(87)
const cpuUsage = ref(65)
const memoryUsage = ref(72)
const ioUsage = ref(45)

// Trends
const executionTimeTrend = ref('improving')
const successRateTrend = ref('stable')
const throughputTrend = ref('improving')
const memoryTrend = ref('degrading')

// Changes
const executionTimeChange = ref(-12)
const successRateChange = ref(2)
const throughputChange = ref(18)
const memoryChange = ref(-5)

// Bottleneck analysis
const bottleneckScore = ref(78)
const criticalBottlenecks = ref(1)
const warningBottlenecks = ref(3)
const optimizationOpportunities = ref(5)

// Pipeline stages with performance data
const pipelineStages = ref([
  { 
    id: 'typer', 
    name: 'Typer', 
    icon: '📝',
    performanceData: [120, 135, 98, 156, 142, 118, 134, 127]
  },
  { 
    id: 'turtle', 
    name: 'Turtle', 
    icon: '🐢',
    performanceData: [89, 92, 87, 95, 91, 88, 94, 90]
  },
  { 
    id: 'ttl2dspy', 
    name: 'TTL→DSPy', 
    icon: '🔄',
    performanceData: [234, 278, 201, 298, 256, 223, 267, 245]
  },
  { 
    id: 'ash', 
    name: 'Ash', 
    icon: '🔥',
    performanceData: [156, 162, 149, 168, 159, 153, 165, 160]
  },
  { 
    id: 'reactor', 
    name: 'Reactor', 
    icon: '⚛️',
    performanceData: [78, 82, 74, 86, 80, 76, 84, 79]
  },
  { 
    id: 'k8s', 
    name: 'K8s', 
    icon: '☸️',
    performanceData: [67, 71, 63, 75, 69, 65, 73, 68]
  }
])

const timeSlots = ref(['00:00', '04:00', '08:00', '12:00', '16:00', '20:00', '24:00', 'Now'])

// Detected bottlenecks
const detectedBottlenecks = ref([
  {
    id: 'ttl_processing',
    title: 'TTL Processing Bottleneck',
    severity: 'critical',
    impact: 156,
    description: 'TTL→DSPy transformation showing high latency during peak hours',
    actions: [
      { id: 'enable_caching', label: 'Enable Caching' },
      { id: 'optimize_parser', label: 'Optimize Parser' }
    ]
  },
  {
    id: 'memory_pressure',
    title: 'Memory Pressure Warning',
    severity: 'warning',
    impact: 43,
    description: 'Memory usage trending upward, potential for memory leaks',
    actions: [
      { id: 'gc_tuning', label: 'Tune GC' },
      { id: 'memory_profiling', label: 'Profile Memory' }
    ]
  },
  {
    id: 'parallel_underutilization',
    title: 'Parallel Processing Underutilized',
    severity: 'info',
    impact: 89,
    description: 'Only using 60% of available parallel processing capacity',
    actions: [
      { id: 'increase_parallelism', label: 'Increase Parallelism' },
      { id: 'load_balancing', label: 'Improve Load Balancing' }
    ]
  }
])

// Smart recommendations
const smartRecommendations = ref([
  {
    id: 'bypass_optimization',
    title: 'Enable Ultra Bypass for Simple Ontologies',
    icon: '⚡',
    description: 'Automatically route simple ontologies through bypass transformations',
    expectedGain: 67,
    effort: 'Low',
    risk: 'Low',
    priority: 'High'
  },
  {
    id: 'parallel_tuning',
    title: 'Optimize Parallel Processing',
    icon: '🔄',
    description: 'Fine-tune parallel execution based on system resources',
    expectedGain: 34,
    effort: 'Medium',
    risk: 'Low',
    priority: 'Medium'
  },
  {
    id: 'caching_layer',
    title: 'Implement Smart Caching',
    icon: '💾',
    description: 'Cache frequently used transformations and intermediate results',
    expectedGain: 45,
    effort: 'High',
    risk: 'Medium',
    priority: 'High'
  }
])

// Live alerts
const liveAlerts = ref([
  {
    id: 'spike_alert',
    title: 'Performance Spike Detected',
    message: 'Execution time increased by 45% in the last 10 minutes',
    severity: 'warning',
    timestamp: new Date()
  }
])

// Baseline metrics for comparison
const baselineExecutionTime = ref(512)
const baselineSuccessRate = ref(93)
const baselineThroughput = ref(35)

// Computed properties
const comparisonArrow = computed(() => ({
  executionTime: executionTimeChange.value < 0 ? '🔽' : '🔼',
  successRate: successRateChange.value > 0 ? '🔼' : '🔽',
  throughput: throughputChange.value > 0 ? '🔼' : '🔽'
}))

// Methods
const formatTrend = (trend, change) => {
  const arrow = trend === 'improving' ? '↗️' : trend === 'degrading' ? '↘️' : '➡️'
  return `${arrow} ${Math.abs(change)}%`
}

const getHealthStatus = (score) => {
  if (score >= 80) return 'excellent'
  if (score >= 60) return 'good'
  if (score >= 40) return 'fair'
  return 'poor'
}

const getBottleneckIcon = (severity) => {
  const icons = {
    critical: '🚨',
    warning: '⚠️',
    info: 'ℹ️'
  }
  return icons[severity] || 'ℹ️'
}

const getAlertIcon = (severity) => {
  const icons = {
    critical: '🚨',
    warning: '⚠️',
    info: '💡'
  }
  return icons[severity] || '💡'
}

const getHeatmapIntensity = (value) => {
  if (value > 200) return 'high'
  if (value > 150) return 'medium'
  if (value > 100) return 'low'
  return 'minimal'
}

const getChangeDirection = (change) => {
  return change > 0 ? 'better' : change < 0 ? 'worse' : 'stable'
}

const refreshData = () => {
  // Simulate data refresh
  avgExecutionTime.value = Math.floor(Math.random() * 100) + 400
  successRate.value = Math.floor(Math.random() * 10) + 90
  throughput.value = Math.floor(Math.random() * 20) + 35
  
  // Update trends randomly
  const trends = ['improving', 'stable', 'degrading']
  executionTimeTrend.value = trends[Math.floor(Math.random() * trends.length)]
  successRateTrend.value = trends[Math.floor(Math.random() * trends.length)]
  throughputTrend.value = trends[Math.floor(Math.random() * trends.length)]
}

const exportReport = () => {
  const report = {
    timestamp: new Date().toISOString(),
    timeRange: selectedTimeRange.value,
    metrics: {
      avgExecutionTime: avgExecutionTime.value,
      successRate: successRate.value,
      throughput: throughput.value,
      memoryEfficiency: memoryEfficiency.value
    },
    bottlenecks: detectedBottlenecks.value,
    recommendations: smartRecommendations.value
  }
  
  const blob = new Blob([JSON.stringify(report, null, 2)], { type: 'application/json' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `performance-report-${Date.now()}.json`
  a.click()
  URL.revokeObjectURL(url)
}

const toggleAutoRefresh = () => {
  autoRefresh.value = !autoRefresh.value
}

const applyOptimization = (optimization) => {
  console.log('Applying optimization:', optimization)
  // Simulate optimization application
}

const previewOptimization = (optimization) => {
  console.log('Previewing optimization:', optimization)
  // Show optimization preview
}

const acknowledgeAlert = (alert) => {
  const index = liveAlerts.value.findIndex(a => a.id === alert.id)
  if (index !== -1) {
    liveAlerts.value[index].acknowledged = true
  }
}

const dismissAlert = (alert) => {
  const index = liveAlerts.value.findIndex(a => a.id === alert.id)
  if (index !== -1) {
    liveAlerts.value.splice(index, 1)
  }
}

const formatTimestamp = (timestamp) => {
  return timestamp.toLocaleTimeString()
}

// Auto-refresh interval
let refreshInterval = null

onMounted(() => {
  if (autoRefresh.value) {
    refreshInterval = setInterval(refreshData, 30000) // Refresh every 30 seconds
  }
})

onUnmounted(() => {
  if (refreshInterval) {
    clearInterval(refreshInterval)
  }
})
</script>

<style scoped>
.performance-dashboard {
  @apply space-y-8;
}

.dashboard-title {
  @apply text-2xl font-bold mb-6;
}

/* Dashboard Controls */
.dashboard-controls {
  @apply bg-gray-50 p-4 rounded-lg flex justify-between items-center;
}

.time-range {
  @apply flex items-center gap-2;
}

.range-label {
  @apply text-sm font-medium;
}

.range-select {
  @apply px-3 py-2 border rounded-lg;
}

.dashboard-actions {
  @apply flex gap-2;
}

.action-btn {
  @apply px-4 py-2 rounded-lg border transition-all duration-200;
}

.action-btn.refresh {
  @apply bg-blue-600 text-white hover:bg-blue-700;
}

.action-btn.export {
  @apply bg-green-600 text-white hover:bg-green-700;
}

.action-btn.active {
  @apply bg-purple-600 text-white;
}

/* KPI Section */
.kpi-section {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.section-title {
  @apply text-lg font-semibold mb-4;
}

.kpi-grid {
  @apply grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6;
}

.kpi-card {
  @apply flex items-center gap-4 p-4 bg-gradient-to-r from-blue-50 to-purple-50 rounded-lg;
}

.kpi-icon {
  @apply text-3xl;
}

.kpi-content {
  @apply flex-1;
}

.kpi-value {
  @apply text-2xl font-bold;
}

.kpi-label {
  @apply text-sm text-gray-600;
}

.kpi-trend {
  @apply text-sm font-medium;
}

.kpi-trend.improving {
  @apply text-green-600;
}

.kpi-trend.degrading {
  @apply text-red-600;
}

.kpi-trend.stable {
  @apply text-gray-600;
}

/* Charts Section */
.charts-section {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.charts-grid {
  @apply grid grid-cols-1 lg:grid-cols-2 gap-6;
}

.chart-container {
  @apply border border-gray-200 rounded-lg p-4;
}

.chart-container.full-width {
  @apply lg:col-span-2;
}

.chart-title {
  @apply font-semibold mb-3;
}

.chart-area {
  @apply h-64 mb-3;
}

.chart-canvas {
  @apply w-full h-full border border-gray-100 rounded;
}

.chart-legend {
  @apply flex gap-4 text-sm;
}

.legend-item {
  @apply flex items-center gap-2;
}

.legend-color {
  @apply w-4 h-4 rounded;
}

.legend-color.ultra-bypass {
  @apply bg-orange-500;
}

.legend-color.parallel {
  @apply bg-green-500;
}

.legend-color.traditional {
  @apply bg-blue-500;
}

/* Resource Breakdown */
.resource-breakdown {
  @apply space-y-3;
}

.resource-item {
  @apply flex items-center gap-3;
}

.resource-label {
  @apply w-16 text-sm font-medium;
}

.resource-bar {
  @apply flex-1 h-4 bg-gray-200 rounded-full overflow-hidden;
}

.resource-fill {
  @apply h-full transition-all duration-500;
}

.resource-fill.cpu {
  @apply bg-blue-500;
}

.resource-fill.memory {
  @apply bg-green-500;
}

.resource-fill.io {
  @apply bg-purple-500;
}

.resource-value {
  @apply w-12 text-sm text-right;
}

/* Heatmap */
.heatmap-container {
  @apply overflow-x-auto;
}

.heatmap-grid {
  @apply min-w-full;
}

.heatmap-header {
  @apply flex border-b-2 border-gray-200;
}

.heatmap-corner {
  @apply w-32 p-2 font-semibold bg-gray-100;
}

.heatmap-time {
  @apply w-20 p-2 text-center font-medium bg-gray-100;
}

.heatmap-row {
  @apply flex border-b border-gray-100;
}

.heatmap-stage {
  @apply w-32 p-2 flex items-center gap-2 bg-gray-50;
}

.stage-icon {
  @apply text-lg;
}

.stage-name {
  @apply text-sm font-medium;
}

.heatmap-cell {
  @apply w-20 p-2 text-center text-xs font-medium;
}

.heatmap-cell.minimal {
  @apply bg-green-100 text-green-800;
}

.heatmap-cell.low {
  @apply bg-yellow-100 text-yellow-800;
}

.heatmap-cell.medium {
  @apply bg-orange-100 text-orange-800;
}

.heatmap-cell.high {
  @apply bg-red-100 text-red-800;
}

/* Bottleneck Section */
.bottleneck-section {
  @apply bg-red-50 p-6 rounded-lg;
}

.bottleneck-content {
  @apply space-y-6;
}

.bottleneck-overview {
  @apply flex gap-8 items-center;
}

.bottleneck-score {
  @apply text-center;
}

.score-value {
  @apply text-4xl font-bold;
}

.score-label {
  @apply text-sm text-gray-600;
}

.score-indicator {
  @apply w-16 h-2 rounded-full mx-auto mt-2;
}

.score-indicator.excellent {
  @apply bg-green-500;
}

.score-indicator.good {
  @apply bg-yellow-500;
}

.score-indicator.fair {
  @apply bg-orange-500;
}

.score-indicator.poor {
  @apply bg-red-500;
}

.bottleneck-summary {
  @apply space-y-2;
}

.summary-item {
  @apply flex items-center gap-2;
}

.summary-icon {
  @apply text-lg;
}

.bottleneck-details {
  @apply space-y-4;
}

.bottleneck-item {
  @apply bg-white p-4 rounded-lg;
}

.bottleneck-item.critical {
  @apply border-l-4 border-red-500;
}

.bottleneck-item.warning {
  @apply border-l-4 border-yellow-500;
}

.bottleneck-item.info {
  @apply border-l-4 border-blue-500;
}

.bottleneck-header {
  @apply flex items-center gap-3 mb-2;
}

.bottleneck-icon {
  @apply text-xl;
}

.bottleneck-title {
  @apply font-semibold flex-1;
}

.bottleneck-impact {
  @apply text-sm text-gray-600;
}

.bottleneck-description {
  @apply text-sm text-gray-700 mb-3;
}

.bottleneck-actions {
  @apply flex gap-2;
}

.bottleneck-action {
  @apply px-3 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700;
}

/* Optimization Section */
.optimization-section {
  @apply bg-blue-50 p-6 rounded-lg;
}

.optimization-grid {
  @apply grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6;
}

.recommendation-card {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.rec-header {
  @apply flex items-center gap-3 mb-3;
}

.rec-icon {
  @apply text-2xl;
}

.rec-title {
  @apply font-semibold flex-1;
}

.rec-impact {
  @apply text-sm text-green-600 font-medium;
}

.rec-description {
  @apply text-sm text-gray-700 mb-4;
}

.rec-metrics {
  @apply grid grid-cols-3 gap-2 mb-4 text-xs;
}

.metric {
  @apply text-center;
}

.metric-label {
  @apply text-gray-600;
}

.metric-value {
  @apply font-semibold;
}

.rec-actions {
  @apply flex gap-2;
}

.rec-btn {
  @apply flex-1 py-2 text-sm font-medium rounded;
}

.rec-btn.preview {
  @apply bg-gray-200 text-gray-800 hover:bg-gray-300;
}

.rec-btn.apply {
  @apply bg-green-600 text-white hover:bg-green-700;
}

/* Comparison Section */
.comparison-section {
  @apply bg-green-50 p-6 rounded-lg;
}

.comparison-content {
  @apply space-y-4;
}

.comparison-selector {
  @apply flex items-center gap-2;
}

.selector-label {
  @apply text-sm font-medium;
}

.selector-dropdown {
  @apply px-3 py-2 border rounded-lg;
}

.comparison-metrics {
  @apply grid grid-cols-1 md:grid-cols-3 gap-6;
}

.comparison-metric {
  @apply bg-white p-4 rounded-lg;
}

.metric-name {
  @apply font-semibold mb-2;
}

.metric-comparison {
  @apply flex items-center gap-3 mb-2;
}

.metric-current {
  @apply text-xl font-bold;
}

.metric-arrow {
  @apply text-lg;
}

.metric-baseline {
  @apply text-lg text-gray-600;
}

.metric-change {
  @apply text-sm font-medium;
}

.metric-change.better {
  @apply text-green-600;
}

.metric-change.worse {
  @apply text-red-600;
}

.metric-change.stable {
  @apply text-gray-600;
}

/* Alerts Section */
.alerts-section {
  @apply bg-yellow-50 p-6 rounded-lg;
}

.alerts-container {
  @apply space-y-3;
}

.alert-item {
  @apply bg-white p-4 rounded-lg flex items-center gap-4;
}

.alert-item.critical {
  @apply border-l-4 border-red-500;
}

.alert-item.warning {
  @apply border-l-4 border-yellow-500;
}

.alert-item.info {
  @apply border-l-4 border-blue-500;
}

.alert-icon {
  @apply text-2xl;
}

.alert-content {
  @apply flex-1;
}

.alert-title {
  @apply font-semibold;
}

.alert-message {
  @apply text-sm text-gray-700;
}

.alert-timestamp {
  @apply text-xs text-gray-500;
}

.alert-actions {
  @apply flex gap-2;
}

.alert-btn {
  @apply w-8 h-8 rounded-full flex items-center justify-center;
}

.alert-btn.ack {
  @apply bg-green-600 text-white hover:bg-green-700;
}

.alert-btn.dismiss {
  @apply bg-red-600 text-white hover:bg-red-700;
}
</style>