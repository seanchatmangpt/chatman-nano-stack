<template>
  <div class="pipeline-control-center">
    <!-- Control Center Header -->
    <div class="control-header">
      <h2 class="center-title">🎯 Pipeline Control Center</h2>
      <div class="header-status">
        <div class="system-status">
          <span class="status-label">System Status:</span>
          <div :class="['status-indicator', systemStatus]">{{ getStatusLabel(systemStatus) }}</div>
        </div>
        <div class="active-sessions">
          <span class="sessions-icon">👥</span>
          <span class="sessions-count">{{ activeSessions }} Active</span>
        </div>
        <div class="system-time">
          <span class="time-icon">🕐</span>
          <span class="time-value">{{ currentTime }}</span>
        </div>
      </div>
    </div>

    <!-- Quick Actions Bar -->
    <div class="quick-actions">
      <button @click="quickTransform" class="quick-btn transform" :disabled="isProcessing">
        ⚡ Quick Transform
      </button>
      <button @click="emergencyStop" class="quick-btn stop" :disabled="!isProcessing">
        🛑 Emergency Stop
      </button>
      <button @click="systemDiagnostic" class="quick-btn diagnostic">
        🔧 System Check
      </button>
      <button @click="exportAllData" class="quick-btn export">
        📊 Export All
      </button>
      <button @click="resetSystem" class="quick-btn reset">
        🔄 Reset System
      </button>
    </div>

    <!-- Main Control Tabs -->
    <div class="control-tabs">
      <div class="tab-headers">
        <button 
          v-for="tab in tabs" 
          :key="tab.id"
          @click="activeTab = tab.id"
          :class="['tab-header', { active: activeTab === tab.id }]"
        >
          <span class="tab-icon">{{ tab.icon }}</span>
          <span class="tab-label">{{ tab.label }}</span>
          <span v-if="tab.badge" class="tab-badge">{{ tab.badge }}</span>
        </button>
      </div>

      <!-- Tab Contents -->
      <div class="tab-content">
        <!-- Pipeline Overview Tab -->
        <div v-show="activeTab === 'overview'" class="overview-tab">
          <div class="overview-grid">
            <!-- System Health Card -->
            <div class="overview-card health">
              <h4 class="card-title">🏥 System Health</h4>
              <div class="health-metrics">
                <div class="health-item">
                  <span class="health-label">CPU:</span>
                  <div class="health-bar">
                    <div class="health-fill cpu" :style="{ width: `${systemMetrics.cpu}%` }"></div>
                  </div>
                  <span class="health-value">{{ systemMetrics.cpu }}%</span>
                </div>
                <div class="health-item">
                  <span class="health-label">Memory:</span>
                  <div class="health-bar">
                    <div class="health-fill memory" :style="{ width: `${systemMetrics.memory}%` }"></div>
                  </div>
                  <span class="health-value">{{ systemMetrics.memory }}%</span>
                </div>
                <div class="health-item">
                  <span class="health-label">Network:</span>
                  <div class="health-bar">
                    <div class="health-fill network" :style="{ width: `${systemMetrics.network}%` }"></div>
                  </div>
                  <span class="health-value">{{ systemMetrics.network }}%</span>
                </div>
              </div>
            </div>

            <!-- Active Pipelines Card -->
            <div class="overview-card pipelines">
              <h4 class="card-title">🔄 Active Pipelines</h4>
              <div class="pipeline-summary">
                <div class="pipeline-stat">
                  <span class="stat-number">{{ pipelineStats.running }}</span>
                  <span class="stat-label">Running</span>
                </div>
                <div class="pipeline-stat">
                  <span class="stat-number">{{ pipelineStats.queued }}</span>
                  <span class="stat-label">Queued</span>
                </div>
                <div class="pipeline-stat">
                  <span class="stat-number">{{ pipelineStats.completed }}</span>
                  <span class="stat-label">Completed</span>
                </div>
                <div class="pipeline-stat">
                  <span class="stat-number">{{ pipelineStats.failed }}</span>
                  <span class="stat-label">Failed</span>
                </div>
              </div>
            </div>

            <!-- Recent Activity Card -->
            <div class="overview-card activity">
              <h4 class="card-title">📈 Recent Activity</h4>
              <div class="activity-list">
                <div 
                  v-for="activity in recentActivity" 
                  :key="activity.id"
                  class="activity-item"
                >
                  <span class="activity-icon">{{ activity.icon }}</span>
                  <span class="activity-text">{{ activity.text }}</span>
                  <span class="activity-time">{{ formatTime(activity.timestamp) }}</span>
                </div>
              </div>
            </div>

            <!-- Quick Stats Card -->
            <div class="overview-card stats">
              <h4 class="card-title">📊 Today's Stats</h4>
              <div class="stats-grid">
                <div class="stat-item">
                  <span class="stat-icon">⚡</span>
                  <span class="stat-value">{{ todayStats.transformations }}</span>
                  <span class="stat-label">Transformations</span>
                </div>
                <div class="stat-item">
                  <span class="stat-icon">⏱️</span>
                  <span class="stat-value">{{ todayStats.avgTime }}ms</span>
                  <span class="stat-label">Avg Time</span>
                </div>
                <div class="stat-item">
                  <span class="stat-icon">✅</span>
                  <span class="stat-value">{{ todayStats.successRate }}%</span>
                  <span class="stat-label">Success Rate</span>
                </div>
                <div class="stat-item">
                  <span class="stat-icon">💾</span>
                  <span class="stat-value">{{ todayStats.dataProcessed }}MB</span>
                  <span class="stat-label">Data Processed</span>
                </div>
              </div>
            </div>
          </div>
        </div>

        <!-- Pipeline Transformer Tab -->
        <div v-show="activeTab === 'transformer'" class="transformer-tab">
          <PipelineTransformer />
        </div>

        <!-- Real-time Executor Tab -->
        <div v-show="activeTab === 'executor'" class="executor-tab">
          <RealtimePipelineExecutor />
        </div>

        <!-- Permutation Matrix Tab -->
        <div v-show="activeTab === 'permutations'" class="permutations-tab">
          <div class="permutation-controls">
            <div class="control-section">
              <h4>🎛️ Permutation Controls</h4>
              <div class="control-buttons">
                <button @click="generateNewPermutation" class="perm-btn generate">
                  🎲 Generate New
                </button>
                <button @click="optimizePermutations" class="perm-btn optimize">
                  ⚡ Optimize All
                </button>
                <button @click="savePermutationSet" class="perm-btn save">
                  💾 Save Set
                </button>
              </div>
            </div>
          </div>
          <PermutationMatrixVisualizer />
        </div>

        <!-- Performance Dashboard Tab -->
        <div v-show="activeTab === 'performance'" class="performance-tab">
          <PerformanceDashboard />
        </div>

        <!-- System Settings Tab -->
        <div v-show="activeTab === 'settings'" class="settings-tab">
          <div class="settings-grid">
            <!-- Pipeline Configuration -->
            <div class="settings-section">
              <h4 class="section-title">🔧 Pipeline Configuration</h4>
              <div class="settings-form">
                <div class="form-group">
                  <label class="form-label">Default Mode:</label>
                  <select v-model="settings.defaultMode" class="form-select">
                    <option value="auto">Auto-Select</option>
                    <option value="speed">Speed Priority</option>
                    <option value="comprehensive">Comprehensive</option>
                    <option value="custom">Custom</option>
                  </select>
                </div>
                <div class="form-group">
                  <label class="form-label">Max Parallel Stages:</label>
                  <input 
                    type="range" 
                    min="1" 
                    max="16" 
                    v-model="settings.maxParallel"
                    class="form-range"
                  />
                  <span class="range-value">{{ settings.maxParallel }}</span>
                </div>
                <div class="form-group">
                  <label class="form-label">Timeout (seconds):</label>
                  <input 
                    type="number" 
                    min="10" 
                    max="300" 
                    v-model="settings.timeout"
                    class="form-input"
                  />
                </div>
              </div>
            </div>

            <!-- Performance Settings -->
            <div class="settings-section">
              <h4 class="section-title">⚡ Performance Settings</h4>
              <div class="settings-form">
                <div class="form-group">
                  <label class="form-checkbox">
                    <input type="checkbox" v-model="settings.enableCaching" />
                    <span class="checkbox-label">Enable Smart Caching</span>
                  </label>
                </div>
                <div class="form-group">
                  <label class="form-checkbox">
                    <input type="checkbox" v-model="settings.enableMetrics" />
                    <span class="checkbox-label">Enable Performance Metrics</span>
                  </label>
                </div>
                <div class="form-group">
                  <label class="form-checkbox">
                    <input type="checkbox" v-model="settings.enableOptimizations" />
                    <span class="checkbox-label">Auto-Apply Optimizations</span>
                  </label>
                </div>
                <div class="form-group">
                  <label class="form-label">Memory Limit (MB):</label>
                  <input 
                    type="number" 
                    min="512" 
                    max="8192" 
                    v-model="settings.memoryLimit"
                    class="form-input"
                  />
                </div>
              </div>
            </div>

            <!-- UI Preferences -->
            <div class="settings-section">
              <h4 class="section-title">🎨 UI Preferences</h4>
              <div class="settings-form">
                <div class="form-group">
                  <label class="form-label">Theme:</label>
                  <select v-model="settings.theme" class="form-select">
                    <option value="light">Light</option>
                    <option value="dark">Dark</option>
                    <option value="auto">Auto</option>
                  </select>
                </div>
                <div class="form-group">
                  <label class="form-label">Refresh Rate (seconds):</label>
                  <select v-model="settings.refreshRate" class="form-select">
                    <option value="1">1 second</option>
                    <option value="5">5 seconds</option>
                    <option value="10">10 seconds</option>
                    <option value="30">30 seconds</option>
                  </select>
                </div>
                <div class="form-group">
                  <label class="form-checkbox">
                    <input type="checkbox" v-model="settings.showAnimations" />
                    <span class="checkbox-label">Enable Animations</span>
                  </label>
                </div>
              </div>
            </div>

            <!-- System Maintenance -->
            <div class="settings-section">
              <h4 class="section-title">🛠️ System Maintenance</h4>
              <div class="maintenance-actions">
                <button @click="clearCache" class="maint-btn clear">
                  🗑️ Clear Cache
                </button>
                <button @click="exportLogs" class="maint-btn export">
                  📄 Export Logs
                </button>
                <button @click="runDiagnostics" class="maint-btn diagnose">
                  🔍 Run Diagnostics
                </button>
                <button @click="backupSettings" class="maint-btn backup">
                  💾 Backup Settings
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Status Footer -->
    <div class="control-footer">
      <div class="footer-info">
        <span class="info-item">
          <span class="info-icon">🔄</span>
          <span class="info-text">Last Update: {{ lastUpdate }}</span>
        </span>
        <span class="info-item">
          <span class="info-icon">📊</span>
          <span class="info-text">Uptime: {{ systemUptime }}</span>
        </span>
        <span class="info-item">
          <span class="info-icon">🌐</span>
          <span class="info-text">Version: 2.1.0</span>
        </span>
      </div>
      <div class="footer-actions">
        <button @click="toggleFullscreen" class="footer-btn">
          {{ isFullscreen ? '🗗' : '🗖' }} {{ isFullscreen ? 'Exit' : 'Fullscreen' }}
        </button>
        <button @click="showHelp" class="footer-btn">
          ❓ Help
        </button>
      </div>
    </div>
  </div>
</template>

<script setup>
// 🎯 ULTRATHINK SWARM 80/20: Pipeline Control Center
// Master control interface integrating all pipeline components

import { ref, computed, onMounted, onUnmounted } from 'vue'
import PipelineTransformer from './PipelineTransformer.vue'
import RealtimePipelineExecutor from './RealtimePipelineExecutor.vue'
import PermutationMatrixVisualizer from './PermutationMatrixVisualizer.vue'
import PerformanceDashboard from './PerformanceDashboard.vue'

// Control center state
const activeTab = ref('overview')
const systemStatus = ref('operational')
const isProcessing = ref(false)
const isFullscreen = ref(false)
const activeSessions = ref(3)
const currentTime = ref('')
const lastUpdate = ref('')
const systemUptime = ref('2d 14h 32m')

// System metrics
const systemMetrics = ref({
  cpu: 45,
  memory: 67,
  network: 89
})

// Pipeline statistics
const pipelineStats = ref({
  running: 2,
  queued: 5,
  completed: 847,
  failed: 12
})

// Today's statistics
const todayStats = ref({
  transformations: 127,
  avgTime: 1240,
  successRate: 96.8,
  dataProcessed: 2340
})

// Recent activity
const recentActivity = ref([
  { id: 1, icon: '⚡', text: 'Ultra bypass transformation completed', timestamp: new Date(Date.now() - 120000) },
  { id: 2, icon: '🔄', text: 'Parallel pipeline started', timestamp: new Date(Date.now() - 300000) },
  { id: 3, icon: '✅', text: 'K8s deployment successful', timestamp: new Date(Date.now() - 600000) },
  { id: 4, icon: '⚠️', text: 'Memory usage threshold exceeded', timestamp: new Date(Date.now() - 900000) },
  { id: 5, icon: '📊', text: 'Performance report generated', timestamp: new Date(Date.now() - 1200000) }
])

// Settings
const settings = ref({
  defaultMode: 'auto',
  maxParallel: 8,
  timeout: 60,
  enableCaching: true,
  enableMetrics: true,
  enableOptimizations: false,
  memoryLimit: 2048,
  theme: 'light',
  refreshRate: 5,
  showAnimations: true
})

// Tab configuration
const tabs = ref([
  { id: 'overview', label: 'Overview', icon: '🏠', badge: null },
  { id: 'transformer', label: 'Transformer', icon: '🔄', badge: null },
  { id: 'executor', label: 'Executor', icon: '⚡', badge: isProcessing.value ? '●' : null },
  { id: 'permutations', label: 'Permutations', icon: '🎲', badge: null },
  { id: 'performance', label: 'Performance', icon: '📊', badge: null },
  { id: 'settings', label: 'Settings', icon: '⚙️', badge: null }
])

// Update intervals
let timeInterval = null
let metricsInterval = null

// Computed properties
const getStatusLabel = (status) => {
  const labels = {
    operational: 'Operational',
    warning: 'Warning',
    critical: 'Critical',
    maintenance: 'Maintenance'
  }
  return labels[status] || status
}

// Methods
const quickTransform = () => {
  isProcessing.value = true
  activeTab.value = 'transformer'
  
  // Simulate quick transformation
  setTimeout(() => {
    isProcessing.value = false
    addActivity('⚡', 'Quick transformation completed')
  }, 3000)
}

const emergencyStop = () => {
  isProcessing.value = false
  systemStatus.value = 'warning'
  addActivity('🛑', 'Emergency stop activated')
  
  setTimeout(() => {
    systemStatus.value = 'operational'
  }, 5000)
}

const systemDiagnostic = () => {
  addActivity('🔧', 'System diagnostic started')
  
  // Simulate diagnostic
  setTimeout(() => {
    addActivity('✅', 'System diagnostic completed - All systems normal')
  }, 2000)
}

const exportAllData = () => {
  const exportData = {
    timestamp: new Date().toISOString(),
    systemMetrics: systemMetrics.value,
    pipelineStats: pipelineStats.value,
    todayStats: todayStats.value,
    settings: settings.value
  }
  
  const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `pipeline-control-export-${Date.now()}.json`
  a.click()
  URL.revokeObjectURL(url)
  
  addActivity('📊', 'System data exported')
}

const resetSystem = () => {
  if (confirm('Are you sure you want to reset the system? This will stop all running processes.')) {
    isProcessing.value = false
    systemStatus.value = 'maintenance'
    addActivity('🔄', 'System reset initiated')
    
    setTimeout(() => {
      systemStatus.value = 'operational'
      addActivity('✅', 'System reset completed')
    }, 3000)
  }
}

const generateNewPermutation = () => {
  addActivity('🎲', 'New permutation generated')
}

const optimizePermutations = () => {
  addActivity('⚡', 'Permutation optimization started')
  setTimeout(() => {
    addActivity('✅', 'Permutation optimization completed')
  }, 1500)
}

const savePermutationSet = () => {
  addActivity('💾', 'Permutation set saved')
}

const clearCache = () => {
  addActivity('🗑️', 'Cache cleared')
}

const exportLogs = () => {
  addActivity('📄', 'Logs exported')
}

const runDiagnostics = () => {
  addActivity('🔍', 'Diagnostics started')
  setTimeout(() => {
    addActivity('✅', 'Diagnostics completed')
  }, 2000)
}

const backupSettings = () => {
  addActivity('💾', 'Settings backed up')
}

const toggleFullscreen = () => {
  isFullscreen.value = !isFullscreen.value
  
  if (isFullscreen.value) {
    document.documentElement.requestFullscreen?.()
  } else {
    document.exitFullscreen?.()
  }
}

const showHelp = () => {
  alert('Pipeline Control Center Help:\n\n• Overview: System status and metrics\n• Transformer: Main transformation interface\n• Executor: Real-time pipeline execution\n• Permutations: Permutation matrix management\n• Performance: Analytics and monitoring\n• Settings: System configuration')
}

const addActivity = (icon, text) => {
  recentActivity.value.unshift({
    id: Date.now(),
    icon,
    text,
    timestamp: new Date()
  })
  
  if (recentActivity.value.length > 10) {
    recentActivity.value = recentActivity.value.slice(0, 10)
  }
}

const formatTime = (timestamp) => {
  const now = new Date()
  const diff = now - timestamp
  const minutes = Math.floor(diff / 60000)
  
  if (minutes < 1) return 'Just now'
  if (minutes < 60) return `${minutes}m ago`
  
  const hours = Math.floor(minutes / 60)
  return `${hours}h ago`
}

const updateTime = () => {
  currentTime.value = new Date().toLocaleTimeString()
  lastUpdate.value = new Date().toLocaleString()
}

const updateMetrics = () => {
  // Simulate metric updates
  systemMetrics.value.cpu = Math.max(10, Math.min(90, systemMetrics.value.cpu + (Math.random() - 0.5) * 10))
  systemMetrics.value.memory = Math.max(20, Math.min(85, systemMetrics.value.memory + (Math.random() - 0.5) * 5))
  systemMetrics.value.network = Math.max(30, Math.min(100, systemMetrics.value.network + (Math.random() - 0.5) * 15))
  
  // Update tab badges
  tabs.value[2].badge = isProcessing.value ? '●' : null
}

// Lifecycle
onMounted(() => {
  updateTime()
  timeInterval = setInterval(updateTime, 1000)
  metricsInterval = setInterval(updateMetrics, 5000)
  
  addActivity('🚀', 'Pipeline Control Center initialized')
})

onUnmounted(() => {
  if (timeInterval) clearInterval(timeInterval)
  if (metricsInterval) clearInterval(metricsInterval)
})
</script>

<style scoped>
.pipeline-control-center {
  @apply min-h-screen bg-gray-50 p-6;
}

/* Control Header */
.control-header {
  @apply bg-white rounded-lg shadow-md p-6 mb-6 flex justify-between items-center;
}

.center-title {
  @apply text-3xl font-bold text-gray-800;
}

.header-status {
  @apply flex gap-6 items-center;
}

.system-status {
  @apply flex items-center gap-2;
}

.status-label {
  @apply text-sm text-gray-600;
}

.status-indicator {
  @apply px-3 py-1 rounded-full text-sm font-medium;
}

.status-indicator.operational {
  @apply bg-green-100 text-green-800;
}

.status-indicator.warning {
  @apply bg-yellow-100 text-yellow-800;
}

.status-indicator.critical {
  @apply bg-red-100 text-red-800;
}

.status-indicator.maintenance {
  @apply bg-blue-100 text-blue-800;
}

.active-sessions, .system-time {
  @apply flex items-center gap-1 text-sm text-gray-600;
}

/* Quick Actions */
.quick-actions {
  @apply bg-white rounded-lg shadow-md p-4 mb-6 flex gap-3 flex-wrap;
}

.quick-btn {
  @apply px-4 py-2 rounded-lg font-medium transition-all duration-200;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.quick-btn.transform {
  @apply bg-green-600 text-white hover:bg-green-700;
}

.quick-btn.stop {
  @apply bg-red-600 text-white hover:bg-red-700;
}

.quick-btn.diagnostic {
  @apply bg-blue-600 text-white hover:bg-blue-700;
}

.quick-btn.export {
  @apply bg-purple-600 text-white hover:bg-purple-700;
}

.quick-btn.reset {
  @apply bg-gray-600 text-white hover:bg-gray-700;
}

/* Control Tabs */
.control-tabs {
  @apply bg-white rounded-lg shadow-md overflow-hidden;
}

.tab-headers {
  @apply flex border-b border-gray-200 overflow-x-auto;
}

.tab-header {
  @apply flex items-center gap-2 px-6 py-4 border-b-2 border-transparent;
  @apply hover:bg-gray-50 transition-all duration-200 whitespace-nowrap;
}

.tab-header.active {
  @apply border-blue-500 bg-blue-50 text-blue-700;
}

.tab-icon {
  @apply text-lg;
}

.tab-label {
  @apply font-medium;
}

.tab-badge {
  @apply bg-red-500 text-white text-xs rounded-full w-2 h-2;
}

.tab-content {
  @apply p-6;
}

/* Overview Tab */
.overview-grid {
  @apply grid grid-cols-1 lg:grid-cols-2 gap-6;
}

.overview-card {
  @apply bg-gray-50 rounded-lg p-6;
}

.card-title {
  @apply text-lg font-semibold mb-4;
}

/* Health Metrics */
.health-metrics {
  @apply space-y-3;
}

.health-item {
  @apply flex items-center gap-3;
}

.health-label {
  @apply w-16 text-sm font-medium;
}

.health-bar {
  @apply flex-1 h-3 bg-gray-200 rounded-full overflow-hidden;
}

.health-fill {
  @apply h-full transition-all duration-500;
}

.health-fill.cpu {
  @apply bg-blue-500;
}

.health-fill.memory {
  @apply bg-green-500;
}

.health-fill.network {
  @apply bg-purple-500;
}

.health-value {
  @apply w-12 text-sm text-right;
}

/* Pipeline Summary */
.pipeline-summary {
  @apply grid grid-cols-2 gap-4;
}

.pipeline-stat {
  @apply text-center;
}

.stat-number {
  @apply block text-2xl font-bold;
}

.stat-label {
  @apply text-sm text-gray-600;
}

/* Activity List */
.activity-list {
  @apply space-y-2 max-h-48 overflow-y-auto;
}

.activity-item {
  @apply flex items-center gap-3 p-2 bg-white rounded;
}

.activity-icon {
  @apply text-lg;
}

.activity-text {
  @apply flex-1 text-sm;
}

.activity-time {
  @apply text-xs text-gray-500;
}

/* Stats Grid */
.stats-grid {
  @apply grid grid-cols-2 gap-4;
}

.stat-item {
  @apply text-center p-3 bg-white rounded;
}

.stat-icon {
  @apply block text-2xl mb-1;
}

.stat-value {
  @apply block text-lg font-bold;
}

.stat-label {
  @apply text-xs text-gray-600;
}

/* Permutation Controls */
.permutation-controls {
  @apply mb-6;
}

.control-section {
  @apply bg-gray-50 p-4 rounded-lg;
}

.control-buttons {
  @apply flex gap-2 mt-2;
}

.perm-btn {
  @apply px-3 py-2 bg-blue-600 text-white rounded hover:bg-blue-700;
}

/* Settings Tab */
.settings-grid {
  @apply grid grid-cols-1 lg:grid-cols-2 gap-6;
}

.settings-section {
  @apply bg-gray-50 rounded-lg p-6;
}

.section-title {
  @apply text-lg font-semibold mb-4;
}

.settings-form {
  @apply space-y-4;
}

.form-group {
  @apply space-y-1;
}

.form-label {
  @apply block text-sm font-medium text-gray-700;
}

.form-select, .form-input {
  @apply w-full px-3 py-2 border rounded-lg;
}

.form-range {
  @apply w-full;
}

.range-value {
  @apply ml-2 text-sm font-medium;
}

.form-checkbox {
  @apply flex items-center gap-2;
}

.checkbox-label {
  @apply text-sm;
}

.maintenance-actions {
  @apply grid grid-cols-2 gap-2;
}

.maint-btn {
  @apply px-3 py-2 bg-gray-600 text-white rounded hover:bg-gray-700 text-sm;
}

/* Control Footer */
.control-footer {
  @apply bg-white rounded-lg shadow-md p-4 mt-6 flex justify-between items-center;
}

.footer-info {
  @apply flex gap-6 text-sm text-gray-600;
}

.info-item {
  @apply flex items-center gap-1;
}

.footer-actions {
  @apply flex gap-2;
}

.footer-btn {
  @apply px-3 py-1 bg-gray-100 hover:bg-gray-200 rounded text-sm;
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .pipeline-control-center {
    @apply p-4;
  }
  
  .control-header {
    @apply flex-col gap-4 items-start;
  }
  
  .header-status {
    @apply flex-col gap-2 items-start;
  }
  
  .overview-grid {
    @apply grid-cols-1;
  }
  
  .settings-grid {
    @apply grid-cols-1;
  }
  
  .footer-info {
    @apply flex-col gap-1;
  }
}
</style>