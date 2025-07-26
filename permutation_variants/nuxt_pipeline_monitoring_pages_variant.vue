<!-- BitActor Nuxt.js Pipeline Monitoring Pages Variant -->
<!-- This variant explores complete Nuxt page patterns with:
     - Full-stack pipeline monitoring pages
     - TTL-aware page rendering and data fetching
     - Real-time WebSocket data integration in pages
     - SSR/SPA hybrid rendering for optimal performance
     - Page-level swarm coordination and management
     - Complete BitActor pipeline visualization pages
-->

<!-- =============================================================================
     Main Dashboard Page (/pages/index.vue)
     ============================================================================= -->
<template name="dashboard-page">
  <NuxtLayout name="cybersecurity-dashboard">
    <div class="dashboard-page">
      <!-- Hero Section with Real-time Overview -->
      <section class="dashboard-hero">
        <div class="hero-content">
          <h1 class="hero-title">
            🛡️ BitActor Cybersecurity Platform
          </h1>
          <p class="hero-description">
            Real-time threat detection and pipeline monitoring with nanosecond TTL precision
          </p>
          
          <div class="hero-metrics">
            <div class="metric-card" :class="systemHealthClass">
              <span class="metric-icon">🏥</span>
              <div class="metric-info">
                <span class="metric-value">{{ systemHealth }}</span>
                <span class="metric-label">System Health</span>
              </div>
            </div>
            
            <div class="metric-card">
              <span class="metric-icon">⚡</span>
              <div class="metric-info">
                <span class="metric-value">{{ activePipelines }}</span>
                <span class="metric-label">Active Pipelines</span>
              </div>
            </div>
            
            <div class="metric-card">
              <span class="metric-icon">🐝</span>
              <div class="metric-info">
                <span class="metric-value">{{ activeSwarms }}</span>
                <span class="metric-label">Active Swarms</span>
              </div>
            </div>
            
            <div class="metric-card" :class="threatLevelClass">
              <span class="metric-icon">🚨</span>
              <div class="metric-info">
                <span class="metric-value">{{ threatLevel }}</span>
                <span class="metric-label">Threat Level</span>
              </div>
            </div>
          </div>
        </div>
      </section>

      <!-- Quick Actions Section -->
      <section class="quick-actions-section">
        <div class="section-header">
          <h2>🚀 Quick Actions</h2>
          <p>Initiate common BitActor operations with TTL constraints</p>
        </div>
        
        <div class="actions-grid">
          <div class="action-card" @click="runQuickPipeline">
            <div class="action-icon">⚡</div>
            <div class="action-content">
              <h3>Run Pipeline</h3>
              <p>Execute full 8-stage pipeline</p>
              <span class="action-ttl">TTL: {{ pipelineTTL }}ms</span>
            </div>
          </div>
          
          <div class="action-card" @click="coordinateSwarms">
            <div class="action-icon">🐝</div>
            <div class="action-content">
              <h3>Coordinate Swarms</h3>
              <p>Sync all active swarms</p>
              <span class="action-ttl">TTL: {{ swarmTTL }}ms</span>
            </div>
          </div>
          
          <div class="action-card" @click="runThreatScan">
            <div class="action-icon">🔍</div>
            <div class="action-content">
              <h3>Threat Scan</h3>
              <p>Full system threat analysis</p>
              <span class="action-ttl">TTL: {{ threatScanTTL }}ms</span>
            </div>
          </div>
          
          <div class="action-card" @click="generateReport">
            <div class="action-icon">📊</div>
            <div class="action-content">
              <h3>Generate Report</h3>
              <p>System performance report</p>
              <span class="action-ttl">TTL: {{ reportTTL }}ms</span>
            </div>
          </div>
        </div>
      </section>

      <!-- Real-time Status Grid -->
      <section class="status-grid-section">
        <div class="section-header">
          <h2>📊 Real-time Status</h2>
          <div class="auto-refresh-control">
            <label>
              <input type="checkbox" v-model="autoRefresh" @change="toggleAutoRefresh">
              Auto-refresh ({{ refreshInterval }}s)
            </label>
          </div>
        </div>
        
        <div class="status-grid">
          <!-- Pipeline Status -->
          <div class="status-panel">
            <div class="panel-header">
              <h3>🔄 Pipeline Status</h3>
              <span class="last-updated">{{ lastPipelineUpdate }}</span>
            </div>
            <div class="panel-content">
              <div class="pipeline-visualization">
                <div 
                  v-for="stage in pipelineStages" 
                  :key="stage.id"
                  class="pipeline-stage"
                  :class="{ 
                    active: stage.active, 
                    completed: stage.completed,
                    error: stage.error 
                  }"
                >
                  <div class="stage-indicator"></div>
                  <span class="stage-name">{{ stage.name }}</span>
                  <span class="stage-time">{{ stage.executionTime }}ms</span>
                </div>
              </div>
            </div>
          </div>

          <!-- Threat Detection -->
          <div class="status-panel">
            <div class="panel-header">
              <h3>🚨 Threat Detection</h3>
              <span class="threat-count">{{ recentThreats.length }} recent</span>
            </div>
            <div class="panel-content">
              <div class="threat-list">
                <div 
                  v-for="threat in recentThreats.slice(0, 5)" 
                  :key="threat.id"
                  class="threat-item"
                  :class="threat.severity"
                >
                  <span class="threat-icon">{{ threat.icon }}</span>
                  <div class="threat-info">
                    <span class="threat-type">{{ threat.type }}</span>
                    <span class="threat-time">{{ formatTime(threat.timestamp) }}</span>
                  </div>
                </div>
              </div>
              <NuxtLink to="/threats" class="view-all-link">
                View all threats →
              </NuxtLink>
            </div>
          </div>

          <!-- Swarm Coordination -->
          <div class="status-panel">
            <div class="panel-header">
              <h3>🐝 Swarm Status</h3>
              <span class="swarm-health">{{ swarmHealthPercentage }}% healthy</span>
            </div>
            <div class="panel-content">
              <div class="swarm-list">
                <div 
                  v-for="swarm in swarmStatus" 
                  :key="swarm.id"
                  class="swarm-item"
                  :class="swarm.healthStatus"
                >
                  <div class="swarm-indicator"></div>
                  <div class="swarm-info">
                    <span class="swarm-name">{{ swarm.name }}</span>
                    <span class="swarm-agents">{{ swarm.activeAgents }}/{{ swarm.totalAgents }}</span>
                  </div>
                  <span class="swarm-load">{{ swarm.load }}%</span>
                </div>
              </div>
              <NuxtLink to="/swarms" class="view-all-link">
                Manage swarms →
              </NuxtLink>
            </div>
          </div>

          <!-- TTL Performance -->
          <div class="status-panel">
            <div class="panel-header">
              <h3>⏱️ TTL Performance</h3>
              <span class="ttl-compliance">{{ ttlComplianceRate }}% compliant</span>
            </div>
            <div class="panel-content">
              <div class="ttl-metrics">
                <div class="ttl-metric">
                  <span class="metric-label">Avg Execution:</span>
                  <span class="metric-value">{{ avgExecutionTime }}ms</span>
                </div>
                <div class="ttl-metric">
                  <span class="metric-label">Violations:</span>
                  <span class="metric-value error">{{ ttlViolations }}</span>
                </div>
                <div class="ttl-metric">
                  <span class="metric-label">Budget Used:</span>
                  <span class="metric-value">{{ ttlBudgetUsed }}%</span>
                </div>
              </div>
              <div class="ttl-chart">
                <!-- Simplified TTL usage chart -->
                <div class="chart-bar" 
                     :style="{ height: `${ttlBudgetUsed}%` }"
                     :class="{ warning: ttlBudgetUsed > 75, critical: ttlBudgetUsed > 90 }">
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      <!-- Recent Activity Feed -->
      <section class="activity-feed-section">
        <div class="section-header">
          <h2>📋 Recent Activity</h2>
          <button class="clear-activity-btn" @click="clearActivityFeed">
            🗑️ Clear
          </button>
        </div>
        
        <div class="activity-feed">
          <div 
            v-for="activity in recentActivity" 
            :key="activity.id"
            class="activity-item"
            :class="activity.type"
          >
            <span class="activity-icon">{{ activity.icon }}</span>
            <div class="activity-content">
              <span class="activity-message">{{ activity.message }}</span>
              <span class="activity-time">{{ formatTime(activity.timestamp) }}</span>
            </div>
            <span class="activity-duration" v-if="activity.duration">
              {{ activity.duration }}ms
            </span>
          </div>
        </div>
      </section>
    </div>
  </NuxtLayout>
</template>

<!-- =============================================================================
     Real-time Monitoring Page (/pages/monitoring.vue)
     ============================================================================= -->
<template name="monitoring-page">
  <NuxtLayout name="monitoring-layout">
    <template #threat-map>
      <div class="threat-map-container">
        <div class="map-controls">
          <select v-model="mapTimeRange" @change="updateThreatMap">
            <option value="1m">Last 1 minute</option>
            <option value="5m">Last 5 minutes</option>
            <option value="15m">Last 15 minutes</option>
            <option value="1h">Last 1 hour</option>
          </select>
        </div>
        
        <div class="threat-map-grid">
          <div 
            v-for="region in threatRegions" 
            :key="region.id"
            class="threat-region"
            :class="region.threatLevel"
            @click="selectRegion(region)"
          >
            <span class="region-name">{{ region.name }}</span>
            <span class="threat-count">{{ region.threatCount }}</span>
          </div>
        </div>
      </div>
    </template>

    <template #pipeline-flow>
      <div class="pipeline-flow-container">
        <svg class="pipeline-svg" viewBox="0 0 800 200">
          <g 
            v-for="(stage, index) in monitoringPipelineStages" 
            :key="stage.id"
            :transform="`translate(${index * 90 + 40}, 100)`"
          >
            <circle 
              :r="20" 
              :fill="stage.color"
              :class="{ 
                active: stage.active,
                processing: stage.processing,
                error: stage.error 
              }"
            />
            <text y="35" text-anchor="middle" class="stage-label">
              {{ stage.name }}
            </text>
            <text y="50" text-anchor="middle" class="stage-time">
              {{ stage.lastExecutionTime }}ms
            </text>
            
            <!-- Connection line -->
            <line 
              v-if="index < monitoringPipelineStages.length - 1"
              x1="20" y1="0" x2="70" y2="0"
              class="connection-line"
              :class="{ active: stage.active && monitoringPipelineStages[index + 1].active }"
            />
          </g>
        </svg>
        
        <div class="pipeline-controls">
          <button class="control-btn" @click="runPipelineStep">
            ▶️ Execute Step
          </button>
          <button class="control-btn" @click="runFullPipeline">
            ⚡ Full Pipeline
          </button>
          <button class="control-btn" @click="stopPipeline">
            ⏹️ Stop
          </button>
        </div>
      </div>
    </template>

    <template #alert-stream>
      <div class="alert-stream-container">
        <div class="stream-controls">
          <button class="stream-toggle" @click="toggleAlertStream">
            {{ alertStreamActive ? '⏸️ Pause' : '▶️ Start' }}
          </button>
          <span class="stream-rate">{{ alertRate }} alerts/min</span>
        </div>
        
        <div class="alert-list">
          <div 
            v-for="alert in realtimeAlerts" 
            :key="alert.id"
            class="alert-item"
            :class="alert.severity"
          >
            <span class="alert-timestamp">{{ formatTimestamp(alert.timestamp) }}</span>
            <span class="alert-type">{{ alert.type }}</span>
            <span class="alert-message">{{ alert.message }}</span>
          </div>
        </div>
      </div>
    </template>

    <template #system-metrics>
      <div class="metrics-container">
        <div class="metrics-grid">
          <div class="metric-tile">
            <span class="metric-icon">💻</span>
            <div class="metric-info">
              <span class="metric-value">{{ systemMetrics.cpuUsage }}%</span>
              <span class="metric-label">CPU Usage</span>
            </div>
          </div>
          
          <div class="metric-tile">
            <span class="metric-icon">🧠</span>
            <div class="metric-info">
              <span class="metric-value">{{ systemMetrics.memoryUsage }}%</span>
              <span class="metric-label">Memory</span>
            </div>
          </div>
          
          <div class="metric-tile">
            <span class="metric-icon">💾</span>
            <div class="metric-info">
              <span class="metric-value">{{ systemMetrics.diskUsage }}%</span>
              <span class="metric-label">Disk</span>
            </div>
          </div>
          
          <div class="metric-tile">
            <span class="metric-icon">🌐</span>
            <div class="metric-info">
              <span class="metric-value">{{ systemMetrics.networkLoad }}%</span>
              <span class="metric-label">Network</span>
            </div>
          </div>
        </div>
      </div>
    </template>

    <template #network-traffic>
      <div class="network-container">
        <div class="traffic-stats">
          <div class="stat-item">
            <span class="stat-label">Inbound:</span>
            <span class="stat-value">{{ networkStats.inbound }} MB/s</span>
          </div>
          <div class="stat-item">
            <span class="stat-label">Outbound:</span>
            <span class="stat-value">{{ networkStats.outbound }} MB/s</span>
          </div>
          <div class="stat-item">
            <span class="stat-label">Connections:</span>
            <span class="stat-value">{{ networkStats.connections }}</span>
          </div>
        </div>
        
        <div class="traffic-visualization">
          <!-- Simplified network traffic visualization -->
          <div class="traffic-flow inbound" :style="{ width: `${networkStats.inbound * 10}%` }"></div>
          <div class="traffic-flow outbound" :style="{ width: `${networkStats.outbound * 10}%` }"></div>
        </div>
      </div>
    </template>

    <template #swarm-status>
      <div class="swarm-status-container">
        <div class="swarm-overview">
          <div class="swarm-stat">
            <span class="stat-label">Total Swarms:</span>
            <span class="stat-value">{{ totalSwarms }}</span>
          </div>
          <div class="swarm-stat">
            <span class="stat-label">Active Agents:</span>
            <span class="stat-value">{{ totalActiveAgents }}</span>
          </div>
        </div>
        
        <div class="swarm-health-grid">
          <div 
            v-for="swarm in monitoringSwarms" 
            :key="swarm.id"
            class="swarm-health-tile"
            :class="swarm.status"
          >
            <div class="swarm-name">{{ swarm.name }}</div>
            <div class="swarm-agents">{{ swarm.activeAgents }}/{{ swarm.totalAgents }}</div>
            <div class="swarm-load-bar">
              <div class="load-fill" :style="{ width: `${swarm.load}%` }"></div>
            </div>
          </div>
        </div>
      </div>
    </template>
  </NuxtLayout>
</template>

<!-- =============================================================================
     Pipeline Analysis Page (/pages/analysis.vue)
     ============================================================================= -->
<template name="analysis-page">
  <NuxtLayout name="analysis-layout">
    <template #overview>
      <div class="analysis-overview">
        <div class="overview-stats">
          <div class="stat-card">
            <h3>📊 Performance Summary</h3>
            <div class="stat-grid">
              <div class="stat-item">
                <span class="stat-value">{{ analysisData.totalExecutions }}</span>
                <span class="stat-label">Total Executions</span>
              </div>
              <div class="stat-item">
                <span class="stat-value">{{ analysisData.avgExecutionTime }}ms</span>
                <span class="stat-label">Avg Execution Time</span>
              </div>
              <div class="stat-item">
                <span class="stat-value">{{ analysisData.successRate }}%</span>
                <span class="stat-label">Success Rate</span>
              </div>
              <div class="stat-item">
                <span class="stat-value">{{ analysisData.ttlCompliance }}%</span>
                <span class="stat-label">TTL Compliance</span>
              </div>
            </div>
          </div>
          
          <div class="stat-card">
            <h3>🚨 Security Summary</h3>
            <div class="security-metrics">
              <div class="security-item">
                <span class="security-icon">🦠</span>
                <div class="security-info">
                  <span class="security-count">{{ analysisData.malwareDetected }}</span>
                  <span class="security-label">Malware Detected</span>
                </div>
              </div>
              <div class="security-item">
                <span class="security-icon">🚪</span>
                <div class="security-info">
                  <span class="security-count">{{ analysisData.intrusionAttempts }}</span>
                  <span class="security-label">Intrusion Attempts</span>
                </div>
              </div>
              <div class="security-item">
                <span class="security-icon">⚠️</span>
                <div class="security-info">
                  <span class="security-count">{{ analysisData.anomaliesDetected }}</span>
                  <span class="security-label">Anomalies</span>
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <div class="recommendations">
          <h3>💡 Recommendations</h3>
          <ul class="recommendation-list">
            <li v-for="rec in analysisData.recommendations" :key="rec.id" 
                class="recommendation-item" :class="rec.priority">
              <span class="rec-icon">{{ rec.icon }}</span>
              <div class="rec-content">
                <span class="rec-title">{{ rec.title }}</span>
                <span class="rec-description">{{ rec.description }}</span>
              </div>
            </li>
          </ul>
        </div>
      </div>
    </template>

    <template #timeline>
      <div class="timeline-analysis">
        <div class="timeline-controls">
          <input 
            v-model="timelineRange" 
            type="range" 
            min="1" 
            max="24" 
            class="timeline-slider"
          >
          <span>{{ timelineRange }} hours</span>
        </div>
        
        <div class="timeline-chart">
          <div class="chart-container">
            <!-- Simplified timeline visualization -->
            <div 
              v-for="point in timelineData" 
              :key="point.timestamp"
              class="timeline-point"
              :style="{ left: `${point.position}%`, bottom: `${point.value}%` }"
              :class="point.type"
            ></div>
          </div>
        </div>
      </div>
    </template>

    <template #patterns>
      <div class="pattern-analysis">
        <div class="pattern-categories">
          <div class="pattern-category">
            <h4>🔄 Execution Patterns</h4>
            <ul class="pattern-list">
              <li v-for="pattern in executionPatterns" :key="pattern.id">
                {{ pattern.description }}
              </li>
            </ul>
          </div>
          
          <div class="pattern-category">
            <h4>🚨 Threat Patterns</h4>
            <ul class="pattern-list">
              <li v-for="pattern in threatPatterns" :key="pattern.id">
                {{ pattern.description }}
              </li>
            </ul>
          </div>
          
          <div class="pattern-category">
            <h4>⏱️ TTL Patterns</h4>
            <ul class="pattern-list">
              <li v-for="pattern in ttlPatterns" :key="pattern.id">
                {{ pattern.description }}
              </li>
            </ul>
          </div>
        </div>
      </div>
    </template>

    <template #correlations>
      <div class="correlation-analysis">
        <div class="correlation-matrix">
          <h4>📊 Event Correlations</h4>
          <div class="matrix-grid">
            <div 
              v-for="correlation in correlationData" 
              :key="correlation.id"
              class="correlation-cell"
              :style="{ opacity: correlation.strength }"
            >
              <span class="correlation-value">{{ correlation.value }}</span>
            </div>
          </div>
        </div>
      </div>
    </template>

    <template #predictions>
      <div class="prediction-analysis">
        <div class="prediction-cards">
          <div class="prediction-card">
            <h4>🔮 System Load Prediction</h4>
            <div class="prediction-chart">
              <div class="prediction-line"></div>
            </div>
            <p>Expected peak load at {{ predictedPeakTime }}</p>
          </div>
          
          <div class="prediction-card">
            <h4>🚨 Threat Forecast</h4>
            <div class="threat-probability">
              <div class="probability-bar" :style="{ width: `${threatProbability}%` }"></div>
            </div>
            <p>{{ threatProbability }}% chance of critical threat in next hour</p>
          </div>
        </div>
      </div>
    </template>
  </NuxtLayout>
</template>

<!-- =============================================================================
     Swarm Management Page (/pages/swarms.vue)
     ============================================================================= -->
<template name="swarms-page">
  <NuxtLayout name="cybersecurity-dashboard">
    <div class="swarms-page">
      <div class="page-header">
        <h1>🐝 Swarm Management</h1>
        <div class="page-actions">
          <button class="create-swarm-btn" @click="createNewSwarm">
            ➕ Create Swarm
          </button>
          <button class="coordinate-all-btn" @click="coordinateAllSwarms">
            🔗 Coordinate All
          </button>
        </div>
      </div>
      
      <div class="swarms-grid">
        <div 
          v-for="swarm in swarmList" 
          :key="swarm.id"
          class="swarm-card"
          :class="swarm.status"
        >
          <div class="swarm-header">
            <h3>{{ swarm.name }}</h3>
            <div class="swarm-status-indicator" :class="swarm.status"></div>
          </div>
          
          <div class="swarm-stats">
            <div class="stat">
              <span class="stat-label">Agents:</span>
              <span class="stat-value">{{ swarm.activeAgents }}/{{ swarm.totalAgents }}</span>
            </div>
            <div class="stat">
              <span class="stat-label">Load:</span>
              <span class="stat-value">{{ swarm.currentLoad }}%</span>
            </div>
            <div class="stat">
              <span class="stat-label">TTL Budget:</span>
              <span class="stat-value">{{ swarm.ttlBudget }}ms</span>
            </div>
          </div>
          
          <div class="swarm-actions">
            <button class="action-btn" @click="manageSwarm(swarm.id)">
              ⚙️ Manage
            </button>
            <button class="action-btn" @click="viewSwarmDetails(swarm.id)">
              📊 Details
            </button>
            <button class="action-btn danger" @click="stopSwarm(swarm.id)">
              ⏹️ Stop
            </button>
          </div>
        </div>
      </div>
      
      <!-- Swarm Creation Modal -->
      <div v-if="showCreateModal" class="modal-overlay" @click="closeCreateModal">
        <div class="modal-content" @click.stop>
          <h2>Create New Swarm</h2>
          <form @submit.prevent="submitNewSwarm">
            <div class="form-group">
              <label>Swarm Name:</label>
              <input v-model="newSwarm.name" type="text" required>
            </div>
            <div class="form-group">
              <label>Topology:</label>
              <select v-model="newSwarm.topology">
                <option value="mesh">Mesh</option>
                <option value="hierarchical">Hierarchical</option>
                <option value="ring">Ring</option>
                <option value="star">Star</option>
              </select>
            </div>
            <div class="form-group">
              <label>Max Agents:</label>
              <input v-model.number="newSwarm.maxAgents" type="number" min="1" max="20">
            </div>
            <div class="form-group">
              <label>TTL Budget (ms):</label>
              <input v-model.number="newSwarm.ttlBudget" type="number" min="1" max="100">
            </div>
            <div class="form-actions">
              <button type="button" @click="closeCreateModal">Cancel</button>
              <button type="submit">Create Swarm</button>
            </div>
          </form>
        </div>
      </div>
    </div>
  </NuxtLayout>
</template>

<!-- =============================================================================
     Page Script with SSR and WebSocket Integration
     ============================================================================= -->
<script setup>
// =============================================================================
// Page Composables and State
// =============================================================================

// Import BitActor composables
const { executePipeline, ttlMetrics } = useBitActorSSR()
const { connectionStatus, executionResults } = useWebSocketPipeline()
const { createSwarm, coordinateSwarms, getSwarmHealth } = useSwarmCoordination()

// Page metadata
definePageMeta({
  layout: false, // Using custom layout
  title: 'BitActor Cybersecurity Platform',
  description: 'Real-time threat detection and pipeline monitoring'
})

// SEO head
useHead({
  title: 'BitActor Cybersecurity Platform',
  meta: [
    { name: 'description', content: 'Real-time threat detection and pipeline monitoring with nanosecond TTL precision' },
    { name: 'keywords', content: 'cybersecurity, threat detection, pipeline monitoring, TTL, BitActor' }
  ]
})

// =============================================================================
// Dashboard Page State
// =============================================================================

// System overview metrics
const systemHealth = ref('HEALTHY')
const activePipelines = ref(3)
const activeSwarms = ref(2)
const threatLevel = ref('LOW')
const autoRefresh = ref(true)
const refreshInterval = ref(5)

// Quick action TTL budgets
const pipelineTTL = ref(64) // 8 stages × 8ms
const swarmTTL = ref(15)
const threatScanTTL = ref(30)
const reportTTL = ref(50)

// Pipeline stages for dashboard
const pipelineStages = ref([
  { id: 'typer', name: 'Typer', active: true, completed: true, error: false, executionTime: 2 },
  { id: 'turtle', name: 'Turtle', active: true, completed: true, error: false, executionTime: 1 },
  { id: 'ttl2dspy', name: 'TTL2DSPy', active: true, completed: false, error: false, executionTime: 0 },
  { id: 'bitactor', name: 'BitActor', active: false, completed: false, error: false, executionTime: 0 },
  { id: 'erlang', name: 'Erlang', active: false, completed: false, error: false, executionTime: 0 },
  { id: 'ash', name: 'Ash', active: false, completed: false, error: false, executionTime: 0 },
  { id: 'reactor', name: 'Reactor', active: false, completed: false, error: false, executionTime: 0 },
  { id: 'k8s', name: 'K8s', active: false, completed: false, error: false, executionTime: 0 }
])

// Recent threats
const recentThreats = ref([
  { id: 1, type: 'Malware', severity: 'high', icon: '🦠', timestamp: Date.now() - 30000 },
  { id: 2, type: 'Intrusion', severity: 'medium', icon: '🚪', timestamp: Date.now() - 120000 },
  { id: 3, type: 'Anomaly', severity: 'low', icon: '⚠️', timestamp: Date.now() - 300000 }
])

// Swarm status
const swarmStatus = ref([
  { id: 'swarm1', name: 'Primary', activeAgents: 8, totalAgents: 8, load: 45, healthStatus: 'healthy' },
  { id: 'swarm2', name: 'Secondary', activeAgents: 6, totalAgents: 8, load: 78, healthStatus: 'warning' }
])

// TTL performance metrics
const ttlComplianceRate = ref(95.2)
const avgExecutionTime = ref(6.4)
const ttlViolations = ref(12)
const ttlBudgetUsed = ref(67)

// Recent activity feed
const recentActivity = ref([
  { id: 1, type: 'success', icon: '✅', message: 'Pipeline execution completed', timestamp: Date.now() - 5000, duration: 6.2 },
  { id: 2, type: 'warning', icon: '⚠️', message: 'TTL violation in reactor stage', timestamp: Date.now() - 15000, duration: 8.9 },
  { id: 3, type: 'info', icon: 'ℹ️', message: 'Swarm coordination initiated', timestamp: Date.now() - 45000, duration: 12.1 }
])

// =============================================================================
// Monitoring Page State
// =============================================================================

// Threat map data
const mapTimeRange = ref('15m')
const threatRegions = ref([
  { id: 'region1', name: 'Internal Network', threatCount: 3, threatLevel: 'low' },
  { id: 'region2', name: 'DMZ', threatCount: 8, threatLevel: 'medium' },
  { id: 'region3', name: 'External', threatCount: 15, threatLevel: 'high' }
])

// Pipeline monitoring
const monitoringPipelineStages = ref([
  { id: 'typer', name: 'Typer', color: '#3b82f6', active: true, processing: false, error: false, lastExecutionTime: 2.1 },
  { id: 'turtle', name: 'Turtle', color: '#8b5cf6', active: true, processing: true, error: false, lastExecutionTime: 1.3 },
  { id: 'ttl2dspy', name: 'TTL2DSPy', color: '#10b981', active: false, processing: false, error: false, lastExecutionTime: 3.2 },
  { id: 'bitactor', name: 'BitActor', color: '#f59e0b', active: false, processing: false, error: false, lastExecutionTime: 2.8 },
  { id: 'erlang', name: 'Erlang', color: '#ef4444', active: false, processing: false, error: false, lastExecutionTime: 1.1 },
  { id: 'ash', name: 'Ash', color: '#06b6d4', active: false, processing: false, error: false, lastExecutionTime: 2.4 },
  { id: 'reactor', name: 'Reactor', color: '#84cc16', active: false, processing: false, error: false, lastExecutionTime: 3.7 },
  { id: 'k8s', name: 'K8s', color: '#6366f1', active: false, processing: false, error: false, lastExecutionTime: 1.5 }
])

// Alert stream
const alertStreamActive = ref(true)
const alertRate = ref(12)
const realtimeAlerts = ref([
  { id: 1, severity: 'high', type: 'Malware', message: 'Suspicious file detected', timestamp: Date.now() },
  { id: 2, severity: 'medium', type: 'Network', message: 'Unusual traffic pattern', timestamp: Date.now() - 10000 },
  { id: 3, severity: 'low', type: 'System', message: 'High CPU usage', timestamp: Date.now() - 25000 }
])

// System metrics
const systemMetrics = ref({
  cpuUsage: 67,
  memoryUsage: 45,
  diskUsage: 23,
  networkLoad: 34
})

// Network statistics
const networkStats = ref({
  inbound: 8.2,
  outbound: 3.7,
  connections: 127
})

// Monitoring swarms
const totalSwarms = ref(3)
const totalActiveAgents = ref(22)
const monitoringSwarms = ref([
  { id: 'swarm1', name: 'Primary', activeAgents: 8, totalAgents: 8, load: 45, status: 'healthy' },
  { id: 'swarm2', name: 'Secondary', activeAgents: 6, totalAgents: 8, load: 78, status: 'warning' },
  { id: 'swarm3', name: 'Analysis', activeAgents: 8, totalAgents: 8, load: 23, status: 'healthy' }
])

// =============================================================================
// Analysis Page State
// =============================================================================

// Analysis data
const analysisData = ref({
  totalExecutions: 1247,
  avgExecutionTime: 6.4,
  successRate: 98.3,
  ttlCompliance: 95.2,
  malwareDetected: 23,
  intrusionAttempts: 7,
  anomaliesDetected: 156,
  recommendations: [
    { id: 1, priority: 'high', icon: '🚨', title: 'Optimize Reactor Stage', description: 'TTL violations increasing in reactor stage' },
    { id: 2, priority: 'medium', icon: '🔧', title: 'Scale Swarm 2', description: 'Add more agents to handle increased load' },
    { id: 3, priority: 'low', icon: 'ℹ️', title: 'Update Threat Signatures', description: 'Refresh malware detection patterns' }
  ]
})

// Timeline analysis
const timelineRange = ref(12)
const timelineData = ref([
  { timestamp: Date.now() - 3600000, position: 10, value: 30, type: 'normal' },
  { timestamp: Date.now() - 1800000, position: 50, value: 75, type: 'warning' },
  { timestamp: Date.now() - 900000, position: 80, value: 45, type: 'normal' }
])

// Pattern analysis
const executionPatterns = ref([
  { id: 1, description: 'Peak execution times between 2-4 PM' },
  { id: 2, description: 'Reactor stage shows consistent 3ms execution time' },
  { id: 3, description: 'TTL violations correlate with high system load' }
])

const threatPatterns = ref([
  { id: 1, description: 'Malware attempts increase on Fridays' },
  { id: 2, description: 'Intrusion attempts from specific IP ranges' },
  { id: 3, description: 'Anomalies cluster around network maintenance windows' }
])

const ttlPatterns = ref([
  { id: 1, description: 'Violations spike during swarm coordination' },
  { id: 2, description: 'Typer stage consistently under 2ms' },
  { id: 3, description: 'K8s deployment stage most variable timing' }
])

// Correlation data
const correlationData = ref([
  { id: 1, value: 0.87, strength: 0.87 },
  { id: 2, value: 0.45, strength: 0.45 },
  { id: 3, value: 0.92, strength: 0.92 }
])

// Predictions
const predictedPeakTime = ref('15:30')
const threatProbability = ref(23)

// =============================================================================
// Swarms Page State
// =============================================================================

// Swarm management
const showCreateModal = ref(false)
const swarmList = ref([
  { id: 'swarm1', name: 'Primary Swarm', activeAgents: 8, totalAgents: 8, currentLoad: 45, ttlBudget: 8, status: 'healthy' },
  { id: 'swarm2', name: 'Secondary Swarm', activeAgents: 6, totalAgents: 8, currentLoad: 78, ttlBudget: 8, status: 'warning' },
  { id: 'swarm3', name: 'Analysis Swarm', activeAgents: 8, totalAgents: 8, currentLoad: 23, ttlBudget: 12, status: 'healthy' }
])

const newSwarm = ref({
  name: '',
  topology: 'hierarchical',
  maxAgents: 8,
  ttlBudget: 8
})

// =============================================================================
// Computed Properties
// =============================================================================

const systemHealthClass = computed(() => `health-${systemHealth.value.toLowerCase()}`)
const threatLevelClass = computed(() => `threat-${threatLevel.value.toLowerCase()}`)
const swarmHealthPercentage = computed(() => 
  Math.round((swarmStatus.value.filter(s => s.healthStatus === 'healthy').length / swarmStatus.value.length) * 100)
)
const lastPipelineUpdate = computed(() => 'Just now')

// =============================================================================
// Page Actions
// =============================================================================

// Dashboard actions
const runQuickPipeline = async () => {
  console.log('🚀 Running quick pipeline')
  const stages = ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
  const result = await executePipeline(stages, { quickRun: true }, pipelineTTL.value)
  console.log('Pipeline result:', result)
}

const coordinateSwarms = async () => {
  console.log('🐝 Coordinating swarms')
  const swarmIds = swarmStatus.value.map(s => s.id)
  const result = await coordinateSwarms(swarmIds, 'hierarchical')
  console.log('Coordination result:', result)
}

const runThreatScan = () => {
  console.log('🔍 Running threat scan')
  // Implement threat scanning logic
}

const generateReport = () => {
  console.log('📊 Generating report')
  // Implement report generation
}

const toggleAutoRefresh = () => {
  console.log(`Auto-refresh ${autoRefresh.value ? 'enabled' : 'disabled'}`)
}

const clearActivityFeed = () => {
  recentActivity.value = []
}

// Monitoring actions
const updateThreatMap = () => {
  console.log(`Updating threat map for range: ${mapTimeRange.value}`)
}

const selectRegion = (region) => {
  console.log('Selected region:', region.name)
}

const runPipelineStep = () => {
  console.log('Running single pipeline step')
}

const runFullPipeline = () => {
  console.log('Running full pipeline')
}

const stopPipeline = () => {
  console.log('Stopping pipeline')
}

const toggleAlertStream = () => {
  alertStreamActive.value = !alertStreamActive.value
  console.log(`Alert stream ${alertStreamActive.value ? 'started' : 'paused'}`)
}

// Swarm management actions
const createNewSwarm = () => {
  showCreateModal.value = true
}

const closeCreateModal = () => {
  showCreateModal.value = false
  newSwarm.value = { name: '', topology: 'hierarchical', maxAgents: 8, ttlBudget: 8 }
}

const submitNewSwarm = async () => {
  console.log('Creating new swarm:', newSwarm.value)
  const result = await createSwarm(newSwarm.value)
  if (result.success) {
    swarmList.value.push({
      id: result.swarmId,
      name: newSwarm.value.name,
      activeAgents: 0,
      totalAgents: newSwarm.value.maxAgents,
      currentLoad: 0,
      ttlBudget: newSwarm.value.ttlBudget,
      status: 'healthy'
    })
    closeCreateModal()
  }
}

const coordinateAllSwarms = () => {
  console.log('Coordinating all swarms')
}

const manageSwarm = (swarmId) => {
  console.log('Managing swarm:', swarmId)
}

const viewSwarmDetails = (swarmId) => {
  console.log('Viewing swarm details:', swarmId)
  navigateTo(`/swarms/${swarmId}`)
}

const stopSwarm = (swarmId) => {
  console.log('Stopping swarm:', swarmId)
}

// =============================================================================
// Utility Functions
// =============================================================================

const formatTime = (timestamp) => {
  const diff = Date.now() - timestamp
  if (diff < 60000) return `${Math.floor(diff / 1000)}s ago`
  if (diff < 3600000) return `${Math.floor(diff / 60000)}m ago`
  return `${Math.floor(diff / 3600000)}h ago`
}

const formatTimestamp = (timestamp) => {
  return new Date(timestamp).toLocaleTimeString()
}

// =============================================================================
// Lifecycle and Data Fetching
// =============================================================================

// SSR data fetching
const { data: initialData } = await useFetch('/api/dashboard/initial', {
  server: true,
  key: 'dashboard-initial'
})

// Apply initial data if available
if (initialData.value) {
  systemHealth.value = initialData.value.systemHealth || 'HEALTHY'
  activePipelines.value = initialData.value.activePipelines || 3
  activeSwarms.value = initialData.value.activeSwarms || 2
  threatLevel.value = initialData.value.threatLevel || 'LOW'
}

// Client-side real-time updates
onMounted(() => {
  // Start real-time data updates
  const updateInterval = setInterval(() => {
    // Simulate real-time data updates
    if (autoRefresh.value) {
      // Update metrics with small random variations
      systemMetrics.value.cpuUsage = Math.max(0, Math.min(100, systemMetrics.value.cpuUsage + (Math.random() - 0.5) * 10))
      systemMetrics.value.memoryUsage = Math.max(0, Math.min(100, systemMetrics.value.memoryUsage + (Math.random() - 0.5) * 5))
      
      // Update network stats
      networkStats.value.inbound = Math.max(0, networkStats.value.inbound + (Math.random() - 0.5) * 2)
      networkStats.value.outbound = Math.max(0, networkStats.value.outbound + (Math.random() - 0.5) * 1)
    }
  }, refreshInterval.value * 1000)
  
  onUnmounted(() => {
    clearInterval(updateInterval)
  })
})

console.log('🚀 BitActor Pipeline Monitoring Pages loaded')
</script>

<!-- =============================================================================
     Page Styles
     ============================================================================= -->
<style scoped>
/* Dashboard Page Styles */
.dashboard-page {
  padding: 2rem;
  max-width: 1400px;
  margin: 0 auto;
}

.dashboard-hero {
  background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
  border-radius: 12px;
  padding: 2rem;
  margin-bottom: 2rem;
  color: white;
}

.hero-title {
  font-size: 2.5rem;
  font-weight: bold;
  margin: 0 0 0.5rem 0;
}

.hero-description {
  font-size: 1.125rem;
  opacity: 0.9;
  margin: 0 0 2rem 0;
}

.hero-metrics {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.metric-card {
  display: flex;
  align-items: center;
  gap: 1rem;
  background: rgba(255, 255, 255, 0.1);
  padding: 1rem;
  border-radius: 8px;
  backdrop-filter: blur(10px);
}

.metric-icon {
  font-size: 2rem;
}

.metric-info {
  display: flex;
  flex-direction: column;
}

.metric-value {
  font-size: 1.5rem;
  font-weight: bold;
}

.metric-label {
  font-size: 0.875rem;
  opacity: 0.8;
}

.quick-actions-section {
  margin-bottom: 2rem;
}

.section-header {
  margin-bottom: 1rem;
}

.section-header h2 {
  font-size: 1.5rem;
  margin: 0 0 0.25rem 0;
}

.section-header p {
  color: #64748b;
  margin: 0;
}

.actions-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}

.action-card {
  display: flex;
  align-items: center;
  gap: 1rem;
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  padding: 1.5rem;
  cursor: pointer;
  transition: all 0.2s;
}

.action-card:hover {
  background: #334155;
  transform: translateY(-2px);
}

.action-icon {
  font-size: 2rem;
  color: #3b82f6;
}

.action-content h3 {
  margin: 0 0 0.25rem 0;
  font-size: 1.125rem;
}

.action-content p {
  margin: 0 0 0.5rem 0;
  color: #94a3b8;
  font-size: 0.875rem;
}

.action-ttl {
  font-size: 0.75rem;
  color: #10b981;
  font-weight: bold;
}

.status-grid-section {
  margin-bottom: 2rem;
}

.auto-refresh-control {
  font-size: 0.875rem;
}

.status-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1rem;
}

.status-panel {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  overflow: hidden;
}

.panel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-bottom: 1px solid #475569;
}

.panel-header h3 {
  margin: 0;
  font-size: 1rem;
}

.panel-content {
  padding: 1rem;
}

.pipeline-visualization {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.pipeline-stage {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.pipeline-stage.active {
  background: rgba(16, 185, 129, 0.1);
}

.pipeline-stage.completed {
  background: rgba(34, 197, 94, 0.1);
}

.pipeline-stage.error {
  background: rgba(239, 68, 68, 0.1);
}

.stage-indicator {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #6b7280;
}

.pipeline-stage.active .stage-indicator {
  background: #10b981;
}

.pipeline-stage.completed .stage-indicator {
  background: #22c55e;
}

.pipeline-stage.error .stage-indicator {
  background: #ef4444;
}

.threat-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.threat-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.threat-item.high {
  background: rgba(239, 68, 68, 0.1);
}

.threat-item.medium {
  background: rgba(245, 158, 11, 0.1);
}

.threat-item.low {
  background: rgba(16, 185, 129, 0.1);
}

.view-all-link {
  color: #3b82f6;
  text-decoration: none;
  font-size: 0.875rem;
}

.activity-feed-section {
  margin-bottom: 2rem;
}

.activity-feed {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  max-height: 400px;
  overflow-y: auto;
}

.activity-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  border-bottom: 1px solid #334155;
}

.activity-item:last-child {
  border-bottom: none;
}

.activity-item.success {
  border-left: 3px solid #10b981;
}

.activity-item.warning {
  border-left: 3px solid #f59e0b;
}

.activity-item.info {
  border-left: 3px solid #3b82f6;
}

/* Monitoring Page Styles */
.threat-map-container,
.pipeline-flow-container,
.alert-stream-container,
.metrics-container,
.network-container,
.swarm-status-container {
  height: 100%;
}

.pipeline-svg {
  width: 100%;
  height: 120px;
  margin-bottom: 1rem;
}

.stage-label,
.stage-time {
  fill: #e2e8f0;
  font-size: 12px;
}

.connection-line {
  stroke: #475569;
  stroke-width: 2;
}

.connection-line.active {
  stroke: #3b82f6;
  stroke-width: 3;
}

.pipeline-controls {
  display: flex;
  gap: 0.5rem;
}

.control-btn {
  padding: 0.5rem 1rem;
  background: #334155;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
}

.control-btn:hover {
  background: #475569;
}

/* Swarms Page Styles */
.swarms-page {
  padding: 2rem;
}

.page-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 2rem;
}

.page-actions {
  display: flex;
  gap: 1rem;
}

.create-swarm-btn,
.coordinate-all-btn {
  padding: 0.75rem 1.5rem;
  background: #3b82f6;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-weight: bold;
}

.swarms-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 1rem;
}

.swarm-card {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  padding: 1.5rem;
}

.swarm-card.healthy {
  border-left: 4px solid #10b981;
}

.swarm-card.warning {
  border-left: 4px solid #f59e0b;
}

.swarm-card.critical {
  border-left: 4px solid #ef4444;
}

.swarm-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 1rem;
}

.swarm-status-indicator {
  width: 12px;
  height: 12px;
  border-radius: 50%;
}

.swarm-status-indicator.healthy {
  background: #10b981;
}

.swarm-status-indicator.warning {
  background: #f59e0b;
}

.swarm-status-indicator.critical {
  background: #ef4444;
}

.swarm-stats {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.stat {
  display: flex;
  justify-content: space-between;
  font-size: 0.875rem;
}

.swarm-actions {
  display: flex;
  gap: 0.5rem;
}

.action-btn {
  flex: 1;
  padding: 0.5rem;
  background: #334155;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.75rem;
}

.action-btn.danger {
  background: #ef4444;
}

.action-btn:hover {
  opacity: 0.8;
}

/* Modal Styles */
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal-content {
  background: #1e293b;
  border-radius: 8px;
  padding: 2rem;
  max-width: 500px;
  width: 100%;
  margin: 1rem;
}

.form-group {
  margin-bottom: 1rem;
}

.form-group label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: bold;
}

.form-group input,
.form-group select {
  width: 100%;
  padding: 0.5rem;
  background: #334155;
  color: white;
  border: 1px solid #475569;
  border-radius: 4px;
}

.form-actions {
  display: flex;
  gap: 1rem;
  justify-content: flex-end;
  margin-top: 2rem;
}

.form-actions button {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-weight: bold;
}

.form-actions button[type="submit"] {
  background: #3b82f6;
  color: white;
}

.form-actions button[type="button"] {
  background: #6b7280;
  color: white;
}

/* Responsive Design */
@media (max-width: 768px) {
  .hero-metrics {
    grid-template-columns: 1fr;
  }
  
  .actions-grid {
    grid-template-columns: 1fr;
  }
  
  .status-grid {
    grid-template-columns: 1fr;
  }
  
  .swarms-grid {
    grid-template-columns: 1fr;
  }
}
</style>