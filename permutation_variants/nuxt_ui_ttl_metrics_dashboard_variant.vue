<!--
BitActor Nuxt UI TTL Metrics Dashboard Variant
Comprehensive TTL constraint monitoring and performance optimization dashboard
Nanosecond precision tracking with real-time violation detection
Advanced analytics for 8-stage pipeline TTL management
No TypeScript - Pure JavaScript with high-performance charting
-->

<template>
  <div class="ttl-metrics-dashboard">
    <!-- TTL Dashboard Header -->
    <header class="ttl-header">
      <div class="header-content">
        <h1 class="header-title">
          <span class="title-icon">⏱️</span>
          TTL Metrics Dashboard
        </h1>
        <div class="ttl-status-overview">
          <div class="global-ttl-status" :class="globalTTLHealthStatus">
            <div class="status-indicator"></div>
            <div class="status-content">
              <span class="status-label">Global TTL Health</span>
              <span class="status-value">{{ globalTTLHealthStatus }}</span>
            </div>
          </div>
          <div class="ttl-budget-display">
            <span class="budget-label">Global Budget:</span>
            <span class="budget-value">{{ globalTTLBudgetMs }}ms</span>
            <span class="budget-ns">({{ globalTTLBudgetNs }} ns)</span>
          </div>
        </div>
      </div>
    </header>

    <!-- TTL Key Metrics Grid -->
    <section class="ttl-metrics-grid">
      <div class="metrics-row">
        <div class="metric-card critical">
          <div class="metric-icon">🚨</div>
          <div class="metric-content">
            <div class="metric-value">{{ totalViolations }}</div>
            <div class="metric-label">TTL Violations</div>
            <div class="metric-subtitle">Last 24h</div>
          </div>
          <div class="metric-trend" :class="violationTrend">
            {{ violationTrend === 'up' ? '↗️' : '↘️' }} {{ violationChange }}%
          </div>
        </div>

        <div class="metric-card warning">
          <div class="metric-icon">⚡</div>
          <div class="metric-content">
            <div class="metric-value">{{ averageExecutionTime }}</div>
            <div class="metric-label">Avg Execution Time</div>
            <div class="metric-subtitle">Per pipeline run</div>
          </div>
          <div class="metric-trend" :class="executionTrend">
            {{ executionTrend === 'up' ? '↗️' : '↘️' }} {{ executionChange }}ms
          </div>
        </div>

        <div class="metric-card success">
          <div class="metric-icon">🎯</div>
          <div class="metric-content">
            <div class="metric-value">{{ ttlCompliance }}%</div>
            <div class="metric-label">TTL Compliance</div>
            <div class="metric-subtitle">Success rate</div>
          </div>
          <div class="metric-trend" :class="complianceTrend">
            {{ complianceTrend === 'up' ? '↗️' : '↘️' }} {{ complianceChange }}%
          </div>
        </div>

        <div class="metric-card info">
          <div class="metric-icon">📊</div>
          <div class="metric-content">
            <div class="metric-value">{{ totalExecutions }}</div>
            <div class="metric-label">Total Executions</div>
            <div class="metric-subtitle">Pipeline runs</div>
          </div>
          <div class="metric-trend" :class="throughputTrend">
            {{ throughputTrend === 'up' ? '↗️' : '↘️' }} {{ throughputChange }}
          </div>
        </div>
      </div>
    </section>

    <!-- TTL Stage Performance Analysis -->
    <section class="stage-performance-section">
      <div class="section-header">
        <h2 class="section-title">Pipeline Stage TTL Performance</h2>
        <div class="performance-controls">
          <select v-model="selectedTimeRange" @change="updateCharts" class="time-range-selector">
            <option value="1h">Last Hour</option>
            <option value="6h">Last 6 Hours</option>
            <option value="24h">Last 24 Hours</option>
            <option value="7d">Last 7 Days</option>
          </select>
          <button @click="exportMetrics" class="export-btn">
            📊 Export Data
          </button>
        </div>
      </div>

      <div class="stage-performance-grid">
        <div v-for="stage in pipelineStages" :key="stage.name" 
             class="stage-performance-card" :class="getStageHealthClass(stage)">
          
          <!-- Stage Header -->
          <div class="stage-header">
            <div class="stage-identity">
              <span class="stage-icon">{{ stage.icon }}</span>
              <div class="stage-info">
                <h3 class="stage-name">{{ stage.name }}</h3>
                <span class="stage-order">Stage {{ stage.order }}</span>
              </div>
            </div>
            <div class="stage-health-indicator" :class="getStageHealthClass(stage)">
              {{ getStageHealthClass(stage) }}
            </div>
          </div>

          <!-- TTL Budget & Usage -->
          <div class="ttl-budget-section">
            <div class="budget-header">
              <span class="budget-label">TTL Budget</span>
              <span class="budget-value">{{ stage.ttlBudgetMs }}ms</span>
            </div>
            
            <div class="ttl-usage-bar">
              <div class="usage-fill" :style="{ 
                width: `${stage.ttlUsagePercent}%`,
                backgroundColor: getTTLUsageColor(stage.ttlUsagePercent)
              }"></div>
              <div class="usage-text">{{ stage.ttlUsagePercent }}% used</div>
            </div>
            
            <div class="usage-details">
              <div class="detail-item">
                <span class="detail-label">Avg Time:</span>
                <span class="detail-value">{{ stage.avgExecutionTimeMs }}ms</span>
              </div>
              <div class="detail-item">
                <span class="detail-label">Max Time:</span>
                <span class="detail-value">{{ stage.maxExecutionTimeMs }}ms</span>
              </div>
              <div class="detail-item">
                <span class="detail-label">Violations:</span>
                <span class="detail-value" :class="{ violation: stage.violations > 0 }">
                  {{ stage.violations }}
                </span>
              </div>
            </div>
          </div>

          <!-- Performance Chart -->
          <div class="stage-chart-container">
            <canvas :ref="`stageChart_${stage.name}`" class="stage-chart"></canvas>
          </div>

          <!-- Nanosecond Precision Metrics -->
          <div class="precision-metrics">
            <h4 class="precision-title">Nanosecond Precision</h4>
            <div class="precision-grid">
              <div class="precision-item">
                <span class="precision-label">Budget (ns):</span>
                <span class="precision-value">{{ stage.ttlBudgetNs.toLocaleString() }}</span>
              </div>
              <div class="precision-item">
                <span class="precision-label">Last Exec (ns):</span>
                <span class="precision-value">{{ stage.lastExecutionNs.toLocaleString() }}</span>
              </div>
              <div class="precision-item">
                <span class="precision-label">Overhead (ns):</span>
                <span class="precision-value">{{ stage.overheadNs.toLocaleString() }}</span>
              </div>
              <div class="precision-item">
                <span class="precision-label">Jitter (ns):</span>
                <span class="precision-value">{{ stage.jitterNs.toLocaleString() }}</span>
              </div>
            </div>
          </div>

          <!-- Action Buttons -->
          <div class="stage-actions">
            <button @click="optimizeStage(stage)" class="optimize-btn">
              🚀 Optimize
            </button>
            <button @click="inspectStage(stage)" class="inspect-btn">
              🔍 Inspect
            </button>
          </div>
        </div>
      </div>
    </section>

    <!-- TTL Violation Analysis -->
    <section class="violation-analysis-section">
      <h2 class="section-title">TTL Violation Analysis</h2>
      
      <div class="violation-grid">
        <!-- Violation Timeline -->
        <div class="violation-panel">
          <h3 class="panel-title">Violation Timeline</h3>
          <div class="timeline-container">
            <canvas ref="violationTimelineChart" class="violation-timeline-chart"></canvas>
          </div>
          <div class="timeline-legend">
            <div class="legend-item">
              <div class="legend-color critical"></div>
              <span>Critical (>2x budget)</span>
            </div>
            <div class="legend-item">
              <div class="legend-color warning"></div>
              <span>Warning (>1.5x budget)</span>
            </div>
            <div class="legend-item">
              <div class="legend-color minor"></div>
              <span>Minor (>1x budget)</span>
            </div>
          </div>
        </div>

        <!-- Violation Breakdown -->
        <div class="violation-panel">
          <h3 class="panel-title">Violation Breakdown by Stage</h3>
          <div class="breakdown-chart-container">
            <canvas ref="violationBreakdownChart" class="breakdown-chart"></canvas>
          </div>
          <div class="breakdown-summary">
            <div v-for="stage in pipelineStages" :key="stage.name" 
                 class="breakdown-item" v-if="stage.violations > 0">
              <span class="breakdown-stage">{{ stage.name }}</span>
              <span class="breakdown-count">{{ stage.violations }}</span>
              <span class="breakdown-percent">{{ getViolationPercent(stage) }}%</span>
            </div>
          </div>
        </div>

        <!-- Root Cause Analysis -->
        <div class="violation-panel">
          <h3 class="panel-title">Root Cause Analysis</h3>
          <div class="root-causes">
            <div v-for="cause in rootCauses" :key="cause.id" 
                 class="cause-item" :class="cause.severity">
              <div class="cause-header">
                <span class="cause-icon">{{ cause.icon }}</span>
                <span class="cause-title">{{ cause.title }}</span>
                <span class="cause-frequency">{{ cause.frequency }}%</span>
              </div>
              <div class="cause-description">{{ cause.description }}</div>
              <div class="cause-recommendation">
                <span class="recommendation-label">💡 Recommendation:</span>
                <span class="recommendation-text">{{ cause.recommendation }}</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- Real-time TTL Monitor -->
    <section class="realtime-monitor-section">
      <h2 class="section-title">Real-time TTL Monitor</h2>
      
      <div class="monitor-grid">
        <!-- Live Pipeline Execution -->
        <div class="monitor-panel">
          <h3 class="panel-title">Live Pipeline Executions</h3>
          <div class="live-executions">
            <div v-for="execution in liveExecutions" :key="execution.id" 
                 class="live-execution" :class="execution.status">
              <div class="execution-header">
                <span class="execution-id">{{ execution.id }}</span>
                <span class="execution-status">{{ execution.status }}</span>
              </div>
              <div class="execution-progress">
                <div class="progress-stages">
                  <div v-for="(stage, index) in execution.stages" :key="index"
                       class="progress-stage" :class="stage.status">
                    <div class="stage-dot"></div>
                    <div class="stage-time">{{ stage.timeMs }}ms</div>
                  </div>
                </div>
                <div class="total-time" :class="{ violation: execution.isViolation }">
                  Total: {{ execution.totalTimeMs }}ms / {{ globalTTLBudgetMs }}ms
                </div>
              </div>
            </div>
          </div>
        </div>

        <!-- TTL Alerts -->
        <div class="monitor-panel">
          <h3 class="panel-title">TTL Alerts</h3>
          <div class="alerts-container">
            <div v-for="alert in ttlAlerts" :key="alert.id" 
                 class="alert-item" :class="alert.severity">
              <div class="alert-header">
                <span class="alert-icon">{{ alert.icon }}</span>
                <span class="alert-title">{{ alert.title }}</span>
                <span class="alert-time">{{ formatTime(alert.timestamp) }}</span>
              </div>
              <div class="alert-message">{{ alert.message }}</div>
              <div class="alert-actions">
                <button @click="acknowledgeAlert(alert)" class="ack-btn">
                  ✓ Acknowledge
                </button>
                <button @click="dismissAlert(alert)" class="dismiss-btn">
                  ✕ Dismiss
                </button>
              </div>
            </div>
          </div>
        </div>

        <!-- Performance Optimization Suggestions -->
        <div class="monitor-panel">
          <h3 class="panel-title">Optimization Suggestions</h3>
          <div class="suggestions-container">
            <div v-for="suggestion in optimizationSuggestions" :key="suggestion.id"
                 class="suggestion-item" :class="suggestion.priority">
              <div class="suggestion-header">
                <span class="suggestion-icon">{{ suggestion.icon }}</span>
                <span class="suggestion-title">{{ suggestion.title }}</span>
                <span class="suggestion-impact">{{ suggestion.impact }}</span>
              </div>
              <div class="suggestion-description">{{ suggestion.description }}</div>
              <div class="suggestion-actions">
                <button @click="applySuggestion(suggestion)" class="apply-btn">
                  🚀 Apply
                </button>
                <button @click="scheduleSuggestion(suggestion)" class="schedule-btn">
                  📅 Schedule
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- TTL Configuration Panel -->
    <section class="ttl-config-section">
      <h2 class="section-title">TTL Configuration</h2>
      
      <div class="config-grid">
        <!-- Global TTL Settings -->
        <div class="config-panel">
          <h3 class="panel-title">Global TTL Settings</h3>
          <div class="config-form">
            <div class="form-group">
              <label>Global TTL Budget (ms):</label>
              <input v-model.number="configTTLBudget" type="number" min="1" max="1000" 
                     @change="updateGlobalTTL" class="config-input">
            </div>
            
            <div class="form-group">
              <label>Precision Mode:</label>
              <select v-model="precisionMode" @change="updatePrecision" class="config-select">
                <option value="nanosecond">Nanosecond</option>
                <option value="microsecond">Microsecond</option>
                <option value="millisecond">Millisecond</option>
              </select>
            </div>
            
            <div class="form-group">
              <label>Violation Tolerance:</label>
              <input v-model.number="violationTolerance" type="number" min="0" max="50" 
                     class="config-input">
              <span class="input-unit">%</span>
            </div>
            
            <div class="form-group">
              <label>Auto-optimization:</label>
              <input v-model="autoOptimization" type="checkbox" class="config-checkbox">
              <span class="checkbox-label">Enable automatic optimization</span>
            </div>
          </div>
        </div>

        <!-- Stage-specific TTL Configuration -->
        <div class="config-panel">
          <h3 class="panel-title">Stage TTL Configuration</h3>
          <div class="stage-config-list">
            <div v-for="stage in pipelineStages" :key="stage.name" class="stage-config-item">
              <div class="stage-config-header">
                <span class="stage-icon">{{ stage.icon }}</span>
                <span class="stage-name">{{ stage.name }}</span>
              </div>
              <div class="stage-config-controls">
                <div class="control-group">
                  <label>Budget (ms):</label>
                  <input v-model.number="stage.ttlBudgetMs" type="number" min="0.1" max="100" 
                         step="0.1" @change="updateStageTTL(stage)" class="stage-input">
                </div>
                <div class="control-group">
                  <label>Priority:</label>
                  <select v-model="stage.priority" @change="updateStagePriority(stage)" class="stage-select">
                    <option value="low">Low</option>
                    <option value="medium">Medium</option>
                    <option value="high">High</option>
                    <option value="critical">Critical</option>
                  </select>
                </div>
              </div>
            </div>
          </div>
        </div>

        <!-- Performance Thresholds -->
        <div class="config-panel">
          <h3 class="panel-title">Performance Thresholds</h3>
          <div class="threshold-config">
            <div class="threshold-group">
              <label>Warning Threshold:</label>
              <input v-model.number="thresholds.warning" type="number" min="50" max="95" 
                     class="threshold-input">
              <span class="threshold-unit">% of budget</span>
            </div>
            
            <div class="threshold-group">
              <label>Critical Threshold:</label>
              <input v-model.number="thresholds.critical" type="number" min="80" max="100" 
                     class="threshold-input">
              <span class="threshold-unit">% of budget</span>
            </div>
            
            <div class="threshold-group">
              <label>Alert Frequency:</label>
              <select v-model="alertFrequency" class="threshold-select">
                <option value="immediate">Immediate</option>
                <option value="1min">Every minute</option>
                <option value="5min">Every 5 minutes</option>
                <option value="15min">Every 15 minutes</option>
              </select>
            </div>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
export default {
  name: 'NuxtUITTLMetricsDashboardVariant',
  
  data() {
    return {
      globalTTLBudgetMs: 8,
      globalTTLBudgetNs: 8_000_000,
      globalTTLHealthStatus: 'healthy',
      selectedTimeRange: '24h',
      
      configTTLBudget: 8,
      precisionMode: 'nanosecond',
      violationTolerance: 10,
      autoOptimization: true,
      alertFrequency: 'immediate',
      
      thresholds: {
        warning: 80,
        critical: 95
      },
      
      totalViolations: 23,
      violationTrend: 'down',
      violationChange: 12,
      
      averageExecutionTime: '6.2ms',
      executionTrend: 'down',
      executionChange: 0.3,
      
      ttlCompliance: 94.2,
      complianceTrend: 'up',
      complianceChange: 2.1,
      
      totalExecutions: 15247,
      throughputTrend: 'up',
      throughputChange: 847,
      
      pipelineStages: [
        {
          order: 1,
          name: 'Typer',
          icon: '📝',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 78,
          avgExecutionTimeMs: 0.82,
          maxExecutionTimeMs: 1.24,
          lastExecutionNs: 823_456,
          overheadNs: 45_123,
          jitterNs: 12_789,
          violations: 3,
          priority: 'medium'
        },
        {
          order: 2,
          name: 'Turtle',
          icon: '🐢',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 65,
          avgExecutionTimeMs: 0.67,
          maxExecutionTimeMs: 0.98,
          lastExecutionNs: 671_234,
          overheadNs: 23_456,
          jitterNs: 8_901,
          violations: 1,
          priority: 'medium'
        },
        {
          order: 3,
          name: 'TTL2DSPy',
          icon: '🔍',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 45,
          avgExecutionTimeMs: 0.47,
          maxExecutionTimeMs: 0.76,
          lastExecutionNs: 469_123,
          overheadNs: 18_345,
          jitterNs: 5_678,
          violations: 0,
          priority: 'high'
        },
        {
          order: 4,
          name: 'BitActor',
          icon: '⚡',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 89,
          avgExecutionTimeMs: 0.91,
          maxExecutionTimeMs: 1.45,
          lastExecutionNs: 912_345,
          overheadNs: 67_890,
          jitterNs: 23_456,
          violations: 8,
          priority: 'critical'
        },
        {
          order: 5,
          name: 'Erlang',
          icon: '🔧',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 58,
          avgExecutionTimeMs: 0.59,
          maxExecutionTimeMs: 0.87,
          lastExecutionNs: 587_901,
          overheadNs: 21_234,
          jitterNs: 7_890,
          violations: 2,
          priority: 'medium'
        },
        {
          order: 6,
          name: 'Ash',
          icon: '🌊',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 71,
          avgExecutionTimeMs: 0.73,
          maxExecutionTimeMs: 1.12,
          lastExecutionNs: 734_567,
          overheadNs: 34_567,
          jitterNs: 11_234,
          violations: 4,
          priority: 'high'
        },
        {
          order: 7,
          name: 'Reactor',
          icon: '🔄',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 42,
          avgExecutionTimeMs: 0.43,
          maxExecutionTimeMs: 0.68,
          lastExecutionNs: 432_109,
          overheadNs: 15_678,
          jitterNs: 4_321,
          violations: 1,
          priority: 'low'
        },
        {
          order: 8,
          name: 'K8s',
          icon: '☸️',
          ttlBudgetMs: 1.0,
          ttlBudgetNs: 1_000_000,
          ttlUsagePercent: 31,
          avgExecutionTimeMs: 0.32,
          maxExecutionTimeMs: 0.51,
          lastExecutionNs: 321_876,
          overheadNs: 12_345,
          jitterNs: 3_456,
          violations: 0,
          priority: 'low'
        }
      ],
      
      liveExecutions: [
        {
          id: 'exec_1753475890_abc',
          status: 'running',
          stages: [
            { status: 'completed', timeMs: 0.8 },
            { status: 'completed', timeMs: 0.7 },
            { status: 'completed', timeMs: 0.5 },
            { status: 'running', timeMs: 0.9 },
            { status: 'pending', timeMs: 0 },
            { status: 'pending', timeMs: 0 },
            { status: 'pending', timeMs: 0 },
            { status: 'pending', timeMs: 0 }
          ],
          totalTimeMs: 3.1,
          isViolation: false
        },
        {
          id: 'exec_1753475891_def',
          status: 'completed',
          stages: [
            { status: 'completed', timeMs: 0.9 },
            { status: 'completed', timeMs: 0.8 },
            { status: 'completed', timeMs: 0.6 },
            { status: 'completed', timeMs: 1.2 },
            { status: 'completed', timeMs: 0.7 },
            { status: 'completed', timeMs: 0.8 },
            { status: 'completed', timeMs: 0.5 },
            { status: 'completed', timeMs: 0.4 }
          ],
          totalTimeMs: 5.9,
          isViolation: false
        }
      ],
      
      ttlAlerts: [
        {
          id: 'alert_001',
          severity: 'critical',
          icon: '🚨',
          title: 'BitActor Stage Critical Violation',
          message: 'Execution time exceeded 145% of TTL budget (1.45ms > 1.0ms)',
          timestamp: Date.now() - 30000
        },
        {
          id: 'alert_002',
          severity: 'warning',
          icon: '⚠️',
          title: 'Ash Stage Performance Degradation',
          message: 'Average execution time trending upward (+15% over last hour)',
          timestamp: Date.now() - 120000
        },
        {
          id: 'alert_003',
          severity: 'info',
          icon: 'ℹ️',
          title: 'TTL Budget Optimization Available',
          message: 'K8s stage consistently under-utilizing TTL budget (-69% avg usage)',
          timestamp: Date.now() - 300000
        }
      ],
      
      optimizationSuggestions: [
        {
          id: 'opt_001',
          priority: 'high',
          icon: '🚀',
          title: 'Redistribute TTL Budget',
          impact: '-23% violations',
          description: 'Move 0.3ms from K8s and Reactor stages to BitActor stage',
          action: 'redistribute_budget'
        },
        {
          id: 'opt_002',
          priority: 'medium',
          icon: '⚡',
          title: 'Enable Parallel Processing',
          impact: '-18% execution time',
          description: 'Run Ash and Reactor stages in parallel where possible',
          action: 'enable_parallel'
        },
        {
          id: 'opt_003',
          priority: 'low',
          icon: '🔧',
          title: 'Adjust Precision Mode',
          impact: '-5% overhead',
          description: 'Switch to microsecond precision for non-critical stages',
          action: 'adjust_precision'
        }
      ],
      
      rootCauses: [
        {
          id: 'cause_001',
          severity: 'critical',
          icon: '🔥',
          title: 'Memory Allocation Bottleneck',
          frequency: 34,
          description: 'BitActor stage experiencing memory allocation delays during peak loads',
          recommendation: 'Implement memory pool pre-allocation for BitActor operations'
        },
        {
          id: 'cause_002',
          severity: 'warning',
          icon: '🌐',
          title: 'Network Latency Spikes',
          frequency: 23,
          description: 'Intermittent network delays affecting Erlang and Ash communication',
          recommendation: 'Implement connection pooling and retry mechanisms with exponential backoff'
        },
        {
          id: 'cause_003',
          severity: 'info',
          icon: '💾',
          title: 'Database Query Optimization',
          frequency: 18,
          description: 'Ash framework queries not utilizing optimal indexing strategies',
          recommendation: 'Review and optimize database indexes for frequently accessed data'
        }
      ]
    }
  },
  
  mounted() {
    this.initializeCharts()
    this.startRealTimeUpdates()
  },
  
  beforeUnmount() {
    this.stopRealTimeUpdates()
  },
  
  methods: {
    getStageHealthClass(stage) {
      if (stage.violations > 5 || stage.ttlUsagePercent > 90) return 'critical'
      if (stage.violations > 2 || stage.ttlUsagePercent > 80) return 'warning'
      if (stage.ttlUsagePercent > 60) return 'caution'
      return 'healthy'
    },
    
    getTTLUsageColor(percent) {
      if (percent > 90) return '#d9534f'
      if (percent > 80) return '#f0ad4e'
      if (percent > 60) return '#5bc0de'
      return '#5cb85c'
    },
    
    getViolationPercent(stage) {
      const total = this.pipelineStages.reduce((sum, s) => sum + s.violations, 0)
      return total > 0 ? Math.round((stage.violations / total) * 100) : 0
    },
    
    formatTime(timestamp) {
      return new Date(timestamp).toLocaleTimeString()
    },
    
    updateCharts() {
      // Update all charts based on selected time range
      this.drawStageCharts()
      this.drawViolationCharts()
    },
    
    exportMetrics() {
      // Export TTL metrics data
      const data = {
        timestamp: new Date().toISOString(),
        globalTTL: this.globalTTLBudgetMs,
        stages: this.pipelineStages.map(stage => ({
          name: stage.name,
          budget: stage.ttlBudgetMs,
          usage: stage.ttlUsagePercent,
          violations: stage.violations,
          avgTime: stage.avgExecutionTimeMs
        })),
        violations: this.totalViolations,
        compliance: this.ttlCompliance
      }
      
      const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `ttl-metrics-${Date.now()}.json`
      a.click()
      URL.revokeObjectURL(url)
    },
    
    optimizeStage(stage) {
      // Trigger stage optimization
      console.log(`Optimizing stage: ${stage.name}`)
      
      // Simulate optimization effect
      stage.ttlUsagePercent = Math.max(30, stage.ttlUsagePercent - 15)
      stage.violations = Math.max(0, stage.violations - 1)
      stage.avgExecutionTimeMs = Math.max(0.1, stage.avgExecutionTimeMs - 0.1)
    },
    
    inspectStage(stage) {
      // Open detailed stage inspector
      console.log(`Inspecting stage: ${stage.name}`)
    },
    
    acknowledgeAlert(alert) {
      alert.acknowledged = true
      setTimeout(() => {
        const index = this.ttlAlerts.indexOf(alert)
        if (index > -1) this.ttlAlerts.splice(index, 1)
      }, 1000)
    },
    
    dismissAlert(alert) {
      const index = this.ttlAlerts.indexOf(alert)
      if (index > -1) this.ttlAlerts.splice(index, 1)
    },
    
    applySuggestion(suggestion) {
      console.log(`Applying optimization: ${suggestion.title}`)
      
      switch (suggestion.action) {
        case 'redistribute_budget':
          this.redistributeTTLBudget()
          break
        case 'enable_parallel':
          this.enableParallelProcessing()
          break
        case 'adjust_precision':
          this.adjustPrecisionMode()
          break
      }
      
      const index = this.optimizationSuggestions.indexOf(suggestion)
      if (index > -1) this.optimizationSuggestions.splice(index, 1)
    },
    
    scheduleSuggestion(suggestion) {
      console.log(`Scheduling optimization: ${suggestion.title}`)
      suggestion.scheduled = true
    },
    
    redistributeTTLBudget() {
      // Redistribute TTL budget between stages
      const k8sStage = this.pipelineStages.find(s => s.name === 'K8s')
      const reactorStage = this.pipelineStages.find(s => s.name === 'Reactor')
      const bitactorStage = this.pipelineStages.find(s => s.name === 'BitActor')
      
      if (k8sStage && reactorStage && bitactorStage) {
        k8sStage.ttlBudgetMs -= 0.15
        reactorStage.ttlBudgetMs -= 0.15
        bitactorStage.ttlBudgetMs += 0.3
        
        // Update nanosecond values
        k8sStage.ttlBudgetNs = k8sStage.ttlBudgetMs * 1_000_000
        reactorStage.ttlBudgetNs = reactorStage.ttlBudgetMs * 1_000_000
        bitactorStage.ttlBudgetNs = bitactorStage.ttlBudgetMs * 1_000_000
      }
    },
    
    enableParallelProcessing() {
      // Enable parallel processing optimization
      const ashStage = this.pipelineStages.find(s => s.name === 'Ash')
      const reactorStage = this.pipelineStages.find(s => s.name === 'Reactor')
      
      if (ashStage && reactorStage) {
        ashStage.avgExecutionTimeMs *= 0.82
        reactorStage.avgExecutionTimeMs *= 0.82
        ashStage.ttlUsagePercent *= 0.82
        reactorStage.ttlUsagePercent *= 0.82
      }
    },
    
    adjustPrecisionMode() {
      // Adjust precision mode for optimization
      this.precisionMode = 'microsecond'
      this.pipelineStages.forEach(stage => {
        if (stage.priority !== 'critical') {
          stage.overheadNs *= 0.95
        }
      })
    },
    
    updateGlobalTTL() {
      this.globalTTLBudgetMs = this.configTTLBudget
      this.globalTTLBudgetNs = this.configTTLBudget * 1_000_000
    },
    
    updatePrecision() {
      // Update precision mode effects
      const multipliers = {
        nanosecond: 1.0,
        microsecond: 0.95,
        millisecond: 0.9
      }
      
      const multiplier = multipliers[this.precisionMode]
      this.pipelineStages.forEach(stage => {
        stage.overheadNs = Math.floor(stage.overheadNs * multiplier)
      })
    },
    
    updateStageTTL(stage) {
      stage.ttlBudgetNs = stage.ttlBudgetMs * 1_000_000
    },
    
    updateStagePriority(stage) {
      // Update stage priority effects
      console.log(`Updated ${stage.name} priority to ${stage.priority}`)
    },
    
    initializeCharts() {
      this.drawStageCharts()
      this.drawViolationCharts()
    },
    
    drawStageCharts() {
      this.pipelineStages.forEach(stage => {
        const canvas = this.$refs[`stageChart_${stage.name}`]?.[0]
        if (canvas) {
          const ctx = canvas.getContext('2d')
          canvas.width = 300
          canvas.height = 100
          this.drawStagePerformanceChart(ctx, stage)
        }
      })
    },
    
    drawStagePerformanceChart(ctx, stage) {
      const width = ctx.canvas.width
      const height = ctx.canvas.height
      
      // Clear canvas
      ctx.clearRect(0, 0, width, height)
      
      // Generate sample time series data
      const dataPoints = 20
      const data = Array.from({ length: dataPoints }, (_, i) => {
        const base = stage.avgExecutionTimeMs
        const variation = (Math.random() - 0.5) * 0.3
        return Math.max(0.1, base + variation)
      })
      
      // Draw background
      ctx.fillStyle = 'rgba(0, 0, 0, 0.1)'
      ctx.fillRect(0, 0, width, height)
      
      // Draw TTL budget line
      const budgetY = height - (stage.ttlBudgetMs / 2) * height
      ctx.strokeStyle = '#d9534f'
      ctx.lineWidth = 2
      ctx.setLineDash([5, 5])
      ctx.beginPath()
      ctx.moveTo(0, budgetY)
      ctx.lineTo(width, budgetY)
      ctx.stroke()
      ctx.setLineDash([])
      
      // Draw performance line
      ctx.strokeStyle = this.getTTLUsageColor(stage.ttlUsagePercent)
      ctx.lineWidth = 2
      ctx.beginPath()
      
      data.forEach((value, index) => {
        const x = (index / (dataPoints - 1)) * width
        const y = height - (value / 2) * height
        
        if (index === 0) {
          ctx.moveTo(x, y)
        } else {
          ctx.lineTo(x, y)
        }
      })
      
      ctx.stroke()
      
      // Draw data points
      ctx.fillStyle = this.getTTLUsageColor(stage.ttlUsagePercent)
      data.forEach((value, index) => {
        const x = (index / (dataPoints - 1)) * width
        const y = height - (value / 2) * height
        
        ctx.beginPath()
        ctx.arc(x, y, 2, 0, 2 * Math.PI)
        ctx.fill()
      })
    },
    
    drawViolationCharts() {
      // Draw violation timeline chart
      const timelineCanvas = this.$refs.violationTimelineChart
      if (timelineCanvas) {
        this.drawViolationTimelineChart(timelineCanvas.getContext('2d'))
      }
      
      // Draw violation breakdown chart
      const breakdownCanvas = this.$refs.violationBreakdownChart
      if (breakdownCanvas) {
        this.drawViolationBreakdownChart(breakdownCanvas.getContext('2d'))
      }
    },
    
    drawViolationTimelineChart(ctx) {
      const canvas = ctx.canvas
      canvas.width = 600
      canvas.height = 200
      
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      
      // Generate sample violation data
      const hours = 24
      const violations = Array.from({ length: hours }, () => Math.floor(Math.random() * 10))
      
      const barWidth = canvas.width / hours
      const maxViolations = Math.max(...violations, 10)
      
      violations.forEach((count, index) => {
        const barHeight = (count / maxViolations) * (canvas.height - 20)
        const x = index * barWidth
        const y = canvas.height - barHeight
        
        // Color based on violation severity
        let color = '#5cb85c'
        if (count > 7) color = '#d9534f'
        else if (count > 4) color = '#f0ad4e'
        else if (count > 2) color = '#5bc0de'
        
        ctx.fillStyle = color
        ctx.fillRect(x, y, barWidth - 2, barHeight)
      })
    },
    
    drawViolationBreakdownChart(ctx) {
      const canvas = ctx.canvas
      canvas.width = 300
      canvas.height = 300
      
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      
      const centerX = canvas.width / 2
      const centerY = canvas.height / 2
      const radius = 100
      
      const stagesWithViolations = this.pipelineStages.filter(s => s.violations > 0)
      const totalViolations = stagesWithViolations.reduce((sum, s) => sum + s.violations, 0)
      
      let currentAngle = 0
      
      stagesWithViolations.forEach(stage => {
        const sliceAngle = (stage.violations / totalViolations) * 2 * Math.PI
        
        ctx.fillStyle = this.getTTLUsageColor(stage.ttlUsagePercent)
        ctx.beginPath()
        ctx.moveTo(centerX, centerY)
        ctx.arc(centerX, centerY, radius, currentAngle, currentAngle + sliceAngle)
        ctx.closePath()
        ctx.fill()
        
        currentAngle += sliceAngle
      })
    },
    
    startRealTimeUpdates() {
      setInterval(() => {
        this.updateRealTimeData()
      }, 2000)
    },
    
    stopRealTimeUpdates() {
      // Stop real-time updates
    },
    
    updateRealTimeData() {
      // Update TTL metrics
      this.pipelineStages.forEach(stage => {
        stage.ttlUsagePercent = Math.max(20, Math.min(100, 
          stage.ttlUsagePercent + (Math.random() - 0.5) * 5))
        stage.avgExecutionTimeMs = Math.max(0.1, 
          stage.avgExecutionTimeMs + (Math.random() - 0.5) * 0.1)
        stage.lastExecutionNs = Math.floor(stage.avgExecutionTimeMs * 1_000_000 + 
          (Math.random() - 0.5) * 100_000)
      })
      
      // Update global health status
      const avgUsage = this.pipelineStages.reduce((sum, s) => sum + s.ttlUsagePercent, 0) / this.pipelineStages.length
      if (avgUsage > 85) this.globalTTLHealthStatus = 'critical'
      else if (avgUsage > 70) this.globalTTLHealthStatus = 'warning'
      else this.globalTTLHealthStatus = 'healthy'
      
      // Update charts
      this.drawStageCharts()
    }
  }
}
</script>

<style scoped>
.ttl-metrics-dashboard {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

/* Header */
.ttl-header {
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 30px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.header-content {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.header-title {
  font-size: 2.2rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 15px;
}

.title-icon {
  font-size: 2.8rem;
}

.ttl-status-overview {
  display: flex;
  align-items: center;
  gap: 30px;
}

.global-ttl-status {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 20px;
  border-radius: 10px;
  background: rgba(255, 255, 255, 0.1);
}

.global-ttl-status.healthy .status-indicator {
  background: #5cb85c;
}

.global-ttl-status.warning .status-indicator {
  background: #f0ad4e;
}

.global-ttl-status.critical .status-indicator {
  background: #d9534f;
}

.status-indicator {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  animation: pulse 2s infinite;
}

.status-content {
  display: flex;
  flex-direction: column;
}

.status-label {
  font-size: 0.9rem;
  opacity: 0.8;
}

.status-value {
  font-weight: 600;
  text-transform: capitalize;
}

.ttl-budget-display {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 1.1rem;
}

.budget-label {
  opacity: 0.8;
}

.budget-value {
  font-weight: 700;
  color: #4a90e2;
}

.budget-ns {
  font-size: 0.9rem;
  opacity: 0.6;
  font-family: monospace;
}

/* Metrics Grid */
.ttl-metrics-grid {
  margin-bottom: 30px;
}

.metrics-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 20px;
}

.metric-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 25px;
  display: flex;
  align-items: center;
  gap: 20px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: all 0.3s ease;
  position: relative;
  overflow: hidden;
}

.metric-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
}

.metric-card.critical {
  border-left: 4px solid #d9534f;
}

.metric-card.warning {
  border-left: 4px solid #f0ad4e;
}

.metric-card.success {
  border-left: 4px solid #5cb85c;
}

.metric-card.info {
  border-left: 4px solid #5bc0de;
}

.metric-icon {
  font-size: 2.5rem;
}

.metric-content {
  flex: 1;
}

.metric-value {
  font-size: 2rem;
  font-weight: 700;
  color: #ffffff;
  display: block;
}

.metric-label {
  font-size: 1rem;
  font-weight: 600;
  opacity: 0.9;
  margin-top: 4px;
}

.metric-subtitle {
  font-size: 0.8rem;
  opacity: 0.6;
  margin-top: 2px;
}

.metric-trend {
  font-size: 0.9rem;
  font-weight: 600;
  padding: 6px 12px;
  border-radius: 20px;
  background: rgba(255, 255, 255, 0.1);
}

.metric-trend.up {
  color: #5cb85c;
}

.metric-trend.down {
  color: #d9534f;
}

/* Stage Performance Section */
.stage-performance-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
  margin-bottom: 30px;
}

.section-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 25px;
}

.section-title {
  font-size: 1.8rem;
  font-weight: 600;
  margin: 0;
}

.performance-controls {
  display: flex;
  gap: 15px;
  align-items: center;
}

.time-range-selector {
  padding: 8px 15px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
}

.export-btn {
  padding: 8px 16px;
  border: none;
  border-radius: 8px;
  background: #4a90e2;
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.export-btn:hover {
  background: #357abd;
  transform: translateY(-2px);
}

.stage-performance-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 25px;
}

.stage-performance-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 25px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: all 0.3s ease;
}

.stage-performance-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
}

.stage-performance-card.critical {
  border-color: #d9534f;
  box-shadow: 0 0 20px rgba(217, 83, 79, 0.3);
}

.stage-performance-card.warning {
  border-color: #f0ad4e;
  box-shadow: 0 0 20px rgba(240, 173, 78, 0.3);
}

.stage-performance-card.caution {
  border-color: #5bc0de;
  box-shadow: 0 0 20px rgba(91, 192, 222, 0.3);
}

.stage-performance-card.healthy {
  border-color: #5cb85c;
  box-shadow: 0 0 20px rgba(92, 184, 92, 0.3);
}

.stage-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.stage-identity {
  display: flex;
  align-items: center;
  gap: 12px;
}

.stage-icon {
  font-size: 1.8rem;
}

.stage-name {
  font-size: 1.3rem;
  font-weight: 600;
  margin: 0;
}

.stage-order {
  font-size: 0.9rem;
  opacity: 0.7;
}

.stage-health-indicator {
  padding: 6px 16px;
  border-radius: 20px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.stage-health-indicator.critical {
  background: #d9534f;
  color: white;
}

.stage-health-indicator.warning {
  background: #f0ad4e;
  color: white;
}

.stage-health-indicator.caution {
  background: #5bc0de;
  color: white;
}

.stage-health-indicator.healthy {
  background: #5cb85c;
  color: white;
}

.ttl-budget-section {
  margin-bottom: 20px;
}

.budget-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 8px;
}

.budget-label {
  font-weight: 500;
  opacity: 0.9;
}

.budget-value {
  font-weight: 700;
  color: #4a90e2;
}

.ttl-usage-bar {
  position: relative;
  height: 8px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 12px;
}

.usage-fill {
  height: 100%;
  transition: width 0.3s ease;
  border-radius: 4px;
}

.usage-text {
  position: absolute;
  right: 8px;
  top: -22px;
  font-size: 0.8rem;
  font-weight: 600;
}

.usage-details {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 10px;
}

.detail-item {
  display: flex;
  justify-content: space-between;
  font-size: 0.9rem;
}

.detail-label {
  opacity: 0.8;
}

.detail-value {
  font-weight: 600;
}

.detail-value.violation {
  color: #d9534f;
}

.stage-chart-container {
  margin: 20px 0;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
  padding: 10px;
}

.stage-chart {
  width: 100%;
  height: 100px;
  display: block;
}

.precision-metrics {
  margin-bottom: 20px;
}

.precision-title {
  font-size: 1rem;
  font-weight: 600;
  margin-bottom: 10px;
  color: #4a90e2;
}

.precision-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 8px;
}

.precision-item {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
}

.precision-label {
  opacity: 0.8;
}

.precision-value {
  font-family: monospace;
  font-weight: 600;
}

.stage-actions {
  display: flex;
  gap: 10px;
}

.optimize-btn, .inspect-btn {
  padding: 8px 16px;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.9rem;
  font-weight: 500;
  transition: all 0.3s ease;
}

.optimize-btn {
  background: #5cb85c;
  color: white;
}

.optimize-btn:hover {
  background: #449d44;
}

.inspect-btn {
  background: rgba(255, 255, 255, 0.1);
  color: white;
}

.inspect-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

/* Violation Analysis */
.violation-analysis-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
  margin-bottom: 30px;
}

.violation-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 25px;
}

.violation-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
}

.panel-title {
  font-size: 1.2rem;
  font-weight: 600;
  margin-bottom: 15px;
  color: #ffffff;
}

.timeline-container {
  margin-bottom: 15px;
}

.violation-timeline-chart, .breakdown-chart {
  width: 100%;
  height: 200px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
  display: block;
}

.timeline-legend {
  display: flex;
  gap: 15px;
  flex-wrap: wrap;
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 6px;
  font-size: 0.8rem;
}

.legend-color {
  width: 10px;
  height: 10px;
  border-radius: 2px;
}

.legend-color.critical {
  background: #d9534f;
}

.legend-color.warning {
  background: #f0ad4e;
}

.legend-color.minor {
  background: #5bc0de;
}

.breakdown-summary {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.breakdown-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 6px;
}

.breakdown-stage {
  font-weight: 500;
}

.breakdown-count, .breakdown-percent {
  font-weight: 600;
  font-size: 0.9rem;
}

.root-causes {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.cause-item {
  padding: 15px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 4px solid;
}

.cause-item.critical {
  border-left-color: #d9534f;
}

.cause-item.warning {
  border-left-color: #f0ad4e;
}

.cause-item.info {
  border-left-color: #5bc0de;
}

.cause-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
}

.cause-icon {
  font-size: 1.2rem;
}

.cause-title {
  font-weight: 600;
  flex: 1;
  margin-left: 8px;
}

.cause-frequency {
  font-weight: 600;
  color: #4a90e2;
}

.cause-description {
  font-size: 0.9rem;
  opacity: 0.9;
  margin-bottom: 8px;
}

.cause-recommendation {
  font-size: 0.85rem;
}

.recommendation-label {
  font-weight: 600;
  color: #5cb85c;
}

.recommendation-text {
  opacity: 0.8;
}

/* Real-time Monitor */
.realtime-monitor-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
  margin-bottom: 30px;
}

.monitor-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
  gap: 25px;
}

.monitor-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  height: 400px;
  display: flex;
  flex-direction: column;
}

.live-executions, .alerts-container, .suggestions-container {
  flex: 1;
  overflow-y: auto;
  margin-top: 10px;
}

.live-execution {
  margin-bottom: 15px;
  padding: 15px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 4px solid;
}

.live-execution.running {
  border-left-color: #5cb85c;
}

.live-execution.completed {
  border-left-color: #5bc0de;
}

.execution-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 10px;
}

.execution-id {
  font-family: monospace;
  font-size: 0.8rem;
  opacity: 0.8;
}

.execution-status {
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.execution-progress {
  margin-top: 10px;
}

.progress-stages {
  display: flex;
  gap: 8px;
  margin-bottom: 8px;
}

.progress-stage {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 4px;
}

.stage-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #888888;
}

.progress-stage.completed .stage-dot {
  background: #5cb85c;
}

.progress-stage.running .stage-dot {
  background: #f0ad4e;
  animation: pulse 1s infinite;
}

.progress-stage.pending .stage-dot {
  background: #888888;
}

.stage-time {
  font-size: 0.7rem;
  opacity: 0.8;
}

.total-time {
  font-size: 0.9rem;
  font-weight: 600;
  text-align: center;
}

.total-time.violation {
  color: #d9534f;
}

.alert-item, .suggestion-item {
  margin-bottom: 15px;
  padding: 15px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 4px solid;
}

.alert-item.critical {
  border-left-color: #d9534f;
}

.alert-item.warning {
  border-left-color: #f0ad4e;
}

.alert-item.info {
  border-left-color: #5bc0de;
}

.suggestion-item.high {
  border-left-color: #d9534f;
}

.suggestion-item.medium {
  border-left-color: #f0ad4e;
}

.suggestion-item.low {
  border-left-color: #5bc0de;
}

.alert-header, .suggestion-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
}

.alert-icon, .suggestion-icon {
  font-size: 1.2rem;
}

.alert-title, .suggestion-title {
  font-weight: 600;
  flex: 1;
  margin-left: 8px;
}

.alert-time, .suggestion-impact {
  font-size: 0.8rem;
  opacity: 0.8;
}

.alert-message, .suggestion-description {
  font-size: 0.9rem;
  opacity: 0.9;
  margin-bottom: 10px;
}

.alert-actions, .suggestion-actions {
  display: flex;
  gap: 8px;
}

.ack-btn, .dismiss-btn, .apply-btn, .schedule-btn {
  padding: 4px 12px;
  border: none;
  border-radius: 4px;
  font-size: 0.8rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.ack-btn, .apply-btn {
  background: #5cb85c;
  color: white;
}

.dismiss-btn, .schedule-btn {
  background: rgba(255, 255, 255, 0.1);
  color: white;
}

.ack-btn:hover, .apply-btn:hover {
  background: #449d44;
}

.dismiss-btn:hover, .schedule-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

/* TTL Configuration */
.ttl-config-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
}

.config-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 25px;
}

.config-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
}

.config-form {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.form-group label {
  font-weight: 500;
  opacity: 0.9;
}

.config-input, .config-select {
  padding: 8px 12px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
}

.config-checkbox {
  margin-right: 8px;
}

.checkbox-label {
  font-size: 0.9rem;
  opacity: 0.9;
}

.input-unit {
  margin-left: 5px;
  opacity: 0.7;
}

.stage-config-list {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.stage-config-item {
  padding: 15px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

.stage-config-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
}

.stage-config-controls {
  display: flex;
  gap: 15px;
}

.control-group {
  display: flex;
  flex-direction: column;
  gap: 5px;
  flex: 1;
}

.control-group label {
  font-size: 0.8rem;
  opacity: 0.8;
}

.stage-input, .stage-select {
  padding: 6px 10px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.8rem;
}

.threshold-config {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.threshold-group {
  display: flex;
  align-items: center;
  gap: 10px;
}

.threshold-group label {
  min-width: 120px;
  font-weight: 500;
  opacity: 0.9;
}

.threshold-input, .threshold-select {
  padding: 6px 10px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
}

.threshold-unit {
  opacity: 0.7;
  font-size: 0.8rem;
}

/* Animations */
@keyframes pulse {
  0%, 100% { opacity: 0.8; }
  50% { opacity: 1; }
}

/* Scrollbar Styles */
.live-executions::-webkit-scrollbar,
.alerts-container::-webkit-scrollbar,
.suggestions-container::-webkit-scrollbar {
  width: 6px;
}

.live-executions::-webkit-scrollbar-track,
.alerts-container::-webkit-scrollbar-track,
.suggestions-container::-webkit-scrollbar-track {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 3px;
}

.live-executions::-webkit-scrollbar-thumb,
.alerts-container::-webkit-scrollbar-thumb,
.suggestions-container::-webkit-scrollbar-thumb {
  background: rgba(255, 255, 255, 0.3);
  border-radius: 3px;
}

.live-executions::-webkit-scrollbar-thumb:hover,
.alerts-container::-webkit-scrollbar-thumb:hover,
.suggestions-container::-webkit-scrollbar-thumb:hover {
  background: rgba(255, 255, 255, 0.5);
}

/* Responsive Design */
@media (max-width: 768px) {
  .header-content {
    flex-direction: column;
    gap: 20px;
  }
  
  .ttl-status-overview {
    flex-direction: column;
    gap: 15px;
  }
  
  .metrics-row {
    grid-template-columns: 1fr;
  }
  
  .stage-performance-grid {
    grid-template-columns: 1fr;
  }
  
  .violation-grid {
    grid-template-columns: 1fr;
  }
  
  .monitor-grid {
    grid-template-columns: 1fr;
  }
  
  .config-grid {
    grid-template-columns: 1fr;
  }
  
  .stage-config-controls {
    flex-direction: column;
  }
  
  .threshold-group {
    flex-direction: column;
    align-items: flex-start;
  }
}
</style>