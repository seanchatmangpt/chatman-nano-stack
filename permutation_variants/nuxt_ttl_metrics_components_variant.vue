<!-- BitActor Nuxt.js TTL Metrics Components Variant -->
<!-- This variant explores specialized TTL metric components with:
     - Real-time TTL usage visualization and monitoring
     - Nanosecond precision TTL constraint displays
     - Interactive TTL budget configuration controls
     - TTL violation alert and notification systems
     - Performance trend analysis for TTL optimization
     - Component-level TTL enforcement and validation
-->

<!-- =============================================================================
     TTL Usage Meter Component
     ============================================================================= -->
<template name="ttl-usage-meter">
  <div class="ttl-usage-meter" :class="meterSizeClass">
    <!-- Circular Progress Meter -->
    <div class="meter-container">
      <svg class="meter-svg" :width="meterSize" :height="meterSize" viewBox="0 0 200 200">
        <!-- Background Circle -->
        <circle
          cx="100"
          cy="100"
          :r="meterRadius"
          fill="none"
          stroke="#334155"
          :stroke-width="strokeWidth"
        />
        
        <!-- Progress Circle -->
        <circle
          cx="100"
          cy="100"
          :r="meterRadius"
          fill="none"
          :stroke="progressColor"
          :stroke-width="strokeWidth"
          stroke-linecap="round"
          :stroke-dasharray="circumference"
          :stroke-dashoffset="progressOffset"
          :class="{ 'pulse-animation': isPulsing }"
          transform="rotate(-90 100 100)"
        />
        
        <!-- Center Content -->
        <g class="meter-content">
          <!-- Usage Percentage -->
          <text
            x="100"
            y="90"
            text-anchor="middle"
            class="usage-percentage"
            :class="usageClass"
          >
            {{ Math.round(usagePercentage) }}%
          </text>
          
          <!-- TTL Label -->
          <text
            x="100"
            y="110"
            text-anchor="middle"
            class="ttl-label"
          >
            TTL Usage
          </text>
          
          <!-- Time Values -->
          <text
            x="100"
            y="130"
            text-anchor="middle"
            class="time-values"
          >
            {{ formatTime(usedTimeNs) }} / {{ formatTime(budgetTimeNs) }}
          </text>
        </g>
      </svg>
      
      <!-- Meter Status Indicators -->
      <div class="meter-indicators">
        <div class="indicator" :class="{ active: usagePercentage >= 75 }">
          ⚠️ Warning
        </div>
        <div class="indicator" :class="{ active: usagePercentage >= 90 }">
          🚨 Critical
        </div>
        <div class="indicator" :class="{ active: hasViolations }">
          ❌ Violations
        </div>
      </div>
    </div>
    
    <!-- Meter Details -->
    <div class="meter-details" v-if="showDetails">
      <div class="detail-row">
        <span class="detail-label">Budget:</span>
        <span class="detail-value">{{ formatTime(budgetTimeNs) }}</span>
      </div>
      <div class="detail-row">
        <span class="detail-label">Used:</span>
        <span class="detail-value" :class="usageClass">{{ formatTime(usedTimeNs) }}</span>
      </div>
      <div class="detail-row">
        <span class="detail-label">Remaining:</span>
        <span class="detail-value">{{ formatTime(remainingTimeNs) }}</span>
      </div>
      <div class="detail-row" v-if="precision">
        <span class="detail-label">Precision:</span>
        <span class="detail-value">{{ precision }}</span>
      </div>
      <div class="detail-row" v-if="violationCount > 0">
        <span class="detail-label">Violations:</span>
        <span class="detail-value error">{{ violationCount }}</span>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     TTL Budget Configuration Component
     ============================================================================= -->
<template name="ttl-budget-config">
  <div class="ttl-budget-config">
    <div class="config-header">
      <h3 class="config-title">⏱️ TTL Budget Configuration</h3>
      <div class="config-actions">
        <button class="reset-btn" @click="resetToDefaults">
          🔄 Reset
        </button>
        <button class="apply-btn" @click="applyConfiguration" :disabled="!hasChanges">
          ✅ Apply
        </button>
      </div>
    </div>
    
    <!-- Global TTL Settings -->
    <div class="config-section">
      <h4 class="section-title">🌐 Global Settings</h4>
      
      <div class="config-group">
        <label class="config-label">Global TTL Budget (ms):</label>
        <div class="input-with-slider">
          <input
            v-model.number="globalBudgetMs"
            type="number"
            min="1"
            max="1000"
            step="1"
            class="budget-input"
            @input="onGlobalBudgetChange"
          >
          <input
            v-model.number="globalBudgetMs"
            type="range"
            min="1"
            max="100"
            step="1"
            class="budget-slider"
            @input="onGlobalBudgetChange"
          >
        </div>
        <div class="budget-distribution">
          <span class="distribution-label">Distribution:</span>
          <div class="distribution-bars">
            <div
              v-for="stage in stageDistribution"
              :key="stage.id"
              class="distribution-bar"
              :style="{ width: `${stage.percentage}%`, backgroundColor: stage.color }"
              :title="`${stage.name}: ${stage.budgetMs}ms (${stage.percentage}%)`"
            ></div>
          </div>
        </div>
      </div>
      
      <div class="config-group">
        <label class="config-label">Precision Level:</label>
        <select v-model="precisionLevel" class="precision-select" @change="onPrecisionChange">
          <option value="nanosecond">Nanosecond (ns)</option>
          <option value="microsecond">Microsecond (μs)</option>
          <option value="millisecond">Millisecond (ms)</option>
        </select>
      </div>
      
      <div class="config-group">
        <label class="config-label">
          <input
            v-model="enforceStrict"
            type="checkbox"
            class="checkbox-input"
            @change="onStrictModeChange"
          >
          Strict TTL Enforcement
        </label>
        <p class="config-description">
          When enabled, operations will be forcibly terminated if they exceed TTL budget
        </p>
      </div>
    </div>
    
    <!-- Per-Stage TTL Settings -->
    <div class="config-section">
      <h4 class="section-title">🔄 Per-Stage Configuration</h4>
      
      <div class="stage-config-grid">
        <div
          v-for="stage in stageConfigs"
          :key="stage.id"
          class="stage-config-card"
          :class="{ modified: stage.modified }"
        >
          <div class="stage-header">
            <span class="stage-icon" :style="{ color: stage.color }">⚡</span>
            <span class="stage-name">{{ stage.name }}</span>
            <span class="stage-percentage">{{ calculateStagePercentage(stage.budgetMs) }}%</span>
          </div>
          
          <div class="stage-controls">
            <label class="control-label">Budget (ms):</label>
            <div class="control-input-group">
              <input
                v-model.number="stage.budgetMs"
                type="number"
                min="0.1"
                max="50"
                step="0.1"
                class="stage-budget-input"
                @input="onStageBudgetChange(stage)"
              >
              <input
                v-model.number="stage.budgetMs"
                type="range"
                min="0.1"
                max="20"
                step="0.1"
                class="stage-budget-slider"
                @input="onStageBudgetChange(stage)"
              >
            </div>
          </div>
          
          <div class="stage-stats">
            <div class="stat-item">
              <span class="stat-label">Avg:</span>
              <span class="stat-value">{{ stage.avgExecutionMs.toFixed(1) }}ms</span>
            </div>
            <div class="stat-item">
              <span class="stat-label">Max:</span>
              <span class="stat-value">{{ stage.maxExecutionMs.toFixed(1) }}ms</span>
            </div>
            <div class="stat-item">
              <span class="stat-label">Violations:</span>
              <span class="stat-value" :class="{ error: stage.violations > 0 }">
                {{ stage.violations }}
              </span>
            </div>
          </div>
          
          <div class="stage-actions">
            <button class="optimize-btn" @click="optimizeStage(stage)">
              🎯 Optimize
            </button>
            <button class="reset-stage-btn" @click="resetStage(stage)">
              🔄 Reset
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Advanced Settings -->
    <div class="config-section">
      <h4 class="section-title">⚙️ Advanced Settings</h4>
      
      <div class="advanced-controls">
        <div class="control-group">
          <label class="control-label">Violation Threshold (%):</label>
          <input
            v-model.number="violationThreshold"
            type="number"
            min="1"
            max="50"
            step="1"
            class="threshold-input"
          >
          <span class="control-description">
            Percentage over budget before flagging as violation
          </span>
        </div>
        
        <div class="control-group">
          <label class="control-label">Auto-Optimization:</label>
          <select v-model="autoOptimization" class="auto-opt-select">
            <option value="disabled">Disabled</option>
            <option value="conservative">Conservative</option>
            <option value="moderate">Moderate</option>
            <option value="aggressive">Aggressive</option>
          </select>
        </div>
        
        <div class="control-group">
          <label class="control-label">
            <input
              v-model="enablePredictive"
              type="checkbox"
              class="checkbox-input"
            >
            Predictive TTL Adjustment
          </label>
          <span class="control-description">
            Automatically adjust budgets based on historical performance
          </span>
        </div>
      </div>
    </div>
    
    <!-- Configuration Preview -->
    <div class="config-preview">
      <h4 class="preview-title">📋 Configuration Preview</h4>
      <div class="preview-content">
        <pre class="config-json">{{ configPreview }}</pre>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     TTL Violation Alert Component
     ============================================================================= -->
<template name="ttl-violation-alert">
  <div class="ttl-violation-alert" :class="alertSeverityClass" v-if="visible">
    <div class="alert-header">
      <span class="alert-icon">{{ alertIcon }}</span>
      <span class="alert-title">{{ alertTitle }}</span>
      <div class="alert-actions">
        <button class="dismiss-btn" @click="dismissAlert" v-if="dismissible">
          ✖️
        </button>
      </div>
    </div>
    
    <div class="alert-content">
      <div class="violation-details">
        <div class="detail-grid">
          <div class="detail-item">
            <span class="detail-label">Stage:</span>
            <span class="detail-value">{{ violation.stage }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Expected:</span>
            <span class="detail-value">{{ formatTime(violation.expectedTimeNs) }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Actual:</span>
            <span class="detail-value error">{{ formatTime(violation.actualTimeNs) }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Overage:</span>
            <span class="detail-value critical">{{ formatTime(violation.overageNs) }}</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Percentage:</span>
            <span class="detail-value">{{ violation.overagePercentage.toFixed(1) }}%</span>
          </div>
          <div class="detail-item">
            <span class="detail-label">Timestamp:</span>
            <span class="detail-value">{{ formatTimestamp(violation.timestamp) }}</span>
          </div>
        </div>
      </div>
      
      <div class="violation-context" v-if="violation.context">
        <h5 class="context-title">📊 Context Information</h5>
        <div class="context-content">
          <div class="context-item" v-if="violation.context.systemLoad">
            <span class="context-label">System Load:</span>
            <span class="context-value">{{ violation.context.systemLoad }}%</span>
          </div>
          <div class="context-item" v-if="violation.context.concurrentExecutions">
            <span class="context-label">Concurrent Executions:</span>
            <span class="context-value">{{ violation.context.concurrentExecutions }}</span>
          </div>
          <div class="context-item" v-if="violation.context.memoryUsage">
            <span class="context-label">Memory Usage:</span>
            <span class="context-value">{{ violation.context.memoryUsage }}%</span>
          </div>
        </div>
      </div>
      
      <div class="violation-recommendations" v-if="violation.recommendations">
        <h5 class="recommendations-title">💡 Recommendations</h5>
        <ul class="recommendations-list">
          <li
            v-for="recommendation in violation.recommendations"
            :key="recommendation.id"
            class="recommendation-item"
            :class="recommendation.priority"
          >
            <span class="rec-icon">{{ recommendation.icon }}</span>
            <span class="rec-text">{{ recommendation.text }}</span>
          </li>
        </ul>
      </div>
    </div>
    
    <div class="alert-footer">
      <div class="alert-metadata">
        <span class="violation-id">ID: {{ violation.id }}</span>
        <span class="alert-age">{{ alertAge }}</span>
      </div>
      <div class="alert-footer-actions">
        <button class="action-btn investigate" @click="investigateViolation">
          🔍 Investigate
        </button>
        <button class="action-btn optimize" @click="optimizeStage">
          🎯 Optimize Stage
        </button>
        <button class="action-btn acknowledge" @click="acknowledgeViolation">
          ✅ Acknowledge
        </button>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     TTL Performance Chart Component
     ============================================================================= -->
<template name="ttl-performance-chart">
  <div class="ttl-performance-chart">
    <div class="chart-header">
      <h3 class="chart-title">📈 TTL Performance Trends</h3>
      <div class="chart-controls">
        <select v-model="timeRange" @change="updateChart" class="time-range-select">
          <option value="1h">Last Hour</option>
          <option value="6h">Last 6 Hours</option>
          <option value="24h">Last 24 Hours</option>
          <option value="7d">Last 7 Days</option>
          <option value="30d">Last 30 Days</option>
        </select>
        
        <select v-model="chartType" @change="updateChart" class="chart-type-select">
          <option value="line">Line Chart</option>
          <option value="area">Area Chart</option>
          <option value="bar">Bar Chart</option>
          <option value="heatmap">Heatmap</option>
        </select>
        
        <button class="refresh-chart-btn" @click="refreshChart">
          🔄 Refresh
        </button>
      </div>
    </div>
    
    <!-- Chart Canvas -->
    <div class="chart-container">
      <canvas
        ref="chartCanvas"
        class="performance-canvas"
        :width="chartWidth"
        :height="chartHeight"
        @mousemove="onChartHover"
        @click="onChartClick"
      ></canvas>
      
      <!-- Chart Tooltip -->
      <div
        v-if="tooltip.visible"
        class="chart-tooltip"
        :style="{ left: tooltip.x + 'px', top: tooltip.y + 'px' }"
      >
        <div class="tooltip-header">
          <span class="tooltip-time">{{ tooltip.time }}</span>
        </div>
        <div class="tooltip-content">
          <div class="tooltip-metric">
            <span class="metric-label">Execution Time:</span>
            <span class="metric-value">{{ tooltip.executionTime }}ms</span>
          </div>
          <div class="tooltip-metric">
            <span class="metric-label">TTL Budget:</span>
            <span class="metric-value">{{ tooltip.budget }}ms</span>
          </div>
          <div class="tooltip-metric">
            <span class="metric-label">Utilization:</span>
            <span class="metric-value" :class="{ warning: tooltip.utilization > 75, critical: tooltip.utilization > 90 }">
              {{ tooltip.utilization }}%
            </span>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Chart Legend -->
    <div class="chart-legend">
      <div class="legend-item" v-for="series in chartSeries" :key="series.id">
        <div class="legend-color" :style="{ backgroundColor: series.color }"></div>
        <span class="legend-label">{{ series.label }}</span>
        <span class="legend-value">{{ series.currentValue }}</span>
      </div>
    </div>
    
    <!-- Chart Statistics -->
    <div class="chart-statistics">
      <div class="stat-group">
        <h4 class="stat-group-title">📊 Performance Statistics</h4>
        <div class="stat-grid">
          <div class="stat-card">
            <span class="stat-label">Average TTL Usage:</span>
            <span class="stat-value">{{ chartStats.avgUsage.toFixed(1) }}%</span>
          </div>
          <div class="stat-card">
            <span class="stat-label">Peak Usage:</span>
            <span class="stat-value">{{ chartStats.peakUsage.toFixed(1) }}%</span>
          </div>
          <div class="stat-card">
            <span class="stat-label">Violations:</span>
            <span class="stat-value error">{{ chartStats.violations }}</span>
          </div>
          <div class="stat-card">
            <span class="stat-label">Efficiency:</span>
            <span class="stat-value">{{ chartStats.efficiency.toFixed(1) }}%</span>
          </div>
        </div>
      </div>
      
      <div class="trend-analysis">
        <h4 class="trend-title">📈 Trend Analysis</h4>
        <div class="trend-indicators">
          <div class="trend-item" :class="trendClass('usage')">
            <span class="trend-icon">{{ trendIcon('usage') }}</span>
            <span class="trend-label">TTL Usage:</span>
            <span class="trend-direction">{{ trendDirection('usage') }}</span>
          </div>
          <div class="trend-item" :class="trendClass('violations')">
            <span class="trend-icon">{{ trendIcon('violations') }}</span>
            <span class="trend-label">Violations:</span>
            <span class="trend-direction">{{ trendDirection('violations') }}</span>
          </div>
          <div class="trend-item" :class="trendClass('performance')">
            <span class="trend-icon">{{ trendIcon('performance') }}</span>
            <span class="trend-label">Performance:</span>
            <span class="trend-direction">{{ trendDirection('performance') }}</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     Real-time TTL Monitor Component
     ============================================================================= -->
<template name="ttl-realtime-monitor">
  <div class="ttl-realtime-monitor">
    <div class="monitor-header">
      <h3 class="monitor-title">📡 Real-time TTL Monitor</h3>
      <div class="monitor-controls">
        <button class="toggle-monitoring-btn" @click="toggleMonitoring" :class="{ active: isMonitoring }">
          {{ isMonitoring ? '⏸️ Pause' : '▶️ Start' }} Monitoring
        </button>
        <div class="update-rate-control">
          <label>Update Rate:</label>
          <select v-model="updateRate" @change="onUpdateRateChange">
            <option value="100">100ms</option>
            <option value="250">250ms</option>
            <option value="500">500ms</option>
            <option value="1000">1s</option>
          </select>
        </div>
      </div>
    </div>
    
    <!-- Active Executions Monitor -->
    <div class="executions-monitor">
      <h4 class="section-title">⚡ Active Executions</h4>
      <div class="executions-grid">
        <div
          v-for="execution in activeExecutions"
          :key="execution.id"
          class="execution-card"
          :class="executionStatusClass(execution)"
        >
          <div class="execution-header">
            <span class="execution-id">{{ execution.id.slice(-8) }}</span>
            <span class="execution-stage">{{ execution.currentStage }}</span>
            <div class="execution-progress-bar">
              <div
                class="progress-fill"
                :style="{ width: `${execution.progressPercentage}%` }"
                :class="executionProgressClass(execution)"
              ></div>
            </div>
          </div>
          
          <div class="execution-metrics">
            <div class="metric">
              <span class="metric-label">Elapsed:</span>
              <span class="metric-value">{{ formatTime(execution.elapsedTimeNs) }}</span>
            </div>
            <div class="metric">
              <span class="metric-label">Budget:</span>
              <span class="metric-value">{{ formatTime(execution.budgetTimeNs) }}</span>
            </div>
            <div class="metric">
              <span class="metric-label">Remaining:</span>
              <span class="metric-value" :class="{ warning: execution.remainingTimeNs < execution.budgetTimeNs * 0.2 }">
                {{ formatTime(execution.remainingTimeNs) }}
              </span>
            </div>
          </div>
          
          <div class="execution-timeline">
            <div
              v-for="stage in execution.stageTimeline"
              :key="stage.name"
              class="timeline-stage"
              :class="{ completed: stage.completed, active: stage.active }"
              :style="{ width: `${stage.budgetPercentage}%` }"
            ></div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- System-wide TTL Metrics -->
    <div class="system-metrics">
      <h4 class="section-title">🌐 System-wide Metrics</h4>
      <div class="metrics-dashboard">
        <div class="metric-tile">
          <div class="tile-header">
            <span class="tile-icon">⚡</span>
            <span class="tile-title">Active Executions</span>
          </div>
          <div class="tile-value">{{ systemMetrics.activeExecutions }}</div>
          <div class="tile-trend" :class="trendClass('executions')">
            {{ trendIcon('executions') }} {{ systemMetrics.executionsTrend }}
          </div>
        </div>
        
        <div class="metric-tile">
          <div class="tile-header">
            <span class="tile-icon">⏱️</span>
            <span class="tile-title">Avg TTL Usage</span>
          </div>
          <div class="tile-value">{{ systemMetrics.avgTTLUsage.toFixed(1) }}%</div>
          <div class="tile-trend" :class="trendClass('usage')">
            {{ trendIcon('usage') }} {{ systemMetrics.usageTrend }}
          </div>
        </div>
        
        <div class="metric-tile">
          <div class="tile-header">
            <span class="tile-icon">🚨</span>
            <span class="tile-title">TTL Violations</span>
          </div>
          <div class="tile-value error">{{ systemMetrics.ttlViolations }}</div>
          <div class="tile-trend" :class="trendClass('violations')">
            {{ trendIcon('violations') }} {{ systemMetrics.violationsTrend }}
          </div>
        </div>
        
        <div class="metric-tile">
          <div class="tile-header">
            <span class="tile-icon">📊</span>
            <span class="tile-title">Efficiency</span>
          </div>
          <div class="tile-value">{{ systemMetrics.efficiency.toFixed(1) }}%</div>
          <div class="tile-trend" :class="trendClass('efficiency')">
            {{ trendIcon('efficiency') }} {{ systemMetrics.efficiencyTrend }}
          </div>
        </div>
      </div>
    </div>
    
    <!-- Real-time Events Feed -->
    <div class="events-feed">
      <h4 class="section-title">📋 TTL Events Feed</h4>
      <div class="events-container">
        <div
          v-for="event in realtimeEvents.slice(0, 10)"
          :key="event.id"
          class="event-item"
          :class="event.type"
        >
          <span class="event-timestamp">{{ formatEventTime(event.timestamp) }}</span>
          <span class="event-icon">{{ event.icon }}</span>
          <span class="event-message">{{ event.message }}</span>
          <span class="event-duration" v-if="event.duration">{{ formatTime(event.duration) }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<!-- =============================================================================
     Component Script with TTL Logic
     ============================================================================= -->
<script setup>
// =============================================================================
// TTL Usage Meter Component
// =============================================================================

// TTL Usage Meter Props
const meterProps = defineProps({
  usedTimeNs: { type: Number, default: 0 },
  budgetTimeNs: { type: Number, default: 8_000_000 }, // 8ms in nanoseconds
  size: { type: String, default: 'medium' }, // small, medium, large
  showDetails: { type: Boolean, default: true },
  precision: { type: String, default: 'nanosecond' },
  violationCount: { type: Number, default: 0 },
  animated: { type: Boolean, default: true }
})

// TTL Usage Meter Computed
const usagePercentage = computed(() => 
  (meterProps.usedTimeNs / meterProps.budgetTimeNs) * 100
)

const remainingTimeNs = computed(() => 
  Math.max(0, meterProps.budgetTimeNs - meterProps.usedTimeNs)
)

const meterSize = computed(() => {
  const sizes = { small: 120, medium: 200, large: 280 }
  return sizes[meterProps.size] || sizes.medium
})

const meterRadius = computed(() => meterSize.value * 0.35)
const strokeWidth = computed(() => meterSize.value * 0.05)
const circumference = computed(() => 2 * Math.PI * meterRadius.value)

const progressOffset = computed(() => 
  circumference.value - (usagePercentage.value / 100) * circumference.value
)

const progressColor = computed(() => {
  if (usagePercentage.value >= 90) return '#ef4444'
  if (usagePercentage.value >= 75) return '#f59e0b'
  return '#10b981'
})

const usageClass = computed(() => {
  if (usagePercentage.value >= 90) return 'critical'
  if (usagePercentage.value >= 75) return 'warning'
  return 'normal'
})

const meterSizeClass = computed(() => `size-${meterProps.size}`)
const hasViolations = computed(() => meterProps.violationCount > 0)
const isPulsing = computed(() => meterProps.animated && usagePercentage.value >= 90)

// =============================================================================
// TTL Budget Configuration Component
// =============================================================================

// Configuration state
const globalBudgetMs = ref(8)
const precisionLevel = ref('nanosecond')
const enforceStrict = ref(true)
const violationThreshold = ref(10)
const autoOptimization = ref('conservative')
const enablePredictive = ref(false)
const hasChanges = ref(false)

// Stage configurations
const stageConfigs = ref([
  { id: 'typer', name: 'Typer', budgetMs: 2, color: '#3b82f6', avgExecutionMs: 1.8, maxExecutionMs: 2.5, violations: 0, modified: false },
  { id: 'turtle', name: 'Turtle', budgetMs: 1, color: '#8b5cf6', avgExecutionMs: 0.9, maxExecutionMs: 1.3, violations: 0, modified: false },
  { id: 'ttl2dspy', name: 'TTL2DSPy', budgetMs: 3, color: '#10b981', avgExecutionMs: 2.7, maxExecutionMs: 3.8, violations: 2, modified: false },
  { id: 'bitactor', name: 'BitActor', budgetMs: 2, color: '#f59e0b', avgExecutionMs: 1.9, maxExecutionMs: 2.2, violations: 0, modified: false },
  { id: 'erlang', name: 'Erlang', budgetMs: 1, color: '#ef4444', avgExecutionMs: 0.8, maxExecutionMs: 1.1, violations: 0, modified: false },
  { id: 'ash', name: 'Ash', budgetMs: 2, color: '#06b6d4', avgExecutionMs: 1.7, maxExecutionMs: 2.4, violations: 1, modified: false },
  { id: 'reactor', name: 'Reactor', budgetMs: 3, color: '#84cc16', avgExecutionMs: 3.2, maxExecutionMs: 4.1, violations: 5, modified: false },
  { id: 'k8s', name: 'K8s', budgetMs: 1, color: '#6366f1', avgExecutionMs: 0.7, maxExecutionMs: 1.0, violations: 0, modified: false }
])

// Stage distribution calculation
const stageDistribution = computed(() => {
  const total = stageConfigs.value.reduce((sum, stage) => sum + stage.budgetMs, 0)
  return stageConfigs.value.map(stage => ({
    ...stage,
    percentage: (stage.budgetMs / total) * 100
  }))
})

// Configuration preview
const configPreview = computed(() => {
  return JSON.stringify({
    globalBudgetMs: globalBudgetMs.value,
    precisionLevel: precisionLevel.value,
    enforceStrict: enforceStrict.value,
    stages: stageConfigs.value.map(stage => ({
      id: stage.id,
      budgetMs: stage.budgetMs
    })),
    advanced: {
      violationThreshold: violationThreshold.value,
      autoOptimization: autoOptimization.value,
      enablePredictive: enablePredictive.value
    }
  }, null, 2)
})

// Configuration methods
const onGlobalBudgetChange = () => {
  hasChanges.value = true
  redistributeBudgets()
}

const redistributeBudgets = () => {
  const currentTotal = stageConfigs.value.reduce((sum, stage) => sum + stage.budgetMs, 0)
  const scaleFactor = globalBudgetMs.value / currentTotal
  
  stageConfigs.value.forEach(stage => {
    stage.budgetMs = Math.max(0.1, Math.round((stage.budgetMs * scaleFactor) * 10) / 10)
    stage.modified = true
  })
}

const calculateStagePercentage = (budgetMs) => {
  return ((budgetMs / globalBudgetMs.value) * 100).toFixed(1)
}

const onStageBudgetChange = (stage) => {
  stage.modified = true
  hasChanges.value = true
  
  // Update global budget to match sum of stages
  globalBudgetMs.value = stageConfigs.value.reduce((sum, s) => sum + s.budgetMs, 0)
}

const optimizeStage = (stage) => {
  // Set budget to 110% of average execution time
  stage.budgetMs = Math.max(0.1, Math.round((stage.avgExecutionMs * 1.1) * 10) / 10)
  stage.modified = true
  hasChanges.value = true
}

const resetStage = (stage) => {
  // Reset to original values (implementation would restore from defaults)
  stage.modified = false
  hasChanges.value = true
}

const resetToDefaults = () => {
  globalBudgetMs.value = 8
  precisionLevel.value = 'nanosecond'
  enforceStrict.value = true
  violationThreshold.value = 10
  autoOptimization.value = 'conservative'
  enablePredictive.value = false
  hasChanges.value = false
  
  stageConfigs.value.forEach(stage => {
    stage.modified = false
  })
}

const applyConfiguration = () => {
  console.log('Applying TTL configuration:', configPreview.value)
  hasChanges.value = false
  // Emit configuration change event
  emit('configurationApplied', {
    globalBudgetMs: globalBudgetMs.value,
    stages: stageConfigs.value,
    advanced: {
      precisionLevel: precisionLevel.value,
      enforceStrict: enforceStrict.value,
      violationThreshold: violationThreshold.value,
      autoOptimization: autoOptimization.value,
      enablePredictive: enablePredictive.value
    }
  })
}

// =============================================================================
// TTL Violation Alert Component
// =============================================================================

// Violation Alert Props
const alertProps = defineProps({
  violation: { type: Object, required: true },
  severity: { type: String, default: 'high' }, // low, medium, high, critical
  dismissible: { type: Boolean, default: true },
  autoHide: { type: Boolean, default: false },
  autoHideDelay: { type: Number, default: 10000 }
})

// Alert state
const visible = ref(true)
const alertAge = ref('Just now')

// Alert computed properties
const alertSeverityClass = computed(() => `severity-${alertProps.severity}`)

const alertIcon = computed(() => {
  const icons = {
    low: '⚠️',
    medium: '🚨',
    high: '🔥',
    critical: '💥'
  }
  return icons[alertProps.severity] || '⚠️'
})

const alertTitle = computed(() => {
  const titles = {
    low: 'TTL Warning',
    medium: 'TTL Violation',
    high: 'Critical TTL Violation',
    critical: 'Severe TTL Violation'
  }
  return titles[alertProps.severity] || 'TTL Alert'
})

// Alert methods
const dismissAlert = () => {
  visible.value = false
  emit('alertDismissed', alertProps.violation.id)
}

const investigateViolation = () => {
  console.log('Investigating violation:', alertProps.violation.id)
  emit('investigateRequested', alertProps.violation)
}

const optimizeStage = () => {
  console.log('Optimizing stage:', alertProps.violation.stage)
  emit('optimizationRequested', alertProps.violation.stage)
}

const acknowledgeViolation = () => {
  console.log('Acknowledging violation:', alertProps.violation.id)
  emit('violationAcknowledged', alertProps.violation.id)
  if (alertProps.autoHide) {
    dismissAlert()
  }
}

// Auto-hide functionality
if (alertProps.autoHide) {
  setTimeout(() => {
    if (visible.value) {
      dismissAlert()
    }
  }, alertProps.autoHideDelay)
}

// =============================================================================
// TTL Performance Chart Component
// =============================================================================

// Chart state
const timeRange = ref('24h')
const chartType = ref('line')
const chartWidth = 800
const chartHeight = 400
const chartCanvas = ref(null)

// Tooltip state
const tooltip = ref({
  visible: false,
  x: 0,
  y: 0,
  time: '',
  executionTime: '',
  budget: '',
  utilization: 0
})

// Chart data
const chartSeries = ref([
  { id: 'execution', label: 'Execution Time', color: '#3b82f6', currentValue: '6.2ms' },
  { id: 'budget', label: 'TTL Budget', color: '#10b981', currentValue: '8.0ms' },
  { id: 'utilization', label: 'Utilization', color: '#f59e0b', currentValue: '77.5%' }
])

const chartStats = ref({
  avgUsage: 67.5,
  peakUsage: 94.2,
  violations: 8,
  efficiency: 85.3
})

// Chart methods
const updateChart = () => {
  console.log(`Updating chart: ${chartType.value} for ${timeRange.value}`)
  // Implementation would update chart data based on time range and type
}

const refreshChart = () => {
  console.log('Refreshing chart data')
  updateChart()
}

const onChartHover = (event) => {
  // Show tooltip on hover (simplified implementation)
  const rect = chartCanvas.value.getBoundingClientRect()
  tooltip.value = {
    visible: true,
    x: event.clientX - rect.left,
    y: event.clientY - rect.top,
    time: '14:32:15',
    executionTime: '6.8',
    budget: '8.0',
    utilization: 85
  }
}

const onChartClick = (event) => {
  console.log('Chart clicked at:', event.offsetX, event.offsetY)
  // Implementation would handle chart interaction
}

// Trend analysis
const trendClass = (metric) => {
  // Simplified trend calculation
  return 'trend-stable' // or 'trend-up', 'trend-down'
}

const trendIcon = (metric) => {
  return '➡️' // or '⬆️', '⬇️'
}

const trendDirection = (metric) => {
  return 'Stable' // or 'Increasing', 'Decreasing'
}

// =============================================================================
// Real-time TTL Monitor Component
// =============================================================================

// Monitor state
const isMonitoring = ref(true)
const updateRate = ref(250)

// Active executions
const activeExecutions = ref([
  {
    id: 'exec_1234567890',
    currentStage: 'reactor',
    progressPercentage: 75,
    elapsedTimeNs: 5_500_000,
    budgetTimeNs: 8_000_000,
    remainingTimeNs: 2_500_000,
    stageTimeline: [
      { name: 'typer', completed: true, active: false, budgetPercentage: 25 },
      { name: 'turtle', completed: true, active: false, budgetPercentage: 12.5 },
      { name: 'ttl2dspy', completed: true, active: false, budgetPercentage: 37.5 },
      { name: 'bitactor', completed: false, active: true, budgetPercentage: 25 }
    ]
  }
])

// System metrics
const systemMetrics = ref({
  activeExecutions: 3,
  executionsTrend: '+2',
  avgTTLUsage: 72.3,
  usageTrend: '+5.2%',
  ttlViolations: 2,
  violationsTrend: '-1',
  efficiency: 87.8,
  efficiencyTrend: '+3.1%'
})

// Real-time events
const realtimeEvents = ref([
  { id: 1, type: 'success', icon: '✅', message: 'Pipeline execution completed successfully', timestamp: Date.now(), duration: 6_200_000 },
  { id: 2, type: 'warning', icon: '⚠️', message: 'TTL budget at 85% utilization', timestamp: Date.now() - 15000 },
  { id: 3, type: 'violation', icon: '🚨', message: 'TTL violation in reactor stage', timestamp: Date.now() - 45000, duration: 9_100_000 }
])

// Monitor methods
const toggleMonitoring = () => {
  isMonitoring.value = !isMonitoring.value
  console.log(`Monitoring ${isMonitoring.value ? 'started' : 'paused'}`)
}

const onUpdateRateChange = () => {
  console.log(`Update rate changed to: ${updateRate.value}ms`)
}

const executionStatusClass = (execution) => {
  const utilization = (execution.elapsedTimeNs / execution.budgetTimeNs) * 100
  if (utilization >= 90) return 'critical'
  if (utilization >= 75) return 'warning'
  return 'normal'
}

const executionProgressClass = (execution) => {
  return executionStatusClass(execution)
}

// =============================================================================
// Shared Utility Functions
// =============================================================================

const formatTime = (nanoseconds) => {
  if (nanoseconds >= 1_000_000) {
    return `${(nanoseconds / 1_000_000).toFixed(1)}ms`
  } else if (nanoseconds >= 1_000) {
    return `${(nanoseconds / 1_000).toFixed(1)}μs`
  } else {
    return `${nanoseconds}ns`
  }
}

const formatTimestamp = (timestamp) => {
  return new Date(timestamp).toLocaleTimeString()
}

const formatEventTime = (timestamp) => {
  const diff = Date.now() - timestamp
  if (diff < 1000) return 'now'
  if (diff < 60000) return `${Math.floor(diff / 1000)}s ago`
  return formatTimestamp(timestamp)
}

// Component emits
const emit = defineEmits([
  'configurationApplied',
  'alertDismissed',
  'investigateRequested',
  'optimizationRequested',
  'violationAcknowledged'
])

console.log('🎛️ TTL Metrics Components loaded')
</script>

<!-- =============================================================================
     Component Styles
     ============================================================================= -->
<style scoped>
/* TTL Usage Meter Styles */
.ttl-usage-meter {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 1rem;
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  color: #e2e8f0;
}

.meter-container {
  position: relative;
  margin-bottom: 1rem;
}

.meter-svg {
  transform: rotate(-90deg);
}

.usage-percentage {
  font-size: 1.5rem;
  font-weight: bold;
  fill: #e2e8f0;
}

.usage-percentage.warning { fill: #f59e0b; }
.usage-percentage.critical { fill: #ef4444; }

.ttl-label {
  font-size: 0.875rem;
  fill: #94a3b8;
}

.time-values {
  font-size: 0.75rem;
  fill: #64748b;
}

.pulse-animation {
  animation: pulse 1s infinite;
}

.meter-indicators {
  display: flex;
  gap: 0.5rem;
  margin-top: 0.5rem;
}

.indicator {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.75rem;
  opacity: 0.3;
  transition: opacity 0.3s;
}

.indicator.active {
  opacity: 1;
  background: rgba(239, 68, 68, 0.2);
}

.meter-details {
  width: 100%;
  font-size: 0.875rem;
}

.detail-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.25rem;
}

.detail-value.error { color: #ef4444; }

/* TTL Budget Configuration Styles */
.ttl-budget-config {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  color: #e2e8f0;
  overflow: hidden;
}

.config-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-bottom: 1px solid #475569;
}

.config-title {
  margin: 0;
  font-size: 1.125rem;
}

.config-actions {
  display: flex;
  gap: 0.5rem;
}

.reset-btn, .apply-btn {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
}

.reset-btn {
  background: #6b7280;
  color: white;
}

.apply-btn {
  background: #10b981;
  color: white;
}

.apply-btn:disabled {
  background: #374151;
  cursor: not-allowed;
}

.config-section {
  padding: 1rem;
  border-bottom: 1px solid #334155;
}

.section-title {
  margin: 0 0 1rem 0;
  font-size: 1rem;
  color: #94a3b8;
}

.config-group {
  margin-bottom: 1.5rem;
}

.config-label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: bold;
}

.input-with-slider {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.budget-input {
  width: 100px;
  padding: 0.5rem;
  background: #334155;
  color: white;
  border: 1px solid #475569;
  border-radius: 4px;
}

.budget-slider {
  flex: 1;
}

.budget-distribution {
  margin-top: 0.5rem;
  font-size: 0.875rem;
}

.distribution-bars {
  display: flex;
  height: 8px;
  border-radius: 4px;
  overflow: hidden;
  margin-top: 0.25rem;
}

.distribution-bar {
  transition: width 0.3s ease;
}

.stage-config-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  gap: 1rem;
}

.stage-config-card {
  background: #334155;
  border: 1px solid #475569;
  border-radius: 6px;
  padding: 1rem;
}

.stage-config-card.modified {
  border-color: #10b981;
}

.stage-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 1rem;
}

.stage-icon {
  font-size: 1.25rem;
}

.stage-controls {
  margin-bottom: 1rem;
}

.control-input-group {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.stage-budget-input {
  width: 80px;
  padding: 0.25rem;
  background: #1e293b;
  color: white;
  border: 1px solid #334155;
  border-radius: 4px;
}

.stage-budget-slider {
  flex: 1;
}

.stage-stats {
  display: flex;
  justify-content: space-between;
  margin-bottom: 1rem;
  font-size: 0.75rem;
}

.stat-item {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.stat-value.error { color: #ef4444; }

.stage-actions {
  display: flex;
  gap: 0.5rem;
}

.optimize-btn, .reset-stage-btn {
  flex: 1;
  padding: 0.5rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.75rem;
}

.optimize-btn {
  background: #3b82f6;
  color: white;
}

.reset-stage-btn {
  background: #6b7280;
  color: white;
}

.config-preview {
  padding: 1rem;
  background: #0f172a;
}

.config-json {
  background: #1e293b;
  padding: 1rem;
  border-radius: 4px;
  font-size: 0.75rem;
  overflow-x: auto;
}

/* TTL Violation Alert Styles */
.ttl-violation-alert {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  color: #e2e8f0;
  margin-bottom: 1rem;
}

.ttl-violation-alert.severity-low { border-left: 4px solid #10b981; }
.ttl-violation-alert.severity-medium { border-left: 4px solid #f59e0b; }
.ttl-violation-alert.severity-high { border-left: 4px solid #ef4444; }
.ttl-violation-alert.severity-critical { 
  border-left: 4px solid #dc2626;
  animation: alertPulse 2s infinite;
}

.alert-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-bottom: 1px solid #475569;
}

.alert-title {
  font-weight: bold;
  margin-left: 0.5rem;
}

.dismiss-btn {
  background: transparent;
  border: none;
  color: #94a3b8;
  cursor: pointer;
  padding: 0.25rem;
}

.alert-content {
  padding: 1rem;
}

.detail-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.detail-item {
  display: flex;
  justify-content: space-between;
  font-size: 0.875rem;
}

.detail-value.error { color: #ef4444; }
.detail-value.critical { color: #dc2626; font-weight: bold; }

.violation-context, .violation-recommendations {
  margin-top: 1rem;
  padding-top: 1rem;
  border-top: 1px solid #334155;
}

.context-title, .recommendations-title {
  margin: 0 0 0.5rem 0;
  font-size: 0.875rem;
  color: #94a3b8;
}

.recommendations-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.recommendation-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  margin-bottom: 0.25rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.recommendation-item.high { background: rgba(239, 68, 68, 0.1); }
.recommendation-item.medium { background: rgba(245, 158, 11, 0.1); }
.recommendation-item.low { background: rgba(16, 185, 129, 0.1); }

.alert-footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-top: 1px solid #475569;
}

.alert-metadata {
  font-size: 0.75rem;
  color: #94a3b8;
}

.alert-footer-actions {
  display: flex;
  gap: 0.5rem;
}

.action-btn {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
}

.action-btn.investigate { background: #3b82f6; color: white; }
.action-btn.optimize { background: #10b981; color: white; }
.action-btn.acknowledge { background: #6b7280; color: white; }

/* TTL Performance Chart Styles */
.ttl-performance-chart {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  color: #e2e8f0;
  overflow: hidden;
}

.chart-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-bottom: 1px solid #475569;
}

.chart-controls {
  display: flex;
  gap: 0.5rem;
}

.time-range-select, .chart-type-select {
  padding: 0.25rem 0.5rem;
  background: #1e293b;
  color: white;
  border: 1px solid #475569;
  border-radius: 4px;
}

.refresh-chart-btn {
  padding: 0.25rem 0.5rem;
  background: #3b82f6;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}

.chart-container {
  position: relative;
  padding: 1rem;
}

.performance-canvas {
  width: 100%;
  background: #0f172a;
  border-radius: 4px;
}

.chart-tooltip {
  position: absolute;
  background: #334155;
  border: 1px solid #475569;
  border-radius: 4px;
  padding: 0.5rem;
  font-size: 0.875rem;
  pointer-events: none;
  z-index: 1000;
}

.chart-legend {
  display: flex;
  justify-content: center;
  gap: 2rem;
  padding: 1rem;
  border-top: 1px solid #334155;
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.legend-color {
  width: 12px;
  height: 12px;
  border-radius: 2px;
}

.chart-statistics {
  padding: 1rem;
  border-top: 1px solid #334155;
}

.stat-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1rem;
  margin-bottom: 1rem;
}

.stat-card {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 0.75rem;
  background: #334155;
  border-radius: 4px;
}

.stat-value.error { color: #ef4444; }

/* Real-time TTL Monitor Styles */
.ttl-realtime-monitor {
  background: #1e293b;
  border: 1px solid #334155;
  border-radius: 8px;
  color: #e2e8f0;
  overflow: hidden;
}

.monitor-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1rem;
  background: #334155;
  border-bottom: 1px solid #475569;
}

.monitor-controls {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.toggle-monitoring-btn {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  background: #6b7280;
  color: white;
}

.toggle-monitoring-btn.active {
  background: #10b981;
}

.executions-monitor {
  padding: 1rem;
  border-bottom: 1px solid #334155;
}

.executions-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 1rem;
}

.execution-card {
  background: #334155;
  border: 1px solid #475569;
  border-radius: 6px;
  padding: 1rem;
}

.execution-card.warning { border-color: #f59e0b; }
.execution-card.critical { border-color: #ef4444; }

.execution-progress-bar {
  width: 100%;
  height: 4px;
  background: #1e293b;
  border-radius: 2px;
  overflow: hidden;
  margin-top: 0.5rem;
}

.progress-fill {
  height: 100%;
  background: #10b981;
  transition: width 0.3s ease;
}

.progress-fill.warning { background: #f59e0b; }
.progress-fill.critical { background: #ef4444; }

.execution-timeline {
  display: flex;
  height: 4px;
  border-radius: 2px;
  overflow: hidden;
  margin-top: 0.5rem;
}

.timeline-stage {
  background: #475569;
  transition: background-color 0.3s;
}

.timeline-stage.completed { background: #10b981; }
.timeline-stage.active { background: #3b82f6; }

.system-metrics {
  padding: 1rem;
  border-bottom: 1px solid #334155;
}

.metrics-dashboard {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1rem;
}

.metric-tile {
  background: #334155;
  border-radius: 6px;
  padding: 1rem;
  text-align: center;
}

.tile-header {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.tile-value {
  font-size: 1.5rem;
  font-weight: bold;
  margin-bottom: 0.25rem;
}

.tile-value.error { color: #ef4444; }

.tile-trend {
  font-size: 0.75rem;
}

.events-feed {
  padding: 1rem;
}

.events-container {
  max-height: 200px;
  overflow-y: auto;
}

.event-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  margin-bottom: 0.25rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.event-item.success { border-left: 3px solid #10b981; }
.event-item.warning { border-left: 3px solid #f59e0b; }
.event-item.violation { border-left: 3px solid #ef4444; }

.event-timestamp {
  font-size: 0.75rem;
  color: #94a3b8;
  min-width: 60px;
}

/* Animations */
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.7; }
}

@keyframes alertPulse {
  0%, 100% { box-shadow: 0 0 0 0 rgba(239, 68, 68, 0.4); }
  50% { box-shadow: 0 0 0 10px rgba(239, 68, 68, 0); }
}

/* Responsive Design */
@media (max-width: 1024px) {
  .stage-config-grid {
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
  }
  
  .executions-grid {
    grid-template-columns: 1fr;
  }
  
  .metrics-dashboard {
    grid-template-columns: repeat(2, 1fr);
  }
}

@media (max-width: 768px) {
  .detail-grid {
    grid-template-columns: 1fr;
  }
  
  .stat-grid {
    grid-template-columns: repeat(2, 1fr);
  }
  
  .metrics-dashboard {
    grid-template-columns: 1fr;
  }
  
  .chart-controls {
    flex-direction: column;
    gap: 0.25rem;
  }
}
</style>