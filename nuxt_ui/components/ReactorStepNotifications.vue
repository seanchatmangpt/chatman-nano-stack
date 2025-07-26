<template>
  <div class="reactor-step-notifications">
    <h2>🔔 Reactor Step Notifications</h2>
    <p class="notification-description">
      Real-time notifications for reverse pipeline: typer ← turtle ← ttl2dspy ← BitActor ← Erlang ← Ash ← Reactor ← k8s
    </p>
    
    <!-- Notification Control Panel -->
    <div class="notification-control-panel">
      <h3>🎛️ Notification Controls</h3>
      <div class="control-sections">
        <div class="control-section">
          <h4>Global Settings</h4>
          <div class="control-grid">
            <div class="control-item">
              <label>Notification Level</label>
              <select v-model="globalNotificationLevel" @change="updateNotificationSettings">
                <option value="all">All Notifications</option>
                <option value="critical">Critical Only (80/20)</option>
                <option value="errors">Errors Only</option>
                <option value="silent">Silent Mode</option>
              </select>
            </div>
            
            <div class="control-item">
              <label>Update Frequency</label>
              <input 
                type="range" 
                v-model="updateFrequency" 
                min="100" 
                max="5000" 
                step="100"
                @input="updateNotificationSettings"
              />
              <span class="frequency-display">{{ updateFrequency }}ms</span>
            </div>
            
            <div class="control-item">
              <label>Auto-Dismiss</label>
              <input 
                type="checkbox" 
                v-model="autoDismiss" 
                @change="updateNotificationSettings"
              />
              <span class="checkbox-label">Auto-dismiss after {{ dismissTimeout }}s</span>
            </div>
            
            <div class="control-item">
              <label>Sound Alerts</label>
              <input 
                type="checkbox" 
                v-model="soundAlerts" 
                @change="updateNotificationSettings"
              />
              <span class="checkbox-label">Enable audio notifications</span>
            </div>
          </div>
        </div>
        
        <div class="control-section">
          <h4>Step Filters</h4>
          <div class="step-filters">
            <div
              v-for="step in pipelineSteps"
              :key="step.name"
              class="step-filter"
              :class="{ active: step.notificationsEnabled }"
              @click="toggleStepNotifications(step)"
            >
              <span class="step-icon">{{ step.icon }}</span>
              <span class="step-name">{{ step.name }}</span>
              <span class="notification-count" v-if="step.notificationCount > 0">
                {{ step.notificationCount }}
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Live Notification Stream -->
    <div class="live-notification-stream">
      <h3>📡 Live Notification Stream</h3>
      <div class="stream-controls">
        <button 
          @click="toggleStream" 
          class="stream-btn"
          :class="{ active: streamActive }"
        >
          {{ streamActive ? '⏸️ Pause' : '▶️ Start' }} Stream
        </button>
        <button @click="clearAllNotifications" class="clear-btn">
          🗑️ Clear All
        </button>
        <button @click="exportNotifications" class="export-btn">
          📊 Export
        </button>
      </div>
      
      <div class="notification-stream" ref="notificationStream">
        <div
          v-for="notification in liveNotifications"
          :key="notification.id"
          class="notification-item"
          :class="[
            notification.level,
            notification.step,
            { 
              critical: notification.critical,
              dismissing: notification.dismissing,
              new: notification.isNew
            }
          ]"
          @click="expandNotification(notification)"
        >
          <div class="notification-header">
            <div class="notification-meta">
              <span class="notification-step">{{ getStepIcon(notification.step) }} {{ notification.step }}</span>
              <span class="notification-timestamp">{{ formatTimestamp(notification.timestamp) }}</span>
              <span class="notification-level-badge" :class="notification.level">
                {{ notification.level.toUpperCase() }}
              </span>
            </div>
            <div class="notification-actions">
              <button @click.stop="pinNotification(notification)" class="action-btn pin">
                📌
              </button>
              <button @click.stop="dismissNotification(notification)" class="action-btn dismiss">
                ✕
              </button>
            </div>
          </div>
          
          <div class="notification-content">
            <div class="notification-title">{{ notification.title }}</div>
            <div class="notification-message">{{ notification.message }}</div>
            
            <div v-if="notification.expanded" class="notification-details">
              <div class="detail-section">
                <h5>📊 Metrics</h5>
                <div class="metrics-grid">
                  <div class="metric" v-for="(value, key) in notification.metrics" :key="key">
                    <span class="metric-label">{{ key }}</span>
                    <span class="metric-value">{{ value }}</span>
                  </div>
                </div>
              </div>
              
              <div class="detail-section" v-if="notification.context">
                <h5>🔍 Context</h5>
                <div class="context-info">
                  <pre>{{ JSON.stringify(notification.context, null, 2) }}</pre>
                </div>
              </div>
              
              <div class="detail-section" v-if="notification.suggestions">
                <h5>💡 Suggestions</h5>
                <div class="suggestions-list">
                  <div
                    v-for="suggestion in notification.suggestions"
                    :key="suggestion.id"
                    class="suggestion-item"
                  >
                    <span class="suggestion-icon">{{ suggestion.icon }}</span>
                    <span class="suggestion-text">{{ suggestion.text }}</span>
                    <button @click="applySuggestion(suggestion)" class="apply-suggestion">
                      Apply
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
          
          <div class="notification-progress" v-if="notification.progress !== undefined">
            <div class="progress-bar">
              <div 
                class="progress-fill" 
                :style="{ width: notification.progress + '%' }"
              ></div>
            </div>
            <span class="progress-text">{{ notification.progress }}% complete</span>
          </div>
        </div>
        
        <div v-if="liveNotifications.length === 0" class="no-notifications">
          <div class="no-notifications-icon">🔕</div>
          <div class="no-notifications-text">No active notifications</div>
          <div class="no-notifications-subtext">System is running smoothly</div>
        </div>
      </div>
    </div>
    
    <!-- Step-by-Step Notification Dashboard -->
    <div class="step-notification-dashboard">
      <h3>📋 Step-by-Step Notification Dashboard</h3>
      <div class="dashboard-grid">
        <div
          v-for="step in pipelineSteps"
          :key="step.name"
          class="step-dashboard"
          :class="{ 
            active: step.active,
            critical: step.criticalNotifications > 0,
            optimized: step.optimized
          }"
        >
          <div class="step-header">
            <div class="step-info">
              <span class="step-icon-large">{{ step.icon }}</span>
              <div class="step-details">
                <div class="step-name">{{ step.name }}</div>
                <div class="step-status">{{ step.status }}</div>
              </div>
            </div>
            <div class="step-metrics">
              <div class="metric-item">
                <span class="metric-count">{{ step.totalNotifications }}</span>
                <span class="metric-label">Total</span>
              </div>
              <div class="metric-item critical">
                <span class="metric-count">{{ step.criticalNotifications }}</span>
                <span class="metric-label">Critical</span>
              </div>
            </div>
          </div>
          
          <div class="step-notification-types">
            <div class="notification-type" v-for="type in step.notificationTypes" :key="type.name">
              <div class="type-header">
                <span class="type-icon">{{ type.icon }}</span>
                <span class="type-name">{{ type.name }}</span>
                <span class="type-count">{{ type.count }}</span>
              </div>
              <div class="type-progress">
                <div class="progress-bar small">
                  <div 
                    class="progress-fill" 
                    :class="type.level"
                    :style="{ width: (type.count / step.totalNotifications * 100) + '%' }"
                  ></div>
                </div>
              </div>
            </div>
          </div>
          
          <div class="step-actions">
            <button @click="viewStepDetails(step)" class="step-action-btn details">
              🔍 Details
            </button>
            <button @click="optimizeStepNotifications(step)" class="step-action-btn optimize">
              ⚡ 80/20 Optimize
            </button>
            <button @click="muteStep(step)" class="step-action-btn mute">
              🔇 Mute
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Notification Permutations Engine -->
    <div class="notification-permutations">
      <h3>🧠 Notification Permutations Engine</h3>
      <div class="permutation-controls">
        <div class="permutation-input">
          <label>Permutation Strategy</label>
          <select v-model="selectedPermutationStrategy" @change="generatePermutations">
            <option value="critical_first">Critical First (80/20)</option>
            <option value="chronological">Chronological Order</option>
            <option value="step_priority">Step Priority Based</option>
            <option value="impact_based">Impact-Based Sorting</option>
            <option value="adaptive">Adaptive Learning</option>
          </select>
        </div>
        
        <div class="permutation-input">
          <label>Combination Rules</label>
          <select v-model="combinationRules" @change="generatePermutations">
            <option value="merge_similar">Merge Similar Notifications</option>
            <option value="batch_by_step">Batch by Step</option>
            <option value="group_by_level">Group by Level</option>
            <option value="smart_clustering">Smart Clustering</option>
          </select>
        </div>
        
        <button @click="generatePermutations" class="generate-btn">
          🔄 Generate Permutations
        </button>
      </div>
      
      <div class="permutation-results">
        <div class="permutation-summary">
          <div class="summary-metric">
            <span class="summary-label">Total Permutations</span>
            <span class="summary-value">{{ generatedPermutations.length }}</span>
          </div>
          <div class="summary-metric">
            <span class="summary-label">80/20 Optimized</span>
            <span class="summary-value">{{ optimizedPermutations }}%</span>
          </div>
          <div class="summary-metric">
            <span class="summary-label">Efficiency Gain</span>
            <span class="summary-value">+{{ efficiencyGain }}%</span>
          </div>
        </div>
        
        <div class="permutation-list">
          <div
            v-for="permutation in generatedPermutations.slice(0, 5)"
            :key="permutation.id"
            class="permutation-item"
            :class="{ optimal: permutation.optimal, active: permutation.active }"
          >
            <div class="permutation-header">
              <span class="permutation-name">{{ permutation.name }}</span>
              <span class="permutation-score">{{ permutation.score }}%</span>
            </div>
            <div class="permutation-description">{{ permutation.description }}</div>
            <div class="permutation-steps">
              <span
                v-for="step in permutation.steps"
                :key="step"
                class="permutation-step"
              >
                {{ getStepIcon(step) }}
              </span>
            </div>
            <button @click="applyPermutation(permutation)" class="apply-permutation">
              {{ permutation.active ? '✅ Active' : '📝 Apply' }}
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- 80/20 Notification Analytics -->
    <div class="notification-analytics">
      <h3>📊 80/20 Notification Analytics</h3>
      <div class="analytics-dashboard">
        <div class="analytics-chart">
          <h4>🎯 Pareto Chart - Notification Impact</h4>
          <canvas ref="paretoChart" width="400" height="250"></canvas>
          <div class="chart-summary">
            <div class="chart-insight">
              <span class="insight-icon">🎯</span>
              <span class="insight-text">{{ paretoInsight }}</span>
            </div>
          </div>
        </div>
        
        <div class="analytics-metrics">
          <h4>📈 Key Metrics</h4>
          <div class="metrics-grid">
            <div class="analytics-metric">
              <div class="metric-icon">🔔</div>
              <div class="metric-content">
                <div class="metric-value">{{ totalNotificationsToday }}</div>
                <div class="metric-label">Notifications Today</div>
                <div class="metric-trend">{{ notificationTrend > 0 ? '📈' : '📉' }} {{ Math.abs(notificationTrend) }}%</div>
              </div>
            </div>
            
            <div class="analytics-metric">
              <div class="metric-icon">⚡</div>
              <div class="metric-content">
                <div class="metric-value">{{ criticalNotificationRatio }}%</div>
                <div class="metric-label">Critical Ratio</div>
                <div class="metric-trend">🎯 80/20 Target: 20%</div>
              </div>
            </div>
            
            <div class="analytics-metric">
              <div class="metric-icon">⏱️</div>
              <div class="metric-content">
                <div class="metric-value">{{ averageResponseTime }}ms</div>
                <div class="metric-label">Avg Response Time</div>
                <div class="metric-trend">🚀 {{ responseTimeImprovement }}% faster</div>
              </div>
            </div>
            
            <div class="analytics-metric">
              <div class="metric-icon">🎯</div>
              <div class="metric-content">
                <div class="metric-value">{{ optimizationEfficiency }}%</div>
                <div class="metric-label">80/20 Efficiency</div>
                <div class="metric-trend">⚡ {{ efficiencyGain }}% gain</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'ReactorStepNotifications',
  
  data() {
    return {
      globalNotificationLevel: 'critical',
      updateFrequency: 1000,
      autoDismiss: true,
      dismissTimeout: 10,
      soundAlerts: false,
      streamActive: true,
      
      pipelineSteps: [
        {
          name: 'k8s',
          icon: '☸️',
          active: true,
          status: 'Running',
          notificationsEnabled: true,
          totalNotifications: 24,
          criticalNotifications: 3,
          notificationCount: 5,
          optimized: true,
          notificationTypes: [
            { name: 'Deployment', icon: '🚀', count: 8, level: 'info' },
            { name: 'Resource', icon: '💾', count: 12, level: 'warning' },
            { name: 'Security', icon: '🔒', count: 3, level: 'critical' },
            { name: 'Network', icon: '🌐', count: 1, level: 'error' }
          ]
        },
        {
          name: 'reactor',
          icon: '⚡',
          active: true,
          status: 'Processing',
          notificationsEnabled: true,
          totalNotifications: 18,
          criticalNotifications: 2,
          notificationCount: 3,
          optimized: true,
          notificationTypes: [
            { name: 'Step Completion', icon: '✅', count: 10, level: 'info' },
            { name: 'Error Recovery', icon: '🔄', count: 5, level: 'warning' },
            { name: 'Performance', icon: '📊', count: 2, level: 'critical' },
            { name: 'Timeout', icon: '⏰', count: 1, level: 'error' }
          ]
        },
        {
          name: 'ash',
          icon: '🔥',
          active: true,
          status: 'Active',
          notificationsEnabled: true,
          totalNotifications: 15,
          criticalNotifications: 1,
          notificationCount: 2,
          optimized: true,
          notificationTypes: [
            { name: 'Resource Actions', icon: '🎯', count: 8, level: 'info' },
            { name: 'Validation', icon: '✔️', count: 4, level: 'warning' },
            { name: 'Authorization', icon: '🔐', count: 2, level: 'critical' },
            { name: 'Database', icon: '🗄️', count: 1, level: 'error' }
          ]
        },
        {
          name: 'erlang',
          icon: '🔧',
          active: false,
          status: 'Optimized Out',
          notificationsEnabled: false,
          totalNotifications: 8,
          criticalNotifications: 0,
          notificationCount: 0,
          optimized: true,
          notificationTypes: [
            { name: 'Process', icon: '⚙️', count: 5, level: 'info' },
            { name: 'Memory', icon: '💽', count: 2, level: 'warning' },
            { name: 'Supervisor', icon: '👥', count: 1, level: 'error' }
          ]
        },
        {
          name: 'bitactor',
          icon: '⚛️',
          active: false,
          status: 'Optimized Out',
          notificationsEnabled: false,
          totalNotifications: 6,
          criticalNotifications: 0,
          notificationCount: 0,
          optimized: true,
          notificationTypes: [
            { name: 'Actor State', icon: '🎭', count: 4, level: 'info' },
            { name: 'Message Queue', icon: '📬', count: 2, level: 'warning' }
          ]
        },
        {
          name: 'ttl2dspy',
          icon: '🧠',
          active: false,
          status: 'Optimized Out',
          notificationsEnabled: false,
          totalNotifications: 4,
          criticalNotifications: 0,
          notificationCount: 0,
          optimized: true,
          notificationTypes: [
            { name: 'Analysis', icon: '🔍', count: 3, level: 'info' },
            { name: 'Transform', icon: '🔄', count: 1, level: 'warning' }
          ]
        },
        {
          name: 'turtle',
          icon: '🐢',
          active: true,
          status: 'Monitoring',
          notificationsEnabled: true,
          totalNotifications: 12,
          criticalNotifications: 1,
          notificationCount: 1,
          optimized: true,
          notificationTypes: [
            { name: 'Parsing', icon: '📝', count: 7, level: 'info' },
            { name: 'Syntax', icon: '🔤', count: 3, level: 'warning' },
            { name: 'Validation', icon: '✅', count: 1, level: 'critical' },
            { name: 'Format', icon: '📄', count: 1, level: 'error' }
          ]
        },
        {
          name: 'typer',
          icon: '🎯',
          active: true,
          status: 'Input Ready',
          notificationsEnabled: true,
          totalNotifications: 9,
          criticalNotifications: 1,
          notificationCount: 1,
          optimized: true,
          notificationTypes: [
            { name: 'Type Check', icon: '🏷️', count: 5, level: 'info' },
            { name: 'Inference', icon: '🤔', count: 2, level: 'warning' },
            { name: 'Error', icon: '❌', count: 1, level: 'critical' },
            { name: 'Warning', icon: '⚠️', count: 1, level: 'warning' }
          ]
        }
      ],
      
      liveNotifications: [],
      notificationIdCounter: 1,
      
      selectedPermutationStrategy: 'critical_first',
      combinationRules: 'merge_similar',
      generatedPermutations: [],
      optimizedPermutations: 85,
      efficiencyGain: 42,
      
      // Analytics data
      totalNotificationsToday: 187,
      notificationTrend: 12,
      criticalNotificationRatio: 18,
      averageResponseTime: 145,
      responseTimeImprovement: 35,
      optimizationEfficiency: 88,
      paretoInsight: "20% of critical notifications account for 80% of system impact"
    }
  },
  
  mounted() {
    this.startNotificationStream()
    this.generatePermutations()
    this.drawParetoChart()
    this.setupWebSocketConnection()
  },
  
  beforeDestroy() {
    this.stopNotificationStream()
  },
  
  methods: {
    setupWebSocketConnection() {
      // Simulate WebSocket connection for real-time notifications
      console.log('Setting up WebSocket connection for reactor step notifications')
      this.simulateIncomingNotifications()
    },
    
    updateNotificationSettings() {
      console.log('Updating notification settings:', {
        level: this.globalNotificationLevel,
        frequency: this.updateFrequency,
        autoDismiss: this.autoDismiss,
        sound: this.soundAlerts
      })
      
      // Apply 80/20 optimization if critical level selected
      if (this.globalNotificationLevel === 'critical') {
        this.apply80_20Optimization()
      }
    },
    
    apply80_20Optimization() {
      // Enable only critical notifications for key steps
      const criticalSteps = ['k8s', 'reactor', 'ash', 'turtle', 'typer']
      
      this.pipelineSteps.forEach(step => {
        if (criticalSteps.includes(step.name)) {
          step.notificationsEnabled = true
          step.optimized = true
        } else {
          step.notificationsEnabled = false
          step.optimized = true
        }
      })
      
      // Filter live notifications to critical only
      this.liveNotifications = this.liveNotifications.filter(n => 
        n.level === 'critical' || n.level === 'error'
      )
    },
    
    toggleStepNotifications(step) {
      step.notificationsEnabled = !step.notificationsEnabled
      
      if (!step.notificationsEnabled) {
        // Remove notifications for this step
        this.liveNotifications = this.liveNotifications.filter(n => n.step !== step.name)
        step.notificationCount = 0
      }
    },
    
    toggleStream() {
      this.streamActive = !this.streamActive
      
      if (this.streamActive) {
        this.startNotificationStream()
      } else {
        this.stopNotificationStream()
      }
    },
    
    startNotificationStream() {
      this.notificationInterval = setInterval(() => {
        if (this.streamActive) {
          this.generateRandomNotification()
        }
      }, this.updateFrequency)
    },
    
    stopNotificationStream() {
      if (this.notificationInterval) {
        clearInterval(this.notificationInterval)
      }
    },
    
    generateRandomNotification() {
      const enabledSteps = this.pipelineSteps.filter(s => s.notificationsEnabled)
      if (enabledSteps.length === 0) return
      
      const randomStep = enabledSteps[Math.floor(Math.random() * enabledSteps.length)]
      const levels = ['info', 'warning', 'error', 'critical']
      const level = levels[Math.floor(Math.random() * levels.length)]
      
      // Apply 80/20 rule - 80% info/warning, 20% error/critical
      const finalLevel = Math.random() < 0.8 ? 
        (Math.random() < 0.6 ? 'info' : 'warning') :
        (Math.random() < 0.5 ? 'error' : 'critical')
      
      const notification = this.createNotification(randomStep.name, finalLevel)
      this.addNotification(notification)
    },
    
    createNotification(stepName, level) {
      const notifications = {
        k8s: {
          info: { title: 'Pod Scaled', message: 'Deployment scaled to 3 replicas' },
          warning: { title: 'Resource Limit', message: 'CPU usage at 85%' },
          error: { title: 'Pod Failed', message: 'Pod restart loop detected' },
          critical: { title: 'Cluster Issue', message: 'Node unavailable - immediate action required' }
        },
        reactor: {
          info: { title: 'Step Completed', message: 'Reactor step executed successfully' },
          warning: { title: 'Slow Execution', message: 'Step taking longer than expected' },
          error: { title: 'Step Failed', message: 'Reactor step execution failed' },
          critical: { title: 'Workflow Blocked', message: 'Critical step failure blocking pipeline' }
        },
        ash: {
          info: { title: 'Resource Created', message: 'New resource instance created' },
          warning: { title: 'Validation Warning', message: 'Data validation warning detected' },
          error: { title: 'Action Failed', message: 'Resource action execution failed' },
          critical: { title: 'Data Corruption', message: 'Critical data integrity issue detected' }
        },
        turtle: {
          info: { title: 'Parsing Complete', message: 'TTL document parsed successfully' },
          warning: { title: 'Syntax Warning', message: 'Deprecated syntax detected' },
          error: { title: 'Parse Error', message: 'Invalid TTL syntax encountered' },
          critical: { title: 'Format Corruption', message: 'Critical document format error' }
        },
        typer: {
          info: { title: 'Types Inferred', message: 'Type inference completed' },
          warning: { title: 'Type Mismatch', message: 'Potential type mismatch detected' },
          error: { title: 'Type Error', message: 'Type checking failed' },
          critical: { title: 'Type System Failure', message: 'Critical type system error' }
        }
      }
      
      const template = notifications[stepName]?.[level] || 
        { title: 'System Event', message: `${level} notification from ${stepName}` }
      
      const notification = {
        id: this.notificationIdCounter++,
        step: stepName,
        level: level,
        title: template.title,
        message: template.message,
        timestamp: new Date(),
        critical: level === 'critical',
        isNew: true,
        expanded: false,
        dismissing: false,
        pinned: false,
        metrics: {
          duration: Math.floor(Math.random() * 1000) + 100,
          memory: Math.floor(Math.random() * 50) + 10,
          cpu: Math.floor(Math.random() * 30) + 5
        },
        context: {
          correlationId: `corr-${Date.now()}`,
          sessionId: 'session-123',
          userId: 'system'
        },
        suggestions: level === 'critical' || level === 'error' ? [
          {
            id: 'suggestion-1',
            icon: '🔧',
            text: 'Apply 80/20 optimization to focus on critical issues',
            action: 'optimize'
          },
          {
            id: 'suggestion-2',
            icon: '🔄',
            text: 'Restart affected component',
            action: 'restart'
          }
        ] : []
      }
      
      if (level === 'info' && Math.random() < 0.3) {
        notification.progress = Math.floor(Math.random() * 100)
      }
      
      return notification
    },
    
    addNotification(notification) {
      // Update step notification count
      const step = this.pipelineSteps.find(s => s.name === notification.step)
      if (step) {
        step.notificationCount++
        if (notification.critical) {
          step.criticalNotifications++
        }
      }
      
      // Add to live stream
      this.liveNotifications.unshift(notification)
      
      // Auto-dismiss after timeout
      if (this.autoDismiss && !notification.critical) {
        setTimeout(() => {
          this.dismissNotification(notification)
        }, this.dismissTimeout * 1000)
      }
      
      // Remove new flag after animation
      setTimeout(() => {
        notification.isNew = false
      }, 500)
      
      // Limit notifications list
      if (this.liveNotifications.length > 50) {
        this.liveNotifications = this.liveNotifications.slice(0, 50)
      }
      
      // Play sound if enabled
      if (this.soundAlerts && notification.critical) {
        this.playNotificationSound()
      }
    },
    
    dismissNotification(notification) {
      notification.dismissing = true
      
      setTimeout(() => {
        const index = this.liveNotifications.findIndex(n => n.id === notification.id)
        if (index > -1) {
          this.liveNotifications.splice(index, 1)
        }
        
        // Update step count
        const step = this.pipelineSteps.find(s => s.name === notification.step)
        if (step && step.notificationCount > 0) {
          step.notificationCount--
          if (notification.critical && step.criticalNotifications > 0) {
            step.criticalNotifications--
          }
        }
      }, 300)
    },
    
    expandNotification(notification) {
      notification.expanded = !notification.expanded
    },
    
    pinNotification(notification) {
      notification.pinned = !notification.pinned
    },
    
    clearAllNotifications() {
      this.liveNotifications.forEach(n => n.dismissing = true)
      
      setTimeout(() => {
        this.liveNotifications = []
        this.pipelineSteps.forEach(step => {
          step.notificationCount = 0
          step.criticalNotifications = 0
        })
      }, 300)
    },
    
    exportNotifications() {
      const exportData = {
        timestamp: new Date().toISOString(),
        notifications: this.liveNotifications,
        settings: {
          level: this.globalNotificationLevel,
          frequency: this.updateFrequency,
          autoDismiss: this.autoDismiss
        },
        analytics: {
          totalToday: this.totalNotificationsToday,
          criticalRatio: this.criticalNotificationRatio,
          efficiency: this.optimizationEfficiency
        }
      }
      
      const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `reactor-notifications-${Date.now()}.json`
      a.click()
      URL.revokeObjectURL(url)
    },
    
    formatTimestamp(timestamp) {
      return new Date(timestamp).toLocaleTimeString()
    },
    
    getStepIcon(stepName) {
      const step = this.pipelineSteps.find(s => s.name === stepName)
      return step ? step.icon : '📦'
    },
    
    playNotificationSound() {
      // Simulate notification sound
      console.log('🔊 Playing notification sound')
    },
    
    simulateIncomingNotifications() {
      // Simulate some initial critical notifications
      setTimeout(() => {
        this.addNotification(this.createNotification('k8s', 'critical'))
      }, 2000)
      
      setTimeout(() => {
        this.addNotification(this.createNotification('reactor', 'error'))
      }, 5000)
      
      setTimeout(() => {
        this.addNotification(this.createNotification('ash', 'warning'))
      }, 8000)
    },
    
    viewStepDetails(step) {
      console.log(`Viewing details for step: ${step.name}`)
      // In real implementation, would open detailed view
    },
    
    optimizeStepNotifications(step) {
      step.optimized = !step.optimized
      
      if (step.optimized) {
        // Apply 80/20 optimization - keep only critical notifications
        const criticalTypes = step.notificationTypes.filter(t => 
          t.level === 'critical' || t.level === 'error'
        )
        
        step.notificationTypes = criticalTypes.length > 0 ? criticalTypes : [step.notificationTypes[0]]
        step.criticalNotifications = Math.max(1, step.criticalNotifications)
      }
    },
    
    muteStep(step) {
      step.notificationsEnabled = false
      this.liveNotifications = this.liveNotifications.filter(n => n.step !== step.name)
      step.notificationCount = 0
    },
    
    generatePermutations() {
      const strategies = {
        critical_first: this.generateCriticalFirstPermutations(),
        chronological: this.generateChronologicalPermutations(),
        step_priority: this.generateStepPriorityPermutations(),
        impact_based: this.generateImpactBasedPermutations(),
        adaptive: this.generateAdaptivePermutations()
      }
      
      this.generatedPermutations = strategies[this.selectedPermutationStrategy] || []
    },
    
    generateCriticalFirstPermutations() {
      return [
        {
          id: 'critical-80-20',
          name: '80/20 Critical Priority',
          description: 'Show only critical notifications from key steps (k8s, reactor, ash)',
          steps: ['k8s', 'reactor', 'ash'],
          score: 92,
          optimal: true,
          active: true
        },
        {
          id: 'error-focus',
          name: 'Error-First Display',
          description: 'Prioritize error and critical notifications across all steps',
          steps: ['k8s', 'reactor', 'ash', 'turtle', 'typer'],
          score: 85,
          optimal: false,
          active: false
        },
        {
          id: 'minimal-critical',
          name: 'Minimal Critical',
          description: 'Ultra-minimal view showing only system-critical notifications',
          steps: ['k8s', 'reactor'],
          score: 88,
          optimal: false,
          active: false
        }
      ]
    },
    
    generateChronologicalPermutations() {
      return [
        {
          id: 'time-ordered',
          name: 'Chronological Order',
          description: 'Display notifications in time order regardless of priority',
          steps: ['k8s', 'reactor', 'ash', 'erlang', 'bitactor', 'ttl2dspy', 'turtle', 'typer'],
          score: 65,
          optimal: false,
          active: false
        },
        {
          id: 'recent-first',
          name: 'Recent First',
          description: 'Show most recent notifications at top',
          steps: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
          score: 70,
          optimal: false,
          active: false
        }
      ]
    },
    
    generateStepPriorityPermutations() {
      return [
        {
          id: 'pipeline-order',
          name: 'Pipeline Step Priority',
          description: 'Follow reverse pipeline order for notification priority',
          steps: ['k8s', 'reactor', 'ash', 'erlang', 'bitactor', 'ttl2dspy', 'turtle', 'typer'],
          score: 78,
          optimal: false,
          active: false
        }
      ]
    },
    
    generateImpactBasedPermutations() {
      return [
        {
          id: 'high-impact',
          name: 'High Impact Priority',
          description: 'Prioritize notifications with highest system impact',
          steps: ['k8s', 'ash', 'reactor', 'turtle'],
          score: 84,
          optimal: false,
          active: false
        }
      ]
    },
    
    generateAdaptivePermutations() {
      return [
        {
          id: 'ai-adaptive',
          name: 'AI Adaptive Ordering',
          description: 'Machine learning based notification prioritization',
          steps: ['k8s', 'reactor', 'ash', 'turtle', 'typer'],
          score: 89,
          optimal: false,
          active: false
        }
      ]
    },
    
    applyPermutation(permutation) {
      // Deactivate all permutations
      this.generatedPermutations.forEach(p => p.active = false)
      
      // Activate selected permutation
      permutation.active = true
      
      // Apply the permutation logic
      this.pipelineSteps.forEach(step => {
        step.notificationsEnabled = permutation.steps.includes(step.name)
      })
      
      // Filter notifications based on permutation
      this.liveNotifications = this.liveNotifications.filter(n => 
        permutation.steps.includes(n.step)
      )
      
      console.log(`Applied permutation: ${permutation.name}`)
    },
    
    applySuggestion(suggestion) {
      console.log(`Applying suggestion: ${suggestion.text}`)
      
      if (suggestion.action === 'optimize') {
        this.apply80_20Optimization()
      } else if (suggestion.action === 'restart') {
        // Simulate component restart
        console.log('Restarting component...')
      }
    },
    
    drawParetoChart() {
      this.$nextTick(() => {
        const canvas = this.$refs.paretoChart
        if (!canvas) return
        
        const ctx = canvas.getContext('2d')
        const width = canvas.width
        const height = canvas.height
        
        // Clear canvas
        ctx.fillStyle = '#1a1a1a'
        ctx.fillRect(0, 0, width, height)
        
        // Sample data for Pareto chart
        const steps = ['k8s', 'reactor', 'ash', 'turtle', 'typer', 'erlang', 'bitactor', 'ttl2dspy']
        const values = [24, 18, 15, 12, 9, 8, 6, 4]
        const total = values.reduce((sum, val) => sum + val, 0)
        
        // Calculate cumulative percentages
        let cumulative = 0
        const cumulativePercentages = values.map(val => {
          cumulative += val
          return (cumulative / total) * 100
        })
        
        // Draw bars
        const barWidth = width / steps.length
        const maxBarHeight = height * 0.7
        
        steps.forEach((step, index) => {
          const barHeight = (values[index] / Math.max(...values)) * maxBarHeight
          const x = index * barWidth
          const y = height - barHeight
          
          // Color bars - red for top 20%, orange for next 30%, gray for rest
          let color = '#666'
          if (index < 2) color = '#ff4444'  // Top 20%
          else if (index < 5) color = '#ff8800'  // Next 30%
          
          ctx.fillStyle = color
          ctx.fillRect(x + 5, y, barWidth - 10, barHeight)
          
          // Draw step labels
          ctx.fillStyle = '#e0e0e0'
          ctx.font = '12px Arial'
          ctx.textAlign = 'center'
          ctx.fillText(step, x + barWidth / 2, height - 5)
          
          // Draw values
          ctx.fillStyle = '#fff'
          ctx.font = 'bold 10px Arial'
          ctx.fillText(values[index], x + barWidth / 2, y - 5)
        })
        
        // Draw 80% line
        ctx.strokeStyle = '#ff8800'
        ctx.lineWidth = 2
        ctx.setLineDash([5, 5])
        const eightyPercentY = height * 0.2
        ctx.beginPath()
        ctx.moveTo(0, eightyPercentY)
        ctx.lineTo(width, eightyPercentY)
        ctx.stroke()
        
        // Label the 80% line
        ctx.fillStyle = '#ff8800'
        ctx.font = 'bold 12px Arial'
        ctx.textAlign = 'left'
        ctx.fillText('80%', 5, eightyPercentY - 5)
      })
    }
  }
}
</script>

<style scoped>
.reactor-step-notifications {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.reactor-step-notifications h2 {
  margin-bottom: 1rem;
  font-size: 2rem;
}

.notification-description {
  margin-bottom: 2rem;
  font-size: 1.1rem;
  color: #00ff84;
  font-weight: 600;
}

.notification-control-panel {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.control-sections {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 2rem;
  margin-top: 1rem;
}

.control-section {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.control-section h4 {
  margin-bottom: 1rem;
  color: #888;
}

.control-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
}

.control-item {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.control-item label {
  font-size: 0.9rem;
  color: #aaa;
  font-weight: 600;
}

.control-item select,
.control-item input[type="range"] {
  padding: 0.5rem;
  background: #3a3a3a;
  border: 1px solid #555;
  border-radius: 6px;
  color: #e0e0e0;
}

.control-item input[type="checkbox"] {
  width: auto;
}

.frequency-display,
.checkbox-label {
  font-size: 0.8rem;
  color: #00ff84;
}

.step-filters {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
  gap: 0.5rem;
}

.step-filter {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 0.75rem;
  background: #3a3a3a;
  border-radius: 8px;
  cursor: pointer;
  transition: all 0.3s ease;
  position: relative;
}

.step-filter:hover {
  background: #4a4a4a;
}

.step-filter.active {
  background: #00ff84;
  color: #000;
}

.step-icon {
  font-size: 1.5rem;
  margin-bottom: 0.25rem;
}

.step-name {
  font-size: 0.8rem;
  font-weight: 600;
}

.notification-count {
  position: absolute;
  top: -8px;
  right: -8px;
  background: #ff4444;
  color: #fff;
  border-radius: 50%;
  width: 20px;
  height: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.7rem;
  font-weight: 700;
}

.live-notification-stream {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.stream-controls {
  display: flex;
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.stream-btn,
.clear-btn,
.export-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  border: none;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.stream-btn {
  background: #00ff84;
  color: #000;
}

.stream-btn.active {
  background: #ff8800;
  color: #fff;
}

.clear-btn {
  background: #ff4444;
  color: #fff;
}

.export-btn {
  background: #0084ff;
  color: #fff;
}

.notification-stream {
  max-height: 400px;
  overflow-y: auto;
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1rem;
}

.notification-item {
  background: #3a3a3a;
  border-radius: 8px;
  padding: 1rem;
  margin-bottom: 1rem;
  border-left: 4px solid #666;
  cursor: pointer;
  transition: all 0.3s ease;
}

.notification-item:last-child {
  margin-bottom: 0;
}

.notification-item.info {
  border-left-color: #0084ff;
}

.notification-item.warning {
  border-left-color: #ff8800;
}

.notification-item.error {
  border-left-color: #ff4444;
}

.notification-item.critical {
  border-left-color: #ff0000;
  background: #2a1a1a;
}

.notification-item.new {
  animation: slideIn 0.5s ease;
}

@keyframes slideIn {
  from {
    transform: translateX(-100%);
    opacity: 0;
  }
  to {
    transform: translateX(0);
    opacity: 1;
  }
}

.notification-item.dismissing {
  animation: slideOut 0.3s ease forwards;
}

@keyframes slideOut {
  to {
    transform: translateX(100%);
    opacity: 0;
  }
}

.notification-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
}

.notification-meta {
  display: flex;
  gap: 1rem;
  align-items: center;
}

.notification-step {
  font-weight: 600;
  color: #00ff84;
}

.notification-timestamp {
  font-size: 0.8rem;
  color: #888;
}

.notification-level-badge {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.7rem;
  font-weight: 700;
  text-transform: uppercase;
}

.notification-level-badge.info {
  background: #0084ff;
  color: #fff;
}

.notification-level-badge.warning {
  background: #ff8800;
  color: #fff;
}

.notification-level-badge.error {
  background: #ff4444;
  color: #fff;
}

.notification-level-badge.critical {
  background: #ff0000;
  color: #fff;
}

.notification-actions {
  display: flex;
  gap: 0.5rem;
}

.action-btn {
  padding: 0.25rem 0.5rem;
  background: #4a4a4a;
  border: none;
  border-radius: 4px;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.action-btn:hover {
  background: #5a5a5a;
}

.notification-content {
  margin-bottom: 0.5rem;
}

.notification-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.notification-message {
  color: #aaa;
  font-size: 0.9rem;
  margin-bottom: 0.5rem;
}

.notification-details {
  background: #2a2a2a;
  border-radius: 6px;
  padding: 1rem;
  margin-top: 0.5rem;
}

.detail-section {
  margin-bottom: 1rem;
}

.detail-section:last-child {
  margin-bottom: 0;
}

.detail-section h5 {
  margin-bottom: 0.5rem;
  color: #888;
}

.metrics-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
  gap: 0.5rem;
}

.metric {
  display: flex;
  justify-content: space-between;
  padding: 0.25rem;
  background: #3a3a3a;
  border-radius: 4px;
}

.metric-label {
  color: #888;
  font-size: 0.8rem;
}

.metric-value {
  color: #00ff84;
  font-weight: 600;
  font-size: 0.8rem;
}

.context-info pre {
  background: #1a1a1a;
  padding: 0.5rem;
  border-radius: 4px;
  font-size: 0.8rem;
  overflow-x: auto;
}

.suggestions-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.suggestion-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  background: #3a3a3a;
  border-radius: 4px;
}

.suggestion-icon {
  font-size: 1.2rem;
}

.suggestion-text {
  flex: 1;
  font-size: 0.9rem;
}

.apply-suggestion {
  padding: 0.25rem 0.5rem;
  background: #00ff84;
  color: #000;
  border: none;
  border-radius: 4px;
  font-size: 0.8rem;
  font-weight: 600;
  cursor: pointer;
}

.notification-progress {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-top: 0.5rem;
}

.progress-bar {
  flex: 1;
  height: 8px;
  background: #2a2a2a;
  border-radius: 4px;
  overflow: hidden;
}

.progress-fill {
  height: 100%;
  background: #00ff84;
  transition: all 0.3s ease;
}

.progress-text {
  font-size: 0.8rem;
  color: #888;
}

.no-notifications {
  text-align: center;
  padding: 2rem;
  color: #666;
}

.no-notifications-icon {
  font-size: 3rem;
  margin-bottom: 1rem;
}

.no-notifications-text {
  font-size: 1.2rem;
  font-weight: 600;
  margin-bottom: 0.5rem;
}

.no-notifications-subtext {
  font-size: 0.9rem;
}

.step-notification-dashboard {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.dashboard-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
  margin-top: 1rem;
}

.step-dashboard {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
  border-left: 4px solid #666;
  transition: all 0.3s ease;
}

.step-dashboard.active {
  border-left-color: #00ff84;
}

.step-dashboard.critical {
  border-left-color: #ff4444;
  background: #2a1a1a;
}

.step-dashboard.optimized {
  border-left-color: #ff8800;
}

.step-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.step-info {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.step-icon-large {
  font-size: 2rem;
}

.step-details {
  display: flex;
  flex-direction: column;
}

.step-name {
  font-weight: 600;
  font-size: 1.1rem;
}

.step-status {
  color: #888;
  font-size: 0.9rem;
}

.step-metrics {
  display: flex;
  gap: 1rem;
}

.metric-item {
  text-align: center;
}

.metric-count {
  display: block;
  font-size: 1.5rem;
  font-weight: 700;
  color: #00ff84;
}

.metric-item.critical .metric-count {
  color: #ff4444;
}

.metric-label {
  font-size: 0.8rem;
  color: #888;
}

.step-notification-types {
  margin-bottom: 1rem;
}

.notification-type {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
}

.type-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex: 1;
}

.type-icon {
  font-size: 1rem;
}

.type-name {
  font-size: 0.9rem;
  color: #aaa;
}

.type-count {
  font-weight: 600;
  color: #e0e0e0;
  margin-left: auto;
  margin-right: 0.5rem;
}

.type-progress {
  width: 60px;
}

.progress-bar.small {
  height: 4px;
}

.progress-fill.info {
  background: #0084ff;
}

.progress-fill.warning {
  background: #ff8800;
}

.progress-fill.critical {
  background: #ff4444;
}

.progress-fill.error {
  background: #ff0000;
}

.step-actions {
  display: flex;
  gap: 0.5rem;
}

.step-action-btn {
  padding: 0.5rem 1rem;
  border-radius: 6px;
  border: none;
  font-size: 0.8rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.step-action-btn.details {
  background: #0084ff;
  color: #fff;
}

.step-action-btn.optimize {
  background: #ff8800;
  color: #fff;
}

.step-action-btn.mute {
  background: #666;
  color: #fff;
}

.notification-permutations {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.permutation-controls {
  display: flex;
  gap: 1rem;
  align-items: end;
  margin-bottom: 1.5rem;
}

.permutation-input {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.permutation-input label {
  font-size: 0.9rem;
  color: #888;
  font-weight: 600;
}

.permutation-input select {
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.generate-btn {
  padding: 0.75rem 1.5rem;
  background: #00ff84;
  color: #000;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.generate-btn:hover {
  background: #00cc6a;
}

.permutation-results {
  display: grid;
  grid-template-columns: 300px 1fr;
  gap: 2rem;
}

.permutation-summary {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.summary-metric {
  display: flex;
  justify-content: space-between;
  margin-bottom: 1rem;
}

.summary-label {
  color: #888;
}

.summary-value {
  color: #00ff84;
  font-weight: 600;
}

.permutation-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.permutation-item {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1rem;
  border-left: 4px solid #666;
  transition: all 0.3s ease;
}

.permutation-item.optimal {
  border-left-color: #00ff84;
}

.permutation-item.active {
  background: #1a2a1a;
  border-left-color: #00ff84;
}

.permutation-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
}

.permutation-name {
  font-weight: 600;
}

.permutation-score {
  color: #00ff84;
  font-weight: 700;
}

.permutation-description {
  color: #aaa;
  font-size: 0.9rem;
  margin-bottom: 0.5rem;
}

.permutation-steps {
  display: flex;
  gap: 0.25rem;
  margin-bottom: 1rem;
}

.permutation-step {
  font-size: 1.2rem;
}

.apply-permutation {
  padding: 0.5rem 1rem;
  background: #0084ff;
  color: #fff;
  border: none;
  border-radius: 6px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-permutation:hover {
  background: #0066cc;
}

.notification-analytics {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.analytics-dashboard {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 2rem;
  margin-top: 1rem;
}

.analytics-chart {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.analytics-chart h4 {
  margin-bottom: 1rem;
  color: #888;
}

.chart-summary {
  margin-top: 1rem;
}

.chart-insight {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.insight-icon {
  font-size: 1.2rem;
}

.insight-text {
  color: #aaa;
  font-size: 0.9rem;
}

.analytics-metrics {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1.5rem;
}

.analytics-metrics h4 {
  margin-bottom: 1rem;
  color: #888;
}

.analytics-metric {
  display: flex;
  align-items: center;
  gap: 1rem;
  margin-bottom: 1rem;
  padding: 1rem;
  background: #3a3a3a;
  border-radius: 8px;
}

.metric-icon {
  font-size: 2rem;
}

.metric-content {
  flex: 1;
}

.metric-value {
  font-size: 1.5rem;
  font-weight: 700;
  color: #00ff84;
  margin-bottom: 0.25rem;
}

.metric-label {
  color: #888;
  font-size: 0.9rem;
  margin-bottom: 0.25rem;
}

.metric-trend {
  color: #aaa;
  font-size: 0.8rem;
}
</style>