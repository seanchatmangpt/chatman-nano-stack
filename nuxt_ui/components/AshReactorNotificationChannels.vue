<template>
  <div class="ash-reactor-notifications">
    <div class="notifications-header">
      <h2>🔥⚛️ Ash Reactor Step Notifications & Channels</h2>
      <p>Real-time pipeline notifications with 80/20 optimized delivery</p>
    </div>

    <!-- Channel Status Overview -->
    <div class="channel-status-grid">
      <div 
        v-for="channel in notificationChannels" 
        :key="channel.id"
        class="channel-card"
        :class="{ 
          active: channel.active, 
          critical: channel.priority === 'critical',
          optimized: channel.optimized 
        }"
      >
        <div class="channel-header">
          <span class="channel-icon">{{ channel.icon }}</span>
          <span class="channel-name">{{ channel.name }}</span>
          <span class="channel-status" :class="channel.status">{{ channel.status }}</span>
        </div>
        
        <div class="channel-metrics">
          <div class="metric">
            <span class="label">Messages/sec</span>
            <span class="value">{{ channel.messageRate }}</span>
          </div>
          <div class="metric">
            <span class="label">Latency</span>
            <span class="value">{{ channel.latency }}ms</span>
          </div>
          <div class="metric">
            <span class="label">Success Rate</span>
            <span class="value">{{ channel.successRate }}%</span>
          </div>
        </div>
        
        <div class="channel-pipeline-stages">
          <div 
            v-for="stage in channel.connectedStages" 
            :key="stage"
            class="pipeline-stage-indicator"
            :class="{ active: activePipelineStages.includes(stage) }"
          >
            {{ getPipelineStageIcon(stage) }}
          </div>
        </div>
      </div>
    </div>

    <!-- Live Notification Stream -->
    <div class="notification-stream">
      <div class="stream-header">
        <h3>📡 Live Notification Stream</h3>
        <div class="stream-controls">
          <button @click="toggleNotifications" class="control-btn" :class="{ active: notificationsEnabled }">
            {{ notificationsEnabled ? '🔇 Pause' : '🔊 Resume' }}
          </button>
          <button @click="clearNotifications" class="control-btn">
            🗑️ Clear
          </button>
          <select v-model="notificationFilter" class="filter-select">
            <option value="all">All Notifications</option>
            <option value="critical">Critical Only</option>
            <option value="ash">Ash Steps Only</option>
            <option value="reactor">Reactor Steps Only</option>
            <option value="pipeline">Pipeline Events</option>
          </select>
        </div>
      </div>
      
      <div class="notification-feed" ref="notificationFeed">
        <div 
          v-for="notification in filteredNotifications" 
          :key="notification.id"
          class="notification-item"
          :class="[notification.level, notification.type]"
        >
          <div class="notification-time">{{ formatTime(notification.timestamp) }}</div>
          <div class="notification-source">
            <span class="source-icon">{{ notification.sourceIcon }}</span>
            <span class="source-name">{{ notification.source }}</span>
          </div>
          <div class="notification-content">
            <div class="notification-title">{{ notification.title }}</div>
            <div class="notification-message">{{ notification.message }}</div>
            <div v-if="notification.data" class="notification-data">
              <pre>{{ JSON.stringify(notification.data, null, 2) }}</pre>
            </div>
          </div>
          <div class="notification-actions" v-if="notification.actions">
            <button 
              v-for="action in notification.actions" 
              :key="action.id"
              @click="executeNotificationAction(notification, action)"
              class="action-btn"
              :class="action.type"
            >
              {{ action.label }}
            </button>
          </div>
        </div>
      </div>
    </div>

    <!-- Ash Reactor Step Tracker -->
    <div class="ash-reactor-tracker">
      <h3>🔥 Ash Resources & ⚛️ Reactor Workflows</h3>
      
      <div class="tracker-tabs">
        <button 
          @click="activeTrackerTab = 'ash'"
          :class="['tab-btn', { active: activeTrackerTab === 'ash' }]"
        >
          🔥 Ash Resources
        </button>
        <button 
          @click="activeTrackerTab = 'reactor'"
          :class="['tab-btn', { active: activeTrackerTab === 'reactor' }]"
        >
          ⚛️ Reactor Workflows
        </button>
        <button 
          @click="activeTrackerTab = 'integration'"
          :class="['tab-btn', { active: activeTrackerTab === 'integration' }]"
        >
          🔗 Integration
        </button>
      </div>
      
      <!-- Ash Resources Tab -->
      <div v-if="activeTrackerTab === 'ash'" class="tracker-content">
        <div class="ash-resources-grid">
          <div 
            v-for="resource in ashResources" 
            :key="resource.id"
            class="resource-card"
            :class="{ active: resource.active, generating: resource.generating }"
          >
            <div class="resource-header">
              <span class="resource-name">{{ resource.name }}</span>
              <span class="resource-status" :class="resource.status">{{ resource.status }}</span>
            </div>
            
            <div class="resource-progress" v-if="resource.generating">
              <div class="progress-bar" :style="{ width: resource.progress + '%' }"></div>
              <span class="progress-text">{{ resource.currentStep }}</span>
            </div>
            
            <div class="resource-details">
              <div class="detail-item">
                <span class="label">Domain</span>
                <span class="value">{{ resource.domain }}</span>
              </div>
              <div class="detail-item">
                <span class="label">Attributes</span>
                <span class="value">{{ resource.attributeCount }}</span>
              </div>
              <div class="detail-item">
                <span class="label">Actions</span>
                <span class="value">{{ resource.actionCount }}</span>
              </div>
            </div>
            
            <div class="resource-notifications">
              <div 
                v-for="notif in resource.recentNotifications" 
                :key="notif.id"
                class="resource-notification"
                :class="notif.level"
              >
                {{ notif.message }}
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <!-- Reactor Workflows Tab -->
      <div v-if="activeTrackerTab === 'reactor'" class="tracker-content">
        <div class="reactor-workflows-grid">
          <div 
            v-for="workflow in reactorWorkflows" 
            :key="workflow.id"
            class="workflow-card"
            :class="{ active: workflow.active, executing: workflow.executing }"
          >
            <div class="workflow-header">
              <span class="workflow-name">{{ workflow.name }}</span>
              <span class="workflow-status" :class="workflow.status">{{ workflow.status }}</span>
            </div>
            
            <div class="workflow-steps">
              <div 
                v-for="step in workflow.steps" 
                :key="step.id"
                class="workflow-step"
                :class="{ 
                  active: step.active, 
                  completed: step.completed, 
                  failed: step.failed 
                }"
              >
                <div class="step-dot"></div>
                <div class="step-info">
                  <span class="step-name">{{ step.name }}</span>
                  <span class="step-duration">{{ step.duration || step.estimatedDuration }}ms</span>
                </div>
              </div>
            </div>
            
            <div class="workflow-notifications">
              <div 
                v-for="notif in workflow.recentNotifications" 
                :key="notif.id"
                class="workflow-notification"
                :class="notif.level"
              >
                {{ notif.message }}
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <!-- Integration Tab -->
      <div v-if="activeTrackerTab === 'integration'" class="tracker-content">
        <div class="integration-flow">
          <h4>🔄 Pipeline Integration Flow</h4>
          <div class="integration-stages">
            <div 
              v-for="(stage, index) in pipelineStages" 
              :key="stage.id"
              class="integration-stage"
              :class="{ 
                active: stage.active, 
                completed: stage.completed,
                critical: stage.critical 
              }"
            >
              <div class="stage-number">{{ index + 1 }}</div>
              <div class="stage-content">
                <div class="stage-icon">{{ stage.icon }}</div>
                <div class="stage-info">
                  <div class="stage-name">{{ stage.name }}</div>
                  <div class="stage-description">{{ stage.description }}</div>
                </div>
              </div>
              <div class="stage-notifications-count">
                {{ stage.notificationCount || 0 }}
              </div>
              
              <!-- Stage Connections -->
              <div v-if="index < pipelineStages.length - 1" class="stage-connection">
                <div class="connection-line" :class="{ active: stage.active }"></div>
                <div class="connection-notifications">
                  <div 
                    v-for="connNotif in stage.connectionNotifications" 
                    :key="connNotif.id"
                    class="connection-notification"
                    :class="connNotif.level"
                  >
                    {{ connNotif.message }}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Permutation Notification Matrix -->
    <div class="permutation-notification-matrix">
      <h3>🔄 Smart Notification Permutations</h3>
      
      <div class="matrix-controls">
        <div class="control-group">
          <label>Notification Strategy</label>
          <select v-model="notificationStrategy" @change="updateNotificationMatrix">
            <option value="all_stages">All Stages</option>
            <option value="critical_only">Critical Only (80/20)</option>
            <option value="adaptive">Adaptive Based on Load</option>
            <option value="minimal">Minimal Essential</option>
            <option value="user_defined">User Defined</option>
          </select>
        </div>
        
        <div class="control-group">
          <label>Delivery Priority</label>
          <select v-model="deliveryPriority" @change="updateNotificationMatrix">
            <option value="latency">Low Latency</option>
            <option value="reliability">High Reliability</option>
            <option value="bandwidth">Bandwidth Efficient</option>
            <option value="balanced">Balanced</option>
          </select>
        </div>
        
        <div class="control-group">
          <label>Swarm Optimization</label>
          <input 
            type="range" 
            v-model.number="swarmOptimizationLevel" 
            min="0" 
            max="10" 
            @input="updateNotificationMatrix"
          >
          <span class="value">{{ swarmOptimizationLevel }}/10</span>
        </div>
      </div>
      
      <div class="notification-permutations">
        <div 
          v-for="permutation in notificationPermutations" 
          :key="permutation.id"
          class="permutation-card"
          :class="{ optimal: permutation.isOptimal, active: permutation.active }"
          @click="activatePermutation(permutation)"
        >
          <div class="permutation-header">
            <span class="permutation-name">{{ permutation.name }}</span>
            <span class="efficiency-score">{{ permutation.efficiency }}%</span>
          </div>
          
          <div class="permutation-channels">
            <div 
              v-for="channel in permutation.channels" 
              :key="channel.id"
              class="permutation-channel"
              :class="{ enabled: channel.enabled }"
            >
              {{ channel.icon }}
            </div>
          </div>
          
          <div class="permutation-metrics">
            <span class="metric">{{ permutation.messageRate }}/s</span>
            <span class="metric">{{ permutation.avgLatency }}ms</span>
            <span class="metric">{{ permutation.reliabilityScore }}%</span>
          </div>
        </div>
      </div>
    </div>

    <!-- Advanced Channel Configuration -->
    <div class="advanced-channel-config">
      <h3>⚙️ Advanced Channel Configuration</h3>
      
      <div class="config-tabs">
        <button 
          @click="activeConfigTab = 'routing'"
          :class="['tab-btn', { active: activeConfigTab === 'routing' }]"
        >
          🛣️ Routing Rules
        </button>
        <button 
          @click="activeConfigTab = 'filters'"
          :class="['tab-btn', { active: activeConfigTab === 'filters' }]"
        >
          🔍 Message Filters
        </button>
        <button 
          @click="activeConfigTab = 'webhooks'"
          :class="['tab-btn', { active: activeConfigTab === 'webhooks' }]"
        >
          🪝 Webhooks
        </button>
      </div>
      
      <!-- Routing Rules -->
      <div v-if="activeConfigTab === 'routing'" class="config-content">
        <div class="routing-rules">
          <div 
            v-for="rule in routingRules" 
            :key="rule.id"
            class="routing-rule"
            :class="{ active: rule.active }"
          >
            <div class="rule-header">
              <span class="rule-name">{{ rule.name }}</span>
              <button @click="toggleRule(rule)" class="toggle-btn" :class="{ active: rule.active }">
                {{ rule.active ? 'Active' : 'Inactive' }}
              </button>
            </div>
            
            <div class="rule-config">
              <div class="rule-condition">
                <span class="label">Condition:</span>
                <span class="value">{{ rule.condition }}</span>
              </div>
              <div class="rule-action">
                <span class="label">Action:</span>
                <span class="value">{{ rule.action }}</span>
              </div>
              <div class="rule-channels">
                <span class="label">Channels:</span>
                <div class="channel-tags">
                  <span 
                    v-for="channelId in rule.targetChannels" 
                    :key="channelId"
                    class="channel-tag"
                  >
                    {{ getChannelName(channelId) }}
                  </span>
                </div>
              </div>
            </div>
          </div>
          
          <button @click="createRoutingRule" class="create-rule-btn">
            ➕ Create New Rule
          </button>
        </div>
      </div>
      
      <!-- Message Filters -->
      <div v-if="activeConfigTab === 'filters'" class="config-content">
        <div class="message-filters">
          <div 
            v-for="filter in messageFilters" 
            :key="filter.id"
            class="message-filter"
            :class="{ active: filter.active }"
          >
            <div class="filter-header">
              <span class="filter-name">{{ filter.name }}</span>
              <button @click="toggleFilter(filter)" class="toggle-btn" :class="{ active: filter.active }">
                {{ filter.active ? 'Active' : 'Inactive' }}
              </button>
            </div>
            
            <div class="filter-config">
              <div class="filter-criteria">
                <div class="criteria-item">
                  <span class="label">Source:</span>
                  <span class="value">{{ filter.sourcePattern || 'Any' }}</span>
                </div>
                <div class="criteria-item">
                  <span class="label">Level:</span>
                  <span class="value">{{ filter.levelFilter || 'Any' }}</span>
                </div>
                <div class="criteria-item">
                  <span class="label">Keywords:</span>
                  <span class="value">{{ filter.keywords ? filter.keywords.join(', ') : 'None' }}</span>
                </div>
              </div>
              
              <div class="filter-stats">
                <span class="stat">Matched: {{ filter.matchCount || 0 }}</span>
                <span class="stat">Filtered: {{ filter.filterCount || 0 }}</span>
              </div>
            </div>
          </div>
          
          <button @click="createMessageFilter" class="create-filter-btn">
            ➕ Create New Filter
          </button>
        </div>
      </div>
      
      <!-- Webhooks -->
      <div v-if="activeConfigTab === 'webhooks'" class="config-content">
        <div class="webhooks-config">
          <div 
            v-for="webhook in webhooks" 
            :key="webhook.id"
            class="webhook-item"
            :class="{ active: webhook.active, failed: webhook.lastStatus === 'failed' }"
          >
            <div class="webhook-header">
              <span class="webhook-name">{{ webhook.name }}</span>
              <span class="webhook-status" :class="webhook.lastStatus">{{ webhook.lastStatus }}</span>
            </div>
            
            <div class="webhook-config">
              <div class="webhook-url">
                <span class="label">URL:</span>
                <span class="value">{{ webhook.url }}</span>
              </div>
              <div class="webhook-method">
                <span class="label">Method:</span>
                <span class="value">{{ webhook.method }}</span>
              </div>
              <div class="webhook-events">
                <span class="label">Events:</span>
                <div class="event-tags">
                  <span 
                    v-for="event in webhook.events" 
                    :key="event"
                    class="event-tag"
                  >
                    {{ event }}
                  </span>
                </div>
              </div>
            </div>
            
            <div class="webhook-stats">
              <span class="stat">Calls: {{ webhook.callCount || 0 }}</span>
              <span class="stat">Success: {{ webhook.successRate || 0 }}%</span>
              <span class="stat">Last: {{ webhook.lastCall ? formatTime(webhook.lastCall) : 'Never' }}</span>
            </div>
          </div>
          
          <button @click="createWebhook" class="create-webhook-btn">
            ➕ Create New Webhook
          </button>
        </div>
      </div>
    </div>

    <!-- Real-time Analytics Dashboard -->
    <div class="analytics-dashboard">
      <h3>📊 Real-time Notification Analytics</h3>
      
      <div class="analytics-grid">
        <div class="analytics-card">
          <h4>📈 Message Volume</h4>
          <div class="volume-chart">
            <div 
              v-for="(volume, index) in messageVolumeHistory" 
              :key="index"
              class="volume-bar"
              :style="{ height: (volume / maxVolume * 100) + '%' }"
            ></div>
          </div>
          <div class="volume-stats">
            <span class="stat">Current: {{ currentMessageVolume }}/s</span>
            <span class="stat">Peak: {{ maxVolume }}/s</span>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>⚡ Latency Distribution</h4>
          <div class="latency-distribution">
            <div class="latency-bucket" v-for="bucket in latencyBuckets" :key="bucket.range">
              <div class="bucket-bar" :style="{ width: (bucket.count / maxLatencyCount * 100) + '%' }"></div>
              <span class="bucket-label">{{ bucket.range }}</span>
              <span class="bucket-count">{{ bucket.count }}</span>
            </div>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>🎯 Channel Efficiency</h4>
          <div class="efficiency-metrics">
            <div 
              v-for="channel in notificationChannels" 
              :key="channel.id"
              class="efficiency-item"
            >
              <span class="channel-name">{{ channel.name }}</span>
              <div class="efficiency-bar">
                <div 
                  class="efficiency-fill" 
                  :style="{ width: channel.efficiency + '%' }"
                  :class="{ 
                    high: channel.efficiency > 80, 
                    medium: channel.efficiency > 60 && channel.efficiency <= 80,
                    low: channel.efficiency <= 60 
                  }"
                ></div>
              </div>
              <span class="efficiency-value">{{ channel.efficiency }}%</span>
            </div>
          </div>
        </div>
        
        <div class="analytics-card">
          <h4>🚨 Error Rate Trends</h4>
          <div class="error-trends">
            <div 
              v-for="(rate, index) in errorRateHistory" 
              :key="index"
              class="error-bar"
              :style="{ height: (rate * 10) + 'px' }"
              :class="{ high: rate > 5, medium: rate > 2 && rate <= 5, low: rate <= 2 }"
            ></div>
          </div>
          <div class="error-stats">
            <span class="stat">Current: {{ currentErrorRate.toFixed(1) }}%</span>
            <span class="stat">Target: < 1%</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'AshReactorNotificationChannels',
  
  data() {
    return {
      // Notification system state
      notificationsEnabled: true,
      notificationFilter: 'all',
      notifications: [],
      
      // Tracker tabs
      activeTrackerTab: 'ash',
      activeConfigTab: 'routing',
      
      // Pipeline stages
      activePipelineStages: [],
      pipelineStages: [
        {
          id: 'typer',
          name: 'Typer',
          icon: '⌨️',
          description: 'Type definition and structure parsing',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'turtle',
          name: 'Turtle',
          icon: '🐢',
          description: 'TTL syntax parsing and validation',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'ttl2dspy',
          name: 'TTL2DSPy',
          icon: '🔄',
          description: 'DSPy agent generation from ontology',
          active: false,
          completed: false,
          critical: false,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'bitactor',
          name: 'BitActor',
          icon: '⚡',
          description: 'High-performance BitActor compilation',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'erlang',
          name: 'Erlang/OTP',
          icon: '🚀',
          description: 'Erlang supervision tree creation',
          active: false,
          completed: false,
          critical: false,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'ash',
          name: 'Ash',
          icon: '🔥',
          description: 'Ash framework resource generation',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'reactor',
          name: 'Reactor',
          icon: '⚛️',
          description: 'Reactor workflow definition creation',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        },
        {
          id: 'k8s',
          name: 'Kubernetes',
          icon: '☸️',
          description: 'K8s deployment manifest generation',
          active: false,
          completed: false,
          critical: true,
          notificationCount: 0,
          connectionNotifications: []
        }
      ],
      
      // Notification channels
      notificationChannels: [
        {
          id: 'ash_resources',
          name: 'Ash Resources',
          icon: '🔥',
          status: 'connected',
          active: true,
          priority: 'critical',
          optimized: true,
          messageRate: 45,
          latency: 12,
          successRate: 98.5,
          efficiency: 95,
          connectedStages: ['ash', 'reactor']
        },
        {
          id: 'reactor_workflows',
          name: 'Reactor Workflows',
          icon: '⚛️',
          status: 'connected',
          active: true,
          priority: 'critical',
          optimized: true,
          messageRate: 38,
          latency: 8,
          successRate: 99.2,
          efficiency: 97,
          connectedStages: ['reactor', 'k8s']
        },
        {
          id: 'pipeline_events',
          name: 'Pipeline Events',
          icon: '🔄',
          status: 'connected',
          active: true,
          priority: 'high',
          optimized: false,
          messageRate: 125,
          latency: 15,
          successRate: 96.8,
          efficiency: 88,
          connectedStages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
        },
        {
          id: 'error_alerts',
          name: 'Error Alerts',
          icon: '🚨',
          status: 'connected',
          active: true,
          priority: 'critical',
          optimized: true,
          messageRate: 8,
          latency: 3,
          successRate: 100,
          efficiency: 100,
          connectedStages: ['bitactor', 'ash', 'reactor']
        },
        {
          id: 'performance_metrics',
          name: 'Performance Metrics',
          icon: '📊',
          status: 'connected',
          active: true,
          priority: 'medium',
          optimized: true,
          messageRate: 85,
          latency: 25,
          successRate: 94.5,
          efficiency: 82,
          connectedStages: ['bitactor', 'erlang', 'k8s']
        },
        {
          id: 'swarm_intelligence',
          name: 'Swarm Intelligence',
          icon: '🧠',
          status: 'connected',
          active: true,
          priority: 'high',
          optimized: true,
          messageRate: 62,
          latency: 18,
          successRate: 97.3,
          efficiency: 91,
          connectedStages: ['ttl2dspy', 'ash', 'reactor']
        }
      ],
      
      // Ash resources
      ashResources: [
        {
          id: 'cyber_threat',
          name: 'CyberThreat',
          status: 'generating',
          active: true,
          generating: true,
          progress: 65,
          currentStep: 'Generating attributes',
          domain: 'Cybersecurity',
          attributeCount: 12,
          actionCount: 8,
          recentNotifications: [
            { id: 1, level: 'info', message: 'Attribute validation completed' },
            { id: 2, level: 'success', message: 'Relationships configured' }
          ]
        },
        {
          id: 'asset_monitor',
          name: 'AssetMonitor',
          status: 'completed',
          active: false,
          generating: false,
          progress: 100,
          currentStep: 'Complete',
          domain: 'Cybersecurity',
          attributeCount: 8,
          actionCount: 6,
          recentNotifications: [
            { id: 3, level: 'success', message: 'Resource generation completed' }
          ]
        },
        {
          id: 'security_control',
          name: 'SecurityControl',
          status: 'pending',
          active: false,
          generating: false,
          progress: 0,
          currentStep: 'Waiting for dependencies',
          domain: 'Cybersecurity',
          attributeCount: 0,
          actionCount: 0,
          recentNotifications: []
        }
      ],
      
      // Reactor workflows
      reactorWorkflows: [
        {
          id: 'threat_analysis',
          name: 'ThreatAnalysisWorkflow',
          status: 'executing',
          active: true,
          executing: true,
          steps: [
            { id: 'parse', name: 'Parse Input', active: false, completed: true, duration: 45 },
            { id: 'analyze', name: 'Analyze Threats', active: true, completed: false, estimatedDuration: 120 },
            { id: 'correlate', name: 'Correlate Data', active: false, completed: false, estimatedDuration: 80 },
            { id: 'respond', name: 'Generate Response', active: false, completed: false, estimatedDuration: 60 }
          ],
          recentNotifications: [
            { id: 4, level: 'info', message: 'Step 2 of 4 executing' },
            { id: 5, level: 'warning', message: 'High memory usage detected' }
          ]
        },
        {
          id: 'asset_discovery',
          name: 'AssetDiscoveryWorkflow',
          status: 'completed',
          active: false,
          executing: false,
          steps: [
            { id: 'scan', name: 'Network Scan', active: false, completed: true, duration: 230 },
            { id: 'identify', name: 'Asset Identification', active: false, completed: true, duration: 180 },
            { id: 'classify', name: 'Asset Classification', active: false, completed: true, duration: 95 },
            { id: 'update', name: 'Update Registry', active: false, completed: true, duration: 40 }
          ],
          recentNotifications: [
            { id: 6, level: 'success', message: 'Workflow completed successfully' }
          ]
        }
      ],
      
      // Notification configuration
      notificationStrategy: 'critical_only',
      deliveryPriority: 'latency',
      swarmOptimizationLevel: 8,
      
      // Notification permutations
      notificationPermutations: [],
      
      // Configuration
      routingRules: [
        {
          id: 'critical_errors',
          name: 'Critical Error Routing',
          active: true,
          condition: 'level === "critical" && source.includes("ash") || source.includes("reactor")',
          action: 'Route to error_alerts channel with high priority',
          targetChannels: ['error_alerts', 'swarm_intelligence']
        },
        {
          id: 'performance_alerts',
          name: 'Performance Alert Routing',
          active: true,
          condition: 'message.includes("performance") || message.includes("latency")',
          action: 'Route to performance metrics channel',
          targetChannels: ['performance_metrics']
        },
        {
          id: 'ash_events',
          name: 'Ash Resource Events',
          active: true,
          condition: 'source === "ash" && type === "resource_event"',
          action: 'Route to Ash resources channel',
          targetChannels: ['ash_resources']
        }
      ],
      
      messageFilters: [
        {
          id: 'debug_filter',
          name: 'Debug Message Filter',
          active: false,
          sourcePattern: '.*',
          levelFilter: 'debug',
          keywords: ['debug', 'trace'],
          matchCount: 1250,
          filterCount: 1150
        },
        {
          id: 'spam_filter',
          name: 'Repetitive Message Filter',
          active: true,
          sourcePattern: 'performance_metrics',
          levelFilter: 'info',
          keywords: ['heartbeat', 'ping'],
          matchCount: 3420,
          filterCount: 3200
        }
      ],
      
      webhooks: [
        {
          id: 'slack_alerts',
          name: 'Slack Error Alerts',
          url: 'https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK',
          method: 'POST',
          active: true,
          lastStatus: 'success',
          events: ['error', 'critical'],
          callCount: 156,
          successRate: 98.7,
          lastCall: Date.now() - 3600000
        },
        {
          id: 'discord_updates',
          name: 'Discord Pipeline Updates',
          url: 'https://discord.com/api/webhooks/YOUR/WEBHOOK',
          method: 'POST',
          active: true,
          lastStatus: 'success',
          events: ['pipeline_start', 'pipeline_complete'],
          callCount: 89,
          successRate: 100,
          lastCall: Date.now() - 1200000
        }
      ],
      
      // Analytics data
      messageVolumeHistory: Array.from({ length: 20 }, () => Math.floor(Math.random() * 150) + 50),
      maxVolume: 200,
      currentMessageVolume: 145,
      
      latencyBuckets: [
        { range: '0-10ms', count: 245 },
        { range: '10-50ms', count: 189 },
        { range: '50-100ms', count: 67 },
        { range: '100-500ms', count: 23 },
        { range: '500ms+', count: 8 }
      ],
      maxLatencyCount: 245,
      
      errorRateHistory: Array.from({ length: 20 }, () => Math.random() * 8),
      currentErrorRate: 1.2,
      
      // Intervals
      notificationInterval: null,
      metricsInterval: null,
      
      // WebSocket connection for real-time notifications
      ws: null,
      wsConnected: false,
      wsReconnectAttempts: 0,
      wsMaxReconnectAttempts: 5,
      wsReconnectDelay: 1000,
      
      // Real-time pipeline synchronization
      realTimePipelineState: {
        currentStage: null,
        activeStages: [],
        executionId: null,
        startTime: null,
        isExecuting: false
      },
      
      // Real-time notification queue
      notificationQueue: [],
      queueProcessingInterval: null,
      
      // Live metrics
      liveMetrics: {
        totalNotificationsSent: 0,
        averageLatency: 0,
        currentThroughput: 0,
        errorRate: 0,
        lastMetricsUpdate: null
      },
      
      // 80/20 Ultrathink Optimization
      ultrathin80_20Config: {
        enabled: true,
        criticalThreshold: 0.8, // 80% threshold for critical notifications
        noiseReductionLevel: 0.7, // Reduce 70% of low-value notifications
        adaptiveLearning: true,
        priorityWeights: {
          ash_resources: 0.9,     // High priority (20% that provides 80% value)
          reactor_workflows: 0.9,  // High priority
          error_alerts: 1.0,      // Critical priority
          pipeline_events: 0.6,   // Medium priority
          performance_metrics: 0.4, // Lower priority
          swarm_intelligence: 0.8  // High priority
        }
      },
      
      // Notification Intelligence
      notificationIntelligence: {
        patterns: new Map(), // Track notification patterns
        frequencies: new Map(), // Track frequency of notification types
        impacts: new Map(), // Track impact scores of different notifications
        learningHistory: [], // Historical learning data
        optimizationSuggestions: []
      },
      
      // Smart filtering
      smartFilters: {
        duplicateDetection: true,
        spamPrevention: true,
        contextualGrouping: true,
        temporalOptimization: true,
        valueBasedFiltering: true
      },
      
      // Pipeline Flow Notification Permutations
      pipelineFlowPermutations: [],
      activeFlowPermutation: null,
      flowPermutationStrategies: {
        sequential_flow: {
          name: 'Sequential Flow',
          description: 'Notifications follow strict pipeline stage order',
          channels: ['pipeline_events', 'ash_resources', 'reactor_workflows'],
          routing: 'sequential',
          optimization: 'latency'
        },
        parallel_optimization: {
          name: 'Parallel Optimization',
          description: 'Notifications from parallel stages grouped together',
          channels: ['ash_resources', 'reactor_workflows', 'swarm_intelligence'],
          routing: 'parallel',
          optimization: 'throughput'
        },
        critical_path: {
          name: 'Critical Path Focus',
          description: 'Only notifications from critical pipeline stages',
          channels: ['error_alerts', 'ash_resources', 'reactor_workflows'],
          routing: 'critical',
          optimization: 'priority'
        },
        adaptive_flow: {
          name: 'Adaptive Flow',
          description: 'Dynamic routing based on pipeline execution state',
          channels: ['swarm_intelligence', 'pipeline_events', 'ash_resources', 'reactor_workflows'],
          routing: 'adaptive',
          optimization: 'efficiency'
        },
        minimal_essential: {
          name: 'Minimal Essential',
          description: 'Only the most essential notifications per stage',
          channels: ['error_alerts', 'ash_resources'],
          routing: 'minimal',
          optimization: 'noise_reduction'
        }
      },
      
      // Flow-based notification combinations
      notificationCombinations: {
        current: [],
        patterns: new Map(),
        effectiveness: new Map(),
        learningData: []
      }
    }
  },
  
  computed: {
    filteredNotifications() {
      let filtered = this.notifications
      
      if (this.notificationFilter !== 'all') {
        filtered = filtered.filter(notif => {
          switch (this.notificationFilter) {
            case 'critical':
              return notif.level === 'critical'
            case 'ash':
              return notif.source.includes('ash') || notif.type === 'ash_event'
            case 'reactor':
              return notif.source.includes('reactor') || notif.type === 'reactor_event'
            case 'pipeline':
              return notif.type === 'pipeline_event'
            default:
              return true
          }
        })
      }
      
      return filtered.slice(0, 100) // Limit to last 100 notifications
    }
  },
  
  mounted() {
    this.generateNotificationPermutations()
    this.generatePipelineFlowPermutations()
    this.startNotificationSimulation()
    this.startMetricsUpdates()
    this.initializeWebSocketConnection()
    this.startNotificationQueueProcessor()
  },
  
  beforeDestroy() {
    if (this.notificationInterval) {
      clearInterval(this.notificationInterval)
    }
    if (this.metricsInterval) {
      clearInterval(this.metricsInterval)
    }
    if (this.queueProcessingInterval) {
      clearInterval(this.queueProcessingInterval)
    }
    this.disconnectWebSocket()
  },
  
  methods: {
    generateNotificationPermutations() {
      const strategies = [
        {
          name: 'Critical Only (80/20)',
          channels: ['error_alerts', 'ash_resources', 'reactor_workflows'],
          efficiency: 95,
          messageRate: 85,
          avgLatency: 8,
          reliabilityScore: 99
        },
        {
          name: 'All Channels',
          channels: this.notificationChannels.map(c => c.id),
          efficiency: 78,
          messageRate: 245,
          avgLatency: 18,
          reliabilityScore: 96
        },
        {
          name: 'Adaptive Smart',
          channels: ['ash_resources', 'reactor_workflows', 'swarm_intelligence', 'error_alerts'],
          efficiency: 92,
          messageRate: 120,
          avgLatency: 12,
          reliabilityScore: 98
        },
        {
          name: 'Performance Focused',
          channels: ['error_alerts', 'performance_metrics'],
          efficiency: 88,
          messageRate: 55,
          avgLatency: 5,
          reliabilityScore: 100
        },
        {
          name: 'Minimal Essential',
          channels: ['error_alerts', 'ash_resources'],
          efficiency: 97,
          messageRate: 45,
          avgLatency: 6,
          reliabilityScore: 99
        }
      ]
      
      this.notificationPermutations = strategies.map((strategy, index) => ({
        id: `perm_${index}`,
        ...strategy,
        channels: strategy.channels.map(channelId => {
          const channel = this.notificationChannels.find(c => c.id === channelId)
          return {
            id: channelId,
            icon: channel?.icon || '📡',
            enabled: true
          }
        }),
        isOptimal: strategy.efficiency > 90,
        active: index === 0 // First one is active by default
      }))
    },
    
    updateNotificationMatrix() {
      // Regenerate permutations based on current settings
      this.generateNotificationPermutations()
      
      // Apply optimization based on swarm level
      if (this.swarmOptimizationLevel > 7) {
        this.notificationPermutations.forEach(perm => {
          perm.efficiency = Math.min(perm.efficiency + 5, 100)
          perm.avgLatency = Math.max(perm.avgLatency - 2, 1)
        })
      }
    },
    
    activatePermutation(permutation) {
      // Deactivate all permutations
      this.notificationPermutations.forEach(p => p.active = false)
      
      // Activate selected permutation
      permutation.active = true
      
      // Update notification channels based on permutation
      this.notificationChannels.forEach(channel => {
        const isEnabled = permutation.channels.some(c => c.id === channel.id)
        channel.active = isEnabled
      })
      
      this.addNotification({
        source: 'notification_system',
        sourceIcon: '🔄',
        title: 'Permutation Activated',
        message: `Switched to "${permutation.name}" notification strategy`,
        level: 'info',
        type: 'system_event'
      })
    },
    
    startNotificationSimulation() {
      if (!this.notificationsEnabled) return
      
      this.notificationInterval = setInterval(() => {
        if (this.notificationsEnabled) {
          this.generateRandomNotification()
        }
      }, Math.random() * 3000 + 1000) // Random interval 1-4 seconds
    },
    
    generateRandomNotification() {
      const sources = [
        { name: 'ash_resources', icon: '🔥' },
        { name: 'reactor_workflows', icon: '⚛️' },
        { name: 'pipeline_events', icon: '🔄' },
        { name: 'error_alerts', icon: '🚨' },
        { name: 'performance_metrics', icon: '📊' },
        { name: 'swarm_intelligence', icon: '🧠' }
      ]
      
      const levels = ['info', 'warning', 'error', 'critical', 'success']
      const types = ['ash_event', 'reactor_event', 'pipeline_event', 'system_event']
      
      const source = sources[Math.floor(Math.random() * sources.length)]
      const level = levels[Math.floor(Math.random() * levels.length)]
      const type = types[Math.floor(Math.random() * types.length)]
      
      const messages = {
        ash_event: [
          'Resource generation completed',
          'Attribute validation started',
          'Action mapping configured',
          'Domain relationship established',
          'Resource compilation finished'
        ],
        reactor_event: [
          'Workflow step initiated',
          'Step execution completed',
          'Workflow paused for dependency',
          'Error recovery triggered',
          'Workflow optimization applied'
        ],
        pipeline_event: [
          'Stage transition initiated',
          'Pipeline optimization detected',
          'Performance threshold exceeded',
          'Critical path activated',
          'Swarm intelligence engaged'
        ],
        system_event: [
          'Channel connection established',
          'Notification filter applied',
          'Routing rule triggered',
          'Webhook delivery completed',
          'System health check passed'
        ]
      }
      
      const messageList = messages[type] || messages.system_event
      const message = messageList[Math.floor(Math.random() * messageList.length)]
      
      this.addNotification({
        source: source.name,
        sourceIcon: source.icon,
        title: this.generateNotificationTitle(type, level),
        message,
        level,
        type,
        data: this.generateNotificationData(type)
      })
    },
    
    generateNotificationTitle(type, level) {
      const titles = {
        ash_event: {
          info: 'Ash Resource Update',
          warning: 'Ash Resource Warning',
          error: 'Ash Resource Error',
          critical: 'Ash Resource Critical',
          success: 'Ash Resource Success'
        },
        reactor_event: {
          info: 'Reactor Workflow Update',
          warning: 'Reactor Workflow Warning',
          error: 'Reactor Workflow Error',
          critical: 'Reactor Workflow Critical',
          success: 'Reactor Workflow Success'
        },
        pipeline_event: {
          info: 'Pipeline Event',
          warning: 'Pipeline Warning',
          error: 'Pipeline Error',
          critical: 'Pipeline Critical',
          success: 'Pipeline Success'
        },
        system_event: {
          info: 'System Update',
          warning: 'System Warning',
          error: 'System Error',
          critical: 'System Critical',
          success: 'System Success'
        }
      }
      
      return titles[type]?.[level] || 'Notification'
    },
    
    generateNotificationData(type) {
      if (Math.random() > 0.3) return null // 30% chance of having data
      
      const dataTypes = {
        ash_event: {
          resource_id: `ash_${Math.floor(Math.random() * 1000)}`,
          attributes: Math.floor(Math.random() * 20) + 1,
          actions: Math.floor(Math.random() * 10) + 1
        },
        reactor_event: {
          workflow_id: `reactor_${Math.floor(Math.random() * 1000)}`,
          step: Math.floor(Math.random() * 5) + 1,
          duration: Math.floor(Math.random() * 500) + 50
        },
        pipeline_event: {
          stage: this.pipelineStages[Math.floor(Math.random() * this.pipelineStages.length)].id,
          optimization: Math.floor(Math.random() * 40) + 10
        },
        system_event: {
          component: 'notification_system',
          version: '1.0.0'
        }
      }
      
      return dataTypes[type] || null
    },
    
    addNotification(notification) {
      const newNotification = {
        id: `notif_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
        timestamp: new Date(),
        actions: this.generateNotificationActions(notification.level, notification.type),
        ...notification
      }
      
      this.notifications.unshift(newNotification)
      
      // Update related resources/workflows
      this.updateResourceNotifications(newNotification)
      
      // Limit notifications to prevent memory issues
      if (this.notifications.length > 1000) {
        this.notifications = this.notifications.slice(0, 1000)
      }
      
      // Auto-scroll notification feed
      this.$nextTick(() => {
        if (this.$refs.notificationFeed) {
          this.$refs.notificationFeed.scrollTop = 0
        }
      })
    },
    
    generateNotificationActions(level, type) {
      if (level === 'critical' || level === 'error') {
        return [
          { id: 'acknowledge', label: 'Acknowledge', type: 'primary' },
          { id: 'investigate', label: 'Investigate', type: 'secondary' },
          { id: 'escalate', label: 'Escalate', type: 'danger' }
        ]
      } else if (type === 'ash_event' || type === 'reactor_event') {
        return [
          { id: 'view_details', label: 'View Details', type: 'info' },
          { id: 'optimize', label: 'Optimize', type: 'success' }
        ]
      }
      
      return null
    },
    
    updateResourceNotifications(notification) {
      if (notification.type === 'ash_event') {
        // Add to Ash resources
        this.ashResources.forEach(resource => {
          if (resource.recentNotifications.length >= 3) {
            resource.recentNotifications.pop()
          }
          resource.recentNotifications.unshift({
            id: notification.id,
            level: notification.level,
            message: notification.message
          })
        })
      } else if (notification.type === 'reactor_event') {
        // Add to Reactor workflows
        this.reactorWorkflows.forEach(workflow => {
          if (workflow.recentNotifications.length >= 3) {
            workflow.recentNotifications.pop()
          }
          workflow.recentNotifications.unshift({
            id: notification.id,
            level: notification.level,
            message: notification.message
          })
        })
      }
    },
    
    executeNotificationAction(notification, action) {
      console.log('Executing action:', action.id, 'for notification:', notification.id)
      
      // Simulate action execution
      this.addNotification({
        source: 'notification_system',
        sourceIcon: '⚙️',
        title: 'Action Executed',
        message: `Action "${action.label}" executed for notification`,
        level: 'info',
        type: 'system_event'
      })
    },
    
    toggleNotifications() {
      this.notificationsEnabled = !this.notificationsEnabled
      
      if (this.notificationsEnabled) {
        this.startNotificationSimulation()
      } else if (this.notificationInterval) {
        clearInterval(this.notificationInterval)
      }
    },
    
    clearNotifications() {
      this.notifications = []
    },
    
    toggleRule(rule) {
      rule.active = !rule.active
    },
    
    toggleFilter(filter) {
      filter.active = !filter.active
    },
    
    createRoutingRule() {
      const newRule = {
        id: `rule_${Date.now()}`,
        name: 'New Routing Rule',
        active: false,
        condition: 'level === "info"',
        action: 'Route to default channel',
        targetChannels: ['pipeline_events']
      }
      
      this.routingRules.push(newRule)
    },
    
    createMessageFilter() {
      const newFilter = {
        id: `filter_${Date.now()}`,
        name: 'New Message Filter',
        active: false,
        sourcePattern: '.*',
        levelFilter: 'debug',
        keywords: [],
        matchCount: 0,
        filterCount: 0
      }
      
      this.messageFilters.push(newFilter)
    },
    
    createWebhook() {
      const newWebhook = {
        id: `webhook_${Date.now()}`,
        name: 'New Webhook',
        url: 'https://example.com/webhook',
        method: 'POST',
        active: false,
        lastStatus: 'pending',
        events: ['error'],
        callCount: 0,
        successRate: 0,
        lastCall: null
      }
      
      this.webhooks.push(newWebhook)
    },
    
    startMetricsUpdates() {
      this.metricsInterval = setInterval(() => {
        // Update message volume
        this.messageVolumeHistory.push(Math.floor(Math.random() * 150) + 50)
        this.messageVolumeHistory = this.messageVolumeHistory.slice(-20)
        this.currentMessageVolume = this.messageVolumeHistory[this.messageVolumeHistory.length - 1]
        
        // Update error rate
        this.errorRateHistory.push(Math.random() * 8)
        this.errorRateHistory = this.errorRateHistory.slice(-20)
        this.currentErrorRate = this.errorRateHistory[this.errorRateHistory.length - 1]
        
        // Update channel metrics
        this.notificationChannels.forEach(channel => {
          channel.messageRate += Math.floor(Math.random() * 10) - 5
          channel.messageRate = Math.max(0, channel.messageRate)
          
          channel.latency += Math.floor(Math.random() * 6) - 3
          channel.latency = Math.max(1, channel.latency)
          
          channel.successRate += (Math.random() - 0.5) * 2
          channel.successRate = Math.max(80, Math.min(100, channel.successRate))
        })
        
        // Calculate 80/20 efficiency for channels
        this.calculateChannelEfficiency()
        
        // Generate swarm intelligence recommendations
        const recommendations = this.generateSwarmIntelligenceRecommendations()
        if (recommendations.length > 0) {
          this.notificationIntelligence.optimizationSuggestions = recommendations
        }
        
        // Emit swarm optimization event with current metrics
        this.$emit('channel-optimized', {
          type: 'metrics_update',
          efficiency: this.notificationChannels.reduce((sum, c) => sum + c.efficiency, 0) / this.notificationChannels.length,
          latency: this.notificationChannels.reduce((sum, c) => sum + c.latency, 0) / this.notificationChannels.length,
          throughput: this.currentMessageVolume,
          optimization80_20: {
            enabled: this.ultrathin80_20Config.enabled,
            criticalThreshold: this.ultrathin80_20Config.criticalThreshold,
            noiseReduction: this.ultrathin80_20Config.noiseReductionLevel
          }
        })
      }, 5000)
    },
    
    getPipelineStageIcon(stage) {
      const stageData = this.pipelineStages.find(s => s.id === stage)
      return stageData?.icon || '🔧'
    },
    
    getChannelName(channelId) {
      const channel = this.notificationChannels.find(c => c.id === channelId)
      return channel?.name || channelId
    },
    
    formatTime(timestamp) {
      return new Date(timestamp).toLocaleTimeString()
    },
    
    // Real-time WebSocket methods
    initializeWebSocketConnection() {
      this.connectWebSocket()
    },
    
    connectWebSocket() {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        return // Already connected
      }
      
      try {
        // Connect to Phoenix WebSocket for real-time notifications
        this.ws = new WebSocket('ws://localhost:4000/socket/websocket')
        
        this.ws.onopen = () => {
          console.log('🔥⚛️ Ash Reactor Notifications WebSocket connected')
          this.wsConnected = true
          this.wsReconnectAttempts = 0
          
          // Join notification channels
          this.joinNotificationChannels()
          
          // Send initial status
          this.sendWebSocketMessage({
            topic: 'notifications:ash_reactor',
            event: 'component_ready',
            payload: {
              component: 'AshReactorNotificationChannels',
              timestamp: Date.now(),
              activeChannels: this.notificationChannels.filter(c => c.active).map(c => c.id)
            }
          })
          
          this.addNotification({
            source: 'websocket_system',
            sourceIcon: '🌐',
            title: 'Real-time Connection Established',
            message: 'Connected to Ash Reactor notification stream',
            level: 'success',
            type: 'system_event'
          })
        }
        
        this.ws.onmessage = (event) => {
          try {
            const message = JSON.parse(event.data)
            this.handleWebSocketMessage(message)
          } catch (error) {
            console.error('Failed to parse WebSocket message:', error)
          }
        }
        
        this.ws.onerror = (error) => {
          console.error('🔥⚛️ WebSocket error:', error)
          this.addNotification({
            source: 'websocket_system',
            sourceIcon: '🚨',
            title: 'WebSocket Connection Error',
            message: 'Real-time notification connection failed',
            level: 'error',
            type: 'system_event'
          })
        }
        
        this.ws.onclose = () => {
          console.log('🔥⚛️ WebSocket connection closed')
          this.wsConnected = false
          
          this.addNotification({
            source: 'websocket_system',
            sourceIcon: '⚠️',
            title: 'Real-time Connection Lost',
            message: 'Attempting to reconnect...',
            level: 'warning',
            type: 'system_event'
          })
          
          // Attempt reconnection
          this.attemptReconnection()
        }
      } catch (error) {
        console.error('Failed to create WebSocket connection:', error)
      }
    },
    
    joinNotificationChannels() {
      const channels = [
        'notifications:ash_reactor',
        'notifications:pipeline_events',
        'notifications:swarm_intelligence',
        'pipeline:execution',
        'ash:resources',
        'reactor:workflows'
      ]
      
      channels.forEach(channel => {
        this.sendWebSocketMessage({
          topic: channel,
          event: 'phx_join',
          payload: {},
          ref: Date.now()
        })
      })
    },
    
    sendWebSocketMessage(message) {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        this.ws.send(JSON.stringify(message))
        return true
      }
      return false
    },
    
    handleWebSocketMessage(message) {
      console.log('🔥⚛️ Received WebSocket message:', message)
      
      switch (message.event) {
        case 'ash_resource_event':
          this.handleAshResourceEvent(message.payload)
          break
        case 'reactor_workflow_event':
          this.handleReactorWorkflowEvent(message.payload)
          break
        case 'pipeline_stage_update':
          this.handlePipelineStageUpdate(message.payload)
          break
        case 'pipeline_execution_start':
          this.handlePipelineExecutionStart(message.payload)
          break
        case 'pipeline_execution_complete':
          this.handlePipelineExecutionComplete(message.payload)
          break
        case 'notification_broadcast':
          this.handleNotificationBroadcast(message.payload)
          break
        case 'swarm_optimization':
          this.handleSwarmOptimization(message.payload)
          break
        case 'telemetry_update':
          this.handleTelemetryUpdate(message.payload)
          break
        case 'phx_reply':
          // Handle Phoenix channel replies
          if (message.payload.status === 'ok') {
            console.log(`Successfully joined channel: ${message.topic}`)
          }
          break
        default:
          console.log('Unknown WebSocket event:', message.event)
      }
    },
    
    handleAshResourceEvent(payload) {
      const { resource_id, event_type, data } = payload
      
      // Update Ash resources state
      const resource = this.ashResources.find(r => r.id === resource_id)
      if (resource) {
        switch (event_type) {
          case 'generation_started':
            resource.status = 'generating'
            resource.generating = true
            resource.progress = 0
            resource.currentStep = data.step || 'Starting generation'
            break
          case 'generation_progress':
            resource.progress = data.progress || 0
            resource.currentStep = data.step || 'Processing'
            break
          case 'generation_complete':
            resource.status = 'completed'
            resource.generating = false
            resource.progress = 100
            resource.currentStep = 'Complete'
            resource.attributeCount = data.attributes || resource.attributeCount
            resource.actionCount = data.actions || resource.actionCount
            break
          case 'validation_error':
            resource.status = 'error'
            resource.generating = false
            resource.currentStep = 'Error: ' + (data.error || 'Validation failed')
            break
        }
      }
      
      // Add real-time notification with 80/20 optimization
      this.queueNotificationWithOptimization({
        source: 'ash_resources',
        sourceIcon: '🔥',
        title: `Ash Resource ${event_type.replace('_', ' ').replace(/\b\w/g, l => l.toUpperCase())}`,
        message: `${resource_id}: ${data.message || event_type}`,
        level: event_type.includes('error') ? 'error' : event_type.includes('complete') ? 'success' : 'info',
        type: 'ash_event',
        data: { resource_id, event_type, ...data }
      })
    },
    
    handleReactorWorkflowEvent(payload) {
      const { workflow_id, event_type, data } = payload
      
      // Update Reactor workflows state
      const workflow = this.reactorWorkflows.find(w => w.id === workflow_id)
      if (workflow) {
        switch (event_type) {
          case 'workflow_started':
            workflow.status = 'executing'
            workflow.executing = true
            workflow.steps.forEach(step => {
              step.status = 'pending'
              step.active = false
            })
            if (workflow.steps.length > 0) {
              workflow.steps[0].active = true
              workflow.steps[0].status = 'active'
            }
            break
          case 'step_started':
            const startStep = workflow.steps.find(s => s.id === data.step_id)
            if (startStep) {
              startStep.active = true
              startStep.status = 'active'
              startStep.startTime = Date.now()
            }
            break
          case 'step_completed':
            const completeStep = workflow.steps.find(s => s.id === data.step_id)
            if (completeStep) {
              completeStep.active = false
              completeStep.completed = true
              completeStep.status = 'completed'
              completeStep.duration = data.duration || 0
            }
            // Activate next step
            const currentIndex = workflow.steps.findIndex(s => s.id === data.step_id)
            if (currentIndex >= 0 && currentIndex < workflow.steps.length - 1) {
              const nextStep = workflow.steps[currentIndex + 1]
              nextStep.active = true
              nextStep.status = 'active'
            }
            break
          case 'workflow_complete':
            workflow.status = 'completed'
            workflow.executing = false
            workflow.steps.forEach(step => {
              step.active = false
              if (step.status === 'active') {
                step.completed = true
                step.status = 'completed'
              }
            })
            break
          case 'workflow_failed':
            workflow.status = 'failed'
            workflow.executing = false
            const failedStep = workflow.steps.find(s => s.active)
            if (failedStep) {
              failedStep.active = false
              failedStep.failed = true
              failedStep.status = 'failed'
            }
            break
        }
      }
      
      // Add real-time notification with 80/20 optimization
      this.queueNotificationWithOptimization({
        source: 'reactor_workflows',
        sourceIcon: '⚛️',
        title: `Reactor Workflow ${event_type.replace('_', ' ').replace(/\b\w/g, l => l.toUpperCase())}`,
        message: `${workflow_id}: ${data.message || event_type}`,
        level: event_type.includes('failed') || event_type.includes('error') ? 'error' : 
               event_type.includes('complete') ? 'success' : 'info',
        type: 'reactor_event',
        data: { workflow_id, event_type, ...data }
      })
    },
    
    handlePipelineStageUpdate(payload) {
      const { stage, status, timestamp } = payload
      
      // Update pipeline stages
      const pipelineStage = this.pipelineStages.find(s => s.id === stage)
      if (pipelineStage) {
        pipelineStage.active = status === 'active'
        pipelineStage.completed = status === 'completed'
        pipelineStage.notificationCount = (pipelineStage.notificationCount || 0) + 1
      }
      
      // Update active pipeline stages
      if (status === 'active') {
        if (!this.activePipelineStages.includes(stage)) {
          this.activePipelineStages.push(stage)
        }
      } else if (status === 'completed') {
        this.activePipelineStages = this.activePipelineStages.filter(s => s !== stage)
      }
      
      // Update real-time pipeline state
      this.realTimePipelineState.currentStage = status === 'active' ? stage : null
      this.realTimePipelineState.activeStages = [...this.activePipelineStages]
      
      // Add real-time notification with 80/20 optimization
      this.queueNotificationWithOptimization({
        source: 'pipeline_events',
        sourceIcon: '🔄',
        title: `Pipeline Stage ${status.replace(/\b\w/g, l => l.toUpperCase())}`,
        message: `Stage ${stage} is now ${status}`,
        level: status === 'completed' ? 'success' : status === 'failed' ? 'error' : 'info',
        type: 'pipeline_event',
        data: { stage, status, timestamp }
      })
    },
    
    handlePipelineExecutionStart(payload) {
      const { execution_id, stages, config } = payload
      
      this.realTimePipelineState.isExecuting = true
      this.realTimePipelineState.executionId = execution_id
      this.realTimePipelineState.startTime = Date.now()
      this.realTimePipelineState.activeStages = []
      
      // Reset all pipeline stages
      this.pipelineStages.forEach(stage => {
        stage.active = false
        stage.completed = false
        stage.notificationCount = 0
      })
      
      this.queueNotificationWithOptimization({
        source: 'pipeline_events',
        sourceIcon: '🚀',
        title: 'Pipeline Execution Started',
        message: `Execution ${execution_id} started with ${stages.length} stages`,
        level: 'info',
        type: 'pipeline_event',
        data: payload
      })
    },
    
    handlePipelineExecutionComplete(payload) {
      const { execution_id, success, duration, results } = payload
      
      this.realTimePipelineState.isExecuting = false
      this.realTimePipelineState.activeStages = []
      
      // Mark all stages as completed
      this.pipelineStages.forEach(stage => {
        stage.active = false
        stage.completed = true
      })
      
      this.queueNotificationWithOptimization({
        source: 'pipeline_events',
        sourceIcon: success ? '✅' : '❌',
        title: `Pipeline Execution ${success ? 'Completed' : 'Failed'}`,
        message: `Execution ${execution_id} ${success ? 'completed successfully' : 'failed'} in ${duration}ms`,
        level: success ? 'success' : 'error',
        type: 'pipeline_event',
        data: payload
      })
    },
    
    handleNotificationBroadcast(payload) {
      // Handle broadcasted notifications from other components with 80/20 optimization
      this.queueNotificationWithOptimization({
        ...payload,
        timestamp: Date.now()
      })
    },
    
    handleSwarmOptimization(payload) {
      const { optimization_type, efficiency_gain, recommendation } = payload
      
      // Update channel optimizations
      if (optimization_type === 'channel_optimization') {
        const channel = this.notificationChannels.find(c => c.id === payload.channel_id)
        if (channel) {
          channel.optimized = true
          channel.efficiency = Math.min(channel.efficiency + efficiency_gain, 100)
          channel.latency = Math.max(channel.latency - 2, 1)
        }
      }
      
      this.queueNotificationWithOptimization({
        source: 'swarm_intelligence',
        sourceIcon: '🧠',
        title: 'Swarm Optimization Applied',
        message: recommendation || `${optimization_type} optimization applied with ${efficiency_gain}% efficiency gain`,
        level: 'success',
        type: 'system_event',
        data: payload
      })
      
      // Emit optimization event to parent
      this.$emit('channel-optimized', {
        type: optimization_type,
        channel: payload.channel_id,
        strategy: 'swarm_intelligence',
        efficiencyGain: efficiency_gain,
        recommendation
      })
    },
    
    handleTelemetryUpdate(payload) {
      // Update live metrics
      this.liveMetrics = {
        ...this.liveMetrics,
        ...payload,
        lastMetricsUpdate: Date.now()
      }
      
      // Update channel metrics based on telemetry
      if (payload.channelMetrics) {
        payload.channelMetrics.forEach(metric => {
          const channel = this.notificationChannels.find(c => c.id === metric.channelId)
          if (channel) {
            channel.messageRate = metric.messageRate || channel.messageRate
            channel.latency = metric.latency || channel.latency
            channel.successRate = metric.successRate || channel.successRate
          }
        })
      }
    },
    
    queueNotification(notification) {
      // Add to notification queue for batch processing
      this.notificationQueue.push({
        ...notification,
        id: `ws_notif_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
        timestamp: new Date(),
        queueTime: Date.now()
      })
      
      // Limit queue size
      if (this.notificationQueue.length > 200) {
        this.notificationQueue = this.notificationQueue.slice(-100)
      }
    },
    
    startNotificationQueueProcessor() {
      this.queueProcessingInterval = setInterval(() => {
        if (this.notificationQueue.length > 0) {
          // Process notifications in batches
          const batchSize = Math.min(5, this.notificationQueue.length)
          const batch = this.notificationQueue.splice(0, batchSize)
          
          batch.forEach(notification => {
            // Calculate processing latency
            const processingLatency = Date.now() - notification.queueTime
            
            // Add notification with latency info
            this.addNotification({
              ...notification,
              processingLatency
            })
            
            // Update live metrics
            this.liveMetrics.totalNotificationsSent++
            this.liveMetrics.averageLatency = 
              (this.liveMetrics.averageLatency + processingLatency) / 2
            
            // Emit notification event to parent
            this.$emit('notification-sent', {
              type: notification.type,
              channel: notification.source,
              priority: notification.level,
              timestamp: notification.timestamp,
              processingLatency
            })
          })
        }
      }, 100) // Process queue every 100ms
    },
    
    attemptReconnection() {
      if (this.wsReconnectAttempts < this.wsMaxReconnectAttempts) {
        this.wsReconnectAttempts++
        const delay = this.wsReconnectDelay * Math.pow(2, this.wsReconnectAttempts - 1)
        
        setTimeout(() => {
          console.log(`🔥⚛️ Attempting WebSocket reconnection (${this.wsReconnectAttempts}/${this.wsMaxReconnectAttempts})`)
          this.connectWebSocket()
        }, delay)
      } else {
        console.error('🔥⚛️ Max WebSocket reconnection attempts reached')
        this.addNotification({
          source: 'websocket_system',
          sourceIcon: '🚨',
          title: 'Real-time Connection Failed',
          message: 'Unable to establish real-time notifications. Operating in offline mode.',
          level: 'error',
          type: 'system_event'
        })
      }
    },
    
    disconnectWebSocket() {
      if (this.ws) {
        this.ws.close()
        this.ws = null
      }
      this.wsConnected = false
    },
    
    // Public methods for external integration
    triggerSuccessNotifications(result) {
      if (result && result.efficiency > 0.7) {
        this.queueNotification({
          source: 'swarm_intelligence',
          sourceIcon: '🎯',
          title: 'High Efficiency Achieved',
          message: `Pipeline execution achieved ${(result.efficiency * 100).toFixed(1)}% efficiency`,
          level: 'success',
          type: 'system_event',
          data: result
        })
      }
    },
    
    applyOptimization(optimization) {
      // Apply optimization to notification channels
      if (optimization.channel) {
        const channel = this.notificationChannels.find(c => c.id === optimization.channel)
        if (channel) {
          channel.optimized = true
          channel.efficiency = Math.min(channel.efficiency + (optimization.efficiencyGain || 5), 100)
          
          this.queueNotification({
            source: 'notification_system',
            sourceIcon: '⚡',
            title: 'Channel Optimization Applied',
            message: `${channel.name} optimized with ${optimization.strategy} strategy`,
            level: 'success',
            type: 'system_event',
            data: optimization
          })
        }
      }
    },
    
    // 80/20 Ultrathink Optimization Methods
    calculateNotificationPriority(notification) {
      if (!this.ultrathin80_20Config.enabled) {
        return 0.5 // Default priority
      }
      
      let priority = 0
      
      // Base priority from channel weight
      const channelWeight = this.ultrathin80_20Config.priorityWeights[notification.source] || 0.5
      priority += channelWeight * 0.4
      
      // Level-based priority
      const levelPriority = {
        'critical': 1.0,
        'error': 0.9,
        'warning': 0.7,
        'success': 0.6,
        'info': 0.3
      }
      priority += (levelPriority[notification.level] || 0.5) * 0.3
      
      // Context-based priority (based on current pipeline state)
      if (this.realTimePipelineState.isExecuting) {
        if (notification.type === 'pipeline_event' || 
            notification.type === 'ash_event' || 
            notification.type === 'reactor_event') {
          priority += 0.2 // Boost pipeline-related notifications during execution
        }
      }
      
      // Frequency-based priority (prevent spam)
      const notificationKey = `${notification.source}:${notification.type}:${notification.title}`
      const frequency = this.notificationIntelligence.frequencies.get(notificationKey) || 0
      if (frequency > 5) {
        priority *= 0.7 // Reduce priority for frequent notifications
      }
      
      // Impact-based priority (learned from historical data)
      const impactScore = this.notificationIntelligence.impacts.get(notificationKey) || 0.5
      priority += impactScore * 0.1
      
      return Math.max(0, Math.min(1, priority))
    },
    
    applyUltraThink80_20Filter(notification) {
      const priority = this.calculateNotificationPriority(notification)
      
      // 80/20 Rule: Only show notifications above the critical threshold
      if (priority < this.ultrathin80_20Config.criticalThreshold) {
        // Apply noise reduction - only show a percentage of low-priority notifications
        const showProbability = 1 - this.ultrathin80_20Config.noiseReductionLevel
        return Math.random() < showProbability
      }
      
      return true // Always show high-priority notifications
    },
    
    analyzeNotificationPatterns(notification) {
      const notificationKey = `${notification.source}:${notification.type}:${notification.title}`
      
      // Update frequency tracking
      const currentFreq = this.notificationIntelligence.frequencies.get(notificationKey) || 0
      this.notificationIntelligence.frequencies.set(notificationKey, currentFreq + 1)
      
      // Track patterns
      const pattern = {
        source: notification.source,
        type: notification.type,
        level: notification.level,
        timestamp: Date.now(),
        pipelineStage: this.realTimePipelineState.currentStage,
        isExecuting: this.realTimePipelineState.isExecuting
      }
      
      if (!this.notificationIntelligence.patterns.has(notificationKey)) {
        this.notificationIntelligence.patterns.set(notificationKey, [])
      }
      
      const patterns = this.notificationIntelligence.patterns.get(notificationKey)
      patterns.push(pattern)
      
      // Keep only recent patterns (last 100)
      if (patterns.length > 100) {
        patterns.splice(0, patterns.length - 100)
      }
      
      // Analyze and generate insights
      this.generateOptimizationInsights()
    },
    
    generateOptimizationInsights() {
      const insights = []
      
      // Analyze notification frequency patterns
      const frequencySorted = Array.from(this.notificationIntelligence.frequencies.entries())
        .sort((a, b) => b[1] - a[1])
      
      // Identify spam/noise notifications (top 20% of frequency)
      const spamThreshold = Math.floor(frequencySorted.length * 0.2)
      const spamNotifications = frequencySorted.slice(0, spamThreshold)
      
      if (spamNotifications.length > 0) {
        insights.push({
          id: 'spam_detection',
          type: 'optimization',
          priority: 'medium',
          title: 'High-Frequency Notifications Detected',
          description: `${spamNotifications.length} notification types are generating high volume`,
          suggestion: 'Consider grouping or filtering these notifications',
          action: () => this.applySmartFiltering(spamNotifications)
        })
      }
      
      // Analyze pipeline correlation
      const pipelineNotifications = Array.from(this.notificationIntelligence.patterns.values())
        .flat()
        .filter(p => p.isExecuting)
      
      if (pipelineNotifications.length > 50) {
        const criticalSources = ['ash_resources', 'reactor_workflows', 'error_alerts']
        const criticalCount = pipelineNotifications.filter(p => 
          criticalSources.includes(p.source)).length
        const criticalRatio = criticalCount / pipelineNotifications.length
        
        if (criticalRatio < 0.3) {
          insights.push({
            id: 'pipeline_noise',
            type: 'optimization',
            priority: 'high',
            title: '80/20 Optimization Opportunity',
            description: `Only ${(criticalRatio * 100).toFixed(1)}% of pipeline notifications are critical`,
            suggestion: 'Apply more aggressive filtering during pipeline execution',
            action: () => this.optimizePipelineNotifications()
          })
        }
      }
      
      // Update optimization suggestions
      this.notificationIntelligence.optimizationSuggestions = insights
      
      // Auto-apply optimizations if adaptive learning is enabled
      if (this.ultrathin80_20Config.adaptiveLearning) {
        insights.forEach(insight => {
          if (insight.priority === 'high' && insight.action) {
            setTimeout(insight.action, 1000) // Apply with slight delay
          }
        })
      }
    },
    
    applySmartFiltering(spamNotifications) {
      // Group similar notifications
      spamNotifications.forEach(([notificationKey, frequency]) => {
        const [source, type, title] = notificationKey.split(':')
        
        // Reduce priority weight for spam sources
        if (this.ultrathin80_20Config.priorityWeights[source] > 0.3) {
          this.ultrathin80_20Config.priorityWeights[source] *= 0.8
        }
        
        // Add to smart filters
        this.smartFilters[`spam_${source}`] = {
          enabled: true,
          threshold: Math.min(frequency * 0.7, 10),
          grouping: true
        }
      })
      
      this.queueNotification({
        source: 'swarm_intelligence',
        sourceIcon: '🧠',
        title: '80/20 Smart Filtering Applied',
        message: `Reduced noise from ${spamNotifications.length} high-frequency notification types`,
        level: 'success',
        type: 'system_event'
      })
    },
    
    optimizePipelineNotifications() {
      // Increase critical threshold during pipeline execution
      const originalThreshold = this.ultrathin80_20Config.criticalThreshold
      this.ultrathin80_20Config.criticalThreshold = 0.9
      
      // Boost weights for critical pipeline notifications
      this.ultrathin80_20Config.priorityWeights.ash_resources = 1.0
      this.ultrathin80_20Config.priorityWeights.reactor_workflows = 1.0
      this.ultrathin80_20Config.priorityWeights.error_alerts = 1.0
      
      // Reduce weights for non-critical notifications
      this.ultrathin80_20Config.priorityWeights.pipeline_events = 0.3
      this.ultrathin80_20Config.priorityWeights.performance_metrics = 0.2
      
      this.queueNotification({
        source: 'swarm_intelligence',
        sourceIcon: '🎯',
        title: 'Pipeline 80/20 Optimization Active',
        message: 'Focusing on critical notifications during pipeline execution',
        level: 'success',
        type: 'system_event'
      })
      
      // Reset after pipeline execution completes
      setTimeout(() => {
        if (!this.realTimePipelineState.isExecuting) {
          this.ultrathin80_20Config.criticalThreshold = originalThreshold
          this.resetPriorityWeights()
        }
      }, 30000) // Reset after 30 seconds if not executing
    },
    
    resetPriorityWeights() {
      // Reset to default weights
      this.ultrathin80_20Config.priorityWeights = {
        ash_resources: 0.9,
        reactor_workflows: 0.9,
        error_alerts: 1.0,
        pipeline_events: 0.6,
        performance_metrics: 0.4,
        swarm_intelligence: 0.8
      }
    },
    
    calculateChannelEfficiency() {
      // Calculate 80/20 efficiency for each channel
      this.notificationChannels.forEach(channel => {
        const channelNotifications = Array.from(this.notificationIntelligence.patterns.entries())
          .filter(([key]) => key.startsWith(channel.id))
        
        if (channelNotifications.length > 0) {
          // Calculate value ratio (critical vs total notifications)
          const totalNotifications = channelNotifications.reduce((sum, [, patterns]) => 
            sum + patterns.length, 0)
          
          const criticalNotifications = channelNotifications.reduce((sum, [, patterns]) => 
            sum + patterns.filter(p => ['critical', 'error'].includes(p.level)).length, 0)
          
          const valueRatio = criticalNotifications / totalNotifications
          
          // 80/20 efficiency: High value ratio = high efficiency
          const efficiency80_20 = Math.min(valueRatio * 125, 100) // Scale to 100%
          
          // Update channel efficiency
          channel.efficiency = Math.round((channel.efficiency + efficiency80_20) / 2)
          
          // Mark as optimized if achieving good 80/20 ratio
          if (valueRatio > 0.2) { // 20% critical notifications is good ratio
            channel.optimized = true
          }
        }
      })
    },
    
    generateSwarmIntelligenceRecommendations() {
      const recommendations = []
      
      // Analyze current efficiency
      const totalNotifications = this.liveMetrics.totalNotificationsSent
      const criticalNotifications = Array.from(this.notificationIntelligence.patterns.values())
        .flat()
        .filter(p => ['critical', 'error'].includes(p.level)).length
      
      const criticalRatio = totalNotifications > 0 ? criticalNotifications / totalNotifications : 0
      
      if (criticalRatio < 0.2) {
        recommendations.push({
          id: 'increase_threshold',
          title: 'Increase Critical Threshold',
          description: `Only ${(criticalRatio * 100).toFixed(1)}% of notifications are critical`,
          impact: 'High',
          effort: 'Low',
          action: () => {
            this.ultrathin80_20Config.criticalThreshold = Math.min(
              this.ultrathin80_20Config.criticalThreshold + 0.1, 0.95)
          }
        })
      }
      
      if (this.liveMetrics.averageLatency > 50) {
        recommendations.push({
          id: 'reduce_noise',
          title: 'Increase Noise Reduction',
          description: `High latency detected (${this.liveMetrics.averageLatency}ms)`,
          impact: 'Medium',
          effort: 'Low',
          action: () => {
            this.ultrathin80_20Config.noiseReductionLevel = Math.min(
              this.ultrathin80_20Config.noiseReductionLevel + 0.1, 0.9)
          }
        })
      }
      
      return recommendations
    },
    
    // Enhanced queue notification with 80/20 filtering
    queueNotificationWithOptimization(notification) {
      // Analyze patterns first
      this.analyzeNotificationPatterns(notification)
      
      // Apply 80/20 filtering
      if (!this.applyUltraThink80_20Filter(notification)) {
        return // Notification filtered out by 80/20 optimization
      }
      
      // Apply smart filters
      if (this.smartFilters.duplicateDetection && this.isDuplicateNotification(notification)) {
        return // Duplicate filtered out
      }
      
      if (this.smartFilters.spamPrevention && this.isSpamNotification(notification)) {
        return // Spam filtered out
      }
      
      // Add optimized notification to queue
      this.queueNotification({
        ...notification,
        priority: this.calculateNotificationPriority(notification),
        filtered: false,
        optimized: true
      })
    },
    
    isDuplicateNotification(notification) {
      // Check last 5 notifications for duplicates
      const recentNotifications = this.notifications.slice(0, 5)
      return recentNotifications.some(recent => 
        recent.source === notification.source &&
        recent.type === notification.type &&
        recent.title === notification.title &&
        (Date.now() - new Date(recent.timestamp).getTime()) < 5000 // Within 5 seconds
      )
    },
    
    isSpamNotification(notification) {
      const notificationKey = `${notification.source}:${notification.type}:${notification.title}`
      const frequency = this.notificationIntelligence.frequencies.get(notificationKey) || 0
      
      // Consider spam if frequency > 10 in short time period
      return frequency > 10
    },
    
    // Pipeline Flow Notification Permutation Methods
    generatePipelineFlowPermutations() {
      this.pipelineFlowPermutations = []
      
      Object.entries(this.flowPermutationStrategies).forEach(([key, strategy]) => {
        const permutation = this.createFlowPermutation(key, strategy)
        this.pipelineFlowPermutations.push(permutation)
      })
      
      // Sort by effectiveness score
      this.pipelineFlowPermutations.sort((a, b) => b.effectivenessScore - a.effectivenessScore)
      
      // Set the most effective as active
      if (this.pipelineFlowPermutations.length > 0) {
        this.activeFlowPermutation = this.pipelineFlowPermutations[0]
        this.applyFlowPermutation(this.activeFlowPermutation)
      }
    },
    
    createFlowPermutation(strategyKey, strategy) {
      // Generate notification combinations based on pipeline stages
      const combinations = this.generateNotificationCombinationsForStrategy(strategy)
      
      // Calculate effectiveness score
      const effectivenessScore = this.calculateFlowEffectiveness(strategy, combinations)
      
      // Create routing rules for this permutation
      const routingRules = this.generateFlowRoutingRules(strategy, combinations)
      
      return {
        id: `flow_perm_${strategyKey}`,
        key: strategyKey,
        name: strategy.name,
        description: strategy.description,
        channels: strategy.channels,
        routing: strategy.routing,
        optimization: strategy.optimization,
        combinations,
        routingRules,
        effectivenessScore,
        active: false,
        metadata: {
          pipelineStagesCovered: this.getPipelineStagesCoverage(combinations),
          expectedLatency: this.calculateExpectedLatency(strategy),
          throughputEstimate: this.calculateThroughputEstimate(strategy),
          noiseReduction: this.calculateNoiseReduction(strategy)
        }
      }
    },
    
    generateNotificationCombinationsForStrategy(strategy) {
      const combinations = []
      
      // Generate combinations based on pipeline stages
      this.pipelineStages.forEach((stage, stageIndex) => {
        const stageNotifications = this.generateStageNotificationCombinations(stage, strategy)
        
        // Add sequential combinations (stage + next stage)
        if (strategy.routing === 'sequential' && stageIndex < this.pipelineStages.length - 1) {
          const nextStage = this.pipelineStages[stageIndex + 1]
          const sequentialCombo = this.createSequentialCombination(stage, nextStage, strategy)
          combinations.push(sequentialCombo)
        }
        
        // Add parallel combinations (concurrent stages)
        if (strategy.routing === 'parallel') {
          const parallelStages = this.getParallelStages(stage)
          if (parallelStages.length > 0) {
            const parallelCombo = this.createParallelCombination(stage, parallelStages, strategy)
            combinations.push(parallelCombo)
          }
        }
        
        // Add individual stage notifications
        combinations.push(...stageNotifications)
      })
      
      return combinations
    },
    
    generateStageNotificationCombinations(stage, strategy) {
      const combinations = []
      
      // Get notification types for this stage
      const stageNotificationTypes = this.getStageNotificationTypes(stage.id)
      
      // Generate combinations based on strategy
      switch (strategy.routing) {
        case 'critical':
          // Only critical notifications
          combinations.push({
            id: `critical_${stage.id}`,
            type: 'critical_only',
            stage: stage.id,
            channels: ['error_alerts'],
            notifications: stageNotificationTypes.filter(n => n.priority === 'critical'),
            weight: 1.0
          })
          break
          
        case 'sequential':
          // All notifications in order
          combinations.push({
            id: `sequential_${stage.id}`,
            type: 'sequential',
            stage: stage.id,
            channels: strategy.channels,
            notifications: stageNotificationTypes,
            weight: 0.8,
            order: stage.order || 0
          })
          break
          
        case 'parallel':
          // Grouped notifications for parallel execution
          combinations.push({
            id: `parallel_${stage.id}`,
            type: 'parallel',
            stage: stage.id,
            channels: strategy.channels,
            notifications: stageNotificationTypes,
            weight: 0.9,
            parallelGroup: this.getStageParallelGroup(stage.id)
          })
          break
          
        case 'adaptive':
          // Context-aware combinations
          const adaptiveWeight = this.calculateAdaptiveWeight(stage)
          combinations.push({
            id: `adaptive_${stage.id}`,
            type: 'adaptive',
            stage: stage.id,
            channels: this.selectAdaptiveChannels(stage, strategy),
            notifications: this.filterAdaptiveNotifications(stageNotificationTypes),
            weight: adaptiveWeight,
            context: this.getStageContext(stage)
          })
          break
          
        case 'minimal':
          // Only essential notifications
          const essentialNotifications = stageNotificationTypes.filter(n => 
            n.priority === 'critical' || n.type === 'error')
          if (essentialNotifications.length > 0) {
            combinations.push({
              id: `minimal_${stage.id}`,
              type: 'minimal',
              stage: stage.id,
              channels: ['error_alerts'],
              notifications: essentialNotifications,
              weight: 1.0
            })
          }
          break
      }
      
      return combinations
    },
    
    createSequentialCombination(currentStage, nextStage, strategy) {
      return {
        id: `seq_${currentStage.id}_${nextStage.id}`,
        type: 'sequential_transition',
        stages: [currentStage.id, nextStage.id],
        channels: strategy.channels,
        notifications: [
          ...this.getStageNotificationTypes(currentStage.id),
          ...this.getStageNotificationTypes(nextStage.id)
        ],
        weight: 0.85,
        transition: {
          from: currentStage.id,
          to: nextStage.id,
          trigger: 'stage_complete'
        }
      }
    },
    
    createParallelCombination(mainStage, parallelStages, strategy) {
      const allStages = [mainStage.id, ...parallelStages.map(s => s.id)]
      
      return {
        id: `parallel_${allStages.join('_')}`,
        type: 'parallel_execution',
        stages: allStages,
        channels: strategy.channels,
        notifications: allStages.flatMap(stageId => this.getStageNotificationTypes(stageId)),
        weight: 0.9,
        parallelGroup: {
          main: mainStage.id,
          parallel: parallelStages.map(s => s.id),
          synchronization: 'any_complete'
        }
      }
    },
    
    getStageNotificationTypes(stageId) {
      // Define notification types for each pipeline stage
      const stageNotifications = {
        typer: [
          { type: 'type_definition_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'type_validation_complete', priority: 'success', channels: ['pipeline_events'] },
          { type: 'type_error', priority: 'error', channels: ['error_alerts'] }
        ],
        turtle: [
          { type: 'ttl_parsing_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'ttl_validation_complete', priority: 'success', channels: ['pipeline_events'] },
          { type: 'ttl_syntax_error', priority: 'error', channels: ['error_alerts'] }
        ],
        ttl2dspy: [
          { type: 'dspy_generation_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'agent_creation_complete', priority: 'success', channels: ['swarm_intelligence'] },
          { type: 'dspy_generation_error', priority: 'error', channels: ['error_alerts'] }
        ],
        bitactor: [
          { type: 'bitactor_compilation_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'performance_optimization', priority: 'success', channels: ['performance_metrics'] },
          { type: 'compilation_error', priority: 'critical', channels: ['error_alerts'] }
        ],
        erlang: [
          { type: 'supervision_tree_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'process_spawned', priority: 'success', channels: ['performance_metrics'] },
          { type: 'supervisor_crash', priority: 'critical', channels: ['error_alerts'] }
        ],
        ash: [
          { type: 'resource_generation_start', priority: 'info', channels: ['ash_resources'] },
          { type: 'resource_complete', priority: 'success', channels: ['ash_resources'] },
          { type: 'resource_validation_error', priority: 'error', channels: ['error_alerts', 'ash_resources'] }
        ],
        reactor: [
          { type: 'workflow_definition_start', priority: 'info', channels: ['reactor_workflows'] },
          { type: 'workflow_step_complete', priority: 'success', channels: ['reactor_workflows'] },
          { type: 'workflow_execution_error', priority: 'critical', channels: ['error_alerts', 'reactor_workflows'] }
        ],
        k8s: [
          { type: 'manifest_generation_start', priority: 'info', channels: ['pipeline_events'] },
          { type: 'deployment_ready', priority: 'success', channels: ['pipeline_events'] },
          { type: 'deployment_failed', priority: 'critical', channels: ['error_alerts'] }
        ]
      }
      
      return stageNotifications[stageId] || []
    },
    
    getParallelStages(stage) {
      // Define which stages can run in parallel
      const parallelGroups = {
        ash: ['bitactor'],
        bitactor: ['ash'],
        ttl2dspy: ['erlang'],
        erlang: ['ttl2dspy']
      }
      
      const parallelStageIds = parallelGroups[stage.id] || []
      return this.pipelineStages.filter(s => parallelStageIds.includes(s.id))
    },
    
    getStageParallelGroup(stageId) {
      const parallelGroups = [
        ['ash', 'bitactor'],
        ['ttl2dspy', 'erlang']
      ]
      
      return parallelGroups.findIndex(group => group.includes(stageId))
    },
    
    calculateAdaptiveWeight(stage) {
      // Calculate weight based on current context
      let weight = 0.5
      
      // Boost weight if stage is currently active
      if (this.realTimePipelineState.activeStages.includes(stage.id)) {
        weight += 0.3
      }
      
      // Boost weight if stage is critical
      if (stage.critical) {
        weight += 0.2
      }
      
      // Adjust based on historical performance
      const stageEfficiency = this.getStageHistoricalEfficiency(stage.id)
      weight += (stageEfficiency - 0.5) * 0.2
      
      return Math.max(0.1, Math.min(1.0, weight))
    },
    
    selectAdaptiveChannels(stage, strategy) {
      // Dynamically select channels based on stage context
      let channels = [...strategy.channels]
      
      // Add error alerts for critical stages
      if (stage.critical && !channels.includes('error_alerts')) {
        channels.push('error_alerts')
      }
      
      // Add performance metrics for optimization-heavy stages
      if (['bitactor', 'erlang', 'k8s'].includes(stage.id)) {
        if (!channels.includes('performance_metrics')) {
          channels.push('performance_metrics')
        }
      }
      
      return channels
    },
    
    filterAdaptiveNotifications(notifications) {
      // Filter notifications based on current system state
      if (this.realTimePipelineState.isExecuting) {
        // During execution, focus on critical and success notifications
        return notifications.filter(n => 
          ['critical', 'error', 'success'].includes(n.priority))
      }
      
      return notifications
    },
    
    getStageContext(stage) {
      return {
        isActive: this.realTimePipelineState.activeStages.includes(stage.id),
        isCritical: stage.critical,
        isExecuting: this.realTimePipelineState.isExecuting,
        notificationCount: stage.notificationCount || 0,
        lastExecution: this.getStageLastExecution(stage.id)
      }
    },
    
    getStageLastExecution(stageId) {
      // Get last execution time for this stage
      const stagePatterns = Array.from(this.notificationIntelligence.patterns.entries())
        .filter(([key]) => key.includes(stageId))
        .map(([, patterns]) => patterns)
        .flat()
        .sort((a, b) => b.timestamp - a.timestamp)
      
      return stagePatterns.length > 0 ? stagePatterns[0].timestamp : null
    },
    
    getStageHistoricalEfficiency(stageId) {
      // Calculate historical efficiency for this stage
      const stageNotifications = Array.from(this.notificationIntelligence.patterns.entries())
        .filter(([key]) => key.includes(stageId))
        .map(([, patterns]) => patterns)
        .flat()
      
      if (stageNotifications.length === 0) return 0.5
      
      const successCount = stageNotifications.filter(n => 
        n.level === 'success').length
      const totalCount = stageNotifications.length
      
      return successCount / totalCount
    },
    
    calculateFlowEffectiveness(strategy, combinations) {
      let score = 0
      
      // Base score from strategy optimization type
      const optimizationScores = {
        latency: 0.8,
        throughput: 0.9,
        priority: 1.0,
        efficiency: 0.95,
        noise_reduction: 0.7
      }
      score += optimizationScores[strategy.optimization] || 0.5
      
      // Coverage score (how many pipeline stages are covered)
      const coverageRatio = this.getPipelineStagesCoverage(combinations) / this.pipelineStages.length
      score += coverageRatio * 0.2
      
      // Channel efficiency score
      const channelEfficiency = this.calculateChannelEfficiencyForStrategy(strategy)
      score += channelEfficiency * 0.15
      
      // Combination quality score
      const combinationQuality = this.assessCombinationQuality(combinations)
      score += combinationQuality * 0.15
      
      return Math.max(0, Math.min(1, score))
    },
    
    getPipelineStagesCoverage(combinations) {
      const coveredStages = new Set()
      combinations.forEach(combo => {
        if (combo.stage) {
          coveredStages.add(combo.stage)
        }
        if (combo.stages) {
          combo.stages.forEach(stage => coveredStages.add(stage))
        }
      })
      return coveredStages.size
    },
    
    calculateChannelEfficiencyForStrategy(strategy) {
      const strategyChannels = this.notificationChannels.filter(c => 
        strategy.channels.includes(c.id))
      
      if (strategyChannels.length === 0) return 0
      
      const avgEfficiency = strategyChannels.reduce((sum, c) => 
        sum + c.efficiency, 0) / strategyChannels.length
      
      return avgEfficiency / 100 // Convert to 0-1 scale
    },
    
    assessCombinationQuality(combinations) {
      if (combinations.length === 0) return 0
      
      // Quality based on weight distribution and variety
      const avgWeight = combinations.reduce((sum, c) => sum + c.weight, 0) / combinations.length
      const typeVariety = new Set(combinations.map(c => c.type)).size / 5 // Max 5 types
      
      return (avgWeight + typeVariety) / 2
    },
    
    generateFlowRoutingRules(strategy, combinations) {
      const rules = []
      
      combinations.forEach(combination => {
        const rule = {
          id: `rule_${combination.id}`,
          name: `${strategy.name} - ${combination.type}`,
          active: true,
          priority: this.getRoutingPriority(combination),
          conditions: this.generateRoutingConditions(combination),
          actions: this.generateRoutingActions(combination),
          metadata: {
            strategy: strategy.routing,
            optimization: strategy.optimization,
            weight: combination.weight
          }
        }
        rules.push(rule)
      })
      
      return rules
    },
    
    getRoutingPriority(combination) {
      const priorityMap = {
        critical_only: 'critical',
        sequential: 'medium',
        parallel: 'high',
        adaptive: 'high',
        minimal: 'critical'
      }
      return priorityMap[combination.type] || 'medium'
    },
    
    generateRoutingConditions(combination) {
      const conditions = {
        sources: combination.stages ? combination.stages : [combination.stage],
        types: combination.notifications ? combination.notifications.map(n => n.type) : [],
        levels: combination.notifications ? 
          [...new Set(combination.notifications.map(n => n.priority))] : [],
        weight: combination.weight
      }
      
      // Add specific conditions based on combination type
      switch (combination.type) {
        case 'sequential_transition':
          conditions.transitionTrigger = combination.transition.trigger
          conditions.fromStage = combination.transition.from
          conditions.toStage = combination.transition.to
          break
        case 'parallel_execution':
          conditions.parallelGroup = combination.parallelGroup
          conditions.synchronization = combination.parallelGroup.synchronization
          break
        case 'adaptive':
          conditions.context = combination.context
          break
      }
      
      return conditions
    },
    
    generateRoutingActions(combination) {
      return {
        route_to: combination.channels,
        priority_boost: combination.weight > 0.8,
        aggregation: combination.type === 'parallel_execution' ? 'group' : 'none',
        delay: combination.type === 'sequential_transition' ? 100 : 0,
        batching: combination.channels.length > 2
      }
    },
    
    applyFlowPermutation(permutation) {
      // Apply the selected flow permutation
      this.activeFlowPermutation = permutation
      
      // Update notification permutations display
      this.notificationPermutations.forEach(p => p.active = false)
      
      // Find and activate corresponding notification permutation
      const matchingPermutation = this.notificationPermutations.find(p => 
        p.name.toLowerCase().includes(permutation.optimization) ||
        p.name.toLowerCase().includes(permutation.routing))
      
      if (matchingPermutation) {
        matchingPermutation.active = true
      }
      
      // Update routing rules
      this.routingRules = [...this.routingRules, ...permutation.routingRules]
      
      // Update current notification combinations
      this.notificationCombinations.current = permutation.combinations
      
      // Emit permutation event
      this.$emit('permutation-executed', {
        id: permutation.id,
        name: permutation.name,
        strategy: permutation.routing,
        optimization: permutation.optimization,
        efficiency: permutation.effectivenessScore * 100,
        timing: {
          totalDuration: permutation.metadata.expectedLatency,
          optimizationSaving: (1 - permutation.metadata.noiseReduction) * 100
        },
        stagesCovered: permutation.metadata.pipelineStagesCovered,
        throughput: permutation.metadata.throughputEstimate
      })
      
      this.queueNotificationWithOptimization({
        source: 'swarm_intelligence',
        sourceIcon: '🔄',
        title: 'Pipeline Flow Permutation Applied',
        message: `Activated "${permutation.name}" with ${permutation.effectivenessScore * 100}% effectiveness`,
        level: 'success',
        type: 'system_event',
        data: {
          permutation: permutation.key,
          effectiveness: permutation.effectivenessScore,
          stagesCovered: permutation.metadata.pipelineStagesCovered
        }
      })
    },
    
    calculateExpectedLatency(strategy) {
      // Calculate expected latency based on strategy
      const baseDurations = {
        typer: 50, turtle: 30, ttl2dspy: 100, bitactor: 200,
        erlang: 100, ash: 150, reactor: 100, k8s: 70
      }
      
      const totalBaseDuration = Object.values(baseDurations).reduce((sum, d) => sum + d, 0)
      
      const optimizationFactors = {
        latency: 0.7,
        throughput: 0.8,
        priority: 0.9,
        efficiency: 0.75,
        noise_reduction: 0.85
      }
      
      return Math.round(totalBaseDuration * (optimizationFactors[strategy.optimization] || 1.0))
    },
    
    calculateThroughputEstimate(strategy) {
      // Estimate throughput based on strategy
      const baseThroughput = 100 // messages per second
      
      const throughputFactors = {
        latency: 1.3,
        throughput: 1.5,
        priority: 1.1,
        efficiency: 1.4,
        noise_reduction: 0.8
      }
      
      return Math.round(baseThroughput * (throughputFactors[strategy.optimization] || 1.0))
    },
    
    calculateNoiseReduction(strategy) {
      // Calculate noise reduction percentage
      const noiseReductionFactors = {
        latency: 0.6,
        throughput: 0.4,
        priority: 0.8,
        efficiency: 0.7,
        noise_reduction: 0.9
      }
      
      return noiseReductionFactors[strategy.optimization] || 0.5
    },
    
    // Public method to switch flow permutations
    switchFlowPermutation(permutationKey) {
      const permutation = this.pipelineFlowPermutations.find(p => p.key === permutationKey)
      if (permutation) {
        this.applyFlowPermutation(permutation)
      }
    },
    
    // Get current flow permutation status
    getFlowPermutationStatus() {
      return {
        active: this.activeFlowPermutation,
        available: this.pipelineFlowPermutations,
        combinations: this.notificationCombinations.current,
        effectiveness: this.activeFlowPermutation?.effectivenessScore || 0
      }
    }
  }
}
</script>

<style scoped>
.ash-reactor-notifications {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}

.notifications-header {
  text-align: center;
  margin-bottom: 3rem;
}

.notifications-header h2 {
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
  background: linear-gradient(45deg, #ff4444, #ff8800, #00ff88);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}

.notifications-header p {
  color: #888;
  font-size: 1.1rem;
}

/* Channel Status Grid */
.channel-status-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
  margin-bottom: 3rem;
}

.channel-card {
  background: #1a1a1a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  transition: all 0.3s ease;
}

.channel-card.active {
  border-color: #00ff88;
  background: #1a2a1a;
}

.channel-card.critical {
  border-left: 4px solid #ff4444;
}

.channel-card.optimized::after {
  content: '⚡';
  position: absolute;
  top: 1rem;
  right: 1rem;
  color: #00ff88;
}

.channel-header {
  display: flex;
  align-items: center;
  gap: 1rem;
  margin-bottom: 1rem;
}

.channel-icon {
  font-size: 1.5rem;
}

.channel-name {
  flex: 1;
  font-weight: 600;
}

.channel-status {
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.channel-status.connected {
  background: #00ff88;
  color: #000;
}

.channel-status.disconnected {
  background: #ff4444;
  color: #fff;
}

.channel-metrics {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
  margin-bottom: 1rem;
}

.channel-metrics .metric {
  text-align: center;
}

.channel-metrics .label {
  display: block;
  font-size: 0.8rem;
  color: #888;
  margin-bottom: 0.25rem;
}

.channel-metrics .value {
  display: block;
  font-weight: 600;
  color: #00ff88;
}

.channel-pipeline-stages {
  display: flex;
  gap: 0.5rem;
  justify-content: center;
}

.pipeline-stage-indicator {
  width: 24px;
  height: 24px;
  background: #333;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.8rem;
  transition: all 0.3s ease;
}

.pipeline-stage-indicator.active {
  background: #00ff88;
  color: #000;
}

/* Notification Stream */
.notification-stream {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.stream-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
}

.stream-header h3 {
  color: #0088ff;
}

.stream-controls {
  display: flex;
  gap: 1rem;
  align-items: center;
}

.control-btn {
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  padding: 0.5rem 1rem;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.control-btn:hover {
  background: #3a3a3a;
  border-color: #0088ff;
}

.control-btn.active {
  background: #0088ff;
  border-color: #0088ff;
  color: #fff;
}

.filter-select {
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  padding: 0.5rem;
  color: #e0e0e0;
}

.notification-feed {
  max-height: 500px;
  overflow-y: auto;
  background: #0a0a0a;
  border-radius: 8px;
  padding: 1rem;
}

.notification-item {
  display: grid;
  grid-template-columns: auto auto 1fr auto;
  gap: 1rem;
  padding: 1rem;
  border-bottom: 1px solid #333;
  border-left: 3px solid #333;
  transition: all 0.3s ease;
}

.notification-item:last-child {
  border-bottom: none;
}

.notification-item.critical {
  border-left-color: #ff4444;
  background: rgba(255, 68, 68, 0.1);
}

.notification-item.error {
  border-left-color: #ff8800;
  background: rgba(255, 136, 0, 0.1);
}

.notification-item.warning {
  border-left-color: #ffff00;
  background: rgba(255, 255, 0, 0.1);
}

.notification-item.success {
  border-left-color: #00ff88;
  background: rgba(0, 255, 136, 0.1);
}

.notification-item.info {
  border-left-color: #0088ff;
  background: rgba(0, 136, 255, 0.1);
}

.notification-time {
  color: #888;
  font-size: 0.8rem;
  white-space: nowrap;
}

.notification-source {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  white-space: nowrap;
}

.source-icon {
  font-size: 1.2rem;
}

.source-name {
  font-weight: 600;
  font-size: 0.9rem;
}

.notification-content {
  min-width: 0;
}

.notification-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.notification-message {
  color: #ccc;
  font-size: 0.9rem;
}

.notification-data {
  margin-top: 0.5rem;
  background: #333;
  border-radius: 4px;
  padding: 0.5rem;
  font-size: 0.8rem;
  overflow-x: auto;
}

.notification-data pre {
  margin: 0;
  color: #00ff88;
}

.notification-actions {
  display: flex;
  gap: 0.5rem;
  align-items: center;
}

.action-btn {
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 4px;
  padding: 0.25rem 0.75rem;
  color: #e0e0e0;
  cursor: pointer;
  font-size: 0.8rem;
  transition: all 0.3s ease;
}

.action-btn.primary {
  background: #0088ff;
  border-color: #0088ff;
  color: #fff;
}

.action-btn.secondary {
  background: #666;
  border-color: #666;
}

.action-btn.danger {
  background: #ff4444;
  border-color: #ff4444;
  color: #fff;
}

.action-btn.success {
  background: #00ff88;
  border-color: #00ff88;
  color: #000;
}

.action-btn:hover {
  transform: translateY(-1px);
  opacity: 0.9;
}

/* Ash Reactor Tracker */
.ash-reactor-tracker {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.ash-reactor-tracker h3 {
  margin-bottom: 1.5rem;
  color: #ff8800;
}

.tracker-tabs {
  display: flex;
  gap: 1rem;
  margin-bottom: 2rem;
}

.tab-btn {
  background: transparent;
  border: 2px solid #333;
  border-radius: 8px;
  padding: 0.75rem 1.5rem;
  color: #888;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.tab-btn:hover {
  border-color: #0088ff;
  color: #e0e0e0;
}

.tab-btn.active {
  background: #0088ff;
  border-color: #0088ff;
  color: #fff;
}

/* Ash Resources Grid */
.ash-resources-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
}

.resource-card {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  transition: all 0.3s ease;
}

.resource-card.active {
  border-color: #ff4444;
}

.resource-card.generating {
  border-color: #ff8800;
  background: rgba(255, 136, 0, 0.1);
}

.resource-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.resource-name {
  font-weight: 600;
  font-size: 1.1rem;
}

.resource-status {
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.resource-status.generating {
  background: #ff8800;
  color: #000;
}

.resource-status.completed {
  background: #00ff88;
  color: #000;
}

.resource-status.pending {
  background: #666;
  color: #fff;
}

.resource-progress {
  margin-bottom: 1rem;
  background: #333;
  border-radius: 4px;
  height: 6px;
  overflow: hidden;
  position: relative;
}

.progress-bar {
  height: 100%;
  background: #ff8800;
  transition: width 0.3s ease;
}

.progress-text {
  position: absolute;
  top: 10px;
  left: 0;
  font-size: 0.8rem;
  color: #888;
}

.resource-details {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
  margin-bottom: 1rem;
}

.detail-item {
  text-align: center;
}

.detail-item .label {
  display: block;
  font-size: 0.8rem;
  color: #888;
  margin-bottom: 0.25rem;
}

.detail-item .value {
  display: block;
  font-weight: 600;
  color: #e0e0e0;
}

.resource-notifications,
.workflow-notifications {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.resource-notification,
.workflow-notification {
  font-size: 0.8rem;
  padding: 0.5rem;
  border-radius: 4px;
  border-left: 3px solid #333;
}

.resource-notification.info,
.workflow-notification.info {
  border-left-color: #0088ff;
  background: rgba(0, 136, 255, 0.1);
}

.resource-notification.success,
.workflow-notification.success {
  border-left-color: #00ff88;
  background: rgba(0, 255, 136, 0.1);
}

.resource-notification.warning,
.workflow-notification.warning {
  border-left-color: #ff8800;
  background: rgba(255, 136, 0, 0.1);
}

/* Reactor Workflows Grid */
.reactor-workflows-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
  gap: 1.5rem;
}

.workflow-card {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  transition: all 0.3s ease;
}

.workflow-card.active {
  border-color: #0088ff;
}

.workflow-card.executing {
  border-color: #ff8800;
  background: rgba(255, 136, 0, 0.1);
}

.workflow-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.workflow-name {
  font-weight: 600;
  font-size: 1.1rem;
}

.workflow-status {
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.workflow-status.executing {
  background: #ff8800;
  color: #000;
}

.workflow-status.completed {
  background: #00ff88;
  color: #000;
}

.workflow-status.pending {
  background: #666;
  color: #fff;
}

.workflow-steps {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.workflow-step {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 0.5rem;
  border-radius: 6px;
  background: #1a1a1a;
}

.workflow-step.active {
  background: rgba(0, 136, 255, 0.2);
}

.workflow-step.completed {
  background: rgba(0, 255, 136, 0.2);
}

.workflow-step.failed {
  background: rgba(255, 68, 68, 0.2);
}

.step-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #666;
}

.workflow-step.active .step-dot {
  background: #0088ff;
}

.workflow-step.completed .step-dot {
  background: #00ff88;
}

.workflow-step.failed .step-dot {
  background: #ff4444;
}

.step-info {
  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.step-name {
  font-weight: 600;
}

.step-duration {
  font-size: 0.8rem;
  color: #888;
}

/* Integration Flow */
.integration-flow h4 {
  margin-bottom: 1.5rem;
  color: #0088ff;
}

.integration-stages {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.integration-stage {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #0a0a0a;
  border-radius: 8px;
  border: 2px solid #333;
  position: relative;
}

.integration-stage.active {
  border-color: #0088ff;
  background: rgba(0, 136, 255, 0.1);
}

.integration-stage.completed {
  border-color: #00ff88;
  background: rgba(0, 255, 136, 0.1);
}

.integration-stage.critical {
  border-left: 4px solid #ff4444;
}

.stage-number {
  width: 32px;
  height: 32px;
  background: #333;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 600;
}

.integration-stage.active .stage-number {
  background: #0088ff;
  color: #fff;
}

.integration-stage.completed .stage-number {
  background: #00ff88;
  color: #000;
}

.stage-content {
  flex: 1;
  display: flex;
  align-items: center;
  gap: 1rem;
}

.stage-icon {
  font-size: 1.5rem;
}

.stage-info {
  flex: 1;
}

.stage-name {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.stage-description {
  font-size: 0.9rem;
  color: #888;
}

.stage-notifications-count {
  background: #333;
  color: #fff;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
}

.stage-connection {
  position: absolute;
  left: 47px;
  top: 100%;
  width: 2px;
  height: 1rem;
  background: #333;
  z-index: 1;
}

.connection-line.active {
  background: #0088ff;
}

/* Permutation Notification Matrix */
.permutation-notification-matrix {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.permutation-notification-matrix h3 {
  margin-bottom: 1.5rem;
  color: #ff00ff;
}

.matrix-controls {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1.5rem;
  margin-bottom: 2rem;
}

.control-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.control-group label {
  font-weight: 600;
  color: #888;
}

.control-group select,
.control-group input[type="range"] {
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.control-group .value {
  color: #00ff88;
  font-weight: 600;
}

.notification-permutations {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
}

.permutation-card {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.permutation-card:hover {
  border-color: #0088ff;
  transform: translateY(-2px);
}

.permutation-card.optimal {
  border-color: #00ff88;
  background: rgba(0, 255, 136, 0.1);
}

.permutation-card.active {
  border-color: #ff00ff;
  background: rgba(255, 0, 255, 0.1);
}

.permutation-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.permutation-name {
  font-weight: 600;
}

.efficiency-score {
  background: #00ff88;
  color: #000;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-weight: 700;
  font-size: 0.9rem;
}

.permutation-channels {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.permutation-channel {
  width: 28px;
  height: 28px;
  background: #333;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.8rem;
}

.permutation-channel.enabled {
  background: #00ff88;
  color: #000;
}

.permutation-metrics {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
  color: #888;
}

/* Advanced Channel Configuration */
.advanced-channel-config {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.advanced-channel-config h3 {
  margin-bottom: 1.5rem;
  color: #0088ff;
}

.config-tabs {
  display: flex;
  gap: 1rem;
  margin-bottom: 2rem;
}

/* Routing Rules */
.routing-rules {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.routing-rule {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 8px;
  padding: 1.5rem;
}

.routing-rule.active {
  border-color: #00ff88;
}

.rule-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.rule-name {
  font-weight: 600;
}

.toggle-btn {
  background: #333;
  border: none;
  border-radius: 6px;
  padding: 0.5rem 1rem;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.toggle-btn.active {
  background: #00ff88;
  color: #000;
}

.rule-config {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.rule-condition,
.rule-action {
  display: flex;
  gap: 1rem;
}

.rule-condition .label,
.rule-action .label {
  font-weight: 600;
  color: #888;
  min-width: 80px;
}

.rule-condition .value,
.rule-action .value {
  font-family: 'Monaco', 'Menlo', monospace;
  background: #333;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  flex: 1;
}

.rule-channels {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.rule-channels .label {
  font-weight: 600;
  color: #888;
  min-width: 80px;
}

.channel-tags {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.channel-tag {
  background: #0088ff;
  color: #fff;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
}

.create-rule-btn,
.create-filter-btn,
.create-webhook-btn {
  background: #00ff88;
  color: #000;
  border: none;
  border-radius: 8px;
  padding: 1rem 2rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  margin-top: 1rem;
}

.create-rule-btn:hover,
.create-filter-btn:hover,
.create-webhook-btn:hover {
  background: #00cc70;
  transform: translateY(-2px);
}

/* Message Filters */
.message-filters {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.message-filter {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 8px;
  padding: 1.5rem;
}

.message-filter.active {
  border-color: #ff8800;
}

.filter-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.filter-name {
  font-weight: 600;
}

.filter-config {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.filter-criteria {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.criteria-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.criteria-item .label {
  font-weight: 600;
  color: #888;
}

.criteria-item .value {
  font-family: 'Monaco', 'Menlo', monospace;
  background: #333;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
}

.filter-stats {
  display: flex;
  gap: 2rem;
}

.filter-stats .stat {
  font-size: 0.8rem;
  color: #888;
}

/* Webhooks */
.webhooks-config {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.webhook-item {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 8px;
  padding: 1.5rem;
}

.webhook-item.active {
  border-color: #0088ff;
}

.webhook-item.failed {
  border-color: #ff4444;
}

.webhook-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.webhook-name {
  font-weight: 600;
}

.webhook-status {
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.webhook-status.success {
  background: #00ff88;
  color: #000;
}

.webhook-status.failed {
  background: #ff4444;
  color: #fff;
}

.webhook-status.pending {
  background: #666;
  color: #fff;
}

.webhook-config {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.webhook-url,
.webhook-method {
  display: flex;
  gap: 1rem;
}

.webhook-url .label,
.webhook-method .label {
  font-weight: 600;
  color: #888;
  min-width: 60px;
}

.webhook-url .value,
.webhook-method .value {
  font-family: 'Monaco', 'Menlo', monospace;
  background: #333;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  word-break: break-all;
}

.webhook-events {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.webhook-events .label {
  font-weight: 600;
  color: #888;
  min-width: 60px;
}

.event-tags {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.event-tag {
  background: #ff8800;
  color: #000;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
}

.webhook-stats {
  display: flex;
  gap: 2rem;
}

.webhook-stats .stat {
  font-size: 0.8rem;
  color: #888;
}

/* Analytics Dashboard */
.analytics-dashboard {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
}

.analytics-dashboard h3 {
  margin-bottom: 1.5rem;
  color: #00ff88;
}

.analytics-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
}

.analytics-card {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
}

.analytics-card h4 {
  margin-bottom: 1rem;
  color: #0088ff;
}

/* Volume Chart */
.volume-chart {
  display: flex;
  align-items: end;
  gap: 2px;
  height: 100px;
  margin-bottom: 1rem;
}

.volume-bar {
  flex: 1;
  background: #00ff88;
  min-height: 2px;
  border-radius: 2px 2px 0 0;
  transition: height 0.3s ease;
}

.volume-stats {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
  color: #888;
}

/* Latency Distribution */
.latency-distribution {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.latency-bucket {
  display: grid;
  grid-template-columns: 1fr auto auto;
  gap: 1rem;
  align-items: center;
}

.bucket-bar {
  height: 8px;
  background: #0088ff;
  border-radius: 4px;
  transition: width 0.3s ease;
}

.bucket-label {
  font-size: 0.8rem;
  color: #888;
  min-width: 60px;
}

.bucket-count {
  font-size: 0.8rem;
  font-weight: 600;
  color: #e0e0e0;
  min-width: 40px;
  text-align: right;
}

/* Channel Efficiency */
.efficiency-metrics {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.efficiency-item {
  display: grid;
  grid-template-columns: auto 1fr auto;
  gap: 1rem;
  align-items: center;
}

.channel-name {
  font-size: 0.8rem;
  font-weight: 600;
  min-width: 80px;
}

.efficiency-bar {
  height: 8px;
  background: #333;
  border-radius: 4px;
  overflow: hidden;
}

.efficiency-fill {
  height: 100%;
  border-radius: 4px;
  transition: width 0.3s ease;
}

.efficiency-fill.high {
  background: #00ff88;
}

.efficiency-fill.medium {
  background: #ff8800;
}

.efficiency-fill.low {
  background: #ff4444;
}

.efficiency-value {
  font-size: 0.8rem;
  font-weight: 600;
  min-width: 40px;
  text-align: right;
}

/* Error Rate Trends */
.error-trends {
  display: flex;
  align-items: end;
  gap: 2px;
  height: 80px;
  margin-bottom: 1rem;
}

.error-bar {
  flex: 1;
  min-height: 2px;
  border-radius: 2px 2px 0 0;
  transition: height 0.3s ease;
}

.error-bar.low {
  background: #00ff88;
}

.error-bar.medium {
  background: #ff8800;
}

.error-bar.high {
  background: #ff4444;
}

.error-stats {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
  color: #888;
}
</style>