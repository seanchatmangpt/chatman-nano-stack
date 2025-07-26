<template>
  <div class="swarm-pipeline-page">
    <!-- Header -->
    <header class="page-header">
      <h1>🔄 Ultrathink Swarm Pipeline Control Center</h1>
      <p>80/20 Optimization with Nuxt UI + Elixir Backend</p>
    </header>
    
    <!-- Tab Navigation -->
    <div class="tab-navigation">
      <button 
        v-for="tab in tabs" 
        :key="tab.id"
        @click="activeTab = tab.id"
        :class="['tab-btn', { active: activeTab === tab.id }]"
      >
        {{ tab.icon }} {{ tab.name }}
      </button>
    </div>
    
    <!-- Tab Content -->
    <div class="tab-content">
      <!-- Visualizer Tab -->
      <div v-if="activeTab === 'visualizer'" class="tab-panel">
        <SwarmPipelineVisualizer 
          ref="visualizer"
          @pipeline-executed="handlePipelineExecution"
        />
      </div>
      
      <!-- Permutation Matrix Tab -->
      <div v-if="activeTab === 'matrix'" class="tab-panel">
        <PermutationMatrix 
          @permutation-selected="handlePermutationSelection"
        />
      </div>
      
      <!-- Ash Reactor Notifications Tab -->
      <div v-if="activeTab === 'notifications'" class="tab-panel">
        <AshReactorNotificationChannels 
          ref="notificationChannels"
          @notification-sent="handleNotificationSent"
          @channel-optimized="handleChannelOptimized"
        />
      </div>
      
      <!-- Pipeline Connector Tab -->
      <div v-if="activeTab === 'connector'" class="tab-panel">
        <UltrathinkPipelineConnector 
          ref="pipelineConnector"
          @pipeline-connected="handlePipelineConnected"
          @permutation-executed="handlePermutationExecuted"
        />
      </div>
      
      <!-- Telemetry Dashboard Tab -->
      <div v-if="activeTab === 'telemetry'" class="tab-panel">
        <SwarmTelemetryDashboard 
          ref="telemetryDashboard"
        />
      </div>
      
      <!-- Live Execution Tab -->
      <div v-if="activeTab === 'execution'" class="tab-panel">
        <div class="execution-panel">
          <h2>🚀 Live Pipeline Execution</h2>
          
          <!-- Input Configuration -->
          <div class="input-config">
            <h3>Configure Input</h3>
            <div class="config-grid">
              <div class="config-item">
                <label>Domain</label>
                <select v-model="executionConfig.domain">
                  <option value="generic">Generic</option>
                  <option value="cybersecurity">Cybersecurity</option>
                  <option value="finance">Finance</option>
                  <option value="healthcare">Healthcare</option>
                </select>
              </div>
              
              <div class="config-item">
                <label>Entities</label>
                <input 
                  type="text" 
                  v-model="executionConfig.entities"
                  placeholder="Comma-separated entities"
                >
              </div>
              
              <div class="config-item">
                <label>Complexity</label>
                <input 
                  type="range" 
                  v-model.number="executionConfig.complexity"
                  min="0" 
                  max="1" 
                  step="0.1"
                >
                <span>{{ executionConfig.complexity }}</span>
              </div>
              
              <div class="config-item">
                <label>Priority</label>
                <select v-model="executionConfig.priority">
                  <option value="low">Low</option>
                  <option value="medium">Medium</option>
                  <option value="high">High</option>
                  <option value="critical">Critical</option>
                </select>
              </div>
            </div>
          </div>
          
          <!-- Execution Controls -->
          <div class="execution-controls">
            <button @click="executePipeline" class="execute-btn" :disabled="executing">
              {{ executing ? 'Executing...' : '🚀 Execute Pipeline' }}
            </button>
            
            <button @click="connectToBackend" class="connect-btn" :disabled="connected">
              {{ connected ? '✅ Connected' : '🔌 Connect to Backend' }}
            </button>
          </div>
          
          <!-- Execution Log -->
          <div class="execution-log">
            <h3>📜 Execution Log</h3>
            <div class="log-entries">
              <div 
                v-for="entry in executionLog" 
                :key="entry.id"
                class="log-entry"
                :class="entry.level"
              >
                <span class="timestamp">{{ formatTime(entry.timestamp) }}</span>
                <span class="message">{{ entry.message }}</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Global Stats -->
    <div class="global-stats">
      <div class="stat">
        <span class="label">Total Executions</span>
        <span class="value">{{ globalStats.totalExecutions }}</span>
      </div>
      <div class="stat">
        <span class="label">Avg Efficiency</span>
        <span class="value">{{ (globalStats.avgEfficiency * 100).toFixed(1) }}%</span>
      </div>
      <div class="stat">
        <span class="label">Best Strategy</span>
        <span class="value">{{ globalStats.bestStrategy }}</span>
      </div>
      <div class="stat">
        <span class="label">Swarm Optimizations</span>
        <span class="value">{{ globalStats.optimizations }}</span>
      </div>
    </div>
  </div>
</template>

<script>
// Import components
import SwarmPipelineVisualizer from '~/components/SwarmPipelineVisualizer.vue'
import PermutationMatrix from '~/components/PermutationMatrix.vue'
import SwarmTelemetryDashboard from '~/components/SwarmTelemetryDashboard.vue'
import AshReactorNotificationChannels from '~/components/AshReactorNotificationChannels.vue'
import UltrathinkPipelineConnector from '~/components/UltrathinkPipelineConnector.vue'

export default {
  name: 'SwarmPipelinePage',
  
  components: {
    SwarmPipelineVisualizer,
    PermutationMatrix,
    SwarmTelemetryDashboard,
    AshReactorNotificationChannels,
    UltrathinkPipelineConnector
  },
  
  data() {
    return {
      // Tab management
      activeTab: 'visualizer',
      tabs: [
        { id: 'visualizer', name: 'Pipeline Visualizer', icon: '🔄' },
        { id: 'matrix', name: 'Permutation Matrix', icon: '🔢' },
        { id: 'notifications', name: 'Ash Reactor Notifications', icon: '🔥⚛️' },
        { id: 'connector', name: 'Pipeline Connector', icon: '🌊' },
        { id: 'telemetry', name: 'Telemetry Dashboard', icon: '📡' },
        { id: 'execution', name: 'Live Execution', icon: '🚀' }
      ],
      
      // Execution configuration
      executionConfig: {
        domain: 'cybersecurity',
        entities: 'ThreatActor, Vulnerability, Attack, Defense, Incident',
        complexity: 0.7,
        priority: 'high'
      },
      
      // Execution state
      executing: false,
      connected: false,
      executionLog: [],
      
      // Global statistics
      globalStats: {
        totalExecutions: 0,
        avgEfficiency: 0.85,
        bestStrategy: 'Skip Optimization',
        optimizations: 0
      },
      
      // WebSocket connection
      ws: null
    }
  },
  
  mounted() {
    this.initializeGlobalStats()
  },
  
  beforeDestroy() {
    if (this.ws) {
      this.ws.close()
    }
  },
  
  methods: {
    initializeGlobalStats() {
      // Load from localStorage or initialize
      const saved = localStorage.getItem('swarmPipelineStats')
      if (saved) {
        this.globalStats = JSON.parse(saved)
      }
    },
    
    saveGlobalStats() {
      localStorage.setItem('swarmPipelineStats', JSON.stringify(this.globalStats))
    },
    
    handlePipelineExecution(result) {
      console.log('Pipeline executed:', result)
      
      // Update global stats
      this.globalStats.totalExecutions++
      this.globalStats.optimizations += result.optimizations || 0
      this.saveGlobalStats()
      
      // Log execution
      this.addLog('Pipeline execution completed', 'success')
    },
    
    handlePermutationSelection(permutation) {
      console.log('Permutation selected:', permutation)
      
      // Apply to visualizer
      if (this.$refs.visualizer) {
        this.$refs.visualizer.selectStrategy({
          id: permutation.strategy.toLowerCase().replace(/\s+/g, '_'),
          name: permutation.strategy,
          efficiency: permutation.efficiency
        })
      }
      
      this.addLog(`Selected permutation: ${permutation.strategy}`, 'info')
    },
    
    async executePipeline() {
      if (this.executing) return
      
      this.executing = true
      this.addLog('Starting pipeline execution...', 'info')
      
      try {
        // Prepare input data
        const input = {
          domain: this.executionConfig.domain,
          entities: this.executionConfig.entities.split(',').map(e => e.trim()),
          complexity: this.executionConfig.complexity,
          priority: this.executionConfig.priority
        }
        
        // If connected to backend, send via WebSocket
        if (this.connected && this.ws) {
          this.ws.send(JSON.stringify({
            type: 'execute_pipeline',
            data: input
          }))
        } else {
          // Otherwise, simulate execution
          await this.simulateExecution(input)
        }
        
        this.addLog('Pipeline execution completed successfully', 'success')
      } catch (error) {
        this.addLog(`Execution failed: ${error.message}`, 'error')
      } finally {
        this.executing = false
      }
    },
    
    async simulateExecution(input) {
      // Simulate pipeline stages
      const stages = ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
      
      for (const stage of stages) {
        this.addLog(`Executing stage: ${stage}`, 'info')
        await this.delay(Math.random() * 500 + 200)
      }
      
      // Update stats
      this.globalStats.totalExecutions++
      this.globalStats.optimizations++
      this.saveGlobalStats()
    },
    
    connectToBackend() {
      if (this.connected) return
      
      this.addLog('Connecting to Elixir backend...', 'info')
      
      try {
        // Connect to Phoenix WebSocket
        this.ws = new WebSocket('ws://localhost:4000/socket/websocket')
        
        this.ws.onopen = () => {
          this.connected = true
          this.addLog('Connected to backend successfully', 'success')
          
          // Join swarm channel
          this.ws.send(JSON.stringify({
            topic: 'swarm:pipeline',
            event: 'phx_join',
            payload: {},
            ref: Date.now()
          }))
        }
        
        this.ws.onmessage = (event) => {
          const message = JSON.parse(event.data)
          this.handleWebSocketMessage(message)
        }
        
        this.ws.onerror = (error) => {
          console.error('WebSocket error:', error)
          this.addLog('Connection error', 'error')
        }
        
        this.ws.onclose = () => {
          this.connected = false
          this.addLog('Disconnected from backend', 'warning')
        }
      } catch (error) {
        this.addLog(`Failed to connect: ${error.message}`, 'error')
      }
    },
    
    handleWebSocketMessage(message) {
      console.log('WebSocket message:', message)
      
      if (message.event === 'telemetry_update') {
        // Forward to telemetry dashboard
        if (this.$refs.telemetryDashboard) {
          this.$refs.telemetryDashboard.handleTelemetryEvent(message.payload)
        }
      } else if (message.event === 'execution_result') {
        // Handle execution result
        this.handlePipelineExecution(message.payload)
      }
    },
    
    addLog(message, level = 'info') {
      this.executionLog.unshift({
        id: Date.now(),
        timestamp: new Date(),
        message,
        level
      })
      
      // Keep only last 50 entries
      if (this.executionLog.length > 50) {
        this.executionLog = this.executionLog.slice(0, 50)
      }
    },
    
    formatTime(timestamp) {
      const date = new Date(timestamp)
      return date.toLocaleTimeString()
    },
    
    delay(ms) {
      return new Promise(resolve => setTimeout(resolve, ms))
    },
    
    // ASH REACTOR notification handlers
    handleNotificationSent(notification) {
      console.log('Notification sent:', notification)
      
      // Add to execution log
      this.addLog(`Notification sent: ${notification.type} -> ${notification.channel}`, 'info')
      
      // Update global stats
      this.globalStats.optimizations += notification.priority === 'critical' ? 2 : 1
      this.saveGlobalStats()
      
      // Forward to telemetry if connected
      if (this.connected && this.ws) {
        this.ws.send(JSON.stringify({
          type: 'notification_event',
          data: {
            type: notification.type,
            channel: notification.channel,
            priority: notification.priority,
            timestamp: Date.now()
          }
        }))
      }
    },
    
    handleChannelOptimized(optimization) {
      console.log('Channel optimized:', optimization)
      
      // Log optimization
      this.addLog(`Channel optimized: ${optimization.channel} - ${optimization.strategy}`, 'success')
      
      // Apply optimization to current execution if running
      if (this.executing && this.$refs.notificationChannels) {
        this.$refs.notificationChannels.applyOptimization(optimization)
      }
      
      // Update stats
      this.globalStats.optimizations += optimization.efficiencyGain || 1
      this.saveGlobalStats()
    },
    
    handlePipelineConnected(connection) {
      console.log('Pipeline connected:', connection)
      
      // Log connection
      this.addLog(`Pipeline connected: ${connection.stages.length} stages, ${connection.optimizations.length} optimizations`, 'success')
      
      // Sync with visualizer if available
      if (this.$refs.visualizer && connection.selectedPermutation) {
        this.$refs.visualizer.selectStrategy(connection.selectedPermutation)
      }
      
      // Update execution config based on connection
      if (connection.config) {
        Object.assign(this.executionConfig, connection.config)
      }
      
      // Update stats
      this.globalStats.optimizations += connection.optimizations.length
      this.saveGlobalStats()
    },
    
    handlePermutationExecuted(result) {
      console.log('Permutation executed:', result)
      
      // Log execution
      this.addLog(`Permutation executed: ${result.name} - ${result.efficiency}% efficiency`, 'success')
      
      // Update global stats with permutation results
      this.globalStats.totalExecutions++
      this.globalStats.avgEfficiency = (this.globalStats.avgEfficiency + result.efficiency) / 2
      
      if (result.efficiency > 0.85) {
        this.globalStats.bestStrategy = result.strategy
      }
      
      this.globalStats.optimizations += result.optimizationSaving || 0
      this.saveGlobalStats()
      
      // Forward execution result to telemetry
      if (this.$refs.telemetryDashboard) {
        this.$refs.telemetryDashboard.handleExecutionResult({
          permutation: result,
          timestamp: Date.now(),
          efficiency: result.efficiency,
          duration: result.timing?.totalDuration || 0
        })
      }
      
      // Trigger notification cascade for successful execution
      if (this.$refs.notificationChannels && result.efficiency > 0.7) {
        this.$refs.notificationChannels.triggerSuccessNotifications(result)
      }
    }
  }
}
</script>

<style scoped>
.swarm-pipeline-page {
  min-height: 100vh;
  background: #0a0a0a;
  color: #e0e0e0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}

.page-header {
  text-align: center;
  padding: 2rem;
  background: linear-gradient(135deg, #1a1a1a, #2a2a2a);
  border-bottom: 2px solid #333;
}

.page-header h1 {
  font-size: 3rem;
  margin-bottom: 0.5rem;
  background: linear-gradient(45deg, #00ff88, #0088ff, #ff00ff);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}

.page-header p {
  color: #888;
  font-size: 1.2rem;
}

.tab-navigation {
  display: flex;
  justify-content: center;
  gap: 1rem;
  padding: 1rem;
  background: #1a1a1a;
  border-bottom: 2px solid #333;
}

.tab-btn {
  padding: 0.75rem 1.5rem;
  background: transparent;
  border: 2px solid #333;
  border-radius: 8px;
  color: #888;
  font-size: 1rem;
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

.tab-content {
  min-height: 600px;
}

.tab-panel {
  animation: fadeIn 0.3s ease;
}

@keyframes fadeIn {
  from {
    opacity: 0;
    transform: translateY(10px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.execution-panel {
  padding: 2rem;
}

.execution-panel h2 {
  margin-bottom: 2rem;
}

.input-config {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 2rem;
}

.input-config h3 {
  margin-bottom: 1.5rem;
}

.config-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
}

.config-item {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.config-item label {
  font-weight: 600;
  color: #888;
}

.config-item input[type="text"],
.config-item select {
  padding: 0.75rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 8px;
  color: #e0e0e0;
  font-size: 1rem;
}

.config-item input[type="range"] {
  width: 100%;
}

.execution-controls {
  display: flex;
  justify-content: center;
  gap: 1rem;
  margin-bottom: 2rem;
}

.execute-btn, .connect-btn {
  padding: 1rem 2rem;
  border-radius: 8px;
  font-size: 1.1rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  border: none;
}

.execute-btn {
  background: #00ff88;
  color: #000;
}

.execute-btn:hover:not(:disabled) {
  background: #00cc70;
  transform: translateY(-2px);
}

.execute-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.connect-btn {
  background: #0088ff;
  color: #fff;
}

.connect-btn:hover:not(:disabled) {
  background: #0066cc;
  transform: translateY(-2px);
}

.connect-btn:disabled {
  background: #00cc70;
  cursor: default;
}

.execution-log {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
}

.execution-log h3 {
  margin-bottom: 1rem;
}

.log-entries {
  max-height: 400px;
  overflow-y: auto;
}

.log-entry {
  display: flex;
  gap: 1rem;
  padding: 0.75rem;
  border-bottom: 1px solid #333;
  font-size: 0.9rem;
}

.log-entry:last-child {
  border-bottom: none;
}

.log-entry .timestamp {
  color: #888;
  white-space: nowrap;
}

.log-entry.success {
  border-left: 3px solid #00ff88;
}

.log-entry.error {
  border-left: 3px solid #ff4444;
}

.log-entry.warning {
  border-left: 3px solid #ff8800;
}

.log-entry.info {
  border-left: 3px solid #0088ff;
}

.global-stats {
  display: flex;
  justify-content: center;
  gap: 3rem;
  padding: 2rem;
  background: #1a1a1a;
  border-top: 2px solid #333;
}

.stat {
  text-align: center;
}

.stat .label {
  display: block;
  color: #888;
  font-size: 0.9rem;
  margin-bottom: 0.5rem;
}

.stat .value {
  display: block;
  font-size: 1.5rem;
  font-weight: 700;
  color: #00ff88;
}
</style>