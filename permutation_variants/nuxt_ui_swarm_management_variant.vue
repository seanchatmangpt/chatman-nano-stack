<!--
BitActor Nuxt UI Swarm Management Variant
Advanced swarm orchestration and management interface for BitActor pipeline
Multi-topology support: hierarchical, mesh, ring, star
Real-time agent coordination with TTL-aware task distribution
No TypeScript - Pure JavaScript with advanced swarm visualization
-->

<template>
  <div class="swarm-management">
    <!-- Swarm Management Header -->
    <header class="swarm-header">
      <div class="header-content">
        <h1 class="header-title">
          <span class="title-icon">🐝</span>
          Swarm Management Console
        </h1>
        <div class="header-actions">
          <button @click="createNewSwarm" class="action-btn primary">
            ➕ Create Swarm
          </button>
          <button @click="startGlobalCoordination" class="action-btn" 
                  :class="{ active: globalCoordination }">
            {{ globalCoordination ? '⏸️' : '▶️' }} Global Coordination
          </button>
          <button @click="refreshSwarms" class="action-btn">
            🔄 Refresh
          </button>
        </div>
      </div>
    </header>

    <!-- Swarm Overview Grid -->
    <section class="swarm-overview">
      <div class="overview-stats">
        <div class="stat-card">
          <div class="stat-icon">🌐</div>
          <div class="stat-content">
            <div class="stat-value">{{ totalSwarms }}</div>
            <div class="stat-label">Active Swarms</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">🤖</div>
          <div class="stat-content">
            <div class="stat-value">{{ totalAgents }}</div>
            <div class="stat-label">Total Agents</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">⚡</div>
          <div class="stat-content">
            <div class="stat-value">{{ activeTasks }}</div>
            <div class="stat-label">Active Tasks</div>
          </div>
        </div>
        
        <div class="stat-card">
          <div class="stat-icon">🎯</div>
          <div class="stat-content">
            <div class="stat-value">{{ coordinationEfficiency }}%</div>
            <div class="stat-label">Efficiency</div>
          </div>
        </div>
      </div>
    </section>

    <!-- Swarm Topology Visualizer -->
    <section class="topology-section">
      <div class="section-header">
        <h2 class="section-title">Swarm Topology Network</h2>
        <div class="topology-controls">
          <select v-model="selectedTopology" @change="updateTopologyView" class="topology-selector">
            <option value="all">All Topologies</option>
            <option value="hierarchical">Hierarchical</option>
            <option value="mesh">Mesh</option>
            <option value="ring">Ring</option>
            <option value="star">Star</option>
          </select>
          <button @click="toggleTopologyAnimation" class="control-btn" 
                  :class="{ active: topologyAnimation }">
            {{ topologyAnimation ? '⏸️' : '▶️' }} Animation
          </button>
        </div>
      </div>
      
      <div class="topology-canvas-container">
        <canvas ref="topologyCanvas" class="topology-canvas" 
                @click="handleTopologyClick" @mousemove="handleTopologyHover"></canvas>
        
        <!-- Topology Legend -->
        <div class="topology-legend">
          <div class="legend-category">
            <h4>Swarm Types</h4>
            <div class="legend-item">
              <div class="legend-marker coordinator"></div>
              <span>Coordinator</span>
            </div>
            <div class="legend-item">
              <div class="legend-marker processor"></div>
              <span>Processor</span>
            </div>
            <div class="legend-item">
              <div class="legend-marker monitor"></div>
              <span>Monitor</span>
            </div>
          </div>
          
          <div class="legend-category">
            <h4>Agent Status</h4>
            <div class="legend-item">
              <div class="legend-marker active"></div>
              <span>Active</span>
            </div>
            <div class="legend-item">
              <div class="legend-marker idle"></div>
              <span>Idle</span>
            </div>
            <div class="legend-item">
              <div class="legend-marker busy"></div>
              <span>Busy</span>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- Swarm Management Grid -->
    <section class="swarm-grid">
      <div class="grid-header">
        <h2 class="section-title">Active Swarms</h2>
        <div class="grid-controls">
          <input v-model="searchQuery" type="text" placeholder="Search swarms..." class="search-input">
          <select v-model="filterStatus" class="filter-select">
            <option value="all">All Status</option>
            <option value="active">Active</option>
            <option value="idle">Idle</option>
            <option value="coordinating">Coordinating</option>
          </select>
        </div>
      </div>
      
      <div class="swarms-grid">
        <div v-for="swarm in filteredSwarms" :key="swarm.id" 
             class="swarm-card" :class="[swarm.status, swarm.topology]">
          
          <!-- Swarm Header -->
          <div class="swarm-card-header">
            <div class="swarm-identity">
              <span class="swarm-icon">{{ getSwarmIcon(swarm.type) }}</span>
              <div class="swarm-info">
                <h3 class="swarm-name">{{ swarm.name }}</h3>
                <span class="swarm-id">{{ swarm.id }}</span>
              </div>
            </div>
            
            <div class="swarm-status-badge" :class="swarm.status">
              {{ swarm.status }}
            </div>
          </div>

          <!-- Swarm Metrics -->
          <div class="swarm-metrics">
            <div class="metric-row">
              <div class="metric">
                <span class="metric-label">Topology</span>
                <span class="metric-value">{{ swarm.topology }}</span>
              </div>
              <div class="metric">
                <span class="metric-label">Agents</span>
                <span class="metric-value">{{ swarm.agents.length }}</span>
              </div>
            </div>
            
            <div class="metric-row">
              <div class="metric">
                <span class="metric-label">Load</span>
                <span class="metric-value">{{ swarm.load }}%</span>
              </div>
              <div class="metric">
                <span class="metric-label">Tasks</span>
                <span class="metric-value">{{ swarm.activeTasks }}</span>
              </div>
            </div>
          </div>

          <!-- TTL Performance -->
          <div class="swarm-ttl">
            <div class="ttl-header">
              <span class="ttl-label">TTL Performance</span>
              <span class="ttl-value">{{ swarm.ttlCompliance }}%</span>
            </div>
            <div class="ttl-bar">
              <div class="ttl-fill" :style="{ 
                width: `${swarm.ttlCompliance}%`,
                backgroundColor: getTTLColor(swarm.ttlCompliance)
              }"></div>
            </div>
          </div>

          <!-- Agent List -->
          <div class="agent-list">
            <div class="agent-list-header">
              <span>Agents ({{ swarm.agents.length }})</span>
              <button @click="toggleAgentList(swarm.id)" class="toggle-btn">
                {{ expandedSwarms.includes(swarm.id) ? '▼' : '▶' }}
              </button>
            </div>
            
            <div v-if="expandedSwarms.includes(swarm.id)" class="agents-container">
              <div v-for="agent in swarm.agents" :key="agent.id" 
                   class="agent-item" :class="agent.status">
                <div class="agent-info">
                  <span class="agent-name">{{ agent.name }}</span>
                  <span class="agent-type">{{ agent.type }}</span>
                </div>
                <div class="agent-metrics">
                  <span class="agent-load">{{ agent.load }}%</span>
                  <span class="agent-tasks">{{ agent.activeTasks }} tasks</span>
                </div>
              </div>
            </div>
          </div>

          <!-- Swarm Actions -->
          <div class="swarm-actions">
            <button @click="inspectSwarm(swarm)" class="action-btn small">
              🔍 Inspect
            </button>
            <button @click="coordinateSwarm(swarm)" class="action-btn small">
              🔄 Coordinate
            </button>
            <button @click="pauseSwarm(swarm)" class="action-btn small" 
                    :class="{ active: swarm.status === 'paused' }">
              {{ swarm.status === 'paused' ? '▶️' : '⏸️' }}
            </button>
            <button @click="removeSwarm(swarm)" class="action-btn small danger">
              🗑️
            </button>
          </div>
        </div>
      </div>
    </section>

    <!-- Real-time Coordination Monitor -->
    <section class="coordination-monitor">
      <h2 class="section-title">Real-time Coordination Monitor</h2>
      
      <div class="monitor-grid">
        <!-- Message Flow -->
        <div class="monitor-panel">
          <h3 class="panel-title">Inter-Swarm Messages</h3>
          <div class="message-stream" ref="messageStream">
            <div v-for="message in recentMessages" :key="message.id" 
                 class="message-item" :class="message.type">
              <div class="message-timestamp">{{ formatTime(message.timestamp) }}</div>
              <div class="message-content">
                <span class="message-from">{{ message.from }}</span>
                <span class="message-arrow">→</span>
                <span class="message-to">{{ message.to }}</span>
                <div class="message-payload">{{ message.payload }}</div>
              </div>
            </div>
          </div>
        </div>

        <!-- Task Distribution -->
        <div class="monitor-panel">
          <h3 class="panel-title">Task Distribution</h3>
          <canvas ref="taskDistributionChart" class="distribution-chart"></canvas>
        </div>

        <!-- Performance Metrics -->
        <div class="monitor-panel">
          <h3 class="panel-title">Performance Metrics</h3>
          <div class="metrics-list">
            <div class="performance-metric">
              <span class="metric-name">Coordination Latency</span>
              <span class="metric-value">{{ coordinationLatency }}ms</span>
              <div class="metric-trend" :class="getLatencyTrend()">
                {{ getLatencyTrend() === 'up' ? '↗️' : '↘️' }}
              </div>
            </div>
            
            <div class="performance-metric">
              <span class="metric-name">Message Throughput</span>
              <span class="metric-value">{{ messageThroughput }}/s</span>
              <div class="metric-trend" :class="getThroughputTrend()">
                {{ getThroughputTrend() === 'up' ? '↗️' : '↘️' }}
              </div>
            </div>
            
            <div class="performance-metric">
              <span class="metric-name">Load Balance</span>
              <span class="metric-value">{{ loadBalance }}%</span>
              <div class="metric-trend" :class="getBalanceTrend()">
                {{ getBalanceTrend() === 'up' ? '↗️' : '↘️' }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- Swarm Inspector Modal -->
    <div v-if="inspectedSwarm" class="swarm-inspector-modal" @click.self="closeInspector">
      <div class="inspector-content">
        <div class="inspector-header">
          <h2>{{ inspectedSwarm.name }} Inspector</h2>
          <button @click="closeInspector" class="close-btn">✕</button>
        </div>
        
        <div class="inspector-body">
          <!-- Swarm Details -->
          <div class="inspector-section">
            <h3>Swarm Configuration</h3>
            <div class="config-grid">
              <div class="config-item">
                <label>Topology:</label>
                <span>{{ inspectedSwarm.topology }}</span>
              </div>
              <div class="config-item">
                <label>Max Agents:</label>
                <span>{{ inspectedSwarm.maxAgents }}</span>
              </div>
              <div class="config-item">
                <label>TTL Budget:</label>
                <span>{{ inspectedSwarm.ttlBudget }}ms</span>
              </div>
              <div class="config-item">
                <label>Created:</label>
                <span>{{ formatTime(inspectedSwarm.created) }}</span>
              </div>
            </div>
          </div>

          <!-- Agent Details -->
          <div class="inspector-section">
            <h3>Agent Performance</h3>
            <div class="agent-performance-list">
              <div v-for="agent in inspectedSwarm.agents" :key="agent.id" 
                   class="agent-performance-item">
                <div class="agent-header">
                  <span class="agent-name">{{ agent.name }}</span>
                  <span class="agent-status" :class="agent.status">{{ agent.status }}</span>
                </div>
                <div class="agent-details">
                  <div class="detail-item">
                    <label>CPU:</label>
                    <span>{{ agent.cpu }}%</span>
                  </div>
                  <div class="detail-item">
                    <label>Memory:</label>
                    <span>{{ agent.memory }}MB</span>
                  </div>
                  <div class="detail-item">
                    <label>Tasks Completed:</label>
                    <span>{{ agent.completedTasks }}</span>
                  </div>
                  <div class="detail-item">
                    <label>Success Rate:</label>
                    <span>{{ agent.successRate }}%</span>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <!-- Performance Charts -->
          <div class="inspector-section">
            <h3>Performance Charts</h3>
            <canvas ref="inspectorChart" class="inspector-chart"></canvas>
          </div>
        </div>
      </div>
    </div>

    <!-- Create Swarm Modal -->
    <div v-if="showCreateModal" class="create-swarm-modal" @click.self="closeCreateModal">
      <div class="create-modal-content">
        <div class="create-header">
          <h2>Create New Swarm</h2>
          <button @click="closeCreateModal" class="close-btn">✕</button>
        </div>
        
        <form @submit.prevent="submitCreateSwarm" class="create-form">
          <div class="form-group">
            <label>Swarm Name:</label>
            <input v-model="newSwarm.name" type="text" required class="form-input">
          </div>
          
          <div class="form-group">
            <label>Topology:</label>
            <select v-model="newSwarm.topology" required class="form-select">
              <option value="hierarchical">Hierarchical</option>
              <option value="mesh">Mesh</option>
              <option value="ring">Ring</option>
              <option value="star">Star</option>
            </select>
          </div>
          
          <div class="form-group">
            <label>Max Agents:</label>
            <input v-model.number="newSwarm.maxAgents" type="number" min="1" max="50" required class="form-input">
          </div>
          
          <div class="form-group">
            <label>TTL Budget (ms):</label>
            <input v-model.number="newSwarm.ttlBudget" type="number" min="1" max="100" required class="form-input">
          </div>
          
          <div class="form-group">
            <label>Swarm Type:</label>
            <select v-model="newSwarm.type" required class="form-select">
              <option value="coordinator">Coordinator</option>
              <option value="processor">Processor</option>
              <option value="monitor">Monitor</option>
            </select>
          </div>
          
          <div class="form-actions">
            <button type="button" @click="closeCreateModal" class="action-btn secondary">
              Cancel
            </button>
            <button type="submit" class="action-btn primary">
              Create Swarm
            </button>
          </div>
        </form>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'NuxtUISwarmManagementVariant',
  
  data() {
    return {
      globalCoordination: false,
      selectedTopology: 'all',
      topologyAnimation: true,
      searchQuery: '',
      filterStatus: 'all',
      expandedSwarms: [],
      inspectedSwarm: null,
      showCreateModal: false,
      
      newSwarm: {
        name: '',
        topology: 'hierarchical',
        maxAgents: 8,
        ttlBudget: 8,
        type: 'coordinator'
      },
      
      swarms: [
        {
          id: 'swarm_001',
          name: 'Main Coordinator',
          type: 'coordinator',
          topology: 'hierarchical',
          status: 'active',
          maxAgents: 12,
          ttlBudget: 8,
          ttlCompliance: 94,
          load: 67,
          activeTasks: 15,
          created: Date.now() - 86400000,
          agents: [
            {
              id: 'agent_001', 
              name: 'Coordinator-Alpha', 
              type: 'coordinator', 
              status: 'active', 
              load: 45, 
              activeTasks: 3,
              cpu: 23, 
              memory: 156, 
              completedTasks: 1247, 
              successRate: 98.5
            },
            {
              id: 'agent_002', 
              name: 'Worker-Beta', 
              type: 'worker', 
              status: 'busy', 
              load: 89, 
              activeTasks: 7,
              cpu: 67, 
              memory: 234, 
              completedTasks: 892, 
              successRate: 96.2
            },
            {
              id: 'agent_003', 
              name: 'Monitor-Gamma', 
              type: 'monitor', 
              status: 'active', 
              load: 34, 
              activeTasks: 2,
              cpu: 12, 
              memory: 89, 
              completedTasks: 567, 
              successRate: 99.1
            }
          ]
        },
        {
          id: 'swarm_002',
          name: 'Data Processor',
          type: 'processor',
          topology: 'mesh',
          status: 'active',
          maxAgents: 16,
          ttlBudget: 8,
          ttlCompliance: 87,
          load: 84,
          activeTasks: 28,
          created: Date.now() - 43200000,
          agents: [
            {
              id: 'agent_004', 
              name: 'Processor-Delta', 
              type: 'processor', 
              status: 'active', 
              load: 78, 
              activeTasks: 12,
              cpu: 56, 
              memory: 312, 
              completedTasks: 2134, 
              successRate: 94.8
            },
            {
              id: 'agent_005', 
              name: 'Processor-Echo', 
              type: 'processor', 
              status: 'busy', 
              load: 91, 
              activeTasks: 16,
              cpu: 73, 
              memory: 287, 
              completedTasks: 1876, 
              successRate: 93.2
            }
          ]
        },
        {
          id: 'swarm_003',
          name: 'Health Monitor',
          type: 'monitor',
          topology: 'star',
          status: 'idle',
          maxAgents: 6,
          ttlBudget: 8,
          ttlCompliance: 98,
          load: 23,
          activeTasks: 6,
          created: Date.now() - 21600000,
          agents: [
            {
              id: 'agent_006', 
              name: 'Monitor-Zeta', 
              type: 'monitor', 
              status: 'active', 
              load: 19, 
              activeTasks: 4,
              cpu: 8, 
              memory: 67, 
              completedTasks: 345, 
              successRate: 99.7
            },
            {
              id: 'agent_007', 
              name: 'Monitor-Eta', 
              type: 'monitor', 
              status: 'idle', 
              load: 5, 
              activeTasks: 0,
              cpu: 3, 
              memory: 45, 
              completedTasks: 123, 
              successRate: 99.9
            }
          ]
        }
      ],
      
      recentMessages: [
        {
          id: 'msg_001',
          from: 'swarm_001',
          to: 'swarm_002',
          type: 'coordination',
          payload: 'Task redistribution request',
          timestamp: Date.now() - 2000
        },
        {
          id: 'msg_002',
          from: 'swarm_002',
          to: 'swarm_001',
          type: 'response',
          payload: 'Capacity available for 8 tasks',
          timestamp: Date.now() - 1500
        },
        {
          id: 'msg_003',
          from: 'swarm_003',
          to: 'swarm_001',
          type: 'health',
          payload: 'Health check: All systems normal',
          timestamp: Date.now() - 1000
        }
      ],
      
      coordinationLatency: 12.4,
      messageThroughput: 847,
      loadBalance: 73,
      
      topologyCanvas: null,
      animationFrame: null
    }
  },
  
  computed: {
    totalSwarms() {
      return this.swarms.length
    },
    
    totalAgents() {
      return this.swarms.reduce((sum, swarm) => sum + swarm.agents.length, 0)
    },
    
    activeTasks() {
      return this.swarms.reduce((sum, swarm) => sum + swarm.activeTasks, 0)
    },
    
    coordinationEfficiency() {
      const totalCompliance = this.swarms.reduce((sum, swarm) => sum + swarm.ttlCompliance, 0)
      return Math.round(totalCompliance / this.swarms.length)
    },
    
    filteredSwarms() {
      let filtered = this.swarms
      
      if (this.searchQuery) {
        filtered = filtered.filter(swarm => 
          swarm.name.toLowerCase().includes(this.searchQuery.toLowerCase()) ||
          swarm.type.toLowerCase().includes(this.searchQuery.toLowerCase())
        )
      }
      
      if (this.filterStatus !== 'all') {
        filtered = filtered.filter(swarm => swarm.status === this.filterStatus)
      }
      
      if (this.selectedTopology !== 'all') {
        filtered = filtered.filter(swarm => swarm.topology === this.selectedTopology)
      }
      
      return filtered
    }
  },
  
  mounted() {
    this.initializeTopologyCanvas()
    this.startRealtimeUpdates()
  },
  
  beforeUnmount() {
    this.stopRealtimeUpdates()
  },
  
  methods: {
    getSwarmIcon(type) {
      const icons = {
        coordinator: '👑',
        processor: '⚙️',
        monitor: '👁️'
      }
      return icons[type] || '🤖'
    },
    
    getTTLColor(compliance) {
      if (compliance >= 95) return '#5cb85c'
      if (compliance >= 80) return '#f0ad4e'
      return '#d9534f'
    },
    
    toggleAgentList(swarmId) {
      const index = this.expandedSwarms.indexOf(swarmId)
      if (index > -1) {
        this.expandedSwarms.splice(index, 1)
      } else {
        this.expandedSwarms.push(swarmId)
      }
    },
    
    createNewSwarm() {
      this.showCreateModal = true
    },
    
    closeCreateModal() {
      this.showCreateModal = false
      this.newSwarm = {
        name: '',
        topology: 'hierarchical',
        maxAgents: 8,
        ttlBudget: 8,
        type: 'coordinator'
      }
    },
    
    submitCreateSwarm() {
      const swarm = {
        id: `swarm_${Date.now()}`,
        name: this.newSwarm.name,
        type: this.newSwarm.type,
        topology: this.newSwarm.topology,
        status: 'active',
        maxAgents: this.newSwarm.maxAgents,
        ttlBudget: this.newSwarm.ttlBudget,
        ttlCompliance: Math.random() * 20 + 80,
        load: Math.random() * 50 + 25,
        activeTasks: Math.floor(Math.random() * 10),
        created: Date.now(),
        agents: this.generateAgents(this.newSwarm.maxAgents)
      }
      
      this.swarms.push(swarm)
      this.closeCreateModal()
    },
    
    generateAgents(count) {
      const agents = []
      const types = ['coordinator', 'worker', 'processor', 'monitor']
      const statuses = ['active', 'idle', 'busy']
      
      for (let i = 0; i < Math.min(count, Math.floor(Math.random() * 5) + 2); i++) {
        agents.push({
          id: `agent_${Date.now()}_${i}`,
          name: `Agent-${String.fromCharCode(65 + i)}`,
          type: types[Math.floor(Math.random() * types.length)],
          status: statuses[Math.floor(Math.random() * statuses.length)],
          load: Math.floor(Math.random() * 100),
          activeTasks: Math.floor(Math.random() * 10),
          cpu: Math.floor(Math.random() * 80) + 10,
          memory: Math.floor(Math.random() * 300) + 50,
          completedTasks: Math.floor(Math.random() * 2000),
          successRate: Math.random() * 10 + 90
        })
      }
      
      return agents
    },
    
    startGlobalCoordination() {
      this.globalCoordination = !this.globalCoordination
    },
    
    refreshSwarms() {
      // Simulate swarm data refresh
      this.swarms.forEach(swarm => {
        swarm.load = Math.max(10, Math.min(100, swarm.load + (Math.random() - 0.5) * 20))
        swarm.ttlCompliance = Math.max(70, Math.min(100, swarm.ttlCompliance + (Math.random() - 0.5) * 10))
        swarm.activeTasks = Math.max(0, swarm.activeTasks + Math.floor((Math.random() - 0.5) * 5))
      })
    },
    
    inspectSwarm(swarm) {
      this.inspectedSwarm = swarm
    },
    
    closeInspector() {
      this.inspectedSwarm = null
    },
    
    coordinateSwarm(swarm) {
      // Simulate coordination
      const message = {
        id: `msg_${Date.now()}`,
        from: 'management_console',
        to: swarm.id,
        type: 'coordination',
        payload: 'Manual coordination triggered',
        timestamp: Date.now()
      }
      
      this.recentMessages.unshift(message)
      if (this.recentMessages.length > 10) {
        this.recentMessages.pop()
      }
    },
    
    pauseSwarm(swarm) {
      swarm.status = swarm.status === 'paused' ? 'active' : 'paused'
    },
    
    removeSwarm(swarm) {
      const index = this.swarms.indexOf(swarm)
      if (index > -1) {
        this.swarms.splice(index, 1)
      }
    },
    
    updateTopologyView() {
      this.drawTopology()
    },
    
    toggleTopologyAnimation() {
      this.topologyAnimation = !this.topologyAnimation
    },
    
    handleTopologyClick(event) {
      // Handle topology canvas clicks
      const rect = event.currentTarget.getBoundingClientRect()
      const x = event.clientX - rect.left
      const y = event.clientY - rect.top
      
      // Find clicked swarm/agent
      console.log('Topology clicked at:', x, y)
    },
    
    handleTopologyHover(event) {
      // Handle topology hover for tooltips
    },
    
    formatTime(timestamp) {
      return new Date(timestamp).toLocaleTimeString()
    },
    
    getLatencyTrend() {
      return Math.random() > 0.5 ? 'up' : 'down'
    },
    
    getThroughputTrend() {
      return Math.random() > 0.5 ? 'up' : 'down'
    },
    
    getBalanceTrend() {
      return Math.random() > 0.5 ? 'up' : 'down'
    },
    
    initializeTopologyCanvas() {
      const canvas = this.$refs.topologyCanvas
      if (!canvas) return
      
      canvas.width = 800
      canvas.height = 500
      this.topologyCanvas = canvas.getContext('2d')
      this.drawTopology()
    },
    
    drawTopology() {
      if (!this.topologyCanvas) return
      
      const ctx = this.topologyCanvas
      const canvas = ctx.canvas
      
      // Clear canvas
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      
      // Draw background
      ctx.fillStyle = 'rgba(26, 26, 46, 0.1)'
      ctx.fillRect(0, 0, canvas.width, canvas.height)
      
      // Draw grid
      ctx.strokeStyle = 'rgba(255, 255, 255, 0.1)'
      ctx.lineWidth = 1
      for (let x = 0; x < canvas.width; x += 40) {
        ctx.beginPath()
        ctx.moveTo(x, 0)
        ctx.lineTo(x, canvas.height)
        ctx.stroke()
      }
      for (let y = 0; y < canvas.height; y += 40) {
        ctx.beginPath()
        ctx.moveTo(0, y)
        ctx.lineTo(canvas.width, y)
        ctx.stroke()
      }
      
      // Draw swarms
      this.filteredSwarms.forEach((swarm, index) => {
        this.drawSwarmTopology(ctx, swarm, index)
      })
    },
    
    drawSwarmTopology(ctx, swarm, index) {
      const centerX = 200 + (index % 3) * 200
      const centerY = 150 + Math.floor(index / 3) * 200
      
      // Draw swarm center
      ctx.fillStyle = this.getSwarmColor(swarm.type)
      ctx.beginPath()
      ctx.arc(centerX, centerY, 20, 0, 2 * Math.PI)
      ctx.fill()
      
      // Draw swarm name
      ctx.fillStyle = '#ffffff'
      ctx.font = '12px Inter'
      ctx.textAlign = 'center'
      ctx.fillText(swarm.name, centerX, centerY + 40)
      
      // Draw agents based on topology
      this.drawAgentsForTopology(ctx, swarm, centerX, centerY)
    },
    
    drawAgentsForTopology(ctx, swarm, centerX, centerY) {
      const agentCount = swarm.agents.length
      
      switch (swarm.topology) {
        case 'hierarchical':
          this.drawHierarchicalAgents(ctx, swarm.agents, centerX, centerY)
          break
        case 'mesh':
          this.drawMeshAgents(ctx, swarm.agents, centerX, centerY)
          break
        case 'ring':
          this.drawRingAgents(ctx, swarm.agents, centerX, centerY)
          break
        case 'star':
          this.drawStarAgents(ctx, swarm.agents, centerX, centerY)
          break
      }
    },
    
    drawHierarchicalAgents(ctx, agents, centerX, centerY) {
      agents.forEach((agent, index) => {
        const level = Math.floor(index / 2) + 1
        const posInLevel = index % 2
        const x = centerX + (posInLevel - 0.5) * 60
        const y = centerY + level * 50
        
        this.drawAgent(ctx, agent, x, y)
        this.drawConnection(ctx, centerX, centerY, x, y)
      })
    },
    
    drawMeshAgents(ctx, agents, centerX, centerY) {
      const radius = 60
      agents.forEach((agent, index) => {
        const angle = (index / agents.length) * 2 * Math.PI
        const x = centerX + Math.cos(angle) * radius
        const y = centerY + Math.sin(angle) * radius
        
        this.drawAgent(ctx, agent, x, y)
        
        // Draw connections to other agents
        agents.forEach((otherAgent, otherIndex) => {
          if (otherIndex > index) {
            const otherAngle = (otherIndex / agents.length) * 2 * Math.PI
            const otherX = centerX + Math.cos(otherAngle) * radius
            const otherY = centerY + Math.sin(otherAngle) * radius
            this.drawConnection(ctx, x, y, otherX, otherY)
          }
        })
      })
    },
    
    drawRingAgents(ctx, agents, centerX, centerY) {
      const radius = 60
      agents.forEach((agent, index) => {
        const angle = (index / agents.length) * 2 * Math.PI
        const x = centerX + Math.cos(angle) * radius
        const y = centerY + Math.sin(angle) * radius
        
        this.drawAgent(ctx, agent, x, y)
        
        // Draw connection to next agent
        const nextIndex = (index + 1) % agents.length
        const nextAngle = (nextIndex / agents.length) * 2 * Math.PI
        const nextX = centerX + Math.cos(nextAngle) * radius
        const nextY = centerY + Math.sin(nextAngle) * radius
        this.drawConnection(ctx, x, y, nextX, nextY)
      })
    },
    
    drawStarAgents(ctx, agents, centerX, centerY) {
      const radius = 60
      agents.forEach((agent, index) => {
        const angle = (index / agents.length) * 2 * Math.PI
        const x = centerX + Math.cos(angle) * radius
        const y = centerY + Math.sin(angle) * radius
        
        this.drawAgent(ctx, agent, x, y)
        this.drawConnection(ctx, centerX, centerY, x, y)
      })
    },
    
    drawAgent(ctx, agent, x, y) {
      // Agent background
      ctx.fillStyle = this.getAgentColor(agent.status)
      ctx.beginPath()
      ctx.arc(x, y, 8, 0, 2 * Math.PI)
      ctx.fill()
      
      // Agent border
      ctx.strokeStyle = '#ffffff'
      ctx.lineWidth = 1
      ctx.stroke()
    },
    
    drawConnection(ctx, x1, y1, x2, y2) {
      ctx.strokeStyle = 'rgba(74, 144, 226, 0.6)'
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
      ctx.stroke()
    },
    
    getSwarmColor(type) {
      const colors = {
        coordinator: '#4a90e2',
        processor: '#5cb85c',
        monitor: '#f0ad4e'
      }
      return colors[type] || '#888888'
    },
    
    getAgentColor(status) {
      const colors = {
        active: '#5cb85c',
        idle: '#888888',
        busy: '#d9534f'
      }
      return colors[status] || '#888888'
    },
    
    startRealtimeUpdates() {
      setInterval(() => {
        this.updateMetrics()
        this.generateMessage()
        if (this.topologyAnimation) {
          this.drawTopology()
        }
      }, 2000)
    },
    
    stopRealtimeUpdates() {
      // Stop intervals if needed
    },
    
    updateMetrics() {
      this.coordinationLatency = Math.max(5, Math.min(50, 
        this.coordinationLatency + (Math.random() - 0.5) * 5))
      this.messageThroughput = Math.max(100, Math.min(2000,
        this.messageThroughput + Math.floor((Math.random() - 0.5) * 100)))
      this.loadBalance = Math.max(30, Math.min(100,
        this.loadBalance + (Math.random() - 0.5) * 10))
    },
    
    generateMessage() {
      if (Math.random() < 0.3) {
        const types = ['coordination', 'response', 'health', 'task']
        const messages = [
          'Task redistribution request',
          'Capacity check',
          'Health status update',
          'Performance metrics',
          'Load balancing adjustment',
          'Agent status change'
        ]
        
        const swarmIds = this.swarms.map(s => s.id)
        const from = swarmIds[Math.floor(Math.random() * swarmIds.length)]
        const to = swarmIds[Math.floor(Math.random() * swarmIds.length)]
        
        if (from !== to) {
          const message = {
            id: `msg_${Date.now()}`,
            from,
            to,
            type: types[Math.floor(Math.random() * types.length)],
            payload: messages[Math.floor(Math.random() * messages.length)],
            timestamp: Date.now()
          }
          
          this.recentMessages.unshift(message)
          if (this.recentMessages.length > 15) {
            this.recentMessages.pop()
          }
        }
      }
    }
  }
}
</script>

<style scoped>
.swarm-management {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

/* Header */
.swarm-header {
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

.header-actions {
  display: flex;
  gap: 15px;
}

.action-btn {
  padding: 12px 20px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
  font-weight: 500;
}

.action-btn:hover {
  background: rgba(255, 255, 255, 0.2);
  transform: translateY(-2px);
}

.action-btn.primary {
  background: #4a90e2;
}

.action-btn.primary:hover {
  background: #357abd;
}

.action-btn.active {
  background: #5cb85c;
}

.action-btn.small {
  padding: 6px 12px;
  font-size: 0.85rem;
}

.action-btn.danger {
  background: #d9534f;
}

/* Overview Stats */
.swarm-overview {
  margin-bottom: 30px;
}

.overview-stats {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 20px;
}

.stat-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  display: flex;
  align-items: center;
  gap: 15px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: transform 0.3s ease;
}

.stat-card:hover {
  transform: translateY(-3px);
}

.stat-icon {
  font-size: 2.5rem;
}

.stat-value {
  font-size: 1.8rem;
  font-weight: 700;
  color: #4a90e2;
}

.stat-label {
  font-size: 0.9rem;
  opacity: 0.8;
}

/* Topology Section */
.topology-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 30px;
}

.section-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.section-title {
  font-size: 1.6rem;
  font-weight: 600;
  margin: 0;
}

.topology-controls {
  display: flex;
  gap: 15px;
  align-items: center;
}

.topology-selector {
  padding: 8px 12px;
  border: none;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
}

.control-btn {
  padding: 8px 16px;
  border: none;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.control-btn.active {
  background: #5cb85c;
}

.topology-canvas-container {
  position: relative;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 12px;
  overflow: hidden;
}

.topology-canvas {
  display: block;
  width: 100%;
  height: 500px;
  cursor: crosshair;
}

.topology-legend {
  position: absolute;
  top: 20px;
  right: 20px;
  background: rgba(0, 0, 0, 0.8);
  padding: 20px;
  border-radius: 8px;
  backdrop-filter: blur(10px);
}

.legend-category {
  margin-bottom: 15px;
}

.legend-category h4 {
  margin: 0 0 10px 0;
  font-size: 0.9rem;
  color: #ffffff;
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 6px;
  font-size: 0.8rem;
}

.legend-marker {
  width: 12px;
  height: 12px;
  border-radius: 50%;
}

.legend-marker.coordinator { background: #4a90e2; }
.legend-marker.processor { background: #5cb85c; }
.legend-marker.monitor { background: #f0ad4e; }
.legend-marker.active { background: #5cb85c; }
.legend-marker.idle { background: #888888; }
.legend-marker.busy { background: #d9534f; }

/* Swarm Grid */
.swarm-grid {
  margin-bottom: 30px;
}

.grid-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.grid-controls {
  display: flex;
  gap: 15px;
}

.search-input, .filter-select {
  padding: 8px 12px;
  border: none;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.search-input::placeholder {
  color: rgba(255, 255, 255, 0.5);
}

.swarms-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 20px;
}

.swarm-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 20px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: all 0.3s ease;
}

.swarm-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
}

.swarm-card.active {
  border-color: #5cb85c;
}

.swarm-card.idle {
  border-color: #888888;
}

.swarm-card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
}

.swarm-identity {
  display: flex;
  align-items: center;
  gap: 12px;
}

.swarm-icon {
  font-size: 1.8rem;
}

.swarm-name {
  font-size: 1.2rem;
  font-weight: 600;
  margin: 0;
}

.swarm-id {
  font-size: 0.8rem;
  opacity: 0.6;
  font-family: monospace;
}

.swarm-status-badge {
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.swarm-status-badge.active {
  background: #5cb85c;
  color: white;
}

.swarm-status-badge.idle {
  background: #888888;
  color: white;
}

.swarm-status-badge.paused {
  background: #f0ad4e;
  color: white;
}

.swarm-metrics {
  margin-bottom: 15px;
}

.metric-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 8px;
}

.metric {
  text-align: center;
}

.metric-label {
  display: block;
  font-size: 0.8rem;
  opacity: 0.7;
}

.metric-value {
  display: block;
  font-size: 1rem;
  font-weight: 600;
}

.swarm-ttl {
  margin-bottom: 15px;
}

.ttl-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 5px;
}

.ttl-label {
  font-size: 0.9rem;
  opacity: 0.8;
}

.ttl-value {
  font-weight: 600;
}

.ttl-bar {
  height: 6px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 3px;
  overflow: hidden;
}

.ttl-fill {
  height: 100%;
  transition: width 0.3s ease;
}

.agent-list {
  margin-bottom: 15px;
}

.agent-list-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  font-size: 0.9rem;
  font-weight: 500;
}

.toggle-btn {
  background: none;
  border: none;
  color: white;
  cursor: pointer;
  font-size: 0.8rem;
}

.agents-container {
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
  padding: 10px;
}

.agent-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px;
  margin-bottom: 5px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.05);
}

.agent-item.active {
  border-left: 3px solid #5cb85c;
}

.agent-item.busy {
  border-left: 3px solid #d9534f;
}

.agent-item.idle {
  border-left: 3px solid #888888;
}

.agent-info {
  display: flex;
  flex-direction: column;
}

.agent-name {
  font-weight: 500;
  font-size: 0.9rem;
}

.agent-type {
  font-size: 0.8rem;
  opacity: 0.7;
}

.agent-metrics {
  text-align: right;
  font-size: 0.8rem;
}

.agent-load {
  font-weight: 600;
}

.swarm-actions {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}

/* Coordination Monitor */
.coordination-monitor {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 30px;
}

.monitor-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
  gap: 20px;
}

.monitor-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  height: 300px;
}

.panel-title {
  font-size: 1.1rem;
  font-weight: 600;
  margin-bottom: 15px;
}

.message-stream {
  height: 240px;
  overflow-y: auto;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
  padding: 10px;
}

.message-item {
  margin-bottom: 10px;
  padding: 8px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 3px solid #4a90e2;
}

.message-item.coordination {
  border-left-color: #4a90e2;
}

.message-item.response {
  border-left-color: #5cb85c;
}

.message-item.health {
  border-left-color: #f0ad4e;
}

.message-timestamp {
  font-size: 0.7rem;
  opacity: 0.6;
  margin-bottom: 3px;
}

.message-content {
  font-size: 0.85rem;
}

.message-from, .message-to {
  font-weight: 600;
  font-family: monospace;
}

.message-arrow {
  margin: 0 8px;
  color: #4a90e2;
}

.message-payload {
  margin-top: 3px;
  opacity: 0.8;
}

.distribution-chart {
  width: 100%;
  height: 240px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
}

.metrics-list {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.performance-metric {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 12px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

.metric-name {
  font-weight: 500;
}

.metric-value {
  font-weight: 600;
  font-size: 1.1rem;
}

.metric-trend {
  font-size: 1.2rem;
}

.metric-trend.up {
  color: #5cb85c;
}

.metric-trend.down {
  color: #d9534f;
}

/* Modals */
.swarm-inspector-modal, .create-swarm-modal {
  position: fixed;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
  backdrop-filter: blur(5px);
}

.inspector-content, .create-modal-content {
  background: rgba(26, 26, 46, 0.95);
  border-radius: 16px;
  max-width: 800px;
  max-height: 90vh;
  overflow-y: auto;
  border: 1px solid rgba(255, 255, 255, 0.3);
  backdrop-filter: blur(20px);
}

.inspector-header, .create-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 25px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.2);
}

.inspector-header h2, .create-header h2 {
  margin: 0;
  font-size: 1.5rem;
  font-weight: 600;
}

.close-btn {
  width: 35px;
  height: 35px;
  border: none;
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.2);
  color: white;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1.2rem;
}

.inspector-body {
  padding: 25px;
}

.inspector-section {
  margin-bottom: 30px;
}

.inspector-section h3 {
  font-size: 1.2rem;
  font-weight: 600;
  margin-bottom: 15px;
  color: #4a90e2;
}

.config-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 15px;
}

.config-item {
  display: flex;
  justify-content: space-between;
  padding: 10px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 6px;
}

.config-item label {
  font-weight: 500;
  opacity: 0.8;
}

.agent-performance-list {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.agent-performance-item {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
  padding: 15px;
}

.agent-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
}

.agent-status {
  padding: 4px 8px;
  border-radius: 12px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.agent-status.active {
  background: #5cb85c;
}

.agent-status.busy {
  background: #d9534f;
}

.agent-status.idle {
  background: #888888;
}

.agent-details {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
  gap: 10px;
}

.detail-item {
  display: flex;
  justify-content: space-between;
  font-size: 0.9rem;
}

.detail-item label {
  opacity: 0.7;
}

.inspector-chart {
  width: 100%;
  height: 200px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
}

/* Create Form */
.create-form {
  padding: 25px;
}

.form-group {
  margin-bottom: 20px;
}

.form-group label {
  display: block;
  font-weight: 500;
  margin-bottom: 5px;
  color: #ffffff;
}

.form-input, .form-select {
  width: 100%;
  padding: 10px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
}

.form-input::placeholder {
  color: rgba(255, 255, 255, 0.5);
}

.form-actions {
  display: flex;
  gap: 15px;
  justify-content: flex-end;
  margin-top: 30px;
}

.action-btn.secondary {
  background: rgba(255, 255, 255, 0.1);
}

/* Responsive Design */
@media (max-width: 768px) {
  .header-content {
    flex-direction: column;
    gap: 20px;
  }
  
  .overview-stats {
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  }
  
  .topology-controls {
    flex-direction: column;
    gap: 10px;
  }
  
  .grid-controls {
    flex-direction: column;
    gap: 10px;
  }
  
  .swarms-grid {
    grid-template-columns: 1fr;
  }
  
  .monitor-grid {
    grid-template-columns: 1fr;
  }
  
  .inspector-content, .create-modal-content {
    width: 95vw;
    margin: 20px;
  }
}

/* Scrollbar Styles */
.message-stream::-webkit-scrollbar {
  width: 6px;
}

.message-stream::-webkit-scrollbar-track {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 3px;
}

.message-stream::-webkit-scrollbar-thumb {
  background: rgba(255, 255, 255, 0.3);
  border-radius: 3px;
}

.message-stream::-webkit-scrollbar-thumb:hover {
  background: rgba(255, 255, 255, 0.5);
}
</style>