<template>
  <div class="pipeline-flow-editor">
    <h2>🎨 Pipeline Flow Editor</h2>
    
    <!-- Toolbar -->
    <div class="editor-toolbar">
      <div class="toolbar-section">
        <h3>Available Stages</h3>
        <div class="stage-palette">
          <div
            v-for="stage in availableStages"
            :key="stage.id"
            class="palette-stage"
            :draggable="true"
            @dragstart="startStageDrag($event, stage)"
            :style="{ backgroundColor: stage.color }"
          >
            <span class="stage-icon">{{ stage.icon }}</span>
            <span class="stage-name">{{ stage.name }}</span>
          </div>
        </div>
      </div>
      
      <div class="toolbar-section">
        <h3>Actions</h3>
        <div class="action-buttons">
          <button @click="clearCanvas" class="action-btn clear">
            🗑️ Clear
          </button>
          <button @click="autoArrange" class="action-btn arrange">
            📐 Auto Arrange
          </button>
          <button @click="validateFlow" class="action-btn validate">
            ✅ Validate
          </button>
          <button @click="optimizeFlow" class="action-btn optimize">
            ⚡ 80/20 Optimize
          </button>
          <button @click="saveFlow" class="action-btn save">
            💾 Save
          </button>
          <button @click="loadPreset" class="action-btn load">
            📂 Load Preset
          </button>
        </div>
      </div>
    </div>
    
    <!-- Canvas -->
    <div class="flow-canvas-container">
      <svg
        ref="flowCanvas"
        :width="canvasWidth"
        :height="canvasHeight"
        class="flow-canvas"
        @drop="handleCanvasDrop($event)"
        @dragover.prevent
        @click="deselectAll"
      >
        <!-- Grid Pattern -->
        <defs>
          <pattern id="grid" width="20" height="20" patternUnits="userSpaceOnUse">
            <path d="M 20 0 L 0 0 0 20" fill="none" stroke="#333" stroke-width="1"/>
          </pattern>
        </defs>
        <rect width="100%" height="100%" fill="url(#grid)" />
        
        <!-- Connections -->
        <g class="connections-layer">
          <path
            v-for="connection in connections"
            :key="connection.id"
            :d="getConnectionPath(connection)"
            stroke="#0088ff"
            stroke-width="3"
            fill="none"
            marker-end="url(#arrowhead)"
            class="connection-line"
            :class="{ invalid: !connection.valid }"
            @click="selectConnection(connection)"
          />
        </g>
        
        <!-- Stage Nodes -->
        <g class="nodes-layer">
          <g
            v-for="node in canvasNodes"
            :key="node.id"
            class="stage-node"
            :class="{ 
              selected: selectedNode?.id === node.id,
              invalid: !node.valid 
            }"
            :transform="`translate(${node.x}, ${node.y})`"
            @mousedown="startNodeDrag($event, node)"
            @click.stop="selectNode(node)"
            @dblclick="editNode(node)"
          >
            <!-- Node Background -->
            <rect
              :width="nodeWidth"
              :height="nodeHeight"
              :fill="node.color"
              :stroke="selectedNode?.id === node.id ? '#00ff88' : '#666'"
              :stroke-width="selectedNode?.id === node.id ? 3 : 1"
              rx="8"
              class="node-bg"
            />
            
            <!-- Node Icon -->
            <text
              :x="nodeWidth / 2"
              :y="35"
              text-anchor="middle"
              class="node-icon"
            >
              {{ node.icon }}
            </text>
            
            <!-- Node Label -->
            <text
              :x="nodeWidth / 2"
              :y="55"
              text-anchor="middle"
              class="node-label"
            >
              {{ node.name }}
            </text>
            
            <!-- Duration Badge -->
            <rect
              :x="nodeWidth - 40"
              :y="5"
              width="35"
              height="15"
              fill="#333"
              rx="8"
              class="duration-badge"
            />
            <text
              :x="nodeWidth - 22"
              :y="15"
              text-anchor="middle"
              class="duration-text"
            >
              {{ node.duration }}ms
            </text>
            
            <!-- Connection Points -->
            <circle
              cx="10"
              cy="35"
              r="5"
              fill="#888"
              class="input-port"
              @mousedown.stop="startConnection($event, node, 'input')"
            />
            <circle
              :cx="nodeWidth - 10"
              :cy="35"
              r="5"
              fill="#888"
              class="output-port"
              @mousedown.stop="startConnection($event, node, 'output')"
            />
            
            <!-- Delete Button -->
            <circle
              :cx="nodeWidth - 10"
              cy="10"
              r="8"
              fill="#ff4444"
              class="delete-btn"
              @click.stop="deleteNode(node)"
            />
            <text
              :x="nodeWidth - 10"
              y="14"
              text-anchor="middle"
              class="delete-icon"
            >
              ×
            </text>
          </g>
        </g>
        
        <!-- Temporary Connection -->
        <line
          v-if="temporaryConnection"
          :x1="temporaryConnection.x1"
          :y1="temporaryConnection.y1"
          :x2="temporaryConnection.x2"
          :y2="temporaryConnection.y2"
          stroke="#00ff88"
          stroke-width="2"
          stroke-dasharray="5,5"
        />
        
        <!-- Arrow marker -->
        <defs>
          <marker
            id="arrowhead"
            markerWidth="10"
            markerHeight="7"
            refX="9"
            refY="3.5"
            orient="auto"
          >
            <polygon
              points="0 0, 10 3.5, 0 7"
              fill="#0088ff"
            />
          </marker>
        </defs>
      </svg>
    </div>
    
    <!-- Properties Panel -->
    <div class="properties-panel" v-if="selectedNode">
      <h3>📝 Node Properties</h3>
      <div class="property-group">
        <label>Name</label>
        <input v-model="selectedNode.name" @change="updateNode" />
      </div>
      <div class="property-group">
        <label>Duration (ms)</label>
        <input type="number" v-model.number="selectedNode.duration" @change="updateNode" />
      </div>
      <div class="property-group">
        <label>Priority</label>
        <select v-model="selectedNode.priority" @change="updateNode">
          <option value="low">Low</option>
          <option value="medium">Medium</option>
          <option value="high">High</option>
          <option value="critical">Critical</option>
        </select>
      </div>
      <div class="property-group">
        <label>Parallel Group</label>
        <input type="number" v-model.number="selectedNode.parallelGroup" @change="updateNode" />
      </div>
    </div>
    
    <!-- Flow Analysis Panel -->
    <div class="flow-analysis">
      <h3>📊 Flow Analysis</h3>
      <div class="analysis-metrics">
        <div class="metric">
          <span class="label">Total Stages</span>
          <span class="value">{{ canvasNodes.length }}</span>
        </div>
        <div class="metric">
          <span class="label">Critical Path</span>
          <span class="value">{{ criticalPathDuration }}ms</span>
        </div>
        <div class="metric">
          <span class="label">Parallel Groups</span>
          <span class="value">{{ uniqueParallelGroups }}</span>
        </div>
        <div class="metric">
          <span class="label">Flow Validity</span>
          <span class="value" :class="{ valid: isFlowValid, invalid: !isFlowValid }">
            {{ isFlowValid ? 'Valid' : 'Invalid' }}
          </span>
        </div>
      </div>
      
      <div class="flow-path">
        <h4>Execution Path</h4>
        <div class="path-visualization">
          <span
            v-for="(pathNode, index) in executionPath"
            :key="pathNode.id"
            class="path-node"
          >
            {{ pathNode.name }}
            <span v-if="index < executionPath.length - 1" class="path-arrow">→</span>
          </span>
        </div>
      </div>
    </div>
    
    <!-- Validation Messages -->
    <div class="validation-messages" v-if="validationMessages.length > 0">
      <h3>⚠️ Validation Issues</h3>
      <div
        v-for="message in validationMessages"
        :key="message.id"
        class="validation-message"
        :class="message.type"
      >
        {{ message.text }}
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'PipelineFlowEditor',
  
  data() {
    return {
      canvasWidth: 1200,
      canvasHeight: 600,
      nodeWidth: 120,
      nodeHeight: 70,
      
      availableStages: [
        { id: 'typer', name: 'typer', icon: '🎯', color: '#ff6b6b', duration: 50 },
        { id: 'turtle', name: 'turtle', icon: '🐢', color: '#4ecdc4', duration: 30 },
        { id: 'ttl2dspy', name: 'ttl2dspy', icon: '🧠', color: '#45b7d1', duration: 100 },
        { id: 'bitactor', name: 'bitactor', icon: '⚛️', color: '#96ceb4', duration: 200 },
        { id: 'erlang', name: 'erlang', icon: '🔧', color: '#dda0dd', duration: 100 },
        { id: 'ash', name: 'ash', icon: '🔥', color: '#f7b731', duration: 150 },
        { id: 'reactor', name: 'reactor', icon: '⚡', color: '#5f27cd', duration: 100 },
        { id: 'k8s', name: 'k8s', icon: '☸️', color: '#0984e3', duration: 70 }
      ],
      
      canvasNodes: [],
      connections: [],
      selectedNode: null,
      selectedConnection: null,
      
      // Drag state
      draggingNode: null,
      dragOffset: { x: 0, y: 0 },
      
      // Connection state
      temporaryConnection: null,
      connectionStart: null,
      
      validationMessages: []
    }
  },
  
  computed: {
    criticalPathDuration() {
      if (this.canvasNodes.length === 0) return 0
      
      // Calculate critical path using topological sort
      const sorted = this.topologicalSort()
      if (sorted.length === 0) return 0
      
      // Calculate longest path (critical path)
      const distances = {}
      sorted.forEach(node => {
        distances[node.id] = node.duration
        
        // Add maximum duration from predecessors
        const predecessors = this.connections
          .filter(c => c.to === node.id)
          .map(c => c.from)
        
        if (predecessors.length > 0) {
          const maxPredecessor = Math.max(...predecessors.map(p => distances[p] || 0))
          distances[node.id] += maxPredecessor
        }
      })
      
      return Math.max(...Object.values(distances))
    },
    
    uniqueParallelGroups() {
      const groups = [...new Set(this.canvasNodes.map(n => n.parallelGroup))]
      return groups.filter(g => g != null).length
    },
    
    isFlowValid() {
      return this.validationMessages.length === 0
    },
    
    executionPath() {
      return this.topologicalSort()
    }
  },
  
  mounted() {
    this.setupEventListeners()
    this.loadDefaultFlow()
  },
  
  beforeDestroy() {
    this.removeEventListeners()
  },
  
  methods: {
    setupEventListeners() {
      document.addEventListener('mousemove', this.handleMouseMove)
      document.addEventListener('mouseup', this.handleMouseUp)
    },
    
    removeEventListeners() {
      document.removeEventListener('mousemove', this.handleMouseMove)
      document.removeEventListener('mouseup', this.handleMouseUp)
    },
    
    loadDefaultFlow() {
      // Load a simple linear flow as default
      this.canvasNodes = [
        { 
          ...this.availableStages[0], 
          id: 'node-1', 
          x: 100, 
          y: 100, 
          valid: true, 
          parallelGroup: 1,
          priority: 'high'
        },
        { 
          ...this.availableStages[1], 
          id: 'node-2', 
          x: 300, 
          y: 100, 
          valid: true, 
          parallelGroup: 1,
          priority: 'medium'
        },
        { 
          ...this.availableStages[5], 
          id: 'node-3', 
          x: 500, 
          y: 100, 
          valid: true, 
          parallelGroup: 1,
          priority: 'high'
        },
        { 
          ...this.availableStages[7], 
          id: 'node-4', 
          x: 700, 
          y: 100, 
          valid: true, 
          parallelGroup: 1,
          priority: 'critical'
        }
      ]
      
      this.connections = [
        { id: 'conn-1', from: 'node-1', to: 'node-2', valid: true },
        { id: 'conn-2', from: 'node-2', to: 'node-3', valid: true },
        { id: 'conn-3', from: 'node-3', to: 'node-4', valid: true }
      ]
      
      this.validateFlow()
    },
    
    startStageDrag(event, stage) {
      event.dataTransfer.setData('stage', JSON.stringify(stage))
      event.dataTransfer.effectAllowed = 'copy'
    },
    
    handleCanvasDrop(event) {
      event.preventDefault()
      const stageData = event.dataTransfer.getData('stage')
      
      if (stageData) {
        const stage = JSON.parse(stageData)
        const rect = this.$refs.flowCanvas.getBoundingClientRect()
        
        const newNode = {
          ...stage,
          id: `node-${Date.now()}`,
          x: event.clientX - rect.left - this.nodeWidth / 2,
          y: event.clientY - rect.top - this.nodeHeight / 2,
          valid: true,
          parallelGroup: 1,
          priority: 'medium'
        }
        
        this.canvasNodes.push(newNode)
        this.validateFlow()
      }
    },
    
    startNodeDrag(event, node) {
      this.draggingNode = node
      const rect = this.$refs.flowCanvas.getBoundingClientRect()
      this.dragOffset = {
        x: event.clientX - rect.left - node.x,
        y: event.clientY - rect.top - node.y
      }
      event.preventDefault()
    },
    
    handleMouseMove(event) {
      if (this.draggingNode) {
        const rect = this.$refs.flowCanvas.getBoundingClientRect()
        this.draggingNode.x = event.clientX - rect.left - this.dragOffset.x
        this.draggingNode.y = event.clientY - rect.top - this.dragOffset.y
        
        // Keep node within canvas bounds
        this.draggingNode.x = Math.max(0, Math.min(this.canvasWidth - this.nodeWidth, this.draggingNode.x))
        this.draggingNode.y = Math.max(0, Math.min(this.canvasHeight - this.nodeHeight, this.draggingNode.y))
      }
      
      if (this.temporaryConnection) {
        const rect = this.$refs.flowCanvas.getBoundingClientRect()
        this.temporaryConnection.x2 = event.clientX - rect.left
        this.temporaryConnection.y2 = event.clientY - rect.top
      }
    },
    
    handleMouseUp() {
      this.draggingNode = null
      this.temporaryConnection = null
      this.connectionStart = null
    },
    
    startConnection(event, node, type) {
      if (type === 'output') {
        this.connectionStart = node
        this.temporaryConnection = {
          x1: node.x + this.nodeWidth - 10,
          y1: node.y + 35,
          x2: node.x + this.nodeWidth - 10,
          y2: node.y + 35
        }
      } else if (type === 'input' && this.connectionStart) {
        // Create connection
        this.createConnection(this.connectionStart, node)
      }
      event.stopPropagation()
    },
    
    createConnection(fromNode, toNode) {
      if (fromNode.id === toNode.id) return
      
      // Check if connection already exists
      const exists = this.connections.some(c => 
        c.from === fromNode.id && c.to === toNode.id
      )
      
      if (!exists) {
        this.connections.push({
          id: `conn-${Date.now()}`,
          from: fromNode.id,
          to: toNode.id,
          valid: true
        })
        
        this.validateFlow()
      }
    },
    
    getConnectionPath(connection) {
      const fromNode = this.canvasNodes.find(n => n.id === connection.from)
      const toNode = this.canvasNodes.find(n => n.id === connection.to)
      
      if (!fromNode || !toNode) return ''
      
      const x1 = fromNode.x + this.nodeWidth - 10
      const y1 = fromNode.y + 35
      const x2 = toNode.x + 10
      const y2 = toNode.y + 35
      
      // Create curved path
      const midX = (x1 + x2) / 2
      return `M ${x1} ${y1} Q ${midX} ${y1} ${midX} ${(y1 + y2) / 2} Q ${midX} ${y2} ${x2} ${y2}`
    },
    
    selectNode(node) {
      this.selectedNode = node
      this.selectedConnection = null
    },
    
    selectConnection(connection) {
      this.selectedConnection = connection
      this.selectedNode = null
    },
    
    deselectAll() {
      this.selectedNode = null
      this.selectedConnection = null
    },
    
    deleteNode(node) {
      // Remove node
      this.canvasNodes = this.canvasNodes.filter(n => n.id !== node.id)
      
      // Remove associated connections
      this.connections = this.connections.filter(c => 
        c.from !== node.id && c.to !== node.id
      )
      
      if (this.selectedNode?.id === node.id) {
        this.selectedNode = null
      }
      
      this.validateFlow()
    },
    
    updateNode() {
      this.validateFlow()
    },
    
    editNode(node) {
      // Focus on properties panel
      this.selectedNode = node
    },
    
    clearCanvas() {
      this.canvasNodes = []
      this.connections = []
      this.selectedNode = null
      this.selectedConnection = null
      this.validationMessages = []
    },
    
    autoArrange() {
      // Arrange nodes in optimal layout
      const sorted = this.topologicalSort()
      
      sorted.forEach((node, index) => {
        node.x = 100 + (index * 200)
        node.y = 100
      })
    },
    
    validateFlow() {
      this.validationMessages = []
      
      // Check for cycles
      if (this.hasCycles()) {
        this.validationMessages.push({
          id: 'cycle',
          type: 'error',
          text: 'Flow contains cycles which are not allowed'
        })
      }
      
      // Check for disconnected nodes
      const disconnected = this.getDisconnectedNodes()
      if (disconnected.length > 0) {
        this.validationMessages.push({
          id: 'disconnected',
          type: 'warning',
          text: `${disconnected.length} nodes are disconnected`
        })
      }
      
      // Check for critical stages
      const criticalStages = ['typer', 'k8s']
      const missingCritical = criticalStages.filter(stage => 
        !this.canvasNodes.some(n => n.name === stage)
      )
      
      if (missingCritical.length > 0) {
        this.validationMessages.push({
          id: 'missing-critical',
          type: 'warning',
          text: `Missing critical stages: ${missingCritical.join(', ')}`
        })
      }
      
      // Update node validity
      this.canvasNodes.forEach(node => {
        node.valid = !disconnected.includes(node)
      })
      
      // Update connection validity
      this.connections.forEach(connection => {
        connection.valid = !this.hasCycles()
      })
    },
    
    hasCycles() {
      // Use DFS to detect cycles
      const visited = new Set()
      const visiting = new Set()
      
      const dfs = (nodeId) => {
        if (visiting.has(nodeId)) return true
        if (visited.has(nodeId)) return false
        
        visiting.add(nodeId)
        
        const outgoing = this.connections
          .filter(c => c.from === nodeId)
          .map(c => c.to)
        
        for (const nextId of outgoing) {
          if (dfs(nextId)) return true
        }
        
        visiting.delete(nodeId)
        visited.add(nodeId)
        return false
      }
      
      for (const node of this.canvasNodes) {
        if (!visited.has(node.id) && dfs(node.id)) {
          return true
        }
      }
      
      return false
    },
    
    getDisconnectedNodes() {
      if (this.canvasNodes.length <= 1) return []
      
      // Find nodes with no connections
      const connectedNodes = new Set()
      this.connections.forEach(c => {
        connectedNodes.add(c.from)
        connectedNodes.add(c.to)
      })
      
      return this.canvasNodes.filter(n => !connectedNodes.has(n.id))
    },
    
    topologicalSort() {
      if (this.hasCycles()) return []
      
      const inDegree = {}
      const nodes = [...this.canvasNodes]
      
      // Initialize in-degree
      nodes.forEach(node => {
        inDegree[node.id] = 0
      })
      
      // Calculate in-degrees
      this.connections.forEach(conn => {
        inDegree[conn.to]++
      })
      
      // Queue nodes with no incoming edges
      const queue = nodes.filter(node => inDegree[node.id] === 0)
      const sorted = []
      
      while (queue.length > 0) {
        const node = queue.shift()
        sorted.push(node)
        
        // Remove edges from this node
        const outgoing = this.connections.filter(c => c.from === node.id)
        outgoing.forEach(conn => {
          inDegree[conn.to]--
          if (inDegree[conn.to] === 0) {
            const targetNode = nodes.find(n => n.id === conn.to)
            if (targetNode) queue.push(targetNode)
          }
        })
      }
      
      return sorted
    },
    
    optimizeFlow() {
      // Apply 80/20 optimization - keep only critical stages
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      
      // Remove non-critical nodes
      this.canvasNodes = this.canvasNodes.filter(node => 
        criticalStages.includes(node.name)
      )
      
      // Remove orphaned connections
      this.connections = this.connections.filter(conn => 
        this.canvasNodes.some(n => n.id === conn.from) &&
        this.canvasNodes.some(n => n.id === conn.to)
      )
      
      // Auto-arrange remaining nodes
      this.autoArrange()
      this.validateFlow()
    },
    
    saveFlow() {
      const flowData = {
        nodes: this.canvasNodes,
        connections: this.connections,
        timestamp: new Date().toISOString()
      }
      
      localStorage.setItem('pipelineFlow', JSON.stringify(flowData))
      console.log('Flow saved to localStorage')
    },
    
    loadPreset() {
      // Load saved flow or show preset options
      const saved = localStorage.getItem('pipelineFlow')
      if (saved) {
        const flowData = JSON.parse(saved)
        this.canvasNodes = flowData.nodes
        this.connections = flowData.connections
        this.validateFlow()
      } else {
        // Load default 80/20 optimized preset
        this.loadDefaultFlow()
        this.optimizeFlow()
      }
    }
  }
}
</script>

<style scoped>
.pipeline-flow-editor {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
  display: grid;
  grid-template-areas: 
    "toolbar toolbar"
    "canvas properties"
    "analysis validation";
  grid-template-columns: 1fr 300px;
  grid-template-rows: auto 1fr auto;
  gap: 1rem;
  height: 100vh;
}

.pipeline-flow-editor h2 {
  grid-area: toolbar;
  margin-bottom: 1rem;
  font-size: 2rem;
}

.editor-toolbar {
  grid-area: toolbar;
  display: flex;
  gap: 2rem;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.toolbar-section h3 {
  margin-bottom: 1rem;
  color: #888;
}

.stage-palette {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.palette-stage {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 0.75rem;
  border-radius: 8px;
  cursor: grab;
  transition: all 0.3s ease;
  min-width: 80px;
}

.palette-stage:hover {
  transform: scale(1.05);
  filter: brightness(1.2);
}

.palette-stage:active {
  cursor: grabbing;
}

.stage-icon {
  font-size: 1.5rem;
  margin-bottom: 0.25rem;
}

.stage-name {
  font-size: 0.8rem;
  font-weight: 600;
  color: #fff;
}

.action-buttons {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.action-btn {
  padding: 0.5rem 1rem;
  border-radius: 6px;
  font-size: 0.9rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  border: none;
}

.action-btn.clear {
  background: #ff4444;
  color: #fff;
}

.action-btn.arrange {
  background: #0088ff;
  color: #fff;
}

.action-btn.validate {
  background: #00ff88;
  color: #000;
}

.action-btn.optimize {
  background: #ff8800;
  color: #fff;
}

.action-btn.save, .action-btn.load {
  background: #666;
  color: #fff;
}

.action-btn:hover {
  transform: translateY(-2px);
  filter: brightness(1.1);
}

.flow-canvas-container {
  grid-area: canvas;
  background: #1a1a1a;
  border-radius: 16px;
  overflow: hidden;
}

.flow-canvas {
  display: block;
  cursor: default;
}

.stage-node {
  cursor: move;
}

.stage-node.selected .node-bg {
  filter: brightness(1.2);
}

.stage-node.invalid .node-bg {
  stroke: #ff4444;
  stroke-dasharray: 5,5;
}

.node-icon {
  font-size: 1.5rem;
  fill: #fff;
}

.node-label {
  font-size: 0.8rem;
  fill: #fff;
  font-weight: 600;
}

.duration-text {
  font-size: 0.7rem;
  fill: #fff;
}

.input-port, .output-port {
  cursor: crosshair;
  transition: all 0.3s ease;
}

.input-port:hover, .output-port:hover {
  fill: #00ff88;
  r: 7;
}

.delete-btn {
  cursor: pointer;
  transition: all 0.3s ease;
}

.delete-btn:hover {
  fill: #ff6666;
}

.delete-icon {
  font-size: 1rem;
  fill: #fff;
  font-weight: 700;
}

.connection-line {
  cursor: pointer;
  transition: all 0.3s ease;
}

.connection-line:hover {
  stroke-width: 4;
}

.connection-line.invalid {
  stroke: #ff4444;
  stroke-dasharray: 5,5;
}

.properties-panel {
  grid-area: properties;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.property-group {
  margin-bottom: 1rem;
}

.property-group label {
  display: block;
  margin-bottom: 0.5rem;
  color: #888;
  font-weight: 600;
}

.property-group input,
.property-group select {
  width: 100%;
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.flow-analysis {
  grid-area: analysis;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.analysis-metrics {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.metric {
  text-align: center;
}

.metric .label {
  display: block;
  color: #888;
  font-size: 0.8rem;
  margin-bottom: 0.25rem;
}

.metric .value {
  display: block;
  font-size: 1.5rem;
  font-weight: 700;
  color: #00ff88;
}

.metric .value.valid {
  color: #00ff88;
}

.metric .value.invalid {
  color: #ff4444;
}

.path-visualization {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
  margin-top: 0.5rem;
}

.path-node {
  background: #2a2a2a;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.8rem;
}

.path-arrow {
  color: #666;
}

.validation-messages {
  grid-area: validation;
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.validation-message {
  padding: 0.75rem;
  margin-bottom: 0.5rem;
  border-radius: 6px;
  border-left: 4px solid #666;
}

.validation-message.error {
  background: #2a1a1a;
  border-left-color: #ff4444;
}

.validation-message.warning {
  background: #2a2a1a;
  border-left-color: #ff8800;
}
</style>