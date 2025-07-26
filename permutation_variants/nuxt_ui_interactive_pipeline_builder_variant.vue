<!--
BitActor Nuxt UI Interactive Pipeline Builder Variant
Drag-and-drop pipeline construction with real-time validation
Visual workflow designer for the 8-stage BitActor pipeline
No TypeScript - Pure JavaScript with advanced drag/drop
-->

<template>
  <div class="pipeline-builder">
    <header class="builder-header">
      <h1 class="header-title">
        <span class="title-icon">🔧</span>
        Interactive Pipeline Builder
      </h1>
      <div class="header-actions">
        <button @click="validatePipeline" class="action-btn">✓ Validate</button>
        <button @click="savePipeline" class="action-btn primary">💾 Save</button>
        <button @click="runPipeline" class="action-btn success">▶️ Run</button>
      </div>
    </header>

    <div class="builder-layout">
      <!-- Stage Palette -->
      <aside class="stage-palette">
        <h3>Available Stages</h3>
        <div class="palette-stages">
          <div v-for="stage in availableStages" :key="stage.name"
               class="palette-stage" draggable="true"
               @dragstart="startDrag($event, stage)">
            <span class="stage-icon">{{ stage.icon }}</span>
            <span class="stage-name">{{ stage.name }}</span>
            <span class="stage-ttl">{{ stage.ttlBudget }}ms</span>
          </div>
        </div>
      </aside>

      <!-- Pipeline Canvas -->
      <main class="pipeline-canvas">
        <div class="canvas-header">
          <h3>Pipeline Design</h3>
          <div class="canvas-controls">
            <button @click="clearCanvas" class="clear-btn">🗑️ Clear</button>
            <button @click="autoArrange" class="arrange-btn">📐 Auto Arrange</button>
          </div>
        </div>
        
        <div class="canvas-area" @drop="dropStage" @dragover.prevent>
          <div v-for="(stage, index) in pipelineStages" :key="stage.id"
               class="canvas-stage" :style="stage.position"
               draggable="true" @dragstart="startMoveDrag($event, stage)">
            
            <div class="stage-container" :class="stage.status">
              <div class="stage-header">
                <span class="stage-icon">{{ stage.icon }}</span>
                <span class="stage-name">{{ stage.name }}</span>
                <button @click="removeStage(stage)" class="remove-btn">✕</button>
              </div>
              
              <div class="stage-config">
                <label>TTL Budget:</label>
                <input v-model.number="stage.ttlBudget" type="number" 
                       min="0.1" max="10" step="0.1" class="ttl-input">
                <span>ms</span>
              </div>
              
              <div class="stage-connections">
                <div class="input-port" @drop="connectStages" @dragover.prevent"></div>
                <div class="output-port" draggable="true" @dragstart="startConnectionDrag($event, stage)"></div>
              </div>
            </div>
          </div>
          
          <!-- Connection Lines -->
          <svg class="connection-overlay" :viewBox="`0 0 ${canvasWidth} ${canvasHeight}`">
            <path v-for="connection in connections" :key="connection.id"
                  :d="connection.path" stroke="#4a90e2" stroke-width="2" 
                  fill="none" marker-end="url(#arrowhead)"/>
            
            <defs>
              <marker id="arrowhead" markerWidth="10" markerHeight="7" 
                     refX="9" refY="3.5" orient="auto">
                <polygon points="0 0, 10 3.5, 0 7" fill="#4a90e2"/>
              </marker>
            </defs>
          </svg>
        </div>
      </main>

      <!-- Properties Panel -->
      <aside class="properties-panel">
        <h3>Pipeline Properties</h3>
        
        <div class="pipeline-summary">
          <div class="summary-item">
            <label>Total Stages:</label>
            <span>{{ pipelineStages.length }}</span>
          </div>
          <div class="summary-item">
            <label>Total TTL Budget:</label>
            <span>{{ totalTTLBudget }}ms</span>
          </div>
          <div class="summary-item">
            <label>Expected Duration:</label>
            <span>{{ estimatedDuration }}ms</span>
          </div>
          <div class="summary-item">
            <label>Validation Status:</label>
            <span :class="validationStatus">{{ validationStatus }}</span>
          </div>
        </div>

        <div v-if="selectedStage" class="stage-details">
          <h4>{{ selectedStage.name }} Configuration</h4>
          <div class="config-form">
            <div class="form-group">
              <label>TTL Budget (ms):</label>
              <input v-model.number="selectedStage.ttlBudget" type="number" class="form-input">
            </div>
            <div class="form-group">
              <label>Priority:</label>
              <select v-model="selectedStage.priority" class="form-select">
                <option value="low">Low</option>
                <option value="medium">Medium</option>
                <option value="high">High</option>
                <option value="critical">Critical</option>
              </select>
            </div>
            <div class="form-group">
              <label>Parallel Execution:</label>
              <input v-model="selectedStage.parallelExecution" type="checkbox" class="form-checkbox">
            </div>
          </div>
        </div>

        <div class="validation-results">
          <h4>Validation Results</h4>
          <div v-for="result in validationResults" :key="result.id"
               class="validation-item" :class="result.severity">
            <span class="validation-icon">{{ result.icon }}</span>
            <span class="validation-message">{{ result.message }}</span>
          </div>
        </div>
      </aside>
    </div>
  </div>
</template>

<script>
export default {
  name: 'NuxtUIInteractivePipelineBuilderVariant',
  
  data() {
    return {
      canvasWidth: 800,
      canvasHeight: 600,
      selectedStage: null,
      draggedStage: null,
      connectionDrag: null,
      
      availableStages: [
        { name: 'Typer', icon: '📝', ttlBudget: 1.0, type: 'input' },
        { name: 'Turtle', icon: '🐢', ttlBudget: 1.0, type: 'transform' },
        { name: 'TTL2DSPy', icon: '🔍', ttlBudget: 1.0, type: 'analyze' },
        { name: 'BitActor', icon: '⚡', ttlBudget: 1.0, type: 'process' },
        { name: 'Erlang', icon: '🔧', ttlBudget: 1.0, type: 'execute' },
        { name: 'Ash', icon: '🌊', ttlBudget: 1.0, type: 'persist' },
        { name: 'Reactor', icon: '🔄', ttlBudget: 1.0, type: 'orchestrate' },
        { name: 'K8s', icon: '☸️', ttlBudget: 1.0, type: 'deploy' }
      ],
      
      pipelineStages: [],
      connections: [],
      
      validationResults: [
        {
          id: 'val_001',
          severity: 'success',
          icon: '✓',
          message: 'Pipeline structure is valid'
        }
      ]
    }
  },
  
  computed: {
    totalTTLBudget() {
      return this.pipelineStages.reduce((sum, stage) => sum + stage.ttlBudget, 0).toFixed(1)
    },
    
    estimatedDuration() {
      return (this.totalTTLBudget * 0.8).toFixed(1)
    },
    
    validationStatus() {
      const hasErrors = this.validationResults.some(r => r.severity === 'error')
      const hasWarnings = this.validationResults.some(r => r.severity === 'warning')
      
      if (hasErrors) return 'error'
      if (hasWarnings) return 'warning'
      return 'valid'
    }
  },
  
  methods: {
    startDrag(event, stage) {
      this.draggedStage = stage
      event.dataTransfer.setData('text/plain', '')
    },
    
    startMoveDrag(event, stage) {
      this.draggedStage = stage
      event.dataTransfer.setData('text/plain', '')
    },
    
    startConnectionDrag(event, stage) {
      this.connectionDrag = stage
      event.dataTransfer.setData('text/plain', '')
    },
    
    dropStage(event) {
      if (!this.draggedStage) return
      
      const rect = event.currentTarget.getBoundingClientRect()
      const x = event.clientX - rect.left
      const y = event.clientY - rect.top
      
      if (this.pipelineStages.find(s => s.id === this.draggedStage.id)) {
        // Moving existing stage
        const stage = this.pipelineStages.find(s => s.id === this.draggedStage.id)
        stage.position = {
          left: `${x - 75}px`,
          top: `${y - 50}px`
        }
      } else {
        // Adding new stage
        const newStage = {
          ...this.draggedStage,
          id: `stage_${Date.now()}`,
          position: {
            left: `${x - 75}px`,
            top: `${y - 50}px`
          },
          status: 'configured',
          priority: 'medium',
          parallelExecution: false
        }
        
        this.pipelineStages.push(newStage)
      }
      
      this.draggedStage = null
    },
    
    connectStages(event) {
      if (!this.connectionDrag) return
      
      // Implementation for connecting stages
      console.log('Connecting stages')
      this.connectionDrag = null
    },
    
    removeStage(stage) {
      const index = this.pipelineStages.indexOf(stage)
      if (index > -1) {
        this.pipelineStages.splice(index, 1)
      }
    },
    
    clearCanvas() {
      this.pipelineStages = []
      this.connections = []
      this.selectedStage = null
    },
    
    autoArrange() {
      this.pipelineStages.forEach((stage, index) => {
        stage.position = {
          left: `${100 + index * 150}px`,
          top: `${200}px`
        }
      })
    },
    
    validatePipeline() {
      this.validationResults = []
      
      if (this.pipelineStages.length === 0) {
        this.validationResults.push({
          id: 'val_empty',
          severity: 'error',
          icon: '❌',
          message: 'Pipeline is empty'
        })
      } else {
        this.validationResults.push({
          id: 'val_success',
          severity: 'success',
          icon: '✓',
          message: 'Pipeline validation passed'
        })
      }
    },
    
    savePipeline() {
      const pipeline = {
        stages: this.pipelineStages,
        connections: this.connections,
        metadata: {
          totalTTL: this.totalTTLBudget,
          created: new Date().toISOString()
        }
      }
      
      console.log('Saving pipeline:', pipeline)
    },
    
    runPipeline() {
      console.log('Running pipeline with', this.pipelineStages.length, 'stages')
    }
  }
}
</script>

<style scoped>
.pipeline-builder {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

.builder-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 20px;
}

.header-title {
  font-size: 2.2rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 15px;
}

.header-actions {
  display: flex;
  gap: 15px;
}

.action-btn {
  padding: 10px 20px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.action-btn.primary {
  background: #4a90e2;
}

.action-btn.success {
  background: #5cb85c;
}

.builder-layout {
  display: grid;
  grid-template-columns: 250px 1fr 300px;
  gap: 20px;
  height: calc(100vh - 140px);
}

.stage-palette, .properties-panel {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  padding: 20px;
  overflow-y: auto;
}

.stage-palette h3, .properties-panel h3 {
  margin-top: 0;
  color: #4a90e2;
}

.palette-stages {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.palette-stage {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px;
  background: rgba(255, 255, 255, 0.1);
  border-radius: 8px;
  cursor: grab;
  transition: all 0.3s ease;
}

.palette-stage:hover {
  background: rgba(255, 255, 255, 0.2);
  transform: translateY(-2px);
}

.stage-icon {
  font-size: 1.2rem;
}

.stage-name {
  font-weight: 500;
  flex: 1;
}

.stage-ttl {
  font-size: 0.8rem;
  opacity: 0.7;
}

.pipeline-canvas {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 20px;
  display: flex;
  flex-direction: column;
}

.canvas-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.canvas-controls {
  display: flex;
  gap: 10px;
}

.clear-btn, .arrange-btn {
  padding: 6px 12px;
  border: none;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  font-size: 0.9rem;
}

.canvas-area {
  flex: 1;
  position: relative;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 12px;
  border: 2px dashed rgba(255, 255, 255, 0.3);
  overflow: hidden;
}

.canvas-stage {
  position: absolute;
  cursor: move;
}

.stage-container {
  width: 150px;
  background: rgba(255, 255, 255, 0.1);
  border-radius: 10px;
  padding: 15px;
  border: 2px solid;
}

.stage-container.configured {
  border-color: #5cb85c;
}

.stage-container.error {
  border-color: #d9534f;
}

.stage-header {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 10px;
}

.remove-btn {
  background: none;
  border: none;
  color: #d9534f;
  cursor: pointer;
  margin-left: auto;
  font-size: 0.8rem;
}

.stage-config {
  margin-bottom: 10px;
}

.stage-config label {
  font-size: 0.8rem;
  opacity: 0.8;
}

.ttl-input {
  width: 60px;
  padding: 2px 4px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.8rem;
}

.stage-connections {
  display: flex;
  justify-content: space-between;
}

.input-port, .output-port {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: #4a90e2;
  cursor: pointer;
}

.connection-overlay {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
}

.pipeline-summary {
  margin-bottom: 20px;
}

.summary-item {
  display: flex;
  justify-content: space-between;
  margin-bottom: 8px;
}

.summary-item label {
  opacity: 0.8;
}

.summary-item span.valid {
  color: #5cb85c;
}

.summary-item span.warning {
  color: #f0ad4e;
}

.summary-item span.error {
  color: #d9534f;
}

.stage-details {
  margin-bottom: 20px;
  padding: 15px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

.config-form {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.form-group label {
  font-size: 0.9rem;
  opacity: 0.9;
}

.form-input, .form-select {
  padding: 6px 8px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
}

.form-checkbox {
  width: auto;
}

.validation-results {
  border-top: 1px solid rgba(255, 255, 255, 0.2);
  padding-top: 15px;
}

.validation-item {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 8px;
  padding: 6px;
  border-radius: 4px;
}

.validation-item.success {
  background: rgba(92, 184, 92, 0.2);
}

.validation-item.warning {
  background: rgba(240, 173, 78, 0.2);
}

.validation-item.error {
  background: rgba(217, 83, 79, 0.2);
}

.validation-icon {
  font-size: 0.9rem;
}

.validation-message {
  font-size: 0.8rem;
}

@media (max-width: 1200px) {
  .builder-layout {
    grid-template-columns: 1fr;
    grid-template-rows: auto 1fr auto;
  }
}
</style>