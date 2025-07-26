<template>
  <div class="pipeline-transformer">
    <!-- Header -->
    <div class="transformer-header">
      <h2 class="text-2xl font-bold flex items-center gap-2">
        <span>🚀</span>
        ULTRATHINK Pipeline Transformer
      </h2>
      <p class="text-gray-600">typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s</p>
    </div>

    <!-- Ontology Input Section -->
    <div class="ontology-input-section">
      <h3 class="section-title">📝 Ontology Configuration</h3>
      
      <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
        <!-- Classes Input -->
        <div class="input-group">
          <label class="input-label">Classes</label>
          <div v-for="(cls, index) in ontology.classes" :key="`class-${index}`" class="flex gap-2 mb-2">
            <input
              v-model="cls.name"
              type="text"
              placeholder="Class name"
              class="input-field flex-1"
            />
            <select v-model="cls.namespace" class="input-field w-32">
              <option value="cyber">cyber</option>
              <option value="custom">custom</option>
            </select>
            <button @click="removeClass(index)" class="btn-danger">×</button>
          </div>
          <button @click="addClass" class="btn-secondary">+ Add Class</button>
        </div>

        <!-- Properties Input -->
        <div class="input-group">
          <label class="input-label">Properties</label>
          <div v-for="(prop, index) in ontology.properties" :key="`prop-${index}`" class="mb-2">
            <div class="flex gap-2">
              <input
                v-model="prop.name"
                type="text"
                placeholder="Property name"
                class="input-field flex-1"
              />
              <button @click="removeProperty(index)" class="btn-danger">×</button>
            </div>
            <div class="flex gap-2 mt-1">
              <input
                v-model="prop.domain"
                type="text"
                placeholder="Domain"
                class="input-field flex-1 text-sm"
              />
              <span class="self-center">→</span>
              <input
                v-model="prop.range"
                type="text"
                placeholder="Range"
                class="input-field flex-1 text-sm"
              />
            </div>
          </div>
          <button @click="addProperty" class="btn-secondary">+ Add Property</button>
        </div>
      </div>
    </div>

    <!-- Transformation Options -->
    <div class="transformation-options">
      <h3 class="section-title">⚙️ Transformation Options</h3>
      
      <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
        <!-- Mode Selection -->
        <div class="option-group">
          <label class="option-label">Transformation Mode</label>
          <select v-model="selectedMode" class="select-field">
            <option v-for="mode in transformationModes" :key="mode.value" :value="mode.value">
              {{ mode.label }}
            </option>
          </select>
          <p class="text-sm text-gray-500 mt-1">{{ selectedModeDescription }}</p>
        </div>

        <!-- Permutation Approach (for custom mode) -->
        <div v-if="selectedMode === 'custom'" class="option-group">
          <label class="option-label">Permutation Approach</label>
          <select v-model="selectedPermutation" class="select-field">
            <option v-for="perm in permutationApproaches" :key="perm.value" :value="perm.value">
              {{ perm.icon }} {{ perm.label }}
            </option>
          </select>
        </div>

        <!-- Output Formats -->
        <div class="option-group">
          <label class="option-label">Output Formats</label>
          <div class="flex flex-wrap gap-2">
            <label v-for="format in outputFormats" :key="format" class="checkbox-label">
              <input
                type="checkbox"
                :value="format"
                v-model="selectedOutputs"
                class="checkbox-field"
              />
              {{ format.toUpperCase() }}
            </label>
          </div>
        </div>
      </div>
    </div>

    <!-- Action Buttons -->
    <div class="action-buttons">
      <button @click="analyzeOntology" :disabled="isProcessing" class="btn-primary">
        📊 Analyze Optimal Path
      </button>
      <button @click="executeTransformation" :disabled="isProcessing" class="btn-success">
        🚀 Execute Transformation
      </button>
      <button @click="visualizePipeline" :disabled="isProcessing" class="btn-info">
        🎨 Visualize Pipeline
      </button>
    </div>

    <!-- Analysis Results -->
    <div v-if="analysisState.optimalPath" class="analysis-results">
      <h3 class="section-title">📊 Analysis Results</h3>
      <div class="result-card">
        <div class="result-header">
          <span class="font-semibold">Optimal Approach:</span>
          <span class="text-lg">{{ formatApproachName(analysisState.optimalPath.approach) }}</span>
        </div>
        <div class="result-metrics">
          <div class="metric">
            <span class="metric-label">Confidence:</span>
            <span class="metric-value">{{ (analysisState.optimalPath.confidence * 100).toFixed(0) }}%</span>
          </div>
          <div class="metric">
            <span class="metric-label">Expected Time:</span>
            <span class="metric-value">{{ analysisState.optimalPath.expectedPerformance.timeMs }}ms</span>
          </div>
          <div class="metric">
            <span class="metric-label">Performance Score:</span>
            <span class="metric-value">{{ analysisState.optimalPath.expectedPerformance.score }}/100</span>
          </div>
        </div>
        <div v-if="analysisState.alternatives.length > 0" class="alternatives">
          <span class="text-sm font-semibold">Alternatives:</span>
          <div class="flex gap-2 mt-1">
            <button
              v-for="alt in analysisState.alternatives"
              :key="alt.approach"
              @click="selectedPermutation = alt.approach"
              class="btn-outline-sm"
            >
              {{ alt.label }}
            </button>
          </div>
        </div>
      </div>
    </div>

    <!-- Progress Visualization -->
    <div v-if="transformationState.isProcessing" class="progress-section">
      <h3 class="section-title">⚡ Transformation Progress</h3>
      <div class="progress-bar-container">
        <div class="progress-bar" :style="{ width: `${transformationState.progress}%` }"></div>
      </div>
      <p class="progress-stage">{{ transformationState.currentStage }}</p>
    </div>

    <!-- Pipeline Visualization -->
    <div v-if="visualizationState.stages.length > 0" class="pipeline-visualization">
      <h3 class="section-title">🎯 Pipeline Stages</h3>
      <div class="pipeline-stages">
        <div
          v-for="(stage, index) in visualizationState.stages"
          :key="`stage-${index}`"
          :class="['pipeline-stage', { active: index === visualizationState.currentStageIndex }]"
        >
          <div class="stage-icon">{{ getStageIcon(stage.stage) }}</div>
          <div class="stage-info">
            <div class="stage-name">{{ stage.stage }}</div>
            <div class="stage-description">{{ stage.description }}</div>
          </div>
          <div class="stage-progress">{{ stage.progress }}%</div>
        </div>
      </div>
    </div>

    <!-- Results Display -->
    <div v-if="transformationState.result" class="results-section">
      <h3 class="section-title">✅ Transformation Results</h3>
      <div class="result-tabs">
        <button
          v-for="output in transformationState.result.outputs"
          :key="output"
          @click="selectedResultTab = output"
          :class="['tab', { active: selectedResultTab === output }]"
        >
          {{ output.toUpperCase() }}
        </button>
      </div>
      <div class="result-content">
        <pre>{{ formatResultContent(selectedResultTab) }}</pre>
      </div>
    </div>

    <!-- Error Display -->
    <div v-if="transformationState.error" class="error-section">
      <h3 class="text-red-600 font-semibold">❌ Error</h3>
      <p class="error-message">{{ transformationState.error }}</p>
    </div>
  </div>
</template>

<script setup>
// 🚀 ULTRATHINK SWARM 80/20: Pipeline Transformer Component
// Pure JavaScript implementation for Nuxt UI

import { ref, computed, reactive } from 'vue'

// Use the pipeline transform composable
const {
  transformationState,
  analysisState,
  visualizationState,
  transformOntology,
  analyzeOntology,
  executePermutation,
  startVisualization,
  permutationApproaches,
  transformationModes
} = usePipelineTransform()

// Component state
const selectedMode = ref('auto')
const selectedPermutation = ref('ultra_bypass')
const selectedOutputs = ref(['ash', 'reactor'])
const selectedResultTab = ref('ash')
const isProcessing = ref(false)

// Ontology data
const ontology = reactive({
  namespaces: [
    { prefix: 'cyber', uri: 'http://cybersecurity.org/' }
  ],
  classes: [
    { name: 'Asset', namespace: 'cyber' },
    { name: 'Threat', namespace: 'cyber' },
    { name: 'Vulnerability', namespace: 'cyber' }
  ],
  properties: [
    { name: 'exploits', namespace: 'cyber', domain: 'cyber:Threat', range: 'cyber:Vulnerability' },
    { name: 'protects', namespace: 'cyber', domain: 'cyber:SecurityControl', range: 'cyber:Asset' }
  ],
  relationships: []
})

// Available output formats
const outputFormats = ['ash', 'reactor', 'k8s', 'dspy', 'erlang']

// Computed properties
const selectedModeDescription = computed(() => {
  const mode = transformationModes.find(m => m.value === selectedMode.value)
  return mode ? mode.description : ''
})

// Methods
const addClass = () => {
  ontology.classes.push({ name: '', namespace: 'cyber' })
}

const removeClass = (index) => {
  ontology.classes.splice(index, 1)
}

const addProperty = () => {
  ontology.properties.push({
    name: '',
    namespace: 'cyber',
    domain: '',
    range: ''
  })
}

const removeProperty = (index) => {
  ontology.properties.splice(index, 1)
}

const analyzeOntology = async () => {
  isProcessing.value = true
  try {
    await analyzeOntology(ontology)
  } catch (error) {
    console.error('Analysis failed:', error)
  } finally {
    isProcessing.value = false
  }
}

const executeTransformation = async () => {
  isProcessing.value = true
  try {
    const options = {
      mode: selectedMode.value,
      outputs: selectedOutputs.value
    }
    
    if (selectedMode.value === 'custom') {
      await executePermutation(selectedPermutation.value, ontology)
    } else {
      await transformOntology(ontology, options)
    }
  } catch (error) {
    console.error('Transformation failed:', error)
  } finally {
    isProcessing.value = false
  }
}

const visualizePipeline = async () => {
  isProcessing.value = true
  try {
    await startVisualization(ontology)
  } catch (error) {
    console.error('Visualization failed:', error)
  } finally {
    isProcessing.value = false
  }
}

const formatApproachName = (approach) => {
  return approach.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())
}

const getStageIcon = (stage) => {
  const icons = {
    typer: '📝',
    turtle: '🐢',
    ttl2dspy: '🔄',
    bitactor: '🎭',
    erlang: '📡',
    ash: '🔥',
    reactor: '⚛️',
    k8s: '☸️',
    complete: '✅'
  }
  return icons[stage] || '📦'
}

const formatResultContent = (tab) => {
  if (!transformationState.result || !transformationState.result.data) return ''
  
  const data = transformationState.result.data
  if (data[tab]) {
    return JSON.stringify(data[tab], null, 2)
  }
  
  return JSON.stringify(data, null, 2)
}
</script>

<style scoped>
/* Component styles */
.pipeline-transformer {
  @apply max-w-6xl mx-auto p-6 space-y-6;
}

.transformer-header {
  @apply text-center mb-8;
}

.section-title {
  @apply text-xl font-semibold mb-4 flex items-center gap-2;
}

.input-group {
  @apply space-y-2;
}

.input-label {
  @apply block text-sm font-medium text-gray-700 mb-2;
}

.input-field {
  @apply px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500;
}

.select-field {
  @apply w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500;
}

.checkbox-label {
  @apply flex items-center gap-2 px-3 py-1 bg-gray-100 rounded cursor-pointer hover:bg-gray-200;
}

.checkbox-field {
  @apply w-4 h-4 text-blue-600;
}

.btn-primary {
  @apply px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 disabled:opacity-50;
}

.btn-success {
  @apply px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700 disabled:opacity-50;
}

.btn-info {
  @apply px-4 py-2 bg-cyan-600 text-white rounded-md hover:bg-cyan-700 disabled:opacity-50;
}

.btn-secondary {
  @apply px-3 py-1 bg-gray-200 text-gray-700 rounded hover:bg-gray-300;
}

.btn-danger {
  @apply px-2 py-1 bg-red-500 text-white rounded hover:bg-red-600;
}

.btn-outline-sm {
  @apply px-3 py-1 border border-gray-300 rounded text-sm hover:bg-gray-100;
}

.action-buttons {
  @apply flex gap-4 justify-center;
}

.result-card {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.result-header {
  @apply flex justify-between items-center mb-4;
}

.result-metrics {
  @apply grid grid-cols-3 gap-4;
}

.metric {
  @apply text-center;
}

.metric-label {
  @apply block text-sm text-gray-600;
}

.metric-value {
  @apply block text-xl font-semibold;
}

.progress-section {
  @apply bg-gray-50 p-6 rounded-lg;
}

.progress-bar-container {
  @apply w-full bg-gray-200 rounded-full h-4 overflow-hidden;
}

.progress-bar {
  @apply bg-blue-600 h-full transition-all duration-300;
}

.progress-stage {
  @apply text-center mt-2 text-gray-600;
}

.pipeline-visualization {
  @apply bg-gray-50 p-6 rounded-lg;
}

.pipeline-stages {
  @apply space-y-2;
}

.pipeline-stage {
  @apply flex items-center gap-4 p-3 bg-white rounded-lg transition-all duration-300;
}

.pipeline-stage.active {
  @apply bg-blue-50 border-2 border-blue-400;
}

.stage-icon {
  @apply text-2xl;
}

.stage-info {
  @apply flex-1;
}

.stage-name {
  @apply font-semibold;
}

.stage-description {
  @apply text-sm text-gray-600;
}

.stage-progress {
  @apply text-sm font-semibold text-gray-500;
}

.results-section {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.result-tabs {
  @apply flex gap-2 mb-4;
}

.tab {
  @apply px-4 py-2 bg-gray-100 rounded-t-lg cursor-pointer hover:bg-gray-200;
}

.tab.active {
  @apply bg-blue-500 text-white;
}

.result-content {
  @apply bg-gray-50 p-4 rounded-lg overflow-auto max-h-96;
}

.error-section {
  @apply bg-red-50 p-4 rounded-lg border border-red-200;
}

.error-message {
  @apply text-red-700;
}

/* Grid layouts */
.ontology-input-section, .transformation-options {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.option-group {
  @apply space-y-2;
}

.option-label {
  @apply block text-sm font-medium text-gray-700;
}

.alternatives {
  @apply mt-4 pt-4 border-t border-gray-200;
}
</style>