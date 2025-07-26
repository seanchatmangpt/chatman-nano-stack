<template>
  <div class="pipeline-visualizer">
    <h3 class="visualizer-title">🎯 Pipeline Flow Visualization</h3>
    
    <!-- Pipeline Flow Diagram -->
    <div class="pipeline-flow">
      <div 
        v-for="(stage, index) in pipelineStages" 
        :key="stage.id"
        class="flow-container"
      >
        <!-- Stage Node -->
        <div 
          :class="[
            'stage-node',
            { 
              'active': currentStageIndex === index,
              'completed': currentStageIndex > index,
              'pending': currentStageIndex < index
            }
          ]"
          @click="selectStage(index)"
        >
          <div class="stage-icon">{{ stage.icon }}</div>
          <div class="stage-label">{{ stage.name }}</div>
          <div v-if="stage.duration" class="stage-duration">{{ stage.duration }}ms</div>
        </div>
        
        <!-- Connector Arrow -->
        <div 
          v-if="index < pipelineStages.length - 1"
          :class="[
            'connector-arrow',
            { 'active': currentStageIndex > index }
          ]"
        >
          <svg width="40" height="40" viewBox="0 0 40 40">
            <path 
              d="M 10 20 L 25 20 M 25 20 L 20 15 M 25 20 L 20 25" 
              stroke="currentColor" 
              stroke-width="2" 
              fill="none"
            />
          </svg>
        </div>
      </div>
    </div>
    
    <!-- Stage Details -->
    <div v-if="selectedStage" class="stage-details">
      <h4 class="detail-title">{{ selectedStage.name }} Details</h4>
      
      <div class="detail-grid">
        <div class="detail-item">
          <span class="detail-label">Status:</span>
          <span :class="['detail-value', getStatusClass(selectedStage)]">
            {{ getStageStatus(selectedStage) }}
          </span>
        </div>
        
        <div class="detail-item">
          <span class="detail-label">Input:</span>
          <span class="detail-value">{{ selectedStage.input }}</span>
        </div>
        
        <div class="detail-item">
          <span class="detail-label">Output:</span>
          <span class="detail-value">{{ selectedStage.output }}</span>
        </div>
        
        <div v-if="selectedStage.duration" class="detail-item">
          <span class="detail-label">Duration:</span>
          <span class="detail-value">{{ selectedStage.duration }}ms</span>
        </div>
      </div>
      
      <!-- Permutation Options for Stage -->
      <div v-if="selectedStage.permutations" class="stage-permutations">
        <h5 class="permutation-title">Available Permutations:</h5>
        <div class="permutation-grid">
          <button
            v-for="perm in selectedStage.permutations"
            :key="perm.id"
            @click="selectPermutation(selectedStage, perm)"
            :class="[
              'permutation-option',
              { 'selected': isPermutationSelected(selectedStage, perm) }
            ]"
          >
            <span class="perm-icon">{{ perm.icon }}</span>
            <span class="perm-name">{{ perm.name }}</span>
            <span class="perm-speed">{{ perm.speed }}</span>
          </button>
        </div>
      </div>
    </div>
    
    <!-- Real-time Metrics -->
    <div class="metrics-panel">
      <h4 class="metrics-title">📊 Pipeline Metrics</h4>
      
      <div class="metrics-grid">
        <div class="metric-card">
          <div class="metric-icon">⚡</div>
          <div class="metric-info">
            <div class="metric-value">{{ totalDuration }}ms</div>
            <div class="metric-label">Total Duration</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">✅</div>
          <div class="metric-info">
            <div class="metric-value">{{ completedStages }}/{{ pipelineStages.length }}</div>
            <div class="metric-label">Completed Stages</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">🎯</div>
          <div class="metric-info">
            <div class="metric-value">{{ successRate }}%</div>
            <div class="metric-label">Success Rate</div>
          </div>
        </div>
        
        <div class="metric-card">
          <div class="metric-icon">🔄</div>
          <div class="metric-info">
            <div class="metric-value">{{ activePermutations }}</div>
            <div class="metric-label">Active Permutations</div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Output Preview -->
    <div v-if="outputs.length > 0" class="output-preview">
      <h4 class="output-title">📦 Generated Outputs</h4>
      
      <div class="output-tabs">
        <button
          v-for="output in outputs"
          :key="output.type"
          @click="selectedOutput = output.type"
          :class="['output-tab', { active: selectedOutput === output.type }]"
        >
          {{ output.icon }} {{ output.type }}
        </button>
      </div>
      
      <div class="output-content">
        <pre>{{ getOutputContent(selectedOutput) }}</pre>
      </div>
    </div>
  </div>
</template>

<script setup>
// 🎨 ULTRATHINK SWARM 80/20: Pipeline Visualizer Component
// JavaScript visualization for pipeline stages and permutations

import { ref, computed, watch } from 'vue'

// Props
const props = defineProps({
  stages: {
    type: Array,
    default: () => []
  },
  currentStage: {
    type: Number,
    default: -1
  },
  results: {
    type: Object,
    default: () => ({})
  }
})

// Emit events
const emit = defineEmits(['stage-selected', 'permutation-selected'])

// Component state
const selectedStage = ref(null)
const selectedStageIndex = ref(0)
const selectedOutput = ref('ash')
const selectedPermutations = ref({})

// Pipeline stages configuration
const pipelineStages = ref([
  {
    id: 'typer',
    name: 'Typer',
    icon: '📝',
    input: 'Raw Ontology',
    output: 'TypedOntology',
    permutations: [
      { id: 'direct', name: 'Direct', icon: '➡️', speed: 'fast' },
      { id: 'validated', name: 'Validated', icon: '✓', speed: 'normal' }
    ]
  },
  {
    id: 'turtle',
    name: 'Turtle',
    icon: '🐢',
    input: 'TypedOntology',
    output: 'TTL',
    permutations: [
      { id: 'standard', name: 'Standard', icon: '📄', speed: 'normal' },
      { id: 'optimized', name: 'Optimized', icon: '⚡', speed: 'fast' }
    ]
  },
  {
    id: 'ttl2dspy',
    name: 'TTL→DSPy',
    icon: '🔄',
    input: 'TTL',
    output: 'DSPy Code',
    permutations: [
      { id: 'direct', name: 'Direct', icon: '➡️', speed: 'fast' },
      { id: 'enhanced', name: 'Enhanced', icon: '✨', speed: 'normal' }
    ]
  },
  {
    id: 'bitactor',
    name: 'BitActor',
    icon: '🎭',
    input: 'DSPy Code',
    output: 'Actor Spec',
    permutations: [
      { id: 'minimal', name: 'Minimal', icon: '📦', speed: 'fast' },
      { id: 'full', name: 'Full', icon: '📚', speed: 'slow' }
    ]
  },
  {
    id: 'erlang',
    name: 'Erlang',
    icon: '📡',
    input: 'Actor Spec',
    output: 'Erlang Code',
    permutations: [
      { id: 'otp', name: 'OTP', icon: '🏗️', speed: 'normal' },
      { id: 'bare', name: 'Bare', icon: '🔧', speed: 'fast' }
    ]
  },
  {
    id: 'ash',
    name: 'Ash',
    icon: '🔥',
    input: 'Erlang Code',
    output: 'Ash Resources',
    permutations: [
      { id: 'resources', name: 'Resources', icon: '📋', speed: 'normal' },
      { id: 'api', name: 'Full API', icon: '🌐', speed: 'slow' }
    ]
  },
  {
    id: 'reactor',
    name: 'Reactor',
    icon: '⚛️',
    input: 'Ash Resources',
    output: 'Workflows',
    permutations: [
      { id: 'basic', name: 'Basic', icon: '📊', speed: 'fast' },
      { id: 'complex', name: 'Complex', icon: '🔀', speed: 'slow' }
    ]
  },
  {
    id: 'k8s',
    name: 'K8s',
    icon: '☸️',
    input: 'Workflows',
    output: 'K8s Manifests',
    permutations: [
      { id: 'deployment', name: 'Deployment', icon: '🚀', speed: 'fast' },
      { id: 'full-stack', name: 'Full Stack', icon: '🏭', speed: 'slow' }
    ]
  }
])

// Available outputs
const outputs = ref([
  { type: 'ash', icon: '🔥', content: null },
  { type: 'reactor', icon: '⚛️', content: null },
  { type: 'k8s', icon: '☸️', content: null },
  { type: 'dspy', icon: '🐍', content: null },
  { type: 'erlang', icon: '📡', content: null }
])

// Computed properties
const currentStageIndex = computed(() => props.currentStage)

const completedStages = computed(() => {
  return pipelineStages.value.filter((_, index) => index < currentStageIndex.value).length
})

const totalDuration = computed(() => {
  return pipelineStages.value
    .filter(stage => stage.duration)
    .reduce((sum, stage) => sum + stage.duration, 0)
})

const successRate = computed(() => {
  const completed = completedStages.value
  return completed > 0 ? Math.round((completed / pipelineStages.value.length) * 100) : 0
})

const activePermutations = computed(() => {
  return Object.keys(selectedPermutations.value).length
})

// Methods
const selectStage = (index) => {
  selectedStageIndex.value = index
  selectedStage.value = pipelineStages.value[index]
  emit('stage-selected', selectedStage.value)
}

const selectPermutation = (stage, permutation) => {
  selectedPermutations.value[stage.id] = permutation.id
  emit('permutation-selected', { stage: stage.id, permutation: permutation.id })
}

const isPermutationSelected = (stage, permutation) => {
  return selectedPermutations.value[stage.id] === permutation.id
}

const getStageStatus = (stage) => {
  const index = pipelineStages.value.indexOf(stage)
  if (currentStageIndex.value > index) return 'Completed'
  if (currentStageIndex.value === index) return 'Processing'
  return 'Pending'
}

const getStatusClass = (stage) => {
  const status = getStageStatus(stage)
  return {
    'status-completed': status === 'Completed',
    'status-processing': status === 'Processing',
    'status-pending': status === 'Pending'
  }
}

const getOutputContent = (outputType) => {
  const output = outputs.value.find(o => o.type === outputType)
  if (!output || !output.content) {
    return `// ${outputType.toUpperCase()} output will appear here after transformation`
  }
  return output.content
}

// Update outputs when results change
watch(() => props.results, (newResults) => {
  if (newResults && newResults.outputs) {
    outputs.value.forEach(output => {
      if (newResults.outputs[output.type]) {
        output.content = JSON.stringify(newResults.outputs[output.type], null, 2)
      }
    })
  }
}, { deep: true })

// Initialize with first stage selected
onMounted(() => {
  if (pipelineStages.value.length > 0) {
    selectStage(0)
  }
})
</script>

<style scoped>
.pipeline-visualizer {
  @apply space-y-6;
}

.visualizer-title {
  @apply text-xl font-bold mb-4;
}

/* Pipeline Flow */
.pipeline-flow {
  @apply flex items-center justify-between overflow-x-auto py-8 px-4 bg-gray-50 rounded-lg;
}

.flow-container {
  @apply flex items-center;
}

.stage-node {
  @apply relative flex flex-col items-center cursor-pointer transition-all duration-300;
  @apply p-4 rounded-lg min-w-[100px];
}

.stage-node.completed {
  @apply bg-green-100 text-green-800;
}

.stage-node.active {
  @apply bg-blue-100 text-blue-800 scale-110 shadow-lg;
}

.stage-node.pending {
  @apply bg-gray-100 text-gray-600;
}

.stage-icon {
  @apply text-3xl mb-2;
}

.stage-label {
  @apply text-sm font-semibold;
}

.stage-duration {
  @apply text-xs text-gray-600 mt-1;
}

.connector-arrow {
  @apply text-gray-400 transition-colors duration-300;
}

.connector-arrow.active {
  @apply text-green-600;
}

/* Stage Details */
.stage-details {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.detail-title {
  @apply text-lg font-semibold mb-4;
}

.detail-grid {
  @apply grid grid-cols-2 gap-4 mb-6;
}

.detail-item {
  @apply flex flex-col;
}

.detail-label {
  @apply text-sm text-gray-600;
}

.detail-value {
  @apply font-medium;
}

.status-completed {
  @apply text-green-600;
}

.status-processing {
  @apply text-blue-600;
}

.status-pending {
  @apply text-gray-500;
}

/* Permutations */
.stage-permutations {
  @apply mt-6 pt-6 border-t;
}

.permutation-title {
  @apply text-sm font-semibold mb-3;
}

.permutation-grid {
  @apply grid grid-cols-2 gap-2;
}

.permutation-option {
  @apply flex items-center gap-2 p-3 rounded-lg border-2 border-gray-200;
  @apply hover:border-blue-400 transition-all duration-200 cursor-pointer;
}

.permutation-option.selected {
  @apply border-blue-500 bg-blue-50;
}

.perm-icon {
  @apply text-xl;
}

.perm-name {
  @apply flex-1 font-medium;
}

.perm-speed {
  @apply text-xs text-gray-600;
}

/* Metrics */
.metrics-panel {
  @apply bg-gray-50 p-6 rounded-lg;
}

.metrics-title {
  @apply text-lg font-semibold mb-4;
}

.metrics-grid {
  @apply grid grid-cols-2 md:grid-cols-4 gap-4;
}

.metric-card {
  @apply bg-white p-4 rounded-lg flex items-center gap-3;
}

.metric-icon {
  @apply text-2xl;
}

.metric-info {
  @apply flex-1;
}

.metric-value {
  @apply text-xl font-bold;
}

.metric-label {
  @apply text-sm text-gray-600;
}

/* Output Preview */
.output-preview {
  @apply bg-white p-6 rounded-lg shadow-md;
}

.output-title {
  @apply text-lg font-semibold mb-4;
}

.output-tabs {
  @apply flex gap-2 mb-4;
}

.output-tab {
  @apply px-4 py-2 rounded-lg bg-gray-100 hover:bg-gray-200 transition-colors;
}

.output-tab.active {
  @apply bg-blue-500 text-white;
}

.output-content {
  @apply bg-gray-50 p-4 rounded-lg overflow-auto max-h-64;
}

.output-content pre {
  @apply text-sm font-mono;
}
</style>