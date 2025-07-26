<template>
  <div class="permutation-selector">
    <h3 class="selector-title">🔄 Pipeline Permutation Selector</h3>
    
    <!-- Quick Actions -->
    <div class="quick-actions">
      <button 
        @click="selectOptimalAutomatically" 
        class="action-btn optimal"
        :disabled="isProcessing"
      >
        🎯 Auto-Select Optimal
      </button>
      <button 
        @click="showAllPermutations = !showAllPermutations" 
        class="action-btn explore"
      >
        🔍 {{ showAllPermutations ? 'Hide' : 'Show' }} All Options
      </button>
      <button 
        @click="createCustomChain" 
        class="action-btn custom"
      >
        🔗 Create Chain
      </button>
    </div>
    
    <!-- Optimal Recommendation -->
    <div v-if="optimalRecommendation" class="optimal-recommendation">
      <h4 class="recommendation-title">🎯 Recommended Permutation</h4>
      <div class="recommendation-card">
        <div class="perm-header">
          <span class="perm-icon">{{ optimalRecommendation.optimal.icon }}</span>
          <span class="perm-name">{{ optimalRecommendation.optimal.name }}</span>
          <span class="perm-score">Score: {{ optimalRecommendation.optimal.score }}</span>
        </div>
        <div class="perm-details">
          <div class="detail-row">
            <span class="detail-label">Speed:</span>
            <span class="detail-value speed">{{ optimalRecommendation.optimal.speed }}</span>
          </div>
          <div class="detail-row">
            <span class="detail-label">Stages:</span>
            <span class="detail-value">{{ formatStages(optimalRecommendation.optimal.stages) }}</span>
          </div>
          <div v-if="optimalRecommendation.optimal.skipStages" class="detail-row">
            <span class="detail-label">Skips:</span>
            <span class="detail-value skip">{{ optimalRecommendation.optimal.skipStages.join(', ') }}</span>
          </div>
        </div>
        <div class="reasoning">
          <p class="reasoning-text">{{ optimalRecommendation.reasoning }}</p>
        </div>
        <button 
          @click="applyPermutation(optimalRecommendation.optimal)" 
          class="apply-btn"
          :disabled="isProcessing"
        >
          ⚡ Apply This Permutation
        </button>
      </div>
      
      <!-- Alternative Options -->
      <div v-if="optimalRecommendation.alternatives.length > 0" class="alternatives">
        <h5 class="alternatives-title">Alternative Options:</h5>
        <div class="alternative-cards">
          <div 
            v-for="alt in optimalRecommendation.alternatives" 
            :key="alt.id"
            class="alternative-card"
            @click="selectAlternative(alt)"
          >
            <span class="alt-icon">{{ alt.icon }}</span>
            <span class="alt-name">{{ alt.name }}</span>
            <span class="alt-speed">{{ alt.speed }}</span>
          </div>
        </div>
      </div>
    </div>
    
    <!-- All Permutations Grid -->
    <div v-if="showAllPermutations" class="all-permutations">
      <h4 class="grid-title">📚 All Available Permutations</h4>
      
      <!-- Filter Controls -->
      <div class="filter-controls">
        <div class="filter-group">
          <label class="filter-label">Speed:</label>
          <select v-model="filterSpeed" class="filter-select">
            <option value="all">All Speeds</option>
            <option value="ultra-fast">Ultra Fast</option>
            <option value="very-fast">Very Fast</option>
            <option value="fast">Fast</option>
            <option value="optimized">Optimized</option>
            <option value="comprehensive">Comprehensive</option>
          </select>
        </div>
        <div class="filter-group">
          <label class="filter-label">Type:</label>
          <select v-model="filterType" class="filter-select">
            <option value="all">All Types</option>
            <option value="bypass">Bypass</option>
            <option value="parallel">Parallel</option>
            <option value="smart">Smart Routing</option>
            <option value="multi">Multi-Output</option>
          </select>
        </div>
      </div>
      
      <!-- Permutations Grid -->
      <div class="permutations-grid">
        <div 
          v-for="perm in filteredPermutations" 
          :key="perm.id"
          :class="['permutation-card', { selected: isSelected(perm) }]"
          @click="togglePermutationSelection(perm)"
        >
          <div class="perm-card-header">
            <span class="perm-icon">{{ perm.icon }}</span>
            <span class="perm-name">{{ perm.name }}</span>
          </div>
          <div class="perm-card-body">
            <div class="perm-info">
              <span class="info-label">Speed:</span>
              <span class="info-value">{{ perm.speed }}</span>
            </div>
            <div class="perm-info">
              <span class="info-label">Stages:</span>
              <span class="info-value">{{ perm.stages.length }}</span>
            </div>
            <div v-if="perm.parallel" class="perm-badge parallel">Parallel</div>
            <div v-if="perm.skipStages" class="perm-badge bypass">Bypass</div>
            <div v-if="perm.orchestrated" class="perm-badge smart">Smart</div>
          </div>
          <div class="perm-card-actions">
            <button 
              @click.stop="applyPermutation(perm)" 
              class="card-action-btn apply"
              :disabled="isProcessing"
            >
              Apply
            </button>
            <button 
              @click.stop="viewPermutationDetails(perm)" 
              class="card-action-btn details"
            >
              Details
            </button>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Permutation Chain Builder -->
    <div v-if="showChainBuilder" class="chain-builder">
      <h4 class="chain-title">🔗 Permutation Chain Builder</h4>
      
      <div class="chain-workspace">
        <div class="chain-sequence">
          <div 
            v-for="(perm, index) in currentChain" 
            :key="`chain-${index}`"
            class="chain-item"
          >
            <span class="chain-icon">{{ perm.icon }}</span>
            <span class="chain-name">{{ perm.name }}</span>
            <button @click="removeFromChain(index)" class="chain-remove">×</button>
            <div v-if="index < currentChain.length - 1" class="chain-connector">→</div>
          </div>
          <div v-if="currentChain.length === 0" class="chain-empty">
            Select permutations to build a chain
          </div>
        </div>
        
        <div class="chain-actions">
          <button 
            @click="executeChain" 
            :disabled="currentChain.length === 0 || isProcessing"
            class="chain-action-btn execute"
          >
            ⚡ Execute Chain
          </button>
          <button 
            @click="saveChain" 
            :disabled="currentChain.length === 0"
            class="chain-action-btn save"
          >
            💾 Save Chain
          </button>
          <button 
            @click="clearChain" 
            class="chain-action-btn clear"
          >
            🗑️ Clear
          </button>
        </div>
      </div>
    </div>
    
    <!-- Optimization Hints -->
    <div v-if="optimizationHints.length > 0" class="optimization-hints">
      <h4 class="hints-title">💡 Optimization Suggestions</h4>
      <div 
        v-for="(hint, index) in optimizationHints" 
        :key="`hint-${index}`"
        class="hint-card"
      >
        <span class="hint-icon">{{ getHintIcon(hint.type) }}</span>
        <span class="hint-message">{{ hint.message }}</span>
        <button 
          v-if="hint.action" 
          @click="applyHintAction(hint.action)"
          class="hint-action"
        >
          Apply
        </button>
      </div>
    </div>
    
    <!-- Performance Analysis -->
    <div v-if="performanceAnalysis" class="performance-analysis">
      <h4 class="analysis-title">📊 Performance Analysis</h4>
      <div class="analysis-content">
        <div class="analysis-metric">
          <span class="metric-label">Total Duration:</span>
          <span class="metric-value">{{ performanceAnalysis.totalDuration }}ms</span>
        </div>
        <div v-if="performanceAnalysis.bottlenecks.length > 0" class="bottlenecks">
          <h5 class="bottleneck-title">🚧 Bottlenecks Detected:</h5>
          <div 
            v-for="bottleneck in performanceAnalysis.bottlenecks" 
            :key="bottleneck.stage"
            class="bottleneck-item"
          >
            <span class="bottleneck-stage">{{ bottleneck.stage }}:</span>
            <span class="bottleneck-duration">{{ bottleneck.duration }}ms</span>
          </div>
        </div>
        <div v-if="performanceAnalysis.recommendations.length > 0" class="recommendations">
          <h5 class="rec-title">📋 Recommendations:</h5>
          <ul class="rec-list">
            <li 
              v-for="(rec, index) in performanceAnalysis.recommendations" 
              :key="`rec-${index}`"
            >
              {{ rec.message }}
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
// 🔄 ULTRATHINK SWARM 80/20: Permutation Selector Component
// JavaScript UI for selecting and managing pipeline permutations

import { ref, computed, watch } from 'vue'

// Props
const props = defineProps({
  ontologyData: {
    type: Object,
    required: true
  },
  requirements: {
    type: Object,
    default: () => ({})
  }
})

// Emit events
const emit = defineEmits(['permutation-selected', 'chain-executed'])

// Use permutation handlers composable
const {
  permutationState,
  generatePermutationCombinations,
  selectOptimalPermutation,
  createPermutationChain,
  applyPermutation: applyPermutationHandler,
  executePermutationChain,
  analyzePermutationPerformance,
  generateOptimizationHints
} = usePermutationHandlers()

// Component state
const showAllPermutations = ref(false)
const showChainBuilder = ref(false)
const filterSpeed = ref('all')
const filterType = ref('all')
const selectedPermutations = ref([])
const currentChain = ref([])
const optimalRecommendation = ref(null)
const optimizationHints = ref([])
const performanceAnalysis = ref(null)
const isProcessing = ref(false)

// Generate permutations on mount
onMounted(() => {
  generatePermutationCombinations(props.ontologyData)
})

// Computed properties
const filteredPermutations = computed(() => {
  let perms = permutationState.availablePermutations
  
  if (filterSpeed.value !== 'all') {
    perms = perms.filter(p => p.speed === filterSpeed.value)
  }
  
  if (filterType.value !== 'all') {
    perms = perms.filter(p => {
      switch (filterType.value) {
        case 'bypass': return p.skipStages && p.skipStages.length > 0
        case 'parallel': return p.parallel === true
        case 'smart': return p.routing === 'adaptive' || p.orchestrated
        case 'multi': return p.outputs && p.outputs.length > 2
        default: return true
      }
    })
  }
  
  return perms
})

// Methods
const selectOptimalAutomatically = () => {
  optimalRecommendation.value = selectOptimalPermutation(props.ontologyData, props.requirements)
  
  // Generate optimization hints
  if (optimalRecommendation.value.optimal) {
    optimizationHints.value = generateOptimizationHints(
      props.ontologyData, 
      optimalRecommendation.value.optimal
    )
  }
}

const applyPermutation = async (permutation) => {
  isProcessing.value = true
  try {
    const result = await applyPermutationHandler(permutation, props.ontologyData)
    emit('permutation-selected', { permutation, result })
    
    // Analyze performance if result includes timing data
    if (result.executionResults) {
      performanceAnalysis.value = analyzePermutationPerformance(result.executionResults)
    }
  } catch (error) {
    console.error('Failed to apply permutation:', error)
  } finally {
    isProcessing.value = false
  }
}

const selectAlternative = (alternative) => {
  optimalRecommendation.value.optimal = alternative
  optimizationHints.value = generateOptimizationHints(props.ontologyData, alternative)
}

const isSelected = (permutation) => {
  return selectedPermutations.value.some(p => p.id === permutation.id)
}

const togglePermutationSelection = (permutation) => {
  const index = selectedPermutations.value.findIndex(p => p.id === permutation.id)
  if (index >= 0) {
    selectedPermutations.value.splice(index, 1)
  } else {
    selectedPermutations.value.push(permutation)
  }
}

const createCustomChain = () => {
  showChainBuilder.value = !showChainBuilder.value
  if (showChainBuilder.value && selectedPermutations.value.length > 0) {
    currentChain.value = [...selectedPermutations.value]
  }
}

const removeFromChain = (index) => {
  currentChain.value.splice(index, 1)
}

const executeChain = async () => {
  if (currentChain.value.length === 0) return
  
  isProcessing.value = true
  try {
    const chainId = createPermutationChain(currentChain.value.map(p => p.id))
    const result = await executePermutationChain(chainId, props.ontologyData)
    emit('chain-executed', result)
  } catch (error) {
    console.error('Failed to execute chain:', error)
  } finally {
    isProcessing.value = false
  }
}

const saveChain = () => {
  const chain = createPermutationChain(currentChain.value.map(p => p.id))
  console.log('Chain saved:', chain)
  // Could emit event or save to localStorage
}

const clearChain = () => {
  currentChain.value = []
}

const viewPermutationDetails = (permutation) => {
  console.log('Viewing details for:', permutation)
  // Could open modal or expand details
}

const formatStages = (stages) => {
  if (stages.length <= 3) {
    return stages.join(' → ')
  }
  return `${stages[0]} → ... → ${stages[stages.length - 1]} (${stages.length} stages)`
}

const getHintIcon = (type) => {
  const icons = {
    performance: '⚡',
    optimization: '🎯',
    warning: '⚠️',
    info: 'ℹ️'
  }
  return icons[type] || '💡'
}

const applyHintAction = (action) => {
  switch (action) {
    case 'use_bypass':
      filterType.value = 'bypass'
      showAllPermutations.value = true
      break
    case 'enable_parallel':
      filterType.value = 'parallel'
      showAllPermutations.value = true
      break
    case 'analyze_skip_opportunities':
      selectOptimalAutomatically()
      break
  }
}

// Watch for ontology changes
watch(() => props.ontologyData, () => {
  generatePermutationCombinations(props.ontologyData)
  if (optimalRecommendation.value) {
    selectOptimalAutomatically()
  }
}, { deep: true })
</script>

<style scoped>
.permutation-selector {
  @apply space-y-6;
}

.selector-title {
  @apply text-xl font-bold mb-4;
}

/* Quick Actions */
.quick-actions {
  @apply flex gap-3 mb-6;
}

.action-btn {
  @apply px-4 py-2 rounded-lg font-medium transition-all duration-200;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.action-btn.optimal {
  @apply bg-blue-600 text-white hover:bg-blue-700;
}

.action-btn.explore {
  @apply bg-gray-200 text-gray-800 hover:bg-gray-300;
}

.action-btn.custom {
  @apply bg-purple-600 text-white hover:bg-purple-700;
}

/* Optimal Recommendation */
.optimal-recommendation {
  @apply bg-blue-50 p-6 rounded-lg;
}

.recommendation-title {
  @apply text-lg font-semibold mb-4;
}

.recommendation-card {
  @apply bg-white p-5 rounded-lg shadow-md;
}

.perm-header {
  @apply flex items-center gap-3 mb-4;
}

.perm-icon {
  @apply text-2xl;
}

.perm-name {
  @apply flex-1 font-semibold text-lg;
}

.perm-score {
  @apply text-sm text-gray-600;
}

.perm-details {
  @apply space-y-2 mb-4;
}

.detail-row {
  @apply flex items-center gap-2;
}

.detail-label {
  @apply text-sm text-gray-600 w-20;
}

.detail-value {
  @apply font-medium;
}

.detail-value.speed {
  @apply text-blue-600;
}

.detail-value.skip {
  @apply text-orange-600 text-sm;
}

.reasoning {
  @apply bg-gray-50 p-3 rounded mb-4;
}

.reasoning-text {
  @apply text-sm text-gray-700;
}

.apply-btn {
  @apply w-full py-2 bg-green-600 text-white rounded-lg hover:bg-green-700;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

/* Alternatives */
.alternatives {
  @apply mt-4;
}

.alternatives-title {
  @apply text-sm font-semibold mb-2;
}

.alternative-cards {
  @apply grid grid-cols-3 gap-2;
}

.alternative-card {
  @apply bg-white p-3 rounded-lg cursor-pointer hover:shadow-md transition-shadow;
  @apply flex flex-col items-center text-center;
}

.alt-icon {
  @apply text-xl mb-1;
}

.alt-name {
  @apply text-sm font-medium;
}

.alt-speed {
  @apply text-xs text-gray-600;
}

/* All Permutations Grid */
.all-permutations {
  @apply bg-gray-50 p-6 rounded-lg;
}

.grid-title {
  @apply text-lg font-semibold mb-4;
}

.filter-controls {
  @apply flex gap-4 mb-4;
}

.filter-group {
  @apply flex items-center gap-2;
}

.filter-label {
  @apply text-sm font-medium;
}

.filter-select {
  @apply px-3 py-1 border rounded-lg;
}

.permutations-grid {
  @apply grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4;
}

.permutation-card {
  @apply bg-white p-4 rounded-lg shadow hover:shadow-md transition-all;
  @apply cursor-pointer border-2 border-transparent;
}

.permutation-card.selected {
  @apply border-blue-500 bg-blue-50;
}

.perm-card-header {
  @apply flex items-center gap-2 mb-3;
}

.perm-card-body {
  @apply space-y-2 mb-3;
}

.perm-info {
  @apply flex justify-between text-sm;
}

.info-label {
  @apply text-gray-600;
}

.info-value {
  @apply font-medium;
}

.perm-badge {
  @apply inline-block px-2 py-1 text-xs font-medium rounded;
  @apply mt-2;
}

.perm-badge.parallel {
  @apply bg-green-100 text-green-800;
}

.perm-badge.bypass {
  @apply bg-orange-100 text-orange-800;
}

.perm-badge.smart {
  @apply bg-purple-100 text-purple-800;
}

.perm-card-actions {
  @apply flex gap-2;
}

.card-action-btn {
  @apply flex-1 py-1 text-sm rounded;
}

.card-action-btn.apply {
  @apply bg-green-600 text-white hover:bg-green-700;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.card-action-btn.details {
  @apply bg-gray-200 text-gray-800 hover:bg-gray-300;
}

/* Chain Builder */
.chain-builder {
  @apply bg-purple-50 p-6 rounded-lg;
}

.chain-title {
  @apply text-lg font-semibold mb-4;
}

.chain-workspace {
  @apply bg-white p-4 rounded-lg;
}

.chain-sequence {
  @apply flex items-center gap-2 min-h-[80px] overflow-x-auto;
}

.chain-item {
  @apply relative flex flex-col items-center p-3 bg-gray-100 rounded-lg;
}

.chain-icon {
  @apply text-xl mb-1;
}

.chain-name {
  @apply text-sm font-medium;
}

.chain-remove {
  @apply absolute -top-2 -right-2 w-6 h-6 bg-red-500 text-white rounded-full;
  @apply flex items-center justify-center text-sm hover:bg-red-600;
}

.chain-connector {
  @apply text-gray-400 mx-2;
}

.chain-empty {
  @apply text-gray-400 italic;
}

.chain-actions {
  @apply flex gap-2 mt-4;
}

.chain-action-btn {
  @apply px-4 py-2 rounded-lg font-medium;
}

.chain-action-btn.execute {
  @apply bg-green-600 text-white hover:bg-green-700;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.chain-action-btn.save {
  @apply bg-blue-600 text-white hover:bg-blue-700;
  @apply disabled:opacity-50 disabled:cursor-not-allowed;
}

.chain-action-btn.clear {
  @apply bg-gray-200 text-gray-800 hover:bg-gray-300;
}

/* Optimization Hints */
.optimization-hints {
  @apply bg-yellow-50 p-4 rounded-lg;
}

.hints-title {
  @apply text-lg font-semibold mb-3;
}

.hint-card {
  @apply flex items-center gap-3 p-3 bg-white rounded-lg mb-2;
}

.hint-icon {
  @apply text-xl;
}

.hint-message {
  @apply flex-1 text-sm;
}

.hint-action {
  @apply px-3 py-1 bg-blue-600 text-white text-sm rounded hover:bg-blue-700;
}

/* Performance Analysis */
.performance-analysis {
  @apply bg-gray-50 p-6 rounded-lg;
}

.analysis-title {
  @apply text-lg font-semibold mb-4;
}

.analysis-content {
  @apply space-y-4;
}

.analysis-metric {
  @apply flex justify-between items-center p-3 bg-white rounded;
}

.metric-label {
  @apply text-gray-600;
}

.metric-value {
  @apply font-bold text-lg;
}

.bottlenecks {
  @apply bg-red-50 p-4 rounded-lg;
}

.bottleneck-title {
  @apply text-sm font-semibold mb-2;
}

.bottleneck-item {
  @apply flex justify-between text-sm mb-1;
}

.bottleneck-stage {
  @apply font-medium;
}

.bottleneck-duration {
  @apply text-red-600;
}

.recommendations {
  @apply bg-blue-50 p-4 rounded-lg;
}

.rec-title {
  @apply text-sm font-semibold mb-2;
}

.rec-list {
  @apply list-disc list-inside text-sm space-y-1;
}
</style>