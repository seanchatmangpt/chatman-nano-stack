<template>
  <div class="min-h-screen bg-gray-100 p-6">
    <div class="container mx-auto">
      <h1 class="text-3xl font-bold mb-6">Semantic Pipeline Transformer</h1>
      
      <!-- Input Section -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold mb-4">Input Data</h2>
        <div class="grid grid-cols-1 lg:grid-cols-2 gap-4">
          <div>
            <label class="block text-sm font-medium mb-2">Input Type</label>
            <select v-model="inputType" class="w-full border rounded px-3 py-2">
              <option value="raw">Raw Text</option>
              <option value="json">JSON</option>
              <option value="csv">CSV</option>
              <option value="ttl">TTL/RDF</option>
            </select>
          </div>
          <div>
            <label class="block text-sm font-medium mb-2">Target Output</label>
            <select v-model="outputType" class="w-full border rounded px-3 py-2">
              <option value="ttl">TTL Ontology</option>
              <option value="ash">Ash Resources</option>
              <option value="reactor">Reactor Workflow</option>
              <option value="k8s">K8s Manifest</option>
            </select>
          </div>
        </div>
        <div class="mt-4">
          <label class="block text-sm font-medium mb-2">Input Data</label>
          <textarea 
            v-model="inputData"
            class="w-full border rounded px-3 py-2 h-32 font-mono text-sm"
            placeholder="Paste your data here..."
          ></textarea>
        </div>
        <button @click="transformData" class="mt-4 bg-blue-600 text-white px-6 py-2 rounded hover:bg-blue-700">
          Transform Through Pipeline
        </button>
      </div>
      
      <!-- Pipeline Visualization -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold mb-4">Transformation Pipeline</h2>
        <PipelineVisualization 
          :stages="pipelineStages"
          :currentStage="currentStage"
        />
      </div>
      
      <!-- Stage Outputs -->
      <div class="grid grid-cols-1 lg:grid-cols-2 xl:grid-cols-3 gap-6">
        <div v-for="stage in completedStages" :key="stage.id" class="bg-white rounded-lg shadow-lg p-4">
          <h3 class="text-lg font-semibold mb-2">{{ stage.name }}</h3>
          <div class="border rounded p-2 bg-gray-50 max-h-48 overflow-y-auto">
            <pre class="text-xs">{{ stage.output }}</pre>
          </div>
          <div class="mt-2 text-sm text-gray-600">
            <span>Processing time: {{ stage.processingTime }}ms</span>
          </div>
        </div>
      </div>
      
      <!-- Final Output -->
      <div v-if="finalOutput" class="bg-white rounded-lg shadow-lg p-6 mt-6">
        <div class="flex justify-between items-center mb-4">
          <h2 class="text-xl font-semibold">Final Output</h2>
          <div class="space-x-2">
            <button @click="copyOutput" class="btn-secondary">Copy</button>
            <button @click="downloadOutput" class="btn-primary">Download</button>
          </div>
        </div>
        <div class="border rounded p-4 bg-gray-50 max-h-96 overflow-y-auto">
          <pre class="text-sm">{{ finalOutput }}</pre>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
const inputType = ref('raw')
const outputType = ref('ttl')
const inputData = ref('')
const currentStage = ref(null)
const completedStages = ref([])
const finalOutput = ref(null)

const pipelineStages = computed(() => {
  const stages = ['typer']
  
  if (outputType.value === 'ttl' || outputType.value === 'ash' || outputType.value === 'reactor') {
    stages.push('turtle', 'ttl2dspy')
  }
  
  if (outputType.value === 'ash' || outputType.value === 'reactor' || outputType.value === 'k8s') {
    stages.push('ash')
  }
  
  if (outputType.value === 'reactor' || outputType.value === 'k8s') {
    stages.push('reactor')
  }
  
  if (outputType.value === 'k8s') {
    stages.push('k8s')
  }
  
  return stages
})

const transformData = async () => {
  completedStages.value = []
  finalOutput.value = null
  
  const { $fetch } = useNuxtApp()
  
  try {
    for (const stage of pipelineStages.value) {
      currentStage.value = stage
      
      const startTime = Date.now()
      const result = await $fetch(`/api/transform/${stage}`, {
        method: 'POST',
        body: {
          input: completedStages.value.length > 0 
            ? completedStages.value[completedStages.value.length - 1].output
            : inputData.value,
          inputType: completedStages.value.length === 0 ? inputType.value : 'processed',
          targetOutput: outputType.value
        }
      })
      
      completedStages.value.push({
        id: stage,
        name: getStageDisplayName(stage),
        output: result.output,
        processingTime: Date.now() - startTime
      })
      
      // Simulate processing delay
      await new Promise(resolve => setTimeout(resolve, 500))
    }
    
    currentStage.value = null
    finalOutput.value = completedStages.value[completedStages.value.length - 1].output
    
  } catch (error) {
    alert('Transformation failed: ' + error.message)
    currentStage.value = null
  }
}

const getStageDisplayName = (stage) => {
  const names = {
    typer: '80/20 Typer',
    turtle: 'Turtle Generator',
    ttl2dspy: 'TTL to DSPy',
    ash: 'Ash Resources',
    reactor: 'Reactor Workflow',
    k8s: 'Kubernetes Manifest'
  }
  return names[stage] || stage
}

const copyOutput = () => {
  navigator.clipboard.writeText(finalOutput.value)
  alert('Output copied to clipboard!')
}

const downloadOutput = () => {
  const blob = new Blob([finalOutput.value], { type: 'text/plain' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `output.${outputType.value}`
  a.click()
  URL.revokeObjectURL(url)
}
</script>

<style>
.btn-primary {
  @apply bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700;
}

.btn-secondary {
  @apply bg-gray-200 text-gray-800 px-4 py-2 rounded hover:bg-gray-300;
}
</style>