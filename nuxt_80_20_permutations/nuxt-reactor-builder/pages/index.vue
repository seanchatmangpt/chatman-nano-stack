<template>
  <div class="min-h-screen bg-gray-100">
    <div class="flex h-screen">
      <!-- Sidebar -->
      <div class="w-64 bg-white shadow-lg p-4">
        <h2 class="text-xl font-bold mb-4">Reactor Steps</h2>
        <div class="space-y-2">
          <StepTemplate 
            v-for="step in availableSteps" 
            :key="step.type"
            :step="step"
            @drag="handleDragStart"
          />
        </div>
      </div>
      
      <!-- Main Canvas -->
      <div class="flex-1">
        <div class="bg-white m-4 rounded-lg shadow-lg h-full p-4">
          <div class="flex justify-between items-center mb-4">
            <h1 class="text-2xl font-bold">Reactor Workflow Builder</h1>
            <div class="space-x-2">
              <button @click="validateWorkflow" class="btn-secondary">Validate</button>
              <button @click="deployToK8s" class="btn-primary">Deploy to K8s</button>
            </div>
          </div>
          
          <VueFlow
            v-model="elements"
            @connect="onConnect"
            @nodeDoubleClick="onNodeDoubleClick"
            class="h-full"
          >
            <Background />
            <Controls />
            <MiniMap />
          </VueFlow>
        </div>
      </div>
      
      <!-- Properties Panel -->
      <div v-if="selectedNode" class="w-80 bg-white shadow-lg p-4">
        <h3 class="text-lg font-bold mb-4">Step Properties</h3>
        <StepProperties 
          :node="selectedNode"
          @update="updateNodeProperties"
        />
      </div>
    </div>
  </div>
</template>

<script setup>
import { VueFlow, Background, Controls, MiniMap } from '@vue-flow/core'

const availableSteps = [
  { type: 'input', name: 'Input', icon: '📥', color: '#10b981' },
  { type: 'transform', name: 'Transform', icon: '🔄', color: '#3b82f6' },
  { type: 'validate', name: 'Validate', icon: '✅', color: '#f59e0b' },
  { type: 'async', name: 'Async Call', icon: '⏳', color: '#8b5cf6' },
  { type: 'branch', name: 'Branch', icon: '🔀', color: '#ef4444' },
  { type: 'output', name: 'Output', icon: '📤', color: '#10b981' }
]

const elements = ref([])
const selectedNode = ref(null)

const handleDragStart = (event, step) => {
  event.dataTransfer.setData('stepType', step.type)
  event.dataTransfer.setData('stepName', step.name)
}

const onConnect = (params) => {
  // Add connection logic
}

const onNodeDoubleClick = (event, node) => {
  selectedNode.value = node
}

const updateNodeProperties = (properties) => {
  if (selectedNode.value) {
    selectedNode.value.data = {
      ...selectedNode.value.data,
      ...properties
    }
  }
}

const validateWorkflow = async () => {
  const { $fetch } = useNuxtApp()
  try {
    const workflow = {
      name: 'My Workflow',
      steps: elements.value.filter(el => el.type === 'node'),
      connections: elements.value.filter(el => el.type === 'edge')
    }
    
    const result = await $fetch('/api/validate-reactor-workflow', {
      method: 'POST',
      body: { workflow }
    })
    
    if (result.valid) {
      alert('Workflow is valid!')
    } else {
      alert('Validation errors: ' + result.errors.join(', '))
    }
  } catch (error) {
    alert('Validation failed: ' + error.message)
  }
}

const deployToK8s = async () => {
  if (confirm('Deploy this workflow to Kubernetes?')) {
    const { $fetch } = useNuxtApp()
    try {
      const result = await $fetch('/api/deploy-reactor-workflow', {
        method: 'POST',
        body: { 
          workflow: elements.value,
          namespace: 'default'
        }
      })
      
      alert('Deployment successful! Deployment ID: ' + result.deploymentId)
    } catch (error) {
      alert('Deployment failed: ' + error.message)
    }
  }
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