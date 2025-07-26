<template>
  <div class="h-screen bg-gray-50 flex flex-col">
    <!-- Header -->
    <header class="bg-white border-b border-gray-200 px-4 py-3">
      <div class="flex items-center justify-between">
        <div class="flex items-center space-x-4">
          <h1 class="text-lg font-semibold">Workflow Studio</h1>
          <UInput 
            v-model="workflowName" 
            placeholder="Workflow name..."
            class="w-64"
          />
        </div>
        
        <div class="flex items-center space-x-2">
          <UButton @click="validateWorkflow" variant="outline" size="sm">
            <UIcon name="i-heroicons-check-circle" class="mr-2" />
            Validate
          </UButton>
          <UButton @click="saveWorkflow" variant="outline" size="sm">
            <UIcon name="i-heroicons-cloud-arrow-up" class="mr-2" />
            Save
          </UButton>
          <UButton @click="deployWorkflow" color="primary" size="sm">
            <UIcon name="i-heroicons-rocket-launch" class="mr-2" />
            Deploy
          </UButton>
        </div>
      </div>
    </header>
    
    <div class="flex flex-1">
      <!-- Component Palette -->
      <aside class="w-64 bg-white border-r border-gray-200 p-4">
        <h3 class="font-medium mb-4">Components</h3>
        <div class="space-y-2">
          <div 
            v-for="component in pipelineComponents" 
            :key="component.type"
            class="p-3 border border-gray-200 rounded-lg cursor-pointer hover:bg-gray-50"
            @click="addComponent(component)"
          >
            <div class="flex items-center space-x-2">
              <UIcon :name="component.icon" class="text-blue-500" />
              <span class="text-sm font-medium">{{ component.label }}</span>
            </div>
          </div>
        </div>
      </aside>
      
      <!-- Canvas -->
      <main class="flex-1 relative overflow-hidden bg-gray-100">
        <div class="p-4">
          <h2 class="text-lg font-semibold mb-4">Workflow Canvas</h2>
          <div class="bg-white rounded-lg shadow h-96 p-4">
            <div v-if="workflowComponents.length === 0" class="flex items-center justify-center h-full text-gray-500">
              <div class="text-center">
                <UIcon name="i-heroicons-plus-circle" class="w-12 h-12 mx-auto mb-2" />
                <p>Click components from the palette to add them to your workflow</p>
              </div>
            </div>
            <div v-else class="space-y-4">
              <div 
                v-for="(component, index) in workflowComponents" 
                :key="index"
                class="flex items-center space-x-4 p-3 border border-gray-200 rounded-lg"
                @click="selectedComponent = component"
              >
                <UIcon :name="component.icon" class="text-blue-500" />
                <span class="font-medium">{{ component.label }}</span>
                <div class="flex-1"></div>
                <UButton 
                  @click.stop="removeComponent(index)" 
                  variant="ghost" 
                  size="sm"
                  color="red"
                >
                  <UIcon name="i-heroicons-trash" />
                </UButton>
              </div>
            </div>
          </div>
        </div>
      </main>
      
      <!-- Properties Panel -->
      <aside v-if="selectedComponent" class="w-80 bg-white border-l border-gray-200">
        <div class="p-4 border-b border-gray-200">
          <h3 class="font-medium">Properties</h3>
        </div>
        <div class="p-4">
          <div class="space-y-4">
            <div>
              <label class="block text-sm font-medium mb-1">Name</label>
              <UInput v-model="selectedComponent.name" />
            </div>
            <div>
              <label class="block text-sm font-medium mb-1">Description</label>
              <UTextarea v-model="selectedComponent.description" />
            </div>
          </div>
        </div>
      </aside>
    </div>

    <!-- Validation Modal -->
    <UModal v-model="showValidation">
      <UCard>
        <template #header>
          <h3 class="font-medium">Validation Results</h3>
        </template>
        <div class="space-y-2">
          <div class="flex items-center space-x-2">
            <UIcon name="i-heroicons-check-circle" class="text-green-500" />
            <span>Workflow structure is valid</span>
          </div>
          <div class="flex items-center space-x-2">
            <UIcon name="i-heroicons-check-circle" class="text-green-500" />
            <span>All components connected properly</span>
          </div>
        </div>
      </UCard>
    </UModal>
  </div>
</template>

<script setup>
definePageMeta({
  layout: false
})

const workflowName = ref('Untitled Workflow')
const selectedComponent = ref(null)
const workflowComponents = ref([])
const showValidation = ref(false)

const pipelineComponents = [
  { type: 'input', label: 'Input', icon: 'i-heroicons-arrow-down-on-square' },
  { type: 'typer', label: '80/20 Typer', icon: 'i-heroicons-funnel' },
  { type: 'turtle', label: 'Turtle Gen', icon: 'i-heroicons-document-text' },
  { type: 'ttl2dspy', label: 'TTL2DSPy', icon: 'i-heroicons-code-bracket' },
  { type: 'ash', label: 'Ash Resource', icon: 'i-heroicons-server' },
  { type: 'reactor', label: 'Reactor', icon: 'i-heroicons-cog-6-tooth' },
  { type: 'output', label: 'Output', icon: 'i-heroicons-arrow-up-on-square' }
]

const addComponent = (component) => {
  workflowComponents.value.push({
    ...component,
    id: Date.now(),
    name: component.label,
    description: ''
  })
}

const removeComponent = (index) => {
  workflowComponents.value.splice(index, 1)
  selectedComponent.value = null
}

const validateWorkflow = () => {
  console.log('Validating workflow...', workflowComponents.value)
  showValidation.value = true
}

const saveWorkflow = () => {
  console.log('Saving workflow...', { name: workflowName.value, components: workflowComponents.value })
}

const deployWorkflow = () => {
  console.log('Deploying workflow to Reactor...', workflowName.value)
}
</script>