<template>
  <div class="min-h-screen bg-gray-950 text-white">
    <!-- Header -->
    <header class="border-b border-gray-800 bg-gray-900/50 backdrop-blur">
      <div class="flex items-center justify-between px-6 py-4">
        <div class="flex items-center space-x-4">
          <UBadge color="green" variant="soft" size="lg">
            <UIcon name="i-heroicons-signal" class="mr-2" />
            CNS COMMAND CENTER
          </UBadge>
          <UKbd @click="commandOpen = true">⌘K</UKbd>
        </div>
        
        <div class="flex items-center space-x-4">
          <UBadge color="green" variant="outline">System Online</UBadge>
          <UButton variant="ghost" size="sm">
            <UIcon name="i-heroicons-bell" />
          </UButton>
        </div>
      </div>
    </header>

    <!-- Command Palette -->
    <UModal v-model="commandOpen">
      <UCard>
        <template #header>
          <h3 class="font-semibold">Command Palette</h3>
        </template>
        <div class="space-y-2">
          <UButton block variant="ghost" @click="restartPipeline">
            <UIcon name="i-heroicons-arrow-path" class="mr-2" />
            Restart Pipeline
          </UButton>
          <UButton block variant="ghost" @click="viewLogs">
            <UIcon name="i-heroicons-document-text" class="mr-2" />
            View Logs
          </UButton>
        </div>
      </UCard>
    </UModal>
    
    <!-- Main Content -->
    <div class="p-6">
      <div class="mb-6">
        <h1 class="text-2xl font-bold mb-2">Mission Control</h1>
        <p class="text-gray-400">Real-time pipeline monitoring and control</p>
      </div>
      
      <!-- Status Grid -->
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
        <UCard 
          v-for="metric in systemMetrics" 
          :key="metric.id"
          class="bg-gray-900 border-gray-700"
        >
          <div class="text-center">
            <div class="text-2xl font-bold mb-1">{{ metric.value }}</div>
            <div class="text-sm text-gray-400 mb-2">{{ metric.label }}</div>
            <UBadge :color="metric.status === 'success' ? 'green' : 'yellow'" size="xs">
              {{ metric.change }}
            </UBadge>
          </div>
        </UCard>
      </div>
      
      <!-- Pipeline Flow -->
      <UCard class="mb-8">
        <template #header>
          <div class="flex items-center justify-between">
            <h2 class="text-lg font-semibold">Pipeline Flow</h2>
            <div class="flex items-center space-x-2">
              <UToggle v-model="autoRefresh" />
              <span class="text-sm text-gray-400">Auto-refresh</span>
            </div>
          </div>
        </template>
        
        <div class="p-4">
          <div class="flex items-center justify-between">
            <div v-for="node in pipelineNodes" :key="node.id" class="flex flex-col items-center">
              <div class="w-16 h-16 bg-blue-600 rounded-full flex items-center justify-center mb-2">
                <UIcon name="i-heroicons-cog-6-tooth" class="text-white" />
              </div>
              <span class="text-sm">{{ node.label }}</span>
              <UBadge :color="node.status === 'active' ? 'green' : 'gray'" size="xs" class="mt-1">
                {{ node.status }}
              </UBadge>
            </div>
          </div>
        </div>
      </UCard>
      
      <!-- Metrics -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <UCard>
          <template #header>
            <h3 class="font-semibold">System Performance</h3>
          </template>
          <div class="space-y-4">
            <div class="flex justify-between">
              <span>CPU Usage</span>
              <span class="text-green-400">23%</span>
            </div>
            <div class="flex justify-between">
              <span>Memory Usage</span>
              <span class="text-blue-400">1.2GB</span>
            </div>
            <div class="flex justify-between">
              <span>Disk Usage</span>
              <span class="text-yellow-400">45%</span>
            </div>
          </div>
        </UCard>
        
        <UCard>
          <template #header>
            <h3 class="font-semibold">Recent Activity</h3>
          </template>
          <div class="space-y-3">
            <div v-for="activity in recentActivities" :key="activity.id" class="flex items-center space-x-3">
              <UIcon name="i-heroicons-check-circle" class="text-green-400" />
              <div>
                <p class="text-sm">{{ activity.message }}</p>
                <p class="text-xs text-gray-400">{{ activity.time }}</p>
              </div>
            </div>
          </div>
        </UCard>
      </div>
    </div>
  </div>
</template>

<script setup>
// No layout needed since we're building it inline
definePageMeta({
  layout: false
})

const commandOpen = ref(false)
const autoRefresh = ref(true)

const systemMetrics = ref([
  { id: 'throughput', label: 'Messages/sec', value: '12.4K', change: '+5.2%', status: 'success' },
  { id: 'latency', label: 'Avg Latency', value: '23ms', change: '-2.1%', status: 'success' },
  { id: 'errors', label: 'Error Rate', value: '0.1%', change: '+0.05%', status: 'warning' },
  { id: 'uptime', label: 'Uptime', value: '99.9%', change: '0%', status: 'success' }
])

const pipelineNodes = ref([
  { id: 'typer', label: '80/20 Typer', status: 'active' },
  { id: 'turtle', label: 'Turtle Gen', status: 'active' },
  { id: 'ttl2dspy', label: 'TTL2DSPy', status: 'active' },
  { id: 'bitactor', label: 'BitActor', status: 'active' },
  { id: 'ash', label: 'Ash', status: 'active' },
  { id: 'reactor', label: 'Reactor', status: 'active' },
  { id: 'k8s', label: 'K8s', status: 'active' }
])

const recentActivities = ref([
  { id: 1, message: 'Pipeline execution completed successfully', time: '2 minutes ago' },
  { id: 2, message: 'New workflow deployed to reactor', time: '5 minutes ago' },
  { id: 3, message: 'BitActor cluster scaled up', time: '10 minutes ago' },
  { id: 4, message: 'TTL validation passed', time: '15 minutes ago' }
])

// Global keyboard shortcuts
onMounted(() => {
  document.addEventListener('keydown', (e) => {
    if (e.metaKey && e.key === 'k') {
      e.preventDefault()
      commandOpen.value = true
    }
  })
})

const restartPipeline = () => {
  console.log('Restarting pipeline...')
  commandOpen.value = false
}

const viewLogs = () => {
  console.log('Viewing logs...')
  commandOpen.value = false
}
</script>