<template>
  <div class="min-h-screen bg-gray-900 text-white p-6">
    <h1 class="text-3xl font-bold mb-6">Real-time Pipeline Monitor</h1>
    
    <div class="grid grid-cols-1 xl:grid-cols-3 gap-6">
      <!-- Pipeline Flow Visualization -->
      <div class="xl:col-span-2 bg-gray-800 rounded-lg p-4">
        <h2 class="text-xl mb-4">Pipeline Flow</h2>
        <div ref="pipelineNetwork" class="h-96"></div>
      </div>
      
      <!-- Metrics Panel -->
      <div class="bg-gray-800 rounded-lg p-4">
        <h2 class="text-xl mb-4">Live Metrics</h2>
        <div class="space-y-4">
          <MetricCard 
            v-for="stage in pipelineStages" 
            :key="stage.id"
            :stage="stage"
            :metrics="stageMetrics[stage.id]"
          />
        </div>
      </div>
    </div>
    
    <!-- Message Stream -->
    <div class="mt-6 bg-gray-800 rounded-lg p-4">
      <h2 class="text-xl mb-4">Message Stream</h2>
      <div class="h-48 overflow-y-auto font-mono text-sm">
        <div v-for="msg in messages" :key="msg.id" class="mb-1">
          <span class="text-gray-400">{{ formatTime(msg.timestamp) }}</span>
          <span :class="getStageColor(msg.stage)" class="ml-2">{{ msg.stage }}</span>
          <span class="ml-2">{{ msg.content }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { Network } from 'vis-network/standalone'

const pipelineNetwork = ref(null)
const { connectToPipeline, disconnectPipeline } = usePipelineWebSocket()

const pipelineStages = [
  { id: 'typer', name: '80/20 Typer', x: 0, y: 0 },
  { id: 'turtle', name: 'Turtle Gen', x: 150, y: 0 },
  { id: 'ttl2dspy', name: 'TTL2DSPy', x: 300, y: 0 },
  { id: 'bitactor', name: 'BitActor', x: 450, y: 0 },
  { id: 'erlang', name: 'Erlang OTP', x: 600, y: 0 },
  { id: 'ash', name: 'Ash Resources', x: 750, y: 0 },
  { id: 'reactor', name: 'Reactor', x: 900, y: 0 },
  { id: 'k8s', name: 'Kubernetes', x: 1050, y: 0 }
]

const stageMetrics = ref({})
const messages = ref([])

onMounted(() => {
  initializePipelineNetwork()
  connectToPipeline({
    onMetrics: updateMetrics,
    onMessage: addMessage
  })
})

onUnmounted(() => {
  disconnectPipeline()
})

const initializePipelineNetwork = () => {
  const nodes = pipelineStages.map(stage => ({
    id: stage.id,
    label: stage.name,
    x: stage.x,
    y: stage.y,
    color: '#10b981',
    font: { color: '#ffffff' }
  }))
  
  const edges = []
  for (let i = 0; i < pipelineStages.length - 1; i++) {
    edges.push({
      from: pipelineStages[i].id,
      to: pipelineStages[i + 1].id,
      arrows: 'to',
      color: '#6b7280'
    })
  }
  
  const data = { nodes, edges }
  const options = {
    physics: false,
    interaction: { hover: true }
  }
  
  new Network(pipelineNetwork.value, data, options)
}

const updateMetrics = (metrics) => {
  stageMetrics.value = metrics
}

const addMessage = (message) => {
  messages.value.unshift({
    ...message,
    id: Date.now() + Math.random()
  })
  if (messages.value.length > 100) {
    messages.value = messages.value.slice(0, 100)
  }
}

const formatTime = (timestamp) => {
  return new Date(timestamp).toLocaleTimeString()
}

const getStageColor = (stage) => {
  const colors = {
    typer: 'text-blue-400',
    turtle: 'text-green-400',
    ttl2dspy: 'text-yellow-400',
    bitactor: 'text-orange-400',
    erlang: 'text-red-400',
    ash: 'text-purple-400',
    reactor: 'text-pink-400',
    k8s: 'text-indigo-400'
  }
  return colors[stage] || 'text-gray-400'
}
</script>