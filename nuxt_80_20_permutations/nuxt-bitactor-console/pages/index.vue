<template>
  <div class="min-h-screen bg-gray-900 text-white">
    <nav class="bg-gray-800 p-4">
      <div class="container mx-auto flex justify-between items-center">
        <h1 class="text-2xl font-bold">BitActor Control Console</h1>
        <div class="flex items-center space-x-4">
          <span class="text-green-400">● Connected</span>
          <span>Node: {{ currentNode }}</span>
        </div>
      </div>
    </nav>
    
    <div class="container mx-auto p-6">
      <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
        <!-- Actor List -->
        <div class="bg-gray-800 rounded-lg p-4">
          <h2 class="text-xl mb-4">Active Actors</h2>
          <div class="space-y-2">
            <ActorCard 
              v-for="actor in actors" 
              :key="actor.id"
              :actor="actor"
              @select="selectActor"
            />
          </div>
          <button @click="spawnActor" class="mt-4 w-full bg-blue-600 px-4 py-2 rounded hover:bg-blue-700">
            + Spawn New Actor
          </button>
        </div>
        
        <!-- Actor Details -->
        <div class="bg-gray-800 rounded-lg p-4">
          <h2 class="text-xl mb-4">Actor Details</h2>
          <div v-if="selectedActor">
            <div class="space-y-4">
              <div>
                <label class="text-gray-400">ID</label>
                <p class="font-mono">{{ selectedActor.id }}</p>
              </div>
              <div>
                <label class="text-gray-400">Type</label>
                <p>{{ selectedActor.type }}</p>
              </div>
              <div>
                <label class="text-gray-400">Status</label>
                <p :class="getStatusColor(selectedActor.status)">
                  {{ selectedActor.status }}
                </p>
              </div>
              <div>
                <label class="text-gray-400">Messages Processed</label>
                <p>{{ formatNumber(selectedActor.messageCount) }}</p>
              </div>
              <div>
                <label class="text-gray-400">Average Latency</label>
                <p>{{ selectedActor.avgLatency }}μs</p>
              </div>
            </div>
            <div class="mt-6 space-y-2">
              <button @click="sendMessage" class="w-full bg-green-600 px-4 py-2 rounded hover:bg-green-700">
                Send Message
              </button>
              <button @click="restartActor" class="w-full bg-yellow-600 px-4 py-2 rounded hover:bg-yellow-700">
                Restart Actor
              </button>
              <button @click="terminateActor" class="w-full bg-red-600 px-4 py-2 rounded hover:bg-red-700">
                Terminate Actor
              </button>
            </div>
          </div>
          <div v-else class="text-gray-500 text-center py-8">
            Select an actor to view details
          </div>
        </div>
        
        <!-- Performance Metrics -->
        <div class="bg-gray-800 rounded-lg p-4">
          <h2 class="text-xl mb-4">System Performance</h2>
          <div class="space-y-4">
            <PerformanceMetric 
              label="Total Actors" 
              :value="systemMetrics.totalActors"
              unit=""
            />
            <PerformanceMetric 
              label="Messages/sec" 
              :value="systemMetrics.messagesPerSec"
              unit="msg/s"
            />
            <PerformanceMetric 
              label="CPU Usage" 
              :value="systemMetrics.cpuUsage"
              unit="%"
              :showBar="true"
            />
            <PerformanceMetric 
              label="Memory Usage" 
              :value="systemMetrics.memoryUsage"
              unit="%"
              :showBar="true"
            />
            <PerformanceMetric 
              label="Network I/O" 
              :value="systemMetrics.networkIO"
              unit="MB/s"
            />
          </div>
        </div>
      </div>
      
      <!-- Message Log -->
      <div class="mt-6 bg-gray-800 rounded-lg p-4">
        <h2 class="text-xl mb-4">Message Log</h2>
        <MessageLog :messages="recentMessages" />
      </div>
    </div>
  </div>
</template>

<script setup>
const { fetchActors, spawnNewActor, sendActorMessage } = useBitActorAPI()

const currentNode = ref('node1@localhost')
const actors = ref([])
const selectedActor = ref(null)
const systemMetrics = ref({
  totalActors: 0,
  messagesPerSec: 0,
  cpuUsage: 0,
  memoryUsage: 0,
  networkIO: 0
})
const recentMessages = ref([])

onMounted(async () => {
  await refreshActors()
  startMetricsPolling()
})

const refreshActors = async () => {
  actors.value = await fetchActors()
  systemMetrics.value.totalActors = actors.value.length
}

const selectActor = (actor) => {
  selectedActor.value = actor
}

const spawnActor = async () => {
  const actorType = prompt('Enter actor type (worker, processor, monitor):')
  if (actorType) {
    await spawnNewActor({ type: actorType })
    await refreshActors()
  }
}

const sendMessage = async () => {
  const message = prompt('Enter message to send:')
  if (message && selectedActor.value) {
    await sendActorMessage(selectedActor.value.id, message)
  }
}

const restartActor = async () => {
  if (confirm('Restart this actor?')) {
    // Implementation
  }
}

const terminateActor = async () => {
  if (confirm('Terminate this actor? This cannot be undone.')) {
    // Implementation
  }
}

const startMetricsPolling = () => {
  setInterval(() => {
    // Update metrics
    systemMetrics.value.messagesPerSec = Math.floor(Math.random() * 10000)
    systemMetrics.value.cpuUsage = Math.floor(Math.random() * 100)
    systemMetrics.value.memoryUsage = Math.floor(Math.random() * 100)
    systemMetrics.value.networkIO = (Math.random() * 100).toFixed(2)
  }, 1000)
}

const getStatusColor = (status) => {
  const colors = {
    active: 'text-green-400',
    idle: 'text-yellow-400',
    error: 'text-red-400',
    terminated: 'text-gray-400'
  }
  return colors[status] || 'text-gray-400'
}

const formatNumber = (num) => {
  return new Intl.NumberFormat().format(num)
}
</script>