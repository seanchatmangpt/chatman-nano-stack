<template>
  <div class="min-h-screen bg-gray-900 text-white p-6">
    <h1 class="text-3xl font-bold mb-6">BitActor Performance Dashboard</h1>
    
    <div class="grid grid-cols-1 lg:grid-cols-2 xl:grid-cols-3 gap-6">
      <!-- Latency Chart -->
      <div class="bg-gray-800 p-4 rounded-lg">
        <h2 class="text-xl mb-4">Latency (μs)</h2>
        <canvas ref="latencyChart"></canvas>
        <div class="mt-2 text-green-400">
          Current: {{ currentLatency }}μs
        </div>
      </div>
      
      <!-- Throughput Chart -->
      <div class="bg-gray-800 p-4 rounded-lg">
        <h2 class="text-xl mb-4">Throughput (ops/sec)</h2>
        <canvas ref="throughputChart"></canvas>
        <div class="mt-2 text-blue-400">
          Current: {{ formatNumber(currentThroughput) }} ops/sec
        </div>
      </div>
      
      <!-- Actor Status -->
      <div class="bg-gray-800 p-4 rounded-lg">
        <h2 class="text-xl mb-4">Actor Status</h2>
        <div v-for="actor in actors" :key="actor.id" class="mb-2">
          <div class="flex justify-between">
            <span>{{ actor.name }}</span>
            <span :class="statusColor(actor.status)">
              {{ actor.status }}
            </span>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Message Log -->
    <div class="mt-8 bg-gray-800 p-4 rounded-lg">
      <h2 class="text-xl mb-4">Live Message Stream</h2>
      <div class="h-64 overflow-y-auto font-mono text-sm">
        <div v-for="message in recentMessages" :key="message.id" class="mb-1">
          <span class="text-gray-400">{{ formatTime(message.timestamp) }}</span>
          <span class="text-yellow-400 ml-2">{{ message.actor }}</span>
          <span class="ml-2">{{ message.content }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted, onUnmounted } from 'vue'

const { connectToBitActor, disconnect } = useBitActorWS()

const currentLatency = ref(0)
const currentThroughput = ref(0)
const actors = ref([])
const recentMessages = ref([])

const latencyChart = ref(null)
const throughputChart = ref(null)

let charts = {}

onMounted(async () => {
  // Initialize charts
  await initializeCharts()
  
  // Connect to BitActor WebSocket
  connectToBitActor({
    onMetrics: (metrics) => {
      currentLatency.value = metrics.latency
      currentThroughput.value = metrics.throughput
      updateCharts(metrics)
    },
    onActorUpdate: (actorData) => {
      updateActorStatus(actorData)
    },
    onMessage: (message) => {
      addMessage(message)
    }
  })
})

onUnmounted(() => {
  disconnect()
})

const initializeCharts = async () => {
  const Chart = await import('chart.js/auto')
  
  // Latency chart
  charts.latency = new Chart.default(latencyChart.value, {
    type: 'line',
    data: {
      labels: [],
      datasets: [{
        label: 'Latency (μs)',
        data: [],
        borderColor: 'rgb(34, 197, 94)',
        backgroundColor: 'rgba(34, 197, 94, 0.1)',
        tension: 0.4
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: false }
      },
      scales: {
        y: { 
          beginAtZero: true,
          grid: { color: 'rgba(255, 255, 255, 0.1)' },
          ticks: { color: 'rgba(255, 255, 255, 0.8)' }
        },
        x: {
          grid: { color: 'rgba(255, 255, 255, 0.1)' },
          ticks: { color: 'rgba(255, 255, 255, 0.8)' }
        }
      }
    }
  })
  
  // Throughput chart
  charts.throughput = new Chart.default(throughputChart.value, {
    type: 'line',
    data: {
      labels: [],
      datasets: [{
        label: 'Throughput (ops/sec)',
        data: [],
        borderColor: 'rgb(59, 130, 246)',
        backgroundColor: 'rgba(59, 130, 246, 0.1)',
        tension: 0.4
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: false }
      },
      scales: {
        y: { 
          beginAtZero: true,
          grid: { color: 'rgba(255, 255, 255, 0.1)' },
          ticks: { color: 'rgba(255, 255, 255, 0.8)' }
        },
        x: {
          grid: { color: 'rgba(255, 255, 255, 0.1)' },
          ticks: { color: 'rgba(255, 255, 255, 0.8)' }
        }
      }
    }
  })
}

const formatNumber = (num) => {
  return new Intl.NumberFormat().format(num)
}

const formatTime = (timestamp) => {
  return new Date(timestamp).toLocaleTimeString()
}

const updateCharts = (metrics) => {
  const now = new Date().toLocaleTimeString()
  
  // Update latency chart
  if (charts.latency) {
    charts.latency.data.labels.push(now)
    charts.latency.data.datasets[0].data.push(metrics.latency)
    
    // Keep only last 20 data points
    if (charts.latency.data.labels.length > 20) {
      charts.latency.data.labels.shift()
      charts.latency.data.datasets[0].data.shift()
    }
    
    charts.latency.update('none')
  }
  
  // Update throughput chart
  if (charts.throughput) {
    charts.throughput.data.labels.push(now)
    charts.throughput.data.datasets[0].data.push(metrics.throughput)
    
    if (charts.throughput.data.labels.length > 20) {
      charts.throughput.data.labels.shift()
      charts.throughput.data.datasets[0].data.shift()
    }
    
    charts.throughput.update('none')
  }
}

const updateActorStatus = (actorData) => {
  const existingActor = actors.value.find(a => a.id === actorData.id)
  if (existingActor) {
    Object.assign(existingActor, actorData)
  } else {
    actors.value.push(actorData)
  }
}

const addMessage = (message) => {
  const messageWithId = {
    ...message,
    id: Date.now() + Math.random(),
    timestamp: Date.now()
  }
  
  recentMessages.value.unshift(messageWithId)
  
  // Keep only last 100 messages
  if (recentMessages.value.length > 100) {
    recentMessages.value = recentMessages.value.slice(0, 100)
  }
}

const statusColor = (status) => {
  const colors = {
    active: 'text-green-400',
    idle: 'text-yellow-400',
    error: 'text-red-400'
  }
  return colors[status] || 'text-gray-400'
}
</script>