<template>
  <div class="min-h-screen bg-gray-50">
    <!-- Header -->
    <header class="bg-white border-b border-gray-200 px-6 py-4">
      <div class="flex items-center justify-between">
        <div>
          <h1 class="text-2xl font-bold text-gray-900">Performance Analytics Hub</h1>
          <p class="text-gray-600 mt-1">Real-time system performance monitoring</p>
        </div>
        
        <div class="flex items-center space-x-4">
          <USelect v-model="timeRange" :options="timeRangeOptions" />
          <UButton @click="refreshData" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-path" />
          </UButton>
          <UButton @click="exportMetrics" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-down-tray" />
          </UButton>
        </div>
      </div>
    </header>
    
    <!-- Overview Cards -->
    <div class="p-6">
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <UCard 
          v-for="metric in overviewMetrics" 
          :key="metric.id"
          class="cursor-pointer hover:shadow-md"
          @click="drillDown(metric)"
        >
          <div class="text-center">
            <div class="text-2xl font-bold mb-1" :class="getColorClass(metric.color)">
              {{ metric.value }}
            </div>
            <div class="text-sm text-gray-600 mb-2">{{ metric.label }}</div>
            <UBadge :color="metric.trend === 'up' ? 'green' : metric.trend === 'down' ? 'red' : 'gray'" size="xs">
              {{ metric.change }}
            </UBadge>
          </div>
        </UCard>
      </div>
      
      <!-- Charts Grid -->
      <div class="grid grid-cols-1 xl:grid-cols-2 gap-6 mb-8">
        <!-- Throughput Chart -->
        <UCard>
          <template #header>
            <div class="flex items-center justify-between">
              <h3 class="font-semibold">Pipeline Throughput</h3>
              <UButton variant="ghost" size="sm">
                <UIcon name="i-heroicons-cog-6-tooth" />
              </UButton>
            </div>
          </template>
          <div class="h-64 flex items-center justify-center bg-gray-50 rounded">
            <div class="text-center text-gray-500">
              <UIcon name="i-heroicons-chart-bar" class="w-12 h-12 mx-auto mb-2" />
              <p>Throughput: 125.4K req/s</p>
              <p class="text-sm">Real-time monitoring active</p>
            </div>
          </div>
        </UCard>
        
        <!-- Latency Distribution -->
        <UCard>
          <template #header>
            <h3 class="font-semibold">Latency Distribution</h3>
          </template>
          <div class="h-64 flex items-center justify-center bg-gray-50 rounded">
            <div class="text-center text-gray-500">
              <UIcon name="i-heroicons-clock" class="w-12 h-12 mx-auto mb-2" />
              <p>P95 Latency: 45ms</p>
              <p class="text-sm">8.2% improvement</p>
            </div>
          </div>
        </UCard>
        
        <!-- Resource Utilization -->
        <UCard>
          <template #header>
            <h3 class="font-semibold">Resource Utilization</h3>
          </template>
          <div class="space-y-4">
            <div class="flex justify-between items-center">
              <span>CPU Usage</span>
              <div class="flex items-center space-x-2">
                <div class="w-24 bg-gray-200 rounded-full h-2">
                  <div class="bg-blue-500 h-2 rounded-full" style="width: 65%"></div>
                </div>
                <span class="text-sm">65%</span>
              </div>
            </div>
            <div class="flex justify-between items-center">
              <span>Memory Usage</span>
              <div class="flex items-center space-x-2">
                <div class="w-24 bg-gray-200 rounded-full h-2">
                  <div class="bg-green-500 h-2 rounded-full" style="width: 45%"></div>
                </div>
                <span class="text-sm">45%</span>
              </div>
            </div>
            <div class="flex justify-between items-center">
              <span>Disk I/O</span>
              <div class="flex items-center space-x-2">
                <div class="w-24 bg-gray-200 rounded-full h-2">
                  <div class="bg-yellow-500 h-2 rounded-full" style="width: 80%"></div>
                </div>
                <span class="text-sm">80%</span>
              </div>
            </div>
          </div>
        </UCard>
        
        <!-- Error Analysis -->
        <UCard>
          <template #header>
            <h3 class="font-semibold">Error Analysis</h3>
          </template>
          <div class="space-y-3">
            <div class="flex justify-between">
              <span class="text-sm">Total Errors</span>
              <span class="text-sm font-medium">147</span>
            </div>
            <div class="flex justify-between">
              <span class="text-sm">Error Rate</span>
              <span class="text-sm font-medium text-orange-600">0.12%</span>
            </div>
            <div class="flex justify-between">
              <span class="text-sm">Most Common</span>
              <span class="text-sm font-medium">Timeout</span>
            </div>
          </div>
        </UCard>
      </div>
      
      <!-- System Topology -->
      <UCard class="mb-8">
        <template #header>
          <div class="flex items-center justify-between">
            <h3 class="font-semibold">System Topology</h3>
            <UButton variant="ghost" size="sm">
              <UIcon name="i-heroicons-eye" />
            </UButton>
          </div>
        </template>
        <div class="p-4">
          <div class="grid grid-cols-2 md:grid-cols-4 lg:grid-cols-7 gap-4">
            <div v-for="node in systemNodes" :key="node.id" class="text-center">
              <div class="w-16 h-16 mx-auto mb-2 rounded-full flex items-center justify-center"
                   :class="node.health === 'healthy' ? 'bg-green-100' : 'bg-yellow-100'">
                <UIcon name="i-heroicons-server" 
                       :class="node.health === 'healthy' ? 'text-green-600' : 'text-yellow-600'" />
              </div>
              <div class="text-xs font-medium">{{ node.label }}</div>
              <UBadge :color="node.health === 'healthy' ? 'green' : 'yellow'" size="xs" class="mt-1">
                {{ node.health }}
              </UBadge>
            </div>
          </div>
        </div>
      </UCard>
      
      <!-- Performance Tables -->
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <!-- BitActor Performance -->
        <UCard>
          <template #header>
            <h3 class="font-semibold">BitActor Performance</h3>
          </template>
          <div class="overflow-x-auto">
            <table class="w-full">
              <thead>
                <tr class="border-b border-gray-200">
                  <th class="text-left p-2 font-medium">Node</th>
                  <th class="text-left p-2 font-medium">CPU</th>
                  <th class="text-left p-2 font-medium">Memory</th>
                  <th class="text-left p-2 font-medium">Status</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="metric in bitactorMetrics" :key="metric.id" class="border-b border-gray-100">
                  <td class="p-2">{{ metric.node }}</td>
                  <td class="p-2">{{ metric.cpu }}%</td>
                  <td class="p-2">{{ metric.memory }}%</td>
                  <td class="p-2">
                    <UBadge :color="metric.status === 'healthy' ? 'green' : 'red'" size="xs">
                      {{ metric.status }}
                    </UBadge>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </UCard>
        
        <!-- K8s Pod Metrics -->
        <UCard>
          <template #header>
            <h3 class="font-semibold">Kubernetes Pods</h3>
          </template>
          <div class="overflow-x-auto">
            <table class="w-full">
              <thead>
                <tr class="border-b border-gray-200">
                  <th class="text-left p-2 font-medium">Pod</th>
                  <th class="text-left p-2 font-medium">CPU</th>
                  <th class="text-left p-2 font-medium">Memory</th>
                  <th class="text-left p-2 font-medium">Status</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="metric in k8sMetrics" :key="metric.id" class="border-b border-gray-100">
                  <td class="p-2">{{ metric.pod }}</td>
                  <td class="p-2">{{ metric.cpu }}%</td>
                  <td class="p-2">{{ metric.memory }}%</td>
                  <td class="p-2">
                    <UBadge :color="metric.status === 'Running' ? 'green' : 'red'" size="xs">
                      {{ metric.status }}
                    </UBadge>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </UCard>
      </div>
    </div>
    
    <!-- Drill-down Modal -->
    <UModal v-model="showDrillDown">
      <UCard>
        <template #header>
          <h3 class="font-medium">{{ drillDownTitle }}</h3>
        </template>
        <div v-if="drillDownData" class="space-y-4">
          <div>
            <h4 class="font-medium mb-2">Current Value</h4>
            <div class="text-2xl font-bold" :class="getColorClass(drillDownData.color)">
              {{ drillDownData.value }}
            </div>
          </div>
          <div>
            <h4 class="font-medium mb-2">Change</h4>
            <UBadge :color="drillDownData.trend === 'up' ? 'green' : drillDownData.trend === 'down' ? 'red' : 'gray'">
              {{ drillDownData.change }}
            </UBadge>
          </div>
          <div>
            <h4 class="font-medium mb-2">Trend</h4>
            <p class="text-sm text-gray-600">{{ drillDownData.trend }} over selected time range</p>
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

const timeRange = ref('1h')
const showDrillDown = ref(false)
const drillDownTitle = ref('')
const drillDownData = ref(null)

const timeRangeOptions = [
  { label: 'Last Hour', value: '1h' },
  { label: 'Last 6 Hours', value: '6h' },
  { label: 'Last Day', value: '24h' },
  { label: 'Last Week', value: '7d' }
]

const overviewMetrics = ref([
  { 
    id: 'throughput', 
    label: 'Total Throughput', 
    value: '125.4K req/s', 
    change: '+12.3%', 
    trend: 'up',
    color: 'green'
  },
  { 
    id: 'latency', 
    label: 'P95 Latency', 
    value: '45ms', 
    change: '-8.2%', 
    trend: 'down',
    color: 'blue'
  },
  { 
    id: 'errors', 
    label: 'Error Rate', 
    value: '0.12%', 
    change: '+0.03%', 
    trend: 'up',
    color: 'orange'
  },
  { 
    id: 'availability', 
    label: 'Availability', 
    value: '99.97%', 
    change: '0%', 
    trend: 'stable',
    color: 'green'
  }
])

const systemNodes = ref([
  { id: 'typer', label: '80/20 Typer', type: 'process', health: 'healthy' },
  { id: 'turtle', label: 'Turtle Gen', type: 'process', health: 'healthy' },
  { id: 'ttl2dspy', label: 'TTL2DSPy', type: 'process', health: 'healthy' },
  { id: 'bitactor', label: 'BitActor Cluster', type: 'cluster', health: 'healthy' },
  { id: 'ash', label: 'Ash Resources', type: 'service', health: 'warning' },
  { id: 'reactor', label: 'Reactor Engine', type: 'service', health: 'healthy' },
  { id: 'k8s', label: 'K8s Cluster', type: 'infrastructure', health: 'healthy' }
])

const bitactorMetrics = ref([
  { id: 1, node: 'bitactor-01', cpu: 65, memory: 72, status: 'healthy' },
  { id: 2, node: 'bitactor-02', cpu: 58, memory: 68, status: 'healthy' },
  { id: 3, node: 'bitactor-03', cpu: 71, memory: 75, status: 'healthy' }
])

const k8sMetrics = ref([
  { id: 1, pod: 'ash-reactor-1', cpu: 45, memory: 60, status: 'Running' },
  { id: 2, pod: 'ash-reactor-2', cpu: 52, memory: 65, status: 'Running' },
  { id: 3, pod: 'ttl-processor-1', cpu: 38, memory: 55, status: 'Running' }
])

const getColorClass = (color) => {
  const colorMap = {
    green: 'text-green-600',
    blue: 'text-blue-600',
    orange: 'text-orange-600',
    red: 'text-red-600'
  }
  return colorMap[color] || 'text-gray-600'
}

const drillDown = (metric) => {
  drillDownTitle.value = `${metric.label} Details`
  drillDownData.value = metric
  showDrillDown.value = true
}

const refreshData = () => {
  console.log('Refreshing performance data...')
}

const exportMetrics = () => {
  console.log('Exporting metrics data...')
}

watch(timeRange, (newRange) => {
  console.log('Time range changed to:', newRange)
})
</script>