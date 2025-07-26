<template>
  <div class="bg-gray-700 rounded p-3">
    <div class="flex justify-between items-center mb-2">
      <h3 class="font-medium">{{ stage.name }}</h3>
      <span class="text-xs px-2 py-1 rounded" :class="statusClass">
        {{ metrics?.status || 'waiting' }}
      </span>
    </div>
    <div class="grid grid-cols-2 gap-2 text-sm">
      <div>
        <span class="text-gray-400">Latency:</span>
        <span class="ml-1 font-mono">{{ metrics?.latency || '--' }}ms</span>
      </div>
      <div>
        <span class="text-gray-400">Msgs/s:</span>
        <span class="ml-1 font-mono">{{ formatNumber(metrics?.throughput) }}</span>
      </div>
    </div>
    <div class="mt-2">
      <div class="h-2 bg-gray-600 rounded">
        <div 
          class="h-full bg-green-500 rounded transition-all duration-300"
          :style="{ width: `${metrics?.cpu || 0}%` }"
        ></div>
      </div>
    </div>
  </div>
</template>

<script setup>
defineProps({
  stage: Object,
  metrics: Object
})

const statusClass = computed(() => {
  const status = props.metrics?.status
  if (status === 'active') return 'bg-green-600 text-white'
  if (status === 'error') return 'bg-red-600 text-white'
  return 'bg-gray-600 text-gray-300'
})

const formatNumber = (num) => {
  if (!num) return '--'
  return new Intl.NumberFormat().format(num)
}
</script>