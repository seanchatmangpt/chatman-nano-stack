<template>
  <UCard 
    :ui="{ 
      base: 'transition-all duration-200 hover:scale-105',
      background: getBackgroundColor(metric.status)
    }"
  >
    <div class="flex items-center justify-between">
      <div>
        <p class="text-sm text-gray-400">{{ metric.label }}</p>
        <p class="text-2xl font-bold">{{ metric.value }}</p>
        <div class="flex items-center mt-1">
          <UIcon 
            :name="getChangeIcon(metric.change)" 
            :class="getChangeColor(metric.change)"
            class="w-4 h-4 mr-1"
          />
          <span :class="getChangeColor(metric.change)" class="text-sm">
            {{ metric.change }}
          </span>
        </div>
      </div>
      <div :class="getStatusColor(metric.status)" class="w-12 h-12 rounded-full flex items-center justify-center">
        <UIcon :name="getStatusIcon(metric.status)" class="w-6 h-6" />
      </div>
    </div>
  </UCard>
</template>

<script setup>
defineProps({
  metric: {
    type: Object,
    required: true
  }
})

const getBackgroundColor = (status) => {
  const colors = {
    success: 'bg-green-950/20 border-green-800/30',
    warning: 'bg-yellow-950/20 border-yellow-800/30',
    error: 'bg-red-950/20 border-red-800/30'
  }
  return colors[status] || 'bg-gray-900'
}

const getStatusColor = (status) => {
  const colors = {
    success: 'bg-green-900/50 text-green-400',
    warning: 'bg-yellow-900/50 text-yellow-400',
    error: 'bg-red-900/50 text-red-400'
  }
  return colors[status] || 'bg-gray-700 text-gray-400'
}

const getStatusIcon = (status) => {
  const icons = {
    success: 'i-heroicons-check-circle',
    warning: 'i-heroicons-exclamation-triangle',
    error: 'i-heroicons-x-circle'
  }
  return icons[status] || 'i-heroicons-question-mark-circle'
}

const getChangeIcon = (change) => {
  if (change.startsWith('+')) return 'i-heroicons-arrow-trending-up'
  if (change.startsWith('-')) return 'i-heroicons-arrow-trending-down'
  return 'i-heroicons-minus'
}

const getChangeColor = (change) => {
  if (change.startsWith('+')) return 'text-green-400'
  if (change.startsWith('-')) return 'text-red-400'
  return 'text-gray-400'
}
</script>