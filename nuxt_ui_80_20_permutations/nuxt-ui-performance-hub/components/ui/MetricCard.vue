<template>
  <UCard 
    class="cursor-pointer transition-all duration-200 hover:shadow-lg hover:scale-105"
    @click="$emit('drill-down', metric)"
  >
    <div class="flex items-center justify-between">
      <div class="flex-1">
        <p class="text-sm font-medium text-gray-600">{{ metric.label }}</p>
        <p class="text-2xl font-bold text-gray-900 mt-1">{{ metric.value }}</p>
        <div class="flex items-center mt-2">
          <UIcon 
            :name="getTrendIcon(metric.trend)" 
            :class="getTrendColor(metric.trend)"
            class="w-4 h-4 mr-1"
          />
          <span :class="getTrendColor(metric.trend)" class="text-sm font-medium">
            {{ metric.change }}
          </span>
        </div>
      </div>
      
      <div :class="getMetricColor(metric.color)" class="w-12 h-12 rounded-full flex items-center justify-center">
        <UIcon :name="getMetricIcon(metric.id)" class="w-6 h-6" />
      </div>
    </div>
    
    <!-- Mini sparkline -->
    <div class="mt-4 h-8">
      <MiniSparkline :data="metric.sparklineData" :color="metric.color" />
    </div>
  </UCard>
</template>

<script setup>
defineEmits(['drill-down'])

defineProps({
  metric: {
    type: Object,
    required: true
  }
})

const getTrendIcon = (trend) => {
  const icons = {
    up: 'i-heroicons-arrow-trending-up',
    down: 'i-heroicons-arrow-trending-down',
    stable: 'i-heroicons-minus'
  }
  return icons[trend] || 'i-heroicons-minus'
}

const getTrendColor = (trend) => {
  const colors = {
    up: 'text-green-600',
    down: 'text-red-600',
    stable: 'text-gray-500'
  }
  return colors[trend] || 'text-gray-500'
}

const getMetricColor = (color) => {
  const colors = {
    green: 'bg-green-100 text-green-600',
    blue: 'bg-blue-100 text-blue-600',
    orange: 'bg-orange-100 text-orange-600',
    red: 'bg-red-100 text-red-600'
  }
  return colors[color] || 'bg-gray-100 text-gray-600'
}

const getMetricIcon = (id) => {
  const icons = {
    throughput: 'i-heroicons-arrow-trending-up',
    latency: 'i-heroicons-clock',
    errors: 'i-heroicons-exclamation-triangle',
    availability: 'i-heroicons-check-circle'
  }
  return icons[id] || 'i-heroicons-chart-bar'
}
</script>