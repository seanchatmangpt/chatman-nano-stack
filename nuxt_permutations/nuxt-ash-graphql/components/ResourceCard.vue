<template>
  <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow cursor-pointer">
    <div class="flex items-center justify-between mb-4">
      <h3 class="text-xl font-semibold text-gray-800">{{ resource.name }}</h3>
      <span class="px-3 py-1 text-sm font-medium rounded-full" :class="typeColors[resource.type] || 'bg-gray-100 text-gray-800'">
        {{ resource.type }}
      </span>
    </div>
    
    <div class="space-y-2 mb-4">
      <div v-for="(value, key) in displayAttributes" :key="key" class="flex justify-between">
        <span class="text-gray-600 text-sm">{{ formatKey(key) }}:</span>
        <span class="text-gray-800 text-sm font-medium">{{ formatValue(value) }}</span>
      </div>
    </div>
    
    <div v-if="resource.relationships && resource.relationships.length > 0" class="border-t pt-3">
      <p class="text-sm text-gray-600 mb-2">Relationships:</p>
      <div class="flex flex-wrap gap-1">
        <span 
          v-for="rel in resource.relationships.slice(0, 3)" 
          :key="rel.target"
          class="px-2 py-1 text-xs bg-blue-100 text-blue-800 rounded"
        >
          {{ rel.type }}
        </span>
        <span v-if="resource.relationships.length > 3" class="px-2 py-1 text-xs bg-gray-100 text-gray-600 rounded">
          +{{ resource.relationships.length - 3 }} more
        </span>
      </div>
    </div>
    
    <div class="mt-4 text-xs text-gray-500">
      Created: {{ formatDate(resource.created_at) }}
    </div>
  </div>
</template>

<script setup>
const props = defineProps({
  resource: {
    type: Object,
    required: true
  }
})

const typeColors = {
  User: 'bg-green-100 text-green-800',
  Order: 'bg-blue-100 text-blue-800',
  Product: 'bg-purple-100 text-purple-800',
  Payment: 'bg-yellow-100 text-yellow-800',
  Admin: 'bg-red-100 text-red-800'
}

const displayAttributes = computed(() => {
  const attrs = props.resource.attributes || {}
  // Show first 3 attributes
  return Object.fromEntries(
    Object.entries(attrs).slice(0, 3)
  )
})

const formatKey = (key) => {
  return key.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())
}

const formatValue = (value) => {
  if (typeof value === 'boolean') return value ? 'Yes' : 'No'
  if (typeof value === 'string' && value.length > 20) return value.substring(0, 20) + '...'
  return value
}

const formatDate = (dateString) => {
  if (!dateString) return 'Unknown'
  return new Date(dateString).toLocaleDateString()
}
</script>