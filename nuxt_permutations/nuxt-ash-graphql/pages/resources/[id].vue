<template>
  <div class="container mx-auto px-4 py-8">
    <nav class="mb-6">
      <button @click="$router.back()" class="text-blue-600 hover:text-blue-800 flex items-center">
        ← Back to Resources
      </button>
    </nav>
    
    <div v-if="pending" class="text-center">
      <div class="spinner"></div>
      Loading resource details...
    </div>
    
    <div v-else-if="resource" class="bg-white rounded-lg shadow-lg p-8">
      <div class="flex justify-between items-start mb-6">
        <div>
          <h1 class="text-3xl font-bold text-gray-800 mb-2">{{ resource.name }}</h1>
          <span class="px-4 py-2 text-sm font-medium rounded-full" :class="typeColors[resource.type] || 'bg-gray-100 text-gray-800'">
            {{ resource.type }}
          </span>
        </div>
        <div class="text-right text-sm text-gray-600">
          <p>ID: {{ resource.id }}</p>
          <p>Created: {{ formatDate(resource.created_at) }}</p>
          <p v-if="resource.updated_at">Updated: {{ formatDate(resource.updated_at) }}</p>
        </div>
      </div>
      
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-8">
        <!-- Attributes -->
        <div>
          <h2 class="text-xl font-semibold mb-4">Attributes</h2>
          <div class="bg-gray-50 rounded-lg p-4">
            <div v-if="resource.attributes && Object.keys(resource.attributes).length > 0" class="space-y-3">
              <div v-for="(value, key) in resource.attributes" :key="key" class="border-b border-gray-200 pb-2 last:border-b-0">
                <dt class="text-sm font-medium text-gray-600">{{ formatKey(key) }}</dt>
                <dd class="text-sm text-gray-800 mt-1">{{ formatValue(value) }}</dd>
              </div>
            </div>
            <p v-else class="text-gray-500 text-sm">No attributes available</p>
          </div>
        </div>
        
        <!-- Relationships -->
        <div>
          <h2 class="text-xl font-semibold mb-4">Relationships</h2>
          <div class="bg-gray-50 rounded-lg p-4">
            <div v-if="resource.relationships && resource.relationships.length > 0" class="space-y-3">
              <div v-for="rel in resource.relationships" :key="rel.target" class="flex justify-between items-center p-3 bg-white rounded border">
                <div>
                  <p class="font-medium text-gray-800">{{ rel.type }}</p>
                  <p class="text-sm text-gray-600">Target: {{ rel.target }}</p>
                </div>
                <button class="text-blue-600 hover:text-blue-800 text-sm font-medium">
                  View →
                </button>
              </div>
            </div>
            <p v-else class="text-gray-500 text-sm">No relationships found</p>
          </div>
        </div>
      </div>
      
      <!-- Actions -->
      <div class="mt-8 pt-6 border-t border-gray-200">
        <div class="flex space-x-4">
          <button class="bg-blue-600 text-white px-6 py-2 rounded-lg hover:bg-blue-700 transition-colors">
            Edit Resource
          </button>
          <button class="bg-gray-200 text-gray-800 px-6 py-2 rounded-lg hover:bg-gray-300 transition-colors">
            Clone Resource
          </button>
          <button class="bg-red-600 text-white px-6 py-2 rounded-lg hover:bg-red-700 transition-colors ml-auto">
            Delete Resource
          </button>
        </div>
      </div>
    </div>
    
    <div v-else class="text-center text-gray-500">
      Resource not found
    </div>
  </div>
</template>

<script setup>
const route = useRoute()
const resourceId = route.params.id

const { data: resource, pending } = await useAsyncQuery(gql`
  query GetResource($id: ID!) {
    resource(id: $id) {
      id
      name
      type
      attributes
      relationships {
        type
        target
      }
      created_at
      updated_at
    }
  }
`, {
  id: resourceId
})

const typeColors = {
  User: 'bg-green-100 text-green-800',
  Order: 'bg-blue-100 text-blue-800', 
  Product: 'bg-purple-100 text-purple-800',
  Payment: 'bg-yellow-100 text-yellow-800',
  Admin: 'bg-red-100 text-red-800'
}

const formatKey = (key) => {
  return key.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())
}

const formatValue = (value) => {
  if (typeof value === 'boolean') return value ? 'Yes' : 'No'
  if (typeof value === 'object') return JSON.stringify(value, null, 2)
  return value
}

const formatDate = (dateString) => {
  if (!dateString) return 'Unknown'
  return new Date(dateString).toLocaleString()
}
</script>

<style scoped>
.spinner {
  border: 4px solid #f3f3f3;
  border-top: 4px solid #3498db;
  border-radius: 50%;
  width: 40px;
  height: 40px;
  animation: spin 2s linear infinite;
  margin: 0 auto;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
</style>