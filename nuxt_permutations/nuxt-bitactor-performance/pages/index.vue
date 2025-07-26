<template>
  <div class="min-h-screen bg-gray-900 text-white flex items-center justify-center">
    <div class="text-center">
      <div class="mb-8">
        <h1 class="text-5xl font-bold mb-4">⚡ BitActor Performance</h1>
        <p class="text-xl text-gray-300">Real-time monitoring and analytics</p>
      </div>
      
      <div class="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
        <div class="bg-gray-800 p-6 rounded-lg">
          <div class="text-3xl mb-2">🚀</div>
          <h3 class="text-lg font-semibold mb-2">Ultra Low Latency</h3>
          <p class="text-gray-400 text-sm">Sub-microsecond actor communication</p>
        </div>
        
        <div class="bg-gray-800 p-6 rounded-lg">
          <div class="text-3xl mb-2">📊</div>
          <h3 class="text-lg font-semibold mb-2">Real-time Metrics</h3>
          <p class="text-gray-400 text-sm">Live performance dashboards</p>
        </div>
        
        <div class="bg-gray-800 p-6 rounded-lg">
          <div class="text-3xl mb-2">🔧</div>
          <h3 class="text-lg font-semibold mb-2">System Control</h3>
          <p class="text-gray-400 text-sm">Actor management and tuning</p>
        </div>
      </div>
      
      <div class="space-x-4">
        <NuxtLink 
          to="/dashboard" 
          class="bg-blue-600 hover:bg-blue-700 text-white px-8 py-3 rounded-lg font-semibold transition-colors inline-block"
        >
          Open Dashboard
        </NuxtLink>
        
        <button 
          @click="connectTest"
          class="bg-gray-700 hover:bg-gray-600 text-white px-8 py-3 rounded-lg font-semibold transition-colors"
        >
          Test Connection
        </button>
      </div>
      
      <div v-if="connectionStatus" class="mt-6 p-4 rounded-lg" :class="connectionStatus.type === 'success' ? 'bg-green-900 text-green-200' : 'bg-red-900 text-red-200'">
        {{ connectionStatus.message }}
      </div>
    </div>
  </div>
</template>

<script setup>
const config = useRuntimeConfig()
const connectionStatus = ref(null)

const connectTest = async () => {
  connectionStatus.value = { type: 'info', message: 'Testing connection...' }
  
  try {
    const ws = new WebSocket(config.public.bitactorWsUrl)
    
    ws.onopen = () => {
      connectionStatus.value = { 
        type: 'success', 
        message: `✅ Connected to BitActor at ${config.public.bitactorWsUrl}` 
      }
      ws.close()
    }
    
    ws.onerror = () => {
      connectionStatus.value = { 
        type: 'error', 
        message: `❌ Failed to connect to ${config.public.bitactorWsUrl}` 
      }
    }
    
    // Timeout after 5 seconds
    setTimeout(() => {
      if (ws.readyState === WebSocket.CONNECTING) {
        ws.close()
        connectionStatus.value = { 
          type: 'error', 
          message: '⏱️ Connection timeout' 
        }
      }
    }, 5000)
    
  } catch (error) {
    connectionStatus.value = { 
      type: 'error', 
      message: `❌ Connection error: ${error.message}` 
    }
  }
}

// Auto-redirect to dashboard after 3 seconds if no interaction
onMounted(() => {
  setTimeout(() => {
    if (!connectionStatus.value) {
      navigateTo('/dashboard')
    }
  }, 10000) // 10 seconds
})
</script>