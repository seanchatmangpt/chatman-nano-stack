<template>
  <div class="min-h-screen bg-gray-950 text-white">
    <!-- Command Bar -->
    <UCommandPalette 
      v-model="commandOpen"
      :groups="commandGroups"
      @select="executeCommand"
    />
    
    <!-- Header -->
    <header class="border-b border-gray-800 bg-gray-900/50 backdrop-blur">
      <div class="flex items-center justify-between px-6 py-4">
        <div class="flex items-center space-x-4">
          <UBadge color="green" variant="soft" size="lg">
            <UIcon name="i-heroicons-signal" class="mr-2" />
            CNS COMMAND CENTER
          </UBadge>
          <UKbd @click="commandOpen = true">⌘K</UKbd>
        </div>
        
        <div class="flex items-center space-x-4">
          <UBadge color="green" variant="outline">System Online</UBadge>
          <UButton variant="ghost" size="sm">
            <UIcon name="i-heroicons-bell" />
          </UButton>
          <UButton variant="ghost" size="sm">
            <UIcon name="i-heroicons-user" />
          </UButton>
        </div>
      </div>
    </header>
    
    <!-- Main Content -->
    <div class="flex h-[calc(100vh-73px)]">
      <!-- Sidebar -->
      <aside class="w-64 border-r border-gray-800 bg-gray-900/30">
        <nav class="p-4">
          <UVerticalNavigation :links="navigationLinks" />
        </nav>
        <div class="p-4 border-t border-gray-800">
          <div class="space-y-2">
            <UButton block variant="ghost" size="sm">
              <UIcon name="i-heroicons-play" class="mr-2" />
              Quick Start
            </UButton>
            <UButton block variant="ghost" size="sm">
              <UIcon name="i-heroicons-stop" class="mr-2" />
              Emergency Stop
            </UButton>
          </div>
        </div>
      </aside>
      
      <!-- Content Area -->
      <main class="flex-1 overflow-auto">
        <slot />
      </main>
      
      <!-- Right Panel -->
      <aside v-if="rightPanelOpen" class="w-80 border-l border-gray-800 bg-gray-900/30">
        <div class="p-4 border-b border-gray-800">
          <div class="flex items-center justify-between">
            <h3 class="font-semibold">Pipeline Inspector</h3>
            <UButton @click="rightPanelOpen = false" variant="ghost" size="sm" icon="i-heroicons-x-mark" />
          </div>
        </div>
        <div class="p-4">
          <div class="space-y-4">
            <div>
              <h4 class="font-medium mb-2">Active Pipeline</h4>
              <div class="text-sm text-gray-400">
                <div class="flex justify-between mb-1">
                  <span>Status:</span>
                  <UBadge color="green" size="xs">Running</UBadge>
                </div>
                <div class="flex justify-between mb-1">
                  <span>Throughput:</span>
                  <span>12.4K/s</span>
                </div>
                <div class="flex justify-between">
                  <span>Latency:</span>
                  <span>23ms</span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </aside>
    </div>
    
    <!-- Global Notifications -->
    <UNotifications />
  </div>
</template>

<script setup>
const commandOpen = ref(false)
const rightPanelOpen = ref(true)

const commandGroups = [
  {
    key: 'pipeline',
    label: 'Pipeline Control',
    commands: [
      { id: 'restart-typer', label: 'Restart 80/20 Typer', icon: 'i-heroicons-arrow-path' },
      { id: 'flush-turtle', label: 'Flush Turtle Cache', icon: 'i-heroicons-trash' },
      { id: 'scale-bitactor', label: 'Scale BitActors', icon: 'i-heroicons-plus' }
    ]
  },
  {
    key: 'view',
    label: 'Views',
    commands: [
      { id: 'toggle-inspector', label: 'Toggle Inspector', icon: 'i-heroicons-magnifying-glass' },
      { id: 'fullscreen-metrics', label: 'Fullscreen Metrics', icon: 'i-heroicons-arrows-pointing-out' }
    ]
  }
]

const navigationLinks = [
  { label: 'Overview', to: '/', icon: 'i-heroicons-home' },
  { label: 'Pipeline Flow', to: '/pipeline', icon: 'i-heroicons-arrow-trending-up' },
  { label: 'BitActor Nodes', to: '/bitactors', icon: 'i-heroicons-cpu-chip' },
  { label: 'Reactor Workflows', to: '/workflows', icon: 'i-heroicons-cog-6-tooth' },
  { label: 'Kubernetes Pods', to: '/k8s', icon: 'i-heroicons-cube' },
  { label: 'System Logs', to: '/logs', icon: 'i-heroicons-document-text' }
]

const executeCommand = (command) => {
  switch (command.id) {
    case 'restart-typer':
      // Implement restart logic
      break
    case 'toggle-inspector':
      rightPanelOpen.value = !rightPanelOpen.value
      break
  }
}

// Global keyboard shortcuts
onMounted(() => {
  document.addEventListener('keydown', (e) => {
    if (e.metaKey && e.key === 'k') {
      e.preventDefault()
      commandOpen.value = true
    }
  })
})
</script>