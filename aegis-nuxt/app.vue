<template>
  <div id="app" class="min-h-screen bg-cyber-dark text-white">
    <!-- Navigation Header -->
    <nav class="bg-cyber-gray border-b border-cyber-blue/20">
      <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div class="flex justify-between h-16">
          <div class="flex items-center">
            <div class="flex-shrink-0">
              <h1 class="text-xl font-bold text-cyber-blue">🛡️ Aegis Fabric</h1>
            </div>
          </div>
          <div class="flex items-center space-x-4">
            <div class="flex items-center space-x-2">
              <div
                class="w-2 h-2 rounded-full bg-cyber-green animate-pulse"
              ></div>
              <span class="text-sm text-cyber-green">Live</span>
            </div>
            <div class="text-sm text-gray-400">
              {{ new Date().toLocaleTimeString() }}
            </div>
          </div>
        </div>
      </div>
    </nav>

    <!-- Main Content -->
    <main class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      <!-- Dashboard Grid -->
      <div class="grid grid-cols-1 lg:grid-cols-3 gap-8">
        <!-- Threat Dashboard -->
        <div class="lg:col-span-2">
          <AegisThreatDashboard />
        </div>

        <!-- Network Topology -->
        <div class="lg:col-span-1">
          <AegisNetworkTopology />
        </div>
      </div>

      <!-- Asset Monitoring -->
      <div class="mt-8">
        <AegisAssetMonitor />
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
// Auto-imports will handle component imports
</script>

<style>
/* Global styles */
body {
  font-family: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
    sans-serif;
}

/* Custom scrollbar */
::-webkit-scrollbar {
  width: 8px;
}

::-webkit-scrollbar-track {
  background: #1a1a1a;
}

::-webkit-scrollbar-thumb {
  background: #00d4ff;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: #00b8e6;
}

/* Cyberpunk animations */
@keyframes cyber-glow {
  0%,
  100% {
    box-shadow: 0 0 5px #00d4ff;
  }
  50% {
    box-shadow: 0 0 20px #00d4ff, 0 0 30px #00d4ff;
  }
}

.cyber-glow {
  animation: cyber-glow 2s ease-in-out infinite;
}
</style>
