<template>
  <div class="network-topology">
    <h2 class="text-2xl font-bold mb-4">🌐 Network Topology</h2>
    
    <!-- Interactive network visualization -->
    <div class="network-viz">
      <ClientOnly>
        <NetworkGraph
          :nodes="networkNodes"
          :edges="networkEdges"
          :threats="activeNetworkThreats"
          @node-selected="onNodeSelected"
        />
      </ClientOnly>
    </div>
    
    <!-- Network segments -->
    <div class="mt-6">
      <h3 class="text-xl font-semibold mb-3">Network Segments</h3>
      <div class="grid grid-cols-3 gap-4">
        <NetworkSegmentCard
          v-for="segment in internalNetworks"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        <NetworkSegmentCard
          v-for="segment in networkAssets"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        <NetworkSegmentCard
          v-for="segment in networkAttacks"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        <NetworkSegmentCard
          v-for="segment in networkDevices"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        <NetworkSegmentCard
          v-for="segment in networkSegments"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        <NetworkSegmentCard
          v-for="segment in publicNetworks"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from TTL ontology on 2025-07-24T20:14:09.971520
import { useAegisFabric } from '~/composables/useAegisFabric'
import type { NetworkNode, NetworkSegment } from '~/types/aegis'

const { 
  networkNodes, 
  networkEdges, 
  activeNetworkThreats,
internalNetworks,
networkAssets,
networkAttacks,
networkDevices,
networkSegments,
publicNetworks,
getSegmentHealth 
} = useAegisFabric()

const onNodeSelected = (node: NetworkNode) => {
  console.log('Network node selected:', node)
  // Handle node selection
}
</script>

<style scoped>
.network-topology {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.network-viz {
  @apply h-96 bg-gray-800 rounded-lg;
}
</style>