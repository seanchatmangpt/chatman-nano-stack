<template>
  <div class="network-asset-component">
    <h3 class="text-lg font-semibold mb-2">Network Asset</h3>
    <p class="text-gray-400 mb-4">Network infrastructure requiring security monitoring</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">IP Address</label>
        <span class="text-gray-300">{{ networkAsset.ipAddress }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">MAC Address</label>
        <span class="text-gray-300">{{ networkAsset.macAddress }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Network Port</label>
        <span class="text-gray-300">{{ networkAsset.port }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Network Protocol</label>
        <span class="text-gray-300">{{ networkAsset.protocol }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Network Segment</label>
        <span class="text-gray-300">{{ networkAsset.networkSegment }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Online Status</label>
        <span class="text-gray-300">{{ networkAsset.isOnline }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Communicates With</label>
        <span class="text-gray-300">{{ networkAsset.communicatesWith }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#NetworkAsset on 2025-07-24T20:14:10.012508
import type { NetworkAsset } from '~/types/aegis'

interface Props {
  networkAsset: NetworkAsset
}

const props = defineProps<Props>()
</script>

<style scoped>
.network-asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>