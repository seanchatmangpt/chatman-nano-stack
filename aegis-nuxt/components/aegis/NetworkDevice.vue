<template>
  <div class="network-device-component">
    <h3 class="text-lg font-semibold mb-2">Network Device</h3>
    <p class="text-gray-400 mb-4">Physical or virtual network infrastructure device</p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#NetworkDevice on 2025-07-24T20:14:10.015450
import type { NetworkDevice } from '~/types/aegis'

interface Props {
  networkDevice: NetworkDevice
}

const props = defineProps<Props>()
</script>

<style scoped>
.network-device-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>