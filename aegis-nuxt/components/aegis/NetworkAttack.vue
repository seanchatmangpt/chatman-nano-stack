<template>
  <div class="network-attack-component">
    <h3 class="text-lg font-semibold mb-2">Network Attack</h3>
    <p class="text-gray-400 mb-4"></p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Packet Count</label>
        <span class="text-gray-300">{{ networkAttack.packetCount }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Byte Count</label>
        <span class="text-gray-300">{{ networkAttack.byteCount }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#NetworkAttack on 2025-07-24T20:14:10.013968
import type { NetworkAttack } from '~/types/aegis'

interface Props {
  networkAttack: NetworkAttack
}

const props = defineProps<Props>()
</script>

<style scoped>
.network-attack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>