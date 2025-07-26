<template>
  <div class="ddo-sattack-component">
    <h3 class="text-lg font-semibold mb-2">Distributed Denial of Service Attack</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#DDoSAttack on 2025-07-24T20:14:09.990303
import type { DDoSAttack } from '~/types/aegis'

interface Props {
  ddoSattack: DDoSAttack
}

const props = defineProps<Props>()
</script>

<style scoped>
.ddo-sattack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>