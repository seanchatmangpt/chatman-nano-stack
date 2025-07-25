<template>
  <div class="lateral-movement-component">
    <h3 class="text-lg font-semibold mb-2">Lateral Movement</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#LateralMovement on 2025-07-24T20:14:10.006158
import type { LateralMovement } from '~/types/aegis'

interface Props {
  lateralMovement: LateralMovement
}

const props = defineProps<Props>()
</script>

<style scoped>
.lateral-movement-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>