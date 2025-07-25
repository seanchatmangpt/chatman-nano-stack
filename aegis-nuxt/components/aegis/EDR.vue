<template>
  <div class="edr-component">
    <h3 class="text-lg font-semibold mb-2">Endpoint Detection and Response</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#EDR on 2025-07-24T20:14:09.997237
import type { EDR } from '~/types/aegis'

interface Props {
  edr: EDR
}

const props = defineProps<Props>()
</script>

<style scoped>
.edr-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>