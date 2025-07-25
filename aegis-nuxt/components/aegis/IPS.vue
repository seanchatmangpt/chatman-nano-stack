<template>
  <div class="ips-component">
    <h3 class="text-lg font-semibold mb-2">Intrusion Prevention System</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#IPS on 2025-07-24T20:14:10.002989
import type { IPS } from '~/types/aegis'

interface Props {
  ips: IPS
}

const props = defineProps<Props>()
</script>

<style scoped>
.ips-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>