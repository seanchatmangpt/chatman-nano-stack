<template>
  <div class="port-scan-component">
    <h3 class="text-lg font-semibold mb-2">Port Scanning</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#PortScan on 2025-07-24T20:14:10.021163
import type { PortScan } from '~/types/aegis'

interface Props {
  portScan: PortScan
}

const props = defineProps<Props>()
</script>

<style scoped>
.port-scan-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>