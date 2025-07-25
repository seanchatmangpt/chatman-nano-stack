<template>
  <div class="data-exfiltration-component">
    <h3 class="text-lg font-semibold mb-2">Data Exfiltration</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#DataExfiltration on 2025-07-24T20:14:09.994424
import type { DataExfiltration } from '~/types/aegis'

interface Props {
  dataExfiltration: DataExfiltration
}

const props = defineProps<Props>()
</script>

<style scoped>
.data-exfiltration-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>