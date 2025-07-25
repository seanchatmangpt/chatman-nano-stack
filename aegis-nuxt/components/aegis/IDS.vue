<template>
  <div class="ids-component">
    <h3 class="text-lg font-semibold mb-2">Intrusion Detection System</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#IDS on 2025-07-24T20:14:10.000101
import type { IDS } from '~/types/aegis'

interface Props {
  ids: IDS
}

const props = defineProps<Props>()
</script>

<style scoped>
.ids-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>