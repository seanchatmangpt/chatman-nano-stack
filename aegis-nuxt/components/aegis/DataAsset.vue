<template>
  <div class="data-asset-component">
    <h3 class="text-lg font-semibold mb-2">Data Asset</h3>
    <p class="text-gray-400 mb-4">Information or data requiring protection</p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#DataAsset on 2025-07-24T20:14:09.993046
import type { DataAsset } from '~/types/aegis'

interface Props {
  dataAsset: DataAsset
}

const props = defineProps<Props>()
</script>

<style scoped>
.data-asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>