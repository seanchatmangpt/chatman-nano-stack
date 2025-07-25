<template>
  <div class="compute-asset-component">
    <h3 class="text-lg font-semibold mb-2">Compute Asset</h3>
    <p class="text-gray-400 mb-4">Computing device or system</p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#ComputeAsset on 2025-07-24T20:14:09.987552
import type { ComputeAsset } from '~/types/aegis'

interface Props {
  computeAsset: ComputeAsset
}

const props = defineProps<Props>()
</script>

<style scoped>
.compute-asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>