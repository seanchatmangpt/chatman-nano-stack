<template>
  <div class="application-asset-component">
    <h3 class="text-lg font-semibold mb-2">Application Asset</h3>
    <p class="text-gray-400 mb-4">Software application or service</p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#ApplicationAsset on 2025-07-24T20:14:09.980400
import type { ApplicationAsset } from '~/types/aegis'

interface Props {
  applicationAsset: ApplicationAsset
}

const props = defineProps<Props>()
</script>

<style scoped>
.application-asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>