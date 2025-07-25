<template>
  <div class="spear-phishing-component">
    <h3 class="text-lg font-semibold mb-2">Spear Phishing</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#SpearPhishing on 2025-07-24T20:14:10.043466
import type { SpearPhishing } from '~/types/aegis'

interface Props {
  spearPhishing: SpearPhishing
}

const props = defineProps<Props>()
</script>

<style scoped>
.spear-phishing-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>