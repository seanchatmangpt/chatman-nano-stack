<template>
  <div class="xss-component">
    <h3 class="text-lg font-semibold mb-2">Cross-Site Scripting</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#XSS on 2025-07-24T20:14:10.061872
import type { XSS } from '~/types/aegis'

interface Props {
  xss: XSS
}

const props = defineProps<Props>()
</script>

<style scoped>
.xss-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>