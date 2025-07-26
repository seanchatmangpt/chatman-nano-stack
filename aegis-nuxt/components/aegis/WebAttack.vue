<template>
  <div class="web-attack-component">
    <h3 class="text-lg font-semibold mb-2">Web Application Attack</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#WebAttack on 2025-07-24T20:14:10.058833
import type { WebAttack } from '~/types/aegis'

interface Props {
  webAttack: WebAttack
}

const props = defineProps<Props>()
</script>

<style scoped>
.web-attack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>