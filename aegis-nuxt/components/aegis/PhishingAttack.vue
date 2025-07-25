<template>
  <div class="phishing-attack-component">
    <h3 class="text-lg font-semibold mb-2">Phishing Attack</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#PhishingAttack on 2025-07-24T20:14:10.019756
import type { PhishingAttack } from '~/types/aegis'

interface Props {
  phishingAttack: PhishingAttack
}

const props = defineProps<Props>()
</script>

<style scoped>
.phishing-attack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>