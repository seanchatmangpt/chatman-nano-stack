<template>
  <div class="man-in-the-middle-attack-component">
    <h3 class="text-lg font-semibold mb-2">Man-in-the-Middle Attack</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#ManInTheMiddleAttack on 2025-07-24T20:14:10.011037
import type { ManInTheMiddleAttack } from '~/types/aegis'

interface Props {
  manInTheMiddleAttack: ManInTheMiddleAttack
}

const props = defineProps<Props>()
</script>

<style scoped>
.man-in-the-middle-attack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>