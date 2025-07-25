<template>
  <div class="worm-component">
    <h3 class="text-lg font-semibold mb-2">Computer Worm</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#Worm on 2025-07-24T20:14:10.060407
import type { Worm } from '~/types/aegis'

interface Props {
  worm: Worm
}

const props = defineProps<Props>()
</script>

<style scoped>
.worm-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>