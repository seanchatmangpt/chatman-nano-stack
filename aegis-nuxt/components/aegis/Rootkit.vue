<template>
  <div class="rootkit-component">
    <h3 class="text-lg font-semibold mb-2">Rootkit</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#Rootkit on 2025-07-24T20:14:10.029397
import type { Rootkit } from '~/types/aegis'

interface Props {
  rootkit: Rootkit
}

const props = defineProps<Props>()
</script>

<style scoped>
.rootkit-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>