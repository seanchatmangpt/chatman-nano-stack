<template>
  <div class="spyware-component">
    <h3 class="text-lg font-semibold mb-2">Spyware</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#Spyware on 2025-07-24T20:14:10.044943
import type { Spyware } from '~/types/aegis'

interface Props {
  spyware: Spyware
}

const props = defineProps<Props>()
</script>

<style scoped>
.spyware-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>