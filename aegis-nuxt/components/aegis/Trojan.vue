<template>
  <div class="trojan-component">
    <h3 class="text-lg font-semibold mb-2">Trojan Horse</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#Trojan on 2025-07-24T20:14:10.050773
import type { Trojan } from '~/types/aegis'

interface Props {
  trojan: Trojan
}

const props = defineProps<Props>()
</script>

<style scoped>
.trojan-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>