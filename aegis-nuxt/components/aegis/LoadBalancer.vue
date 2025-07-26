<template>
  <div class="load-balancer-component">
    <h3 class="text-lg font-semibold mb-2">Load Balancer</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#LoadBalancer on 2025-07-24T20:14:10.007892
import type { LoadBalancer } from '~/types/aegis'

interface Props {
  loadBalancer: LoadBalancer
}

const props = defineProps<Props>()
</script>

<style scoped>
.load-balancer-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>