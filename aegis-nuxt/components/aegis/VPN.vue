<template>
  <div class="vpn-component">
    <h3 class="text-lg font-semibold mb-2">VPN Gateway</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#VPN on 2025-07-24T20:14:10.053795
import type { VPN } from '~/types/aegis'

interface Props {
  vpn: VPN
}

const props = defineProps<Props>()
</script>

<style scoped>
.vpn-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>