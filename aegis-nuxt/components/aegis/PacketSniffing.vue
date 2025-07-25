<template>
  <div class="packet-sniffing-component">
    <h3 class="text-lg font-semibold mb-2">Packet Sniffing</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#PacketSniffing on 2025-07-24T20:14:10.018354
import type { PacketSniffing } from '~/types/aegis'

interface Props {
  packetSniffing: PacketSniffing
}

const props = defineProps<Props>()
</script>

<style scoped>
.packet-sniffing-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>