<template>
  <div class="alert-component">
    <h3 class="text-lg font-semibold mb-2">Security Alert</h3>
    <p class="text-gray-400 mb-4">Automated security notification</p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#Alert on 2025-07-24T20:14:09.973787
import type { Alert } from '~/types/aegis'

interface Props {
  alert: Alert
}

const props = defineProps<Props>()
</script>

<style scoped>
.alert-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>