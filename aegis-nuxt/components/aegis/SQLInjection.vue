<template>
  <div class="sqlinjection-component">
    <h3 class="text-lg font-semibold mb-2">SQL Injection</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#SQLInjection on 2025-07-24T20:14:10.035842
import type { SQLInjection } from '~/types/aegis'

interface Props {
  sqlinjection: SQLInjection
}

const props = defineProps<Props>()
</script>

<style scoped>
.sqlinjection-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>