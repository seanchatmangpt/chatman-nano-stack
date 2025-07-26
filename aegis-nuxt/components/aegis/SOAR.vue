<template>
  <div class="soar-component">
    <h3 class="text-lg font-semibold mb-2">Security Orchestration, Automation and Response</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#SOAR on 2025-07-24T20:14:10.034352
import type { SOAR } from '~/types/aegis'

interface Props {
  soar: SOAR
}

const props = defineProps<Props>()
</script>

<style scoped>
.soar-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>