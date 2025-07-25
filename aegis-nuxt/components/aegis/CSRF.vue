<template>
  <div class="csrf-component">
    <h3 class="text-lg font-semibold mb-2">Cross-Site Request Forgery</h3>
    <p class="text-gray-400 mb-4"></p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#CSRF on 2025-07-24T20:14:09.986173
import type { CSRF } from '~/types/aegis'

interface Props {
  csrf: CSRF
}

const props = defineProps<Props>()
</script>

<style scoped>
.csrf-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>