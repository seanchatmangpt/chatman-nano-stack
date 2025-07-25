<template>
  <div class="security-control-component">
    <h3 class="text-lg font-semibold mb-2">Security Control</h3>
    <p class="text-gray-400 mb-4">Protective measure or safeguard</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Protects Asset</label>
        <span class="text-gray-300">{{ securityControl.protects }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Real-time Processing Enabled</label>
        <span class="text-gray-300">{{ securityControl.realTimeProcessing }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Detection Accuracy (%)</label>
        <span class="text-gray-300">{{ securityControl.detectionAccuracy }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Response Time (milliseconds)</label>
        <span class="text-gray-300">{{ securityControl.responseTime }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#SecurityControl on 2025-07-24T20:14:10.037266
import type { SecurityControl } from '~/types/aegis'

interface Props {
  securityControl: SecurityControl
}

const props = defineProps<Props>()
</script>

<style scoped>
.security-control-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>