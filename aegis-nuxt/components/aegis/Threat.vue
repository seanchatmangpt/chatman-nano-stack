<template>
  <div class="threat-component">
    <h3 class="text-lg font-semibold mb-2">Cybersecurity Threat</h3>
    <p class="text-gray-400 mb-4">Potential danger to security assets</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Threat Identifier</label>
        <span class="text-gray-300">{{ threat.threatID }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Threat Severity (1-10)</label>
        <span class="text-gray-300">{{ threat.severity }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Detection Confidence (0.0-1.0)</label>
        <span class="text-gray-300">{{ threat.confidence }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">First Detection Time</label>
        <span class="text-gray-300">{{ threat.firstSeen }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Last Detection Time</label>
        <span class="text-gray-300">{{ threat.lastSeen }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Targets Asset</label>
        <span class="text-gray-300">{{ threat.targets }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Detected By Control</label>
        <span class="text-gray-300">{{ threat.detectedBy }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Mitigated By Control</label>
        <span class="text-gray-300">{{ threat.mitigatedBy }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#Threat on 2025-07-24T20:14:10.047779
import type { Threat } from '~/types/aegis'

interface Props {
  threat: Threat
}

const props = defineProps<Props>()
</script>

<style scoped>
.threat-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>