<template>
  <div class="security-event-component">
    <h3 class="text-lg font-semibold mb-2">Security Event</h3>
    <p class="text-gray-400 mb-4">Observed security-relevant occurrence</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Event Identifier</label>
        <span class="text-gray-300">{{ securityEvent.eventID }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Event Type</label>
        <span class="text-gray-300">{{ securityEvent.eventType }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Event Timestamp</label>
        <span class="text-gray-300">{{ securityEvent.eventTimestamp }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Event Severity (1-10)</label>
        <span class="text-gray-300">{{ securityEvent.eventSeverity }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Risk Score (0.0-10.0)</label>
        <span class="text-gray-300">{{ securityEvent.riskScore }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">False Positive Probability (0.0-1.0)</label>
        <span class="text-gray-300">{{ securityEvent.falsePositiveProbability }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Processing Latency (nanoseconds)</label>
        <span class="text-gray-300">{{ securityEvent.processingLatency }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">CPU Ticks Used</label>
        <span class="text-gray-300">{{ securityEvent.ticksUsed }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">SIMD Optimized Processing</label>
        <span class="text-gray-300">{{ securityEvent.simdOptimized }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#SecurityEvent on 2025-07-24T20:14:10.038697
import type { SecurityEvent } from '~/types/aegis'

interface Props {
  securityEvent: SecurityEvent
}

const props = defineProps<Props>()
</script>

<style scoped>
.security-event-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>