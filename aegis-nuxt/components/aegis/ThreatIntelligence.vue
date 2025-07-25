<template>
  <div class="threat-intelligence-component">
    <h3 class="text-lg font-semibold mb-2">Threat Intelligence</h3>
    <p class="text-gray-400 mb-4">Information about current and emerging threats</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Intelligence Source</label>
        <span class="text-gray-300">{{ threatIntelligence.source }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Source Reliability (0.0-1.0)</label>
        <span class="text-gray-300">{{ threatIntelligence.reliability }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#ThreatIntelligence on 2025-07-24T20:14:10.049346
import type { ThreatIntelligence } from '~/types/aegis'

interface Props {
  threatIntelligence: ThreatIntelligence
}

const props = defineProps<Props>()
</script>

<style scoped>
.threat-intelligence-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>