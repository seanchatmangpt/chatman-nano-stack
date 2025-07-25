<template>
  <div class="security-incident-component">
    <h3 class="text-lg font-semibold mb-2">Security Incident</h3>
    <p class="text-gray-400 mb-4">Confirmed security breach or compromise</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Incident Identifier</label>
        <span class="text-gray-300">{{ securityIncident.incidentID }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Incident Status</label>
        <span class="text-gray-300">{{ securityIncident.incidentStatus }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Containment Time (minutes)</label>
        <span class="text-gray-300">{{ securityIncident.containmentTime }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Recovery Time (minutes)</label>
        <span class="text-gray-300">{{ securityIncident.recoveryTime }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Business Impact Level</label>
        <span class="text-gray-300">{{ securityIncident.businessImpact }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Assigned Security Analyst</label>
        <span class="text-gray-300">{{ securityIncident.assignedAnalyst }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#SecurityIncident on 2025-07-24T20:14:10.040131
import type { SecurityIncident } from '~/types/aegis'

interface Props {
  securityIncident: SecurityIncident
}

const props = defineProps<Props>()
</script>

<style scoped>
.security-incident-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>