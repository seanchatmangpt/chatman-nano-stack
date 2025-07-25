<template>
  <div class="attack-component">
    <h3 class="text-lg font-semibold mb-2">Cyber Attack</h3>
    <p class="text-gray-400 mb-4">Active exploitation attempt or malicious activity</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Attack Identifier</label>
        <span class="text-gray-300">{{ attack.attackID }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Attack Vector</label>
        <span class="text-gray-300">{{ attack.attackVector }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Source IP Address</label>
        <span class="text-gray-300">{{ attack.sourceIP }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Destination IP Address</label>
        <span class="text-gray-300">{{ attack.destinationIP }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Source Port</label>
        <span class="text-gray-300">{{ attack.sourcePort }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Destination Port</label>
        <span class="text-gray-300">{{ attack.destinationPort }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Attack Timestamp</label>
        <span class="text-gray-300">{{ attack.attackTimestamp }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Attack Duration (seconds)</label>
        <span class="text-gray-300">{{ attack.duration }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Exploits Vulnerability</label>
        <span class="text-gray-300">{{ attack.exploits }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Affects Asset</label>
        <span class="text-gray-300">{{ attack.affects }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#Attack on 2025-07-24T20:14:09.983327
import type { Attack } from '~/types/aegis'

interface Props {
  attack: Attack
}

const props = defineProps<Props>()
</script>

<style scoped>
.attack-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>