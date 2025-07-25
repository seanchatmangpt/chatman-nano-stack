<template>
  <div class="ioc-component">
    <h3 class="text-lg font-semibold mb-2">Indicator of Compromise</h3>
    <p class="text-gray-400 mb-4">Observable evidence of security breach</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">IOC Type</label>
        <span class="text-gray-300">{{ ioc.iocType }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">IOC Value</label>
        <span class="text-gray-300">{{ ioc.iocValue }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Reputation Score (-1.0 to 1.0)</label>
        <span class="text-gray-300">{{ ioc.reputation }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#IOC on 2025-07-24T20:14:10.001560
import type { IOC } from '~/types/aegis'

interface Props {
  ioc: IOC
}

const props = defineProps<Props>()
</script>

<style scoped>
.ioc-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>