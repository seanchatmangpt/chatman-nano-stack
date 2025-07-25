<template>
  <div class="asset-component">
    <h3 class="text-lg font-semibold mb-2">Security Asset</h3>
    <p class="text-gray-400 mb-4">Any system, device, or resource that requires protection</p>
    
    <div class="properties">
      <div class="property mb-2">
        <label class="block text-sm font-medium">Asset Identifier</label>
        <span class="text-gray-300">{{ asset.assetID }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Asset Type</label>
        <span class="text-gray-300">{{ asset.assetType }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Asset Owner</label>
        <span class="text-gray-300">{{ asset.assetOwner }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Asset Criticality (1-10)</label>
        <span class="text-gray-300">{{ asset.criticality }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Confidentiality Classification</label>
        <span class="text-gray-300">{{ asset.confidentialityLevel }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Has Vulnerability</label>
        <span class="text-gray-300">{{ asset.hasVulnerability }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">Generates Event</label>
        <span class="text-gray-300">{{ asset.generatesEvent }}</span>
      </div>
      <div class="property mb-2">
        <label class="block text-sm font-medium">BitActor Instance ID</label>
        <span class="text-gray-300">{{ asset.bitactorID }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from http://cns.ai/ontology/cybersecurity#Asset on 2025-07-24T20:14:09.981881
import type { Asset } from '~/types/aegis'

interface Props {
  asset: Asset
}

const props = defineProps<Props>()
</script>

<style scoped>
.asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>