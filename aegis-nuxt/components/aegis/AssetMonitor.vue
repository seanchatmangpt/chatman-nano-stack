<template>
  <div class="asset-monitor">
    <h2 class="text-2xl font-bold mb-4">📊 Asset Monitoring</h2>
    
    <!-- Asset health overview -->
    <div class="grid grid-cols-3 gap-4 mb-6">
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">Application Asset</h3>
        <AssetHealthIndicator
          :health="getApplicationAssetHealth()"
          :count="applicationAssets.length"
        />
      </div>
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">Security Asset</h3>
        <AssetHealthIndicator
          :health="getAssetHealth()"
          :count="assets.length"
        />
      </div>
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">Compute Asset</h3>
        <AssetHealthIndicator
          :health="getComputeAssetHealth()"
          :count="computeAssets.length"
        />
      </div>
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">Data Asset</h3>
        <AssetHealthIndicator
          :health="getDataAssetHealth()"
          :count="dataAssets.length"
        />
      </div>
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">Network Asset</h3>
        <AssetHealthIndicator
          :health="getNetworkAssetHealth()"
          :count="networkAssets.length"
        />
      </div>
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">User Asset</h3>
        <AssetHealthIndicator
          :health="getUserAssetHealth()"
          :count="userAssets.length"
        />
      </div>
    </div>
    
    <!-- Real-time asset grid -->
    <div class="asset-grid">
      <AssetGrid
        :assets="allAssets"
        :update-interval="250"
        @asset-clicked="onAssetClicked"
      />
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from TTL ontology on 2025-07-24T20:14:09.967768
import { useAegisFabric } from '~/composables/useAegisFabric'
import type { Asset } from '~/types/aegis'

const { 
applicationAssets,
  getApplicationAssetHealth,
assets,
  getAssetHealth,
computeAssets,
  getComputeAssetHealth,
dataAssets,
  getDataAssetHealth,
networkAssets,
  getNetworkAssetHealth,
userAssets,
  getUserAssetHealth,
allAssets 
} = useAegisFabric()

const onAssetClicked = (asset: Asset) => {
  console.log('Asset clicked:', asset)
  // Handle asset click
}
</script>

<style scoped>
.asset-monitor {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.asset-grid {
  @apply grid grid-cols-6 gap-2;
}
</style>