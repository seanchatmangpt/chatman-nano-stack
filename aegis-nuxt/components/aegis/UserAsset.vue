<template>
  <div class="user-asset-component">
    <h3 class="text-lg font-semibold mb-2">User Asset</h3>
    <p class="text-gray-400 mb-4">Human user account or identity</p>
    
  </div>
</template>

<script setup lang="ts">
// Generated from http://cns.ai/ontology/cybersecurity#UserAsset on 2025-07-24T20:14:10.052294
import type { UserAsset } from '~/types/aegis'

interface Props {
  userAsset: UserAsset
}

const props = defineProps<Props>()
</script>

<style scoped>
.user-asset-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>