<template>
  <div class="cns_quant-app">
    <h1>{{ config.title }}</h1>
    <div class="dashboard">
      <stat-cards :stats="stats" />
      <main-content :data="mainData" />
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'

const config = {
  title: 'CNS Quant - Financial Trading Platform',
  domain: 'finance'
}

const stats = ref([])
const mainData = ref([])

onMounted(async () => {
  // Fetch dashboard data
  const response = await fetch('/api/dashboard')
  const data = await response.json()
  stats.value = data.stats
  mainData.value = data.data
})
</script>