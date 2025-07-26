<template>
  <div class="container mx-auto px-4 py-8">
    <h1 class="text-4xl font-bold mb-8">Ash GraphQL Dashboard</h1>
    
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      <ResourceCard 
        v-for="resource in resources" 
        :key="resource.id"
        :resource="resource"
        @click="viewResource(resource)"
      />
    </div>
    
    <div v-if="loading" class="text-center mt-8">
      <div class="spinner"></div>
      Loading resources...
    </div>
  </div>
</template>

<script setup>
const { $apollo } = useNuxtApp()

const { data: resources, pending: loading } = await useAsyncQuery(gql`
  query GetResources {
    resources {
      id
      name
      type
      attributes
      created_at
    }
  }
`)

const viewResource = (resource) => {
  navigateTo(`/resources/${resource.id}`)
}
</script>

<style scoped>
.spinner {
  border: 4px solid #f3f3f3;
  border-top: 4px solid #3498db;
  border-radius: 50%;
  width: 40px;
  height: 40px;
  animation: spin 2s linear infinite;
  margin: 0 auto;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
</style>