<template>
  <div class="container mx-auto px-4 py-8">
    <Head>
      <Title>{{ ontology.title }} - Semantic Ontology</Title>
      <Meta name="description" :content="ontology.description" />
      <script type="application/ld+json">{{ jsonLd }}</script>
    </Head>
    
    <header class="mb-8">
      <h1 class="text-4xl font-bold mb-4">{{ ontology.title }}</h1>
      <p class="text-xl text-gray-600">{{ ontology.description }}</p>
    </header>
    
    <div class="grid grid-cols-1 lg:grid-cols-3 gap-8">
      <!-- Classes -->
      <div class="bg-white p-6 rounded-lg shadow">
        <h2 class="text-2xl font-semibold mb-4">Classes</h2>
        <div v-for="cls in ontology.classes" :key="cls.uri" class="mb-4 p-3 border rounded">
          <h3 class="font-medium">{{ cls.name }}</h3>
          <p class="text-sm text-gray-600">{{ cls.comment }}</p>
          <div class="mt-2">
            <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">
              {{ cls.type }}
            </span>
          </div>
        </div>
      </div>
      
      <!-- Properties -->
      <div class="bg-white p-6 rounded-lg shadow">
        <h2 class="text-2xl font-semibold mb-4">Properties</h2>
        <div v-for="prop in ontology.properties" :key="prop.uri" class="mb-4 p-3 border rounded">
          <h3 class="font-medium">{{ prop.name }}</h3>
          <p class="text-sm text-gray-600">{{ prop.comment }}</p>
          <div class="mt-2 text-xs">
            <span class="text-gray-500">Domain:</span> {{ prop.domain }}<br>
            <span class="text-gray-500">Range:</span> {{ prop.range }}
          </div>
        </div>
      </div>
      
      <!-- Relationships -->
      <div class="bg-white p-6 rounded-lg shadow">
        <h2 class="text-2xl font-semibold mb-4">Relationships</h2>
        <div class="space-y-3">
          <div v-for="rel in ontology.relationships" :key="rel.id" 
               class="p-3 bg-gray-50 rounded">
            <div class="flex items-center space-x-2">
              <span class="font-medium">{{ rel.source }}</span>
              <span class="text-gray-400">→</span>
              <span class="font-medium">{{ rel.target }}</span>
            </div>
            <div class="text-sm text-gray-600 mt-1">
              via {{ rel.property }}
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Raw TTL Display -->
    <div class="mt-8 bg-gray-900 text-green-400 p-6 rounded-lg">
      <h2 class="text-xl font-semibold mb-4 text-white">Raw TTL</h2>
      <pre class="text-sm overflow-x-auto">{{ rawTTL }}</pre>
    </div>
  </div>
</template>

<script setup>
const { parseTTL, generateJsonLd } = useTTLParser()

const { data: ontology } = await useFetch('/api/ontology')
const { data: rawTTL } = await useFetch('/api/ontology/raw')

const jsonLd = computed(() => {
  return JSON.stringify(generateJsonLd(ontology.value), null, 2)
})

// SEO optimizations
useSeoMeta({
  title: ontology.value?.title,
  ogTitle: ontology.value?.title,
  description: ontology.value?.description,
  ogDescription: ontology.value?.description,
  ogType: 'website',
  twitterCard: 'summary_large_image'
})
</script>