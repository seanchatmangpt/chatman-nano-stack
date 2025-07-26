<template>
  <div>
    <Head>
      <Title>CNS Forge Semantic Web Platform</Title>
      <Meta name="description" content="Explore CNS Forge ontologies, BitActor semantic models, and Ash resource definitions through our semantic web platform" />
      <Meta name="keywords" content="semantic web, ontology, BitActor, Ash, CNS Forge, RDF, TTL, OWL" />
      <Meta property="og:title" content="CNS Forge Semantic Web Platform" />
      <Meta property="og:description" content="Comprehensive semantic web platform for the CNS Forge ecosystem" />
      <Meta property="og:type" content="website" />
      <Meta name="twitter:card" content="summary_large_image" />
      
      <!-- JSON-LD structured data -->
      <script type="application/ld+json">{{
        JSON.stringify({
          "@context": "https://schema.org",
          "@type": "WebSite",
          "name": "CNS Forge Semantic Web Platform",
          "description": "Explore CNS Forge ontologies and semantic models",
          "url": "https://semantic.cns-forge.com",
          "publisher": {
            "@type": "Organization",
            "name": "CNS Forge",
            "description": "Cognitive Nano Stack for high-performance systems"
          },
          "mainEntity": {
            "@type": "Dataset",
            "name": "CNS Forge Ontology",
            "description": "Comprehensive ontology covering BitActors, Ash resources, and semantic models",
            "keywords": ["semantic web", "ontology", "BitActor", "performance"],
            "license": "https://opensource.org/licenses/MIT"
          }
        })
      }}</script>
    </Head>
    
    <div class="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100">
      <!-- Hero Section -->
      <section class="py-20 px-4">
        <div class="container mx-auto text-center">
          <h1 class="text-5xl font-bold text-gray-800 mb-6">
            🐢 CNS Forge Semantic Web
          </h1>
          <p class="text-xl text-gray-600 mb-8 max-w-3xl mx-auto">
            Explore comprehensive ontologies covering BitActors, Ash resources, and semantic models. 
            Server-side rendered for optimal SEO and semantic web compliance.
          </p>
          
          <div class="flex flex-wrap justify-center gap-4 mb-12">
            <NuxtLink to="/ontology" class="bg-blue-600 text-white px-8 py-3 rounded-lg font-semibold hover:bg-blue-700 transition-colors">
              Explore Ontology
            </NuxtLink>
            <NuxtLink to="/schema" class="bg-gray-200 text-gray-800 px-8 py-3 rounded-lg font-semibold hover:bg-gray-300 transition-colors">
              View Schema
            </NuxtLink>
            <a href="/api/ontology/raw" target="_blank" class="bg-green-600 text-white px-8 py-3 rounded-lg font-semibold hover:bg-green-700 transition-colors">
              Download TTL
            </a>
          </div>
        </div>
      </section>
      
      <!-- Features Grid -->
      <section class="py-16 px-4">
        <div class="container mx-auto">
          <h2 class="text-3xl font-bold text-center mb-12">Semantic Web Features</h2>
          
          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">🏗️</div>
              <h3 class="text-xl font-semibold mb-3">Ontology Design</h3>
              <p class="text-gray-600">Comprehensive OWL ontologies covering the entire CNS Forge ecosystem with proper semantic relationships.</p>
            </div>
            
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">⚡</div>
              <h3 class="text-xl font-semibold mb-3">BitActor Integration</h3>
              <p class="text-gray-600">Semantic models for high-performance BitActor systems with latency and throughput properties.</p>
            </div>
            
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">🔥</div>
              <h3 class="text-xl font-semibold mb-3">Ash Resources</h3>
              <p class="text-gray-600">Elixir Ash framework resources mapped to semantic web standards for API integration.</p>
            </div>
            
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">🌐</div>
              <h3 class="text-xl font-semibold mb-3">W3C Standards</h3>
              <p class="text-gray-600">Full compliance with RDF, OWL, and SPARQL standards for maximum interoperability.</p>
            </div>
            
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">🔍</div>
              <h3 class="text-xl font-semibold mb-3">SEO Optimized</h3>
              <p class="text-gray-600">Server-side rendering with structured data markup for optimal search engine visibility.</p>
            </div>
            
            <div class="bg-white p-6 rounded-lg shadow-lg">
              <div class="text-4xl mb-4">📊</div>
              <h3 class="text-xl font-semibold mb-3">SPARQL Queries</h3>
              <p class="text-gray-600">Query semantic data using standard SPARQL endpoints for advanced data discovery.</p>
            </div>
          </div>
        </div>
      </section>
      
      <!-- Statistics Section -->
      <section class="py-16 px-4 bg-white">
        <div class="container mx-auto">
          <h2 class="text-3xl font-bold text-center mb-12">Ontology Statistics</h2>
          
          <div class="grid grid-cols-2 md:grid-cols-4 gap-8">
            <div class="text-center">
              <div class="text-4xl font-bold text-blue-600 mb-2">{{ stats.totalClasses }}</div>
              <div class="text-gray-600">OWL Classes</div>
            </div>
            <div class="text-center">
              <div class="text-4xl font-bold text-green-600 mb-2">{{ stats.totalProperties }}</div>
              <div class="text-gray-600">Properties</div>
            </div>
            <div class="text-center">
              <div class="text-4xl font-bold text-purple-600 mb-2">{{ stats.totalRelationships }}</div>
              <div class="text-gray-600">Relationships</div>
            </div>
            <div class="text-center">
              <div class="text-4xl font-bold text-orange-600 mb-2">100%</div>
              <div class="text-gray-600">W3C Compliant</div>
            </div>
          </div>
        </div>
      </section>
      
      <!-- CTA Section -->
      <section class="py-16 px-4 bg-gray-900 text-white">
        <div class="container mx-auto text-center">
          <h2 class="text-3xl font-bold mb-6">Ready to Explore?</h2>
          <p class="text-xl mb-8 max-w-2xl mx-auto">
            Dive into our comprehensive semantic web platform and discover the power of linked data for high-performance systems.
          </p>
          <div class="flex flex-wrap justify-center gap-4">
            <NuxtLink to="/ontology" class="bg-blue-600 text-white px-8 py-3 rounded-lg font-semibold hover:bg-blue-700 transition-colors">
              Browse Ontology
            </NuxtLink>
            <NuxtLink to="/schema" class="bg-transparent border-2 border-white text-white px-8 py-3 rounded-lg font-semibold hover:bg-white hover:text-gray-900 transition-colors">
              View JSON-LD Schema
            </NuxtLink>
          </div>
        </div>
      </section>
    </div>
  </div>
</template>

<script setup>
// Fetch ontology statistics for display
const { data: ontologyData } = await useFetch('/api/ontology')

const stats = computed(() => {
  return ontologyData.value?.statistics || {
    totalClasses: 0,
    totalProperties: 0, 
    totalRelationships: 0
  }
})

// SEO meta configuration
useSeoMeta({
  title: 'CNS Forge Semantic Web Platform',
  ogTitle: 'CNS Forge Semantic Web Platform',
  description: 'Explore CNS Forge ontologies, BitActor semantic models, and Ash resource definitions through our semantic web platform',
  ogDescription: 'Comprehensive semantic web platform for the CNS Forge ecosystem',
  ogImage: '/semantic-web-og.jpg',
  twitterCard: 'summary_large_image'
})
</script>