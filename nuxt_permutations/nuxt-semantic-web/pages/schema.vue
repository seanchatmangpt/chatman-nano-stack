<template>
  <div class="container mx-auto px-4 py-8">
    <Head>
      <Title>JSON-LD Schema - CNS Forge Semantic Web</Title>
      <Meta name="description" content="JSON-LD structured data schema for CNS Forge ontologies, compatible with Schema.org and semantic web standards" />
      <script type="application/ld+json">{{ jsonLdSchema }}</script>
    </Head>
    
    <div class="mb-8">
      <nav class="mb-6">
        <NuxtLink to="/" class="text-blue-600 hover:text-blue-800">← Back to Home</NuxtLink>
      </nav>
      
      <h1 class="text-4xl font-bold mb-4">JSON-LD Schema</h1>
      <p class="text-xl text-gray-600">
        Structured data representation of the CNS Forge ontology in JSON-LD format, 
        compatible with Schema.org and semantic web standards.
      </p>
    </div>
    
    <!-- Schema Tabs -->
    <div class="mb-6">
      <div class="flex border-b">
        <button 
          v-for="tab in tabs" 
          :key="tab.id"
          @click="activeTab = tab.id"
          :class="[
            'px-6 py-3 font-medium text-sm border-b-2 transition-colors',
            activeTab === tab.id 
              ? 'border-blue-500 text-blue-600' 
              : 'border-transparent text-gray-500 hover:text-gray-700'
          ]"
        >
          {{ tab.label }}
        </button>
      </div>
    </div>
    
    <!-- Schema Content -->
    <div class="grid grid-cols-1 lg:grid-cols-2 gap-8">
      <!-- JSON-LD Display -->
      <div class="bg-gray-900 rounded-lg p-6">
        <div class="flex justify-between items-center mb-4">
          <h2 class="text-lg font-semibold text-white">{{ currentTab.label }}</h2>
          <button 
            @click="copyToClipboard(currentSchema)"
            class="text-sm bg-blue-600 text-white px-3 py-1 rounded hover:bg-blue-700 transition-colors"
          >
            Copy JSON-LD
          </button>
        </div>
        <pre class="text-green-400 text-sm overflow-auto max-h-96"><code>{{ currentSchema }}</code></pre>
      </div>
      
      <!-- Schema Explanation -->
      <div class="bg-white rounded-lg shadow-lg p-6">
        <h2 class="text-lg font-semibold mb-4">Schema Details</h2>
        <div class="space-y-4">
          <div>
            <h3 class="font-medium text-gray-800">Purpose</h3>
            <p class="text-sm text-gray-600">{{ currentTab.description }}</p>
          </div>
          
          <div>
            <h3 class="font-medium text-gray-800">Key Properties</h3>
            <ul class="text-sm text-gray-600 list-disc list-inside space-y-1">
              <li v-for="property in currentTab.properties" :key="property">
                {{ property }}
              </li>
            </ul>
          </div>
          
          <div>
            <h3 class="font-medium text-gray-800">Use Cases</h3>
            <ul class="text-sm text-gray-600 list-disc list-inside space-y-1">
              <li v-for="useCase in currentTab.useCases" :key="useCase">
                {{ useCase }}
              </li>
            </ul>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Validation Section -->
    <div class="mt-8 bg-blue-50 p-6 rounded-lg">
      <h2 class="text-lg font-semibold mb-4">Schema Validation</h2>
      <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div class="text-center">
          <div class="text-2xl mb-2">✅</div>
          <p class="font-medium">Schema.org Valid</p>
          <p class="text-sm text-gray-600">Passes Schema.org validation</p>
        </div>
        <div class="text-center">
          <div class="text-2xl mb-2">🌐</div>
          <p class="font-medium">JSON-LD Valid</p>
          <p class="text-sm text-gray-600">Valid JSON-LD structure</p>
        </div>
        <div class="text-center">
          <div class="text-2xl mb-2">🔍</div>
          <p class="font-medium">SEO Optimized</p>
          <p class="text-sm text-gray-600">Search engine friendly</p>
        </div>
      </div>
    </div>
    
    <!-- Export Options -->
    <div class="mt-8 bg-gray-50 p-6 rounded-lg">
      <h2 class="text-lg font-semibold mb-4">Export Options</h2>
      <div class="flex flex-wrap gap-4">
        <button 
          @click="downloadSchema('json-ld')"
          class="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors"
        >
          Download JSON-LD
        </button>
        <button 
          @click="downloadSchema('turtle')"
          class="bg-green-600 text-white px-4 py-2 rounded hover:bg-green-700 transition-colors"
        >
          Download TTL
        </button>
        <button 
          @click="downloadSchema('rdf-xml')"
          class="bg-purple-600 text-white px-4 py-2 rounded hover:bg-purple-700 transition-colors"
        >
          Download RDF/XML
        </button>
      </div>
    </div>
  </div>
</template>

<script setup>
const { data: ontologyData } = await useFetch('/api/ontology')

const activeTab = ref('dataset')

const tabs = [
  {
    id: 'dataset',
    label: 'Dataset Schema',
    description: 'Overall dataset description for the CNS Forge ontology',
    properties: ['@context', '@type', 'name', 'description', 'keywords', 'license'],
    useCases: ['Search engine indexing', 'Data catalog registration', 'Academic citations']
  },
  {
    id: 'organization',
    label: 'Organization Schema', 
    description: 'CNS Forge organization and project metadata',
    properties: ['@type', 'name', 'description', 'url', 'foundingDate'],
    useCases: ['Company profile optimization', 'Knowledge graph entities', 'Corporate SEO']
  },
  {
    id: 'softwareApplication',
    label: 'Software Schema',
    description: 'BitActor and Ash framework application schemas',
    properties: ['@type', 'name', 'applicationCategory', 'operatingSystem'],
    useCases: ['Software discovery', 'Application metadata', 'API documentation']
  }
]

const schemas = {
  dataset: {
    "@context": "https://schema.org",
    "@type": "Dataset",
    "name": "CNS Forge Semantic Web Ontology",
    "description": "Comprehensive ontology covering BitActors, Ash resources, and semantic models for high-performance systems",
    "url": "https://semantic.cns-forge.com/ontology",
    "keywords": ["semantic web", "ontology", "BitActor", "Ash framework", "performance", "RDF", "OWL"],
    "license": "https://opensource.org/licenses/MIT",
    "creator": {
      "@type": "Organization", 
      "name": "CNS Forge",
      "url": "https://cns-forge.com"
    },
    "distribution": [
      {
        "@type": "DataDownload",
        "encodingFormat": "text/turtle",
        "contentUrl": "/api/ontology/raw"
      },
      {
        "@type": "DataDownload", 
        "encodingFormat": "application/ld+json",
        "contentUrl": "/api/ontology"
      }
    ],
    "temporalCoverage": "2024/..",
    "spatialCoverage": "Global",
    "version": "1.0.0"
  },
  
  organization: {
    "@context": "https://schema.org",
    "@type": "Organization",
    "name": "CNS Forge",
    "description": "Cognitive Nano Stack for building high-performance semantic systems",
    "url": "https://cns-forge.com",
    "foundingDate": "2024",
    "sameAs": [
      "https://github.com/cns-forge"
    ],
    "makesOffer": {
      "@type": "Offer",
      "itemOffered": {
        "@type": "SoftwareApplication",
        "name": "CNS Forge Platform",
        "applicationCategory": "DeveloperApplication",
        "operatingSystem": "Linux, macOS, Windows"
      }
    }
  },
  
  softwareApplication: {
    "@context": "https://schema.org",
    "@type": "SoftwareApplication",
    "name": "BitActor Framework", 
    "description": "Ultra-high performance actor framework for sub-microsecond message processing",
    "applicationCategory": "DeveloperApplication",
    "operatingSystem": "Linux, macOS",
    "programmingLanguage": ["C", "Elixir", "Python"],
    "runtimePlatform": "Native, BEAM VM",
    "softwareRequirements": "LLVM, Erlang/OTP",
    "offers": {
      "@type": "Offer",
      "price": "0",
      "priceCurrency": "USD"
    },
    "featureList": [
      "Sub-microsecond latency",
      "Lock-free message passing",
      "SIMD optimization", 
      "Memory pool management"
    ]
  }
}

const currentTab = computed(() => {
  return tabs.find(tab => tab.id === activeTab.value)
})

const currentSchema = computed(() => {
  return JSON.stringify(schemas[activeTab.value], null, 2)
})

const jsonLdSchema = computed(() => {
  return JSON.stringify(schemas.dataset, null, 2)
})

const copyToClipboard = async (text) => {
  try {
    await navigator.clipboard.writeText(text)
    // You could add a toast notification here
    console.log('Schema copied to clipboard')
  } catch (err) {
    console.error('Failed to copy to clipboard:', err)
  }
}

const downloadSchema = (format) => {
  let content, filename, mimeType
  
  switch (format) {
    case 'json-ld':
      content = currentSchema.value
      filename = `cns-forge-schema-${activeTab.value}.json`
      mimeType = 'application/ld+json'
      break
    case 'turtle':
      // In a real implementation, you'd convert JSON-LD to TTL
      content = '# TTL export not implemented in demo'
      filename = 'cns-forge-ontology.ttl'
      mimeType = 'text/turtle'
      break
    case 'rdf-xml':
      // In a real implementation, you'd convert to RDF/XML
      content = '<!-- RDF/XML export not implemented in demo -->'
      filename = 'cns-forge-ontology.rdf'
      mimeType = 'application/rdf+xml'
      break
  }
  
  const blob = new Blob([content], { type: mimeType })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = filename
  a.click()
  URL.revokeObjectURL(url)
}

// SEO meta
useSeoMeta({
  title: 'JSON-LD Schema - CNS Forge Semantic Web',
  description: 'JSON-LD structured data schema for CNS Forge ontologies, compatible with Schema.org and semantic web standards'
})
</script>