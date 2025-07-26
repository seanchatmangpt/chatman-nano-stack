<template>
  <div class="min-h-screen bg-gray-100">
    <div class="container mx-auto p-6">
      <h1 class="text-3xl font-bold mb-6">Visual Ontology Editor</h1>
      
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <!-- TTL Editor -->
        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="flex justify-between items-center mb-4">
            <h2 class="text-xl font-semibold">TTL Editor</h2>
            <div class="space-x-2">
              <button @click="validateTTL" class="btn-secondary">Validate</button>
              <button @click="generateAsh" class="btn-primary">Generate Ash</button>
            </div>
          </div>
          <MonacoEditor
            v-model="ttlContent"
            language="turtle"
            :options="editorOptions"
            class="h-96"
          />
        </div>
        
        <!-- Visual Graph -->
        <div class="bg-white rounded-lg shadow-lg p-4">
          <h2 class="text-xl font-semibold mb-4">Visual Representation</h2>
          <div ref="ontologyGraph" class="h-96"></div>
        </div>
      </div>
      
      <!-- Generated Resources -->
      <div class="mt-6 bg-white rounded-lg shadow-lg p-4">
        <h2 class="text-xl font-semibold mb-4">Generated Ash Resources</h2>
        <div v-if="ashResources.length > 0" class="space-y-4">
          <ResourcePreview 
            v-for="resource in ashResources" 
            :key="resource.name"
            :resource="resource"
          />
        </div>
        <div v-else class="text-gray-500 text-center py-8">
          No resources generated yet. Edit TTL and click "Generate Ash" to create resources.
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { Network } from 'vis-network/standalone'

const ttlContent = ref(`@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.com/ontology#> .

ex:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "System user entity" .

ex:hasEmail a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string .`)

const ontologyGraph = ref(null)
const ashResources = ref([])

const editorOptions = {
  minimap: { enabled: false },
  fontSize: 14,
  wordWrap: 'on'
}

onMounted(() => {
  updateGraph()
})

watch(ttlContent, debounce(() => {
  updateGraph()
}, 500))

const updateGraph = async () => {
  try {
    const parsed = await parseTTL(ttlContent.value)
    renderGraph(parsed)
  } catch (error) {
    console.error('TTL parsing error:', error)
  }
}

const validateTTL = async () => {
  const { $fetch } = useNuxtApp()
  try {
    const result = await $fetch('/api/validate-ttl', {
      method: 'POST',
      body: { ttl: ttlContent.value }
    })
    
    if (result.valid) {
      alert('TTL is valid!')
    } else {
      alert('Validation errors: ' + result.errors.join(', '))
    }
  } catch (error) {
    alert('Validation failed: ' + error.message)
  }
}

const generateAsh = async () => {
  const { $fetch } = useNuxtApp()
  try {
    const result = await $fetch('/api/generate-ash', {
      method: 'POST',
      body: { ttl: ttlContent.value }
    })
    
    ashResources.value = result.resources
  } catch (error) {
    alert('Generation failed: ' + error.message)
  }
}

const parseTTL = async (ttl) => {
  // Simple TTL parser for demo
  const classes = []
  const properties = []
  
  const lines = ttl.split('\n')
  let currentSubject = null
  
  for (const line of lines) {
    if (line.includes('a owl:Class')) {
      const match = line.match(/(\w+:\w+)\s+a\s+owl:Class/)
      if (match) {
        currentSubject = match[1]
        classes.push({
          id: currentSubject,
          label: currentSubject.split(':')[1]
        })
      }
    } else if (line.includes('a owl:DatatypeProperty') || line.includes('a owl:ObjectProperty')) {
      const match = line.match(/(\w+:\w+)\s+a\s+owl:(\w+Property)/)
      if (match) {
        properties.push({
          id: match[1],
          label: match[1].split(':')[1],
          type: match[2]
        })
      }
    }
  }
  
  return { classes, properties }
}

const renderGraph = (parsed) => {
  const nodes = parsed.classes.map((cls, i) => ({
    id: cls.id,
    label: cls.label,
    color: '#3b82f6',
    x: i * 200,
    y: 0
  }))
  
  const edges = parsed.properties.map(prop => ({
    from: prop.domain || 'unknown',
    to: prop.range || 'unknown',
    label: prop.label,
    arrows: 'to'
  }))
  
  const data = { nodes, edges }
  const options = {
    physics: {
      stabilization: false
    }
  }
  
  new Network(ontologyGraph.value, data, options)
}
</script>

<style>
.btn-primary {
  @apply bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700;
}

.btn-secondary {
  @apply bg-gray-200 text-gray-800 px-4 py-2 rounded hover:bg-gray-300;
}
</style>