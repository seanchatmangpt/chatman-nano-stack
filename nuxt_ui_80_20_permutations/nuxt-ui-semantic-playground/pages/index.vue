<template>
  <div class="h-screen bg-white flex flex-col">
    <!-- Header -->
    <header class="bg-gray-50 border-b border-gray-200 px-6 py-4">
      <div class="flex items-center justify-between">
        <div class="flex items-center space-x-4">
          <h1 class="text-xl font-bold text-gray-900">Semantic Web Playground</h1>
          <UBadge color="blue" variant="soft">{{ currentFormat }}</UBadge>
        </div>
        
        <div class="flex items-center space-x-2">
          <FormatSelector v-model="currentFormat" />
          <UButton @click="validateSemantic" variant="outline" size="sm">
            <UIcon name="i-heroicons-check-circle" class="mr-2" />
            Validate
          </UButton>
          <UButton @click="transformPipeline" color="primary" size="sm">
            <UIcon name="i-heroicons-arrow-path" class="mr-2" />
            Transform
          </UButton>
          <UButton @click="exportResults" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-down-tray" class="mr-2" />
            Export
          </UButton>
        </div>
      </div>
    </header>
    
    <!-- Main Content -->
    <div class="flex flex-1">
      <!-- Left Panel - Code Editor -->
      <div class="w-1/2 border-r border-gray-200 flex flex-col">
        <div class="bg-gray-50 border-b border-gray-200 px-4 py-2">
          <div class="flex items-center justify-between">
            <h3 class="font-medium text-gray-900">Semantic Model Editor</h3>
            <div class="flex items-center space-x-2">
              <EditorSettings @settings="updateEditorSettings" />
              <UButton @click="loadExample" variant="ghost" size="xs">
                Load Example
              </UButton>
            </div>
          </div>
        </div>
        
        <div class="flex-1 relative">
          <MonacoEditor 
            ref="editor"
            v-model="semanticContent"
            :language="editorLanguage"
            :options="editorOptions"
            @change="onContentChange"
          />
          
          <!-- Editor overlay for live validation -->
          <div v-if="validationErrors.length" class="absolute bottom-4 right-4">
            <ValidationPanel :errors="validationErrors" />
          </div>
        </div>
        
        <!-- Editor toolbar -->
        <div class="bg-gray-50 border-t border-gray-200 px-4 py-2">
          <div class="flex items-center justify-between text-sm text-gray-600">
            <div class="flex items-center space-x-4">
              <span>Lines: {{ lineCount }}</span>
              <span>Size: {{ fileSize }}</span>
              <span :class="validationStatus.color">{{ validationStatus.text }}</span>
            </div>
            
            <div class="flex items-center space-x-2">
              <UButton @click="formatCode" variant="ghost" size="xs">
                Format
              </UButton>
              <UButton @click="minifyCode" variant="ghost" size="xs">
                Minify
              </UButton>
            </div>
          </div>
        </div>
      </div>
      
      <!-- Right Panel - Visualization & Tools -->
      <div class="w-1/2 flex flex-col">
        <!-- Tab Navigation -->
        <div class="bg-gray-50 border-b border-gray-200">
          <nav class="flex space-x-8 px-4">
            <button 
              v-for="tab in visualizationTabs" 
              :key="tab.id"
              @click="activeTab = tab.id"
              :class="[
                'py-3 px-1 border-b-2 font-medium text-sm',
                activeTab === tab.id 
                  ? 'border-blue-500 text-blue-600' 
                  : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
              ]"
            >
              <UIcon :name="tab.icon" class="mr-2" />
              {{ tab.label }}
            </button>
          </nav>
        </div>
        
        <!-- Tab Content -->
        <div class="flex-1 overflow-hidden">
          <!-- Graph Visualization -->
          <div v-show="activeTab === 'graph'" class="h-full">
            <SemanticGraph 
              ref="graph"
              :data="graphData"
              :layout="graphLayout"
              @node-select="selectNode"
              @edge-select="selectEdge"
            />
            
            <!-- Graph controls -->
            <div class="absolute top-4 right-4">
              <GraphControls 
                @layout="changeGraphLayout"
                @filter="filterGraph"
                @export="exportGraph"
              />
            </div>
          </div>
          
          <!-- Tree View -->
          <div v-show="activeTab === 'tree'" class="h-full p-4 overflow-auto">
            <SemanticTree 
              :data="treeData"
              :expandable="true"
              @node-select="selectTreeNode"
            />
          </div>
          
          <!-- Table View -->
          <div v-show="activeTab === 'table'" class="h-full p-4">
            <SemanticTable 
              :data="tableData"
              :columns="tableColumns"
              :sortable="true"
              :filterable="true"
            />
          </div>
          
          <!-- Pipeline Preview -->
          <div v-show="activeTab === 'pipeline'" class="h-full p-4">
            <PipelinePreview 
              :stages="pipelineStages"
              :current-data="semanticContent"
              @stage-select="previewStage"
            />
          </div>
          
          <!-- Validation Results -->
          <div v-show="activeTab === 'validation'" class="h-full p-4 overflow-auto">
            <ValidationResults 
              :results="validationResults"
              :suggestions="validationSuggestions"
            />
          </div>
        </div>
      </div>
    </div>
    
    <!-- Bottom Panel - Properties & Inspector -->
    <div v-if="selectedItem" class="h-64 border-t border-gray-200 bg-gray-50">
      <div class="flex h-full">
        <!-- Properties -->
        <div class="w-1/2 border-r border-gray-200 p-4">
          <h4 class="font-medium text-gray-900 mb-3">Properties</h4>
          <PropertyEditor 
            :item="selectedItem"
            @update="updateItemProperties"
          />
        </div>
        
        <!-- Inspector -->
        <div class="w-1/2 p-4">
          <h4 class="font-medium text-gray-900 mb-3">Inspector</h4>
          <ItemInspector :item="selectedItem" />
        </div>
      </div>
    </div>
    
    <!-- Transformation Progress Modal -->
    <UModal v-model="showTransformProgress">
      <UCard>
        <template #header>
          <h3 class="font-medium">Pipeline Transformation</h3>
        </template>
        <TransformationProgress 
          :stages="transformationStages"
          :current-stage="currentTransformStage"
        />
      </UCard>
    </UModal>
  </div>
</template>

<script setup>
const { $fetch } = useNuxtApp()

const currentFormat = ref('ttl')
const semanticContent = ref('')
const activeTab = ref('graph')
const selectedItem = ref(null)
const showTransformProgress = ref(false)

const validationErrors = ref([])
const validationResults = ref(null)
const validationSuggestions = ref([])

const graphData = ref({ nodes: [], edges: [] })
const treeData = ref([])
const tableData = ref([])
const pipelineStages = ref([])
const transformationStages = ref([])
const currentTransformStage = ref(0)

const editorLanguage = computed(() => {
  const languages = {
    'ttl': 'turtle',
    'rdf': 'xml',
    'json-ld': 'json',
    'n3': 'n3',
    'sparql': 'sparql'
  }
  return languages[currentFormat.value] || 'turtle'
})

const editorOptions = ref({
  theme: 'vs-light',
  fontSize: 14,
  minimap: { enabled: true },
  wordWrap: 'on',
  lineNumbers: 'on',
  folding: true,
  renderLineHighlight: 'all'
})

const visualizationTabs = [
  { id: 'graph', label: 'Graph', icon: 'i-heroicons-share' },
  { id: 'tree', label: 'Tree', icon: 'i-heroicons-bars-3-bottom-left' },
  { id: 'table', label: 'Table', icon: 'i-heroicons-table-cells' },
  { id: 'pipeline', label: 'Pipeline', icon: 'i-heroicons-arrow-trending-up' },
  { id: 'validation', label: 'Validation', icon: 'i-heroicons-check-circle' }
]

const lineCount = computed(() => {
  return semanticContent.value.split('\n').length
})

const fileSize = computed(() => {
  const bytes = new Blob([semanticContent.value]).size
  return bytes < 1024 ? `${bytes}B` : `${(bytes / 1024).toFixed(1)}KB`
})

const validationStatus = computed(() => {
  if (validationErrors.value.length === 0) {
    return { text: 'Valid', color: 'text-green-600' }
  } else {
    return { text: `${validationErrors.value.length} errors`, color: 'text-red-600' }
  }
})

const graphLayout = ref('dagre')

onMounted(() => {
  loadDefaultExample()
  setupLiveValidation()
})

const loadDefaultExample = () => {
  semanticContent.value = `@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

# CNS Pipeline Ontology Example
ex:PipelineStage rdf:type owl:Class ;
    rdfs:label "Pipeline Stage" ;
    rdfs:comment "A stage in the CNS processing pipeline" .

ex:Typer rdf:type ex:PipelineStage ;
    rdfs:label "80/20 Typer" ;
    ex:processes ex:InputData ;
    ex:outputs ex:TypedData .

ex:TurtleGenerator rdf:type ex:PipelineStage ;
    rdfs:label "Turtle Generator" ;
    ex:processes ex:TypedData ;
    ex:outputs ex:RDFTurtle .

ex:TTL2DSPy rdf:type ex:PipelineStage ;
    rdfs:label "TTL to DSPy Converter" ;
    ex:processes ex:RDFTurtle ;
    ex:outputs ex:DSPyModel .

ex:BitActor rdf:type ex:PipelineStage ;
    rdfs:label "BitActor Processing" ;
    ex:processes ex:DSPyModel ;
    ex:outputs ex:ProcessedData .

ex:AshResource rdf:type ex:PipelineStage ;
    rdfs:label "Ash Resource Generator" ;
    ex:processes ex:ProcessedData ;
    ex:outputs ex:ElixirResource .

ex:Reactor rdf:type ex:PipelineStage ;
    rdfs:label "Reactor Workflow" ;
    ex:processes ex:ElixirResource ;
    ex:outputs ex:WorkflowDefinition .`
  
  parseSemanticContent()
}

const onContentChange = () => {
  parseSemanticContent()
  validateLive()
}

const parseSemanticContent = () => {
  // Parse semantic content and update visualizations
  graphData.value = parseToGraph(semanticContent.value)
  treeData.value = parseToTree(semanticContent.value)
  tableData.value = parseToTable(semanticContent.value)
}

const parseToGraph = (content) => {
  // Simplified RDF parsing for demo
  const lines = content.split('\n').filter(line => line.trim() && !line.startsWith('#'))
  const nodes = new Set()
  const edges = []
  
  lines.forEach(line => {
    const match = line.match(/(\S+)\s+(\S+)\s+(\S+)/)
    if (match) {
      const [, subject, predicate, object] = match
      nodes.add(subject)
      nodes.add(object)
      edges.push({
        id: `${subject}-${predicate}-${object}`,
        source: subject,
        target: object,
        label: predicate.split(':')[1] || predicate
      })
    }
  })
  
  return {
    nodes: Array.from(nodes).map(id => ({
      id,
      label: id.split(':')[1] || id,
      type: id.includes('Stage') ? 'stage' : 'data'
    })),
    edges
  }
}

const parseToTree = (content) => {
  // Parse to hierarchical tree structure
  return [
    {
      id: 'pipeline',
      label: 'CNS Pipeline',
      children: [
        { id: 'typer', label: '80/20 Typer' },
        { id: 'turtle', label: 'Turtle Generator' },
        { id: 'ttl2dspy', label: 'TTL2DSPy' },
        { id: 'bitactor', label: 'BitActor' },
        { id: 'ash', label: 'Ash Resources' },
        { id: 'reactor', label: 'Reactor' }
      ]
    }
  ]
}

const parseToTable = (content) => {
  // Parse to tabular data
  const lines = content.split('\n').filter(line => line.trim() && !line.startsWith('#'))
  return lines.map((line, index) => {
    const match = line.match(/(\S+)\s+(\S+)\s+(\S+)/)
    if (match) {
      const [, subject, predicate, object] = match
      return {
        id: index,
        subject: subject.split(':')[1] || subject,
        predicate: predicate.split(':')[1] || predicate,
        object: object.split(':')[1] || object
      }
    }
    return null
  }).filter(Boolean)
}

const validateLive = async () => {
  // Real-time validation
  try {
    const result = await $fetch('/api/semantic/validate', {
      method: 'POST',
      body: {
        content: semanticContent.value,
        format: currentFormat.value
      }
    })
    validationErrors.value = result.errors || []
    validationResults.value = result
  } catch (error) {
    console.error('Validation failed:', error)
  }
}

const validateSemantic = async () => {
  await validateLive()
  activeTab.value = 'validation'
}

const transformPipeline = async () => {
  showTransformProgress.value = true
  currentTransformStage.value = 0
  
  transformationStages.value = [
    { name: '80/20 Typer', status: 'pending' },
    { name: 'Turtle Generation', status: 'pending' },
    { name: 'TTL2DSPy', status: 'pending' },
    { name: 'BitActor Processing', status: 'pending' },
    { name: 'Ash Resources', status: 'pending' },
    { name: 'Reactor Workflow', status: 'pending' }
  ]
  
  // Simulate pipeline transformation
  for (let i = 0; i < transformationStages.value.length; i++) {
    transformationStages.value[i].status = 'running'
    currentTransformStage.value = i
    
    // Simulate API call
    await new Promise(resolve => setTimeout(resolve, 1000))
    
    transformationStages.value[i].status = 'completed'
  }
  
  setTimeout(() => {
    showTransformProgress.value = false
  }, 1000)
}

const selectNode = (node) => {
  selectedItem.value = { type: 'node', data: node }
}

const selectEdge = (edge) => {
  selectedItem.value = { type: 'edge', data: edge }
}

const selectTreeNode = (node) => {
  selectedItem.value = { type: 'tree-node', data: node }
}

const updateItemProperties = (properties) => {
  if (selectedItem.value) {
    selectedItem.value.data = { ...selectedItem.value.data, ...properties }
  }
}

const changeGraphLayout = (layout) => {
  graphLayout.value = layout
}

const filterGraph = (filter) => {
  // Apply graph filtering
}

const exportGraph = () => {
  // Export graph visualization
}

const formatCode = () => {
  // Format semantic content
}

const minifyCode = () => {
  // Minify semantic content
}

const loadExample = () => {
  loadDefaultExample()
}

const exportResults = () => {
  // Export current semantic model
}

const setupLiveValidation = () => {
  // Setup real-time validation
}

const updateEditorSettings = (settings) => {
  editorOptions.value = { ...editorOptions.value, ...settings }
}

const previewStage = (stage) => {
  // Preview pipeline stage transformation
}

const tableColumns = [
  { key: 'subject', label: 'Subject' },
  { key: 'predicate', label: 'Predicate' },
  { key: 'object', label: 'Object' }
]
</script>