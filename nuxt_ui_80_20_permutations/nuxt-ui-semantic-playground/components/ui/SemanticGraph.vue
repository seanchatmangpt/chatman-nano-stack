<template>
  <div ref="graphContainer" class="w-full h-full relative">
    <div id="cy" class="w-full h-full"></div>
    
    <!-- Graph legend -->
    <div class="absolute top-4 left-4 bg-white rounded-lg shadow-md p-3">
      <h4 class="font-medium text-sm text-gray-900 mb-2">Legend</h4>
      <div class="space-y-1 text-xs">
        <div class="flex items-center">
          <div class="w-3 h-3 rounded-full bg-blue-500 mr-2"></div>
          <span>Pipeline Stage</span>
        </div>
        <div class="flex items-center">
          <div class="w-3 h-3 rounded-full bg-green-500 mr-2"></div>
          <span>Data Resource</span>
        </div>
        <div class="flex items-center">
          <div class="w-3 h-3 rounded-full bg-purple-500 mr-2"></div>
          <span>Class Definition</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import cytoscape from 'cytoscape'
import dagre from 'cytoscape-dagre'
import cola from 'cytoscape-cola'

// Register extensions
cytoscape.use(dagre)
cytoscape.use(cola)

const emit = defineEmits(['node-select', 'edge-select'])

const props = defineProps({
  data: {
    type: Object,
    required: true
  },
  layout: {
    type: String,
    default: 'dagre'
  }
})

const graphContainer = ref(null)
let cy = null

onMounted(() => {
  initializeGraph()
})

watch(() => props.data, () => {
  updateGraph()
}, { deep: true })

watch(() => props.layout, () => {
  if (cy) {
    cy.layout({ name: props.layout }).run()
  }
})

const initializeGraph = () => {
  cy = cytoscape({
    container: document.getElementById('cy'),
    
    elements: convertDataToCytoscape(props.data),
    
    style: [
      {
        selector: 'node',
        style: {
          'background-color': '#60a5fa',
          'label': 'data(label)',
          'text-valign': 'center',
          'text-halign': 'center',
          'color': '#ffffff',
          'font-size': '12px',
          'font-weight': 'bold',
          'width': '60px',
          'height': '60px',
          'border-width': 2,
          'border-color': '#3b82f6'
        }
      },
      {
        selector: 'node[type="stage"]',
        style: {
          'background-color': '#3b82f6',
          'border-color': '#1d4ed8'
        }
      },
      {
        selector: 'node[type="data"]',
        style: {
          'background-color': '#10b981',
          'border-color': '#059669'
        }
      },
      {
        selector: 'node[type="class"]',
        style: {
          'background-color': '#8b5cf6',
          'border-color': '#7c3aed'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 2,
          'line-color': '#6b7280',
          'target-arrow-color': '#6b7280',
          'target-arrow-shape': 'triangle',
          'label': 'data(label)',
          'font-size': '10px',
          'text-rotation': 'autorotate',
          'text-margin-y': -10
        }
      },
      {
        selector: 'node:selected',
        style: {
          'border-width': 4,
          'border-color': '#f59e0b'
        }
      },
      {
        selector: 'edge:selected',
        style: {
          'width': 4,
          'line-color': '#f59e0b'
        }
      }
    ],
    
    layout: {
      name: props.layout,
      directed: true,
      padding: 30,
      spacingFactor: 1.2
    }
  })
  
  // Event handlers
  cy.on('tap', 'node', (event) => {
    const node = event.target
    emit('node-select', {
      id: node.id(),
      label: node.data('label'),
      type: node.data('type')
    })
  })
  
  cy.on('tap', 'edge', (event) => {
    const edge = event.target
    emit('edge-select', {
      id: edge.id(),
      label: edge.data('label'),
      source: edge.source().id(),
      target: edge.target().id()
    })
  })
}

const convertDataToCytoscape = (data) => {
  const elements = []
  
  // Add nodes
  data.nodes.forEach(node => {
    elements.push({
      data: {
        id: node.id,
        label: node.label,
        type: node.type
      }
    })
  })
  
  // Add edges
  data.edges.forEach(edge => {
    elements.push({
      data: {
        id: edge.id,
        source: edge.source,
        target: edge.target,
        label: edge.label
      }
    })
  })
  
  return elements
}

const updateGraph = () => {
  if (cy) {
    cy.elements().remove()
    cy.add(convertDataToCytoscape(props.data))
    cy.layout({ name: props.layout }).run()
  }
}

defineExpose({ cy })
</script>