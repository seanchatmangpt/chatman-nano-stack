<template>
  <div class="h-screen bg-gray-50 flex flex-col">
    <!-- Header -->
    <header class="bg-white border-b border-gray-200 p-4">
      <div class="flex items-center justify-between mb-4">
        <h1 class="text-xl font-semibold">Data Explorer</h1>
        <div class="flex items-center space-x-2">
          <UButton @click="refreshData" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-path" />
          </UButton>
          <UButton @click="exportData" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-down-tray" />
          </UButton>
        </div>
      </div>
      
      <div class="flex items-center space-x-4">
        <UInput 
          v-model="searchQuery"
          placeholder="Search across all data..."
          class="flex-1"
          icon="i-heroicons-magnifying-glass"
        />
        <USelect v-model="selectedDataType" :options="dataTypes" />
      </div>
    </header>
    
    <!-- Content area -->
    <div class="flex flex-1">
      <!-- Data tree navigation -->
      <aside class="w-64 bg-white border-r border-gray-200 p-4">
        <h3 class="font-medium mb-4">Data Sources</h3>
        <div class="space-y-2">
          <div 
            v-for="source in dataSources" 
            :key="source.id"
            class="p-2 rounded cursor-pointer hover:bg-gray-100"
            :class="{ 'bg-blue-100': selectedSource?.id === source.id }"
            @click="selectDataSource(source)"
          >
            <div class="flex items-center space-x-2">
              <UIcon :name="source.icon" class="text-blue-500" />
              <span class="text-sm">{{ source.label }}</span>
            </div>
            <div class="text-xs text-gray-500 ml-6">{{ source.count }} records</div>
          </div>
        </div>
      </aside>
      
      <!-- Main data view -->
      <main class="flex-1 p-4">
        <div class="bg-white rounded-lg shadow h-full">
          <div class="p-4 border-b border-gray-200">
            <div class="flex items-center justify-between">
              <h2 class="text-lg font-semibold">
                {{ selectedSource?.label || 'Select a data source' }}
              </h2>
              <div class="flex items-center space-x-2">
                <span class="text-sm text-gray-500">{{ filteredData.length }} records</span>
                <UButton @click="viewMode = 'table'" :variant="viewMode === 'table' ? 'solid' : 'ghost'" size="sm">
                  <UIcon name="i-heroicons-table-cells" />
                </UButton>
                <UButton @click="viewMode = 'grid'" :variant="viewMode === 'grid' ? 'solid' : 'ghost'" size="sm">
                  <UIcon name="i-heroicons-squares-2x2" />
                </UButton>
              </div>
            </div>
          </div>
          
          <div class="p-4">
            <div v-if="!selectedSource" class="flex items-center justify-center h-64 text-gray-500">
              <div class="text-center">
                <UIcon name="i-heroicons-folder-open" class="w-12 h-12 mx-auto mb-2" />
                <p>Select a data source to explore</p>
              </div>
            </div>
            
            <div v-else-if="viewMode === 'table'" class="space-y-4">
              <div class="overflow-x-auto">
                <table class="w-full border-collapse">
                  <thead>
                    <tr class="border-b border-gray-200">
                      <th v-for="column in tableColumns" :key="column" class="text-left p-2 font-medium">
                        {{ column }}
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr 
                      v-for="row in paginatedData" 
                      :key="row.id"
                      class="border-b border-gray-100 hover:bg-gray-50 cursor-pointer"
                      @click="selectRow(row)"
                    >
                      <td v-for="column in tableColumns" :key="column" class="p-2">
                        {{ row[column] }}
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
              
              <!-- Pagination -->
              <div class="flex items-center justify-between">
                <span class="text-sm text-gray-500">
                  Showing {{ (currentPage - 1) * pageSize + 1 }} to {{ Math.min(currentPage * pageSize, filteredData.length) }} of {{ filteredData.length }}
                </span>
                <div class="flex items-center space-x-2">
                  <UButton @click="currentPage--" :disabled="currentPage === 1" size="sm" variant="outline">
                    Previous
                  </UButton>
                  <span class="text-sm">{{ currentPage }} / {{ totalPages }}</span>
                  <UButton @click="currentPage++" :disabled="currentPage === totalPages" size="sm" variant="outline">
                    Next
                  </UButton>
                </div>
              </div>
            </div>
            
            <div v-else class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              <UCard 
                v-for="item in paginatedData" 
                :key="item.id"
                class="cursor-pointer hover:shadow-md"
                @click="selectRow(item)"
              >
                <div class="space-y-2">
                  <div class="font-medium">{{ item.name || item.id }}</div>
                  <div class="text-sm text-gray-500">{{ item.type || item.category }}</div>
                  <div class="text-xs text-gray-400">{{ item.description || 'No description' }}</div>
                </div>
              </UCard>
            </div>
          </div>
        </div>
      </main>
      
      <!-- Data preview panel -->
      <aside v-if="selectedRow" class="w-80 bg-white border-l border-gray-200 p-4">
        <div class="border-b border-gray-200 pb-4 mb-4">
          <h3 class="font-medium">Data Preview</h3>
        </div>
        <div class="space-y-4">
          <div v-for="(value, key) in selectedRow" :key="key">
            <label class="block text-sm font-medium text-gray-700">{{ key }}</label>
            <div class="text-sm text-gray-900 bg-gray-50 p-2 rounded">{{ value }}</div>
          </div>
        </div>
      </aside>
    </div>
  </div>
</template>

<script setup>
definePageMeta({
  layout: false
})

const searchQuery = ref('')
const viewMode = ref('table')
const selectedDataType = ref('all')
const selectedSource = ref(null)
const selectedRow = ref(null)
const currentPage = ref(1)
const pageSize = 10

const dataTypes = [
  { label: 'All Types', value: 'all' },
  { label: 'Pipeline Data', value: 'pipeline' },
  { label: 'Runtime Data', value: 'runtime' },
  { label: 'Ash Resources', value: 'ash' }
]

const dataSources = ref([
  {
    id: 'typer-output',
    label: '80/20 Typer Output',
    icon: 'i-heroicons-funnel',
    count: 1250,
    type: 'pipeline'
  },
  {
    id: 'turtle-files',
    label: 'Turtle Files',
    icon: 'i-heroicons-document-text',
    count: 845,
    type: 'pipeline'
  },
  {
    id: 'ttl-ontologies',
    label: 'TTL Ontologies',
    icon: 'i-heroicons-code-bracket',
    count: 234,
    type: 'pipeline'
  },
  {
    id: 'ash-resources',
    label: 'Ash Resources',
    icon: 'i-heroicons-server',
    count: 156,
    type: 'ash'
  }
])

const currentData = ref([])
const tableColumns = ref(['id', 'name', 'type', 'status', 'created'])

const filteredData = computed(() => {
  if (!searchQuery.value) return currentData.value
  return currentData.value.filter(item => 
    JSON.stringify(item).toLowerCase().includes(searchQuery.value.toLowerCase())
  )
})

const totalPages = computed(() => Math.ceil(filteredData.value.length / pageSize))

const paginatedData = computed(() => {
  const start = (currentPage.value - 1) * pageSize
  return filteredData.value.slice(start, start + pageSize)
})

const selectDataSource = (source) => {
  selectedSource.value = source
  selectedRow.value = null
  currentPage.value = 1
  
  // Generate sample data based on source
  currentData.value = Array.from({ length: source.count }, (_, i) => ({
    id: `${source.id}_${i + 1}`,
    name: `${source.label} Item ${i + 1}`,
    type: source.type,
    status: Math.random() > 0.1 ? 'active' : 'inactive',
    created: new Date(Date.now() - Math.random() * 30 * 24 * 60 * 60 * 1000).toLocaleDateString(),
    description: `Sample ${source.label.toLowerCase()} data record`
  })).slice(0, Math.min(source.count, 100)) // Limit for demo
}

const selectRow = (row) => {
  selectedRow.value = row
}

const refreshData = () => {
  if (selectedSource.value) {
    selectDataSource(selectedSource.value)
  }
}

const exportData = () => {
  console.log('Exporting data...', filteredData.value)
}

// Load first data source by default
onMounted(() => {
  selectDataSource(dataSources.value[0])
})
</script>
  <div class="h-screen bg-gray-50 flex flex-col">
    <!-- Header with search and filters -->
    <header class="bg-white border-b border-gray-200 p-4">
      <div class="flex items-center justify-between mb-4">
        <h1 class="text-xl font-semibold">Data Explorer</h1>
        <div class="flex items-center space-x-2">
          <UButton @click="refreshData" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-path" />
          </UButton>
          <UButton @click="exportData" variant="outline" size="sm">
            <UIcon name="i-heroicons-arrow-down-tray" />
          </UButton>
        </div>
      </div>
      
      <div class="flex items-center space-x-4">
        <UInput 
          v-model="searchQuery"
          placeholder="Search across all data..."
          class="flex-1"
          icon="i-heroicons-magnifying-glass"
        />
        <DataFilters @filter="applyFilters" />
        <ViewToggle v-model="viewMode" />
      </div>
    </header>
    
    <!-- Content area -->
    <div class="flex flex-1">
      <!-- Data tree navigation -->
      <aside class="w-64 bg-white border-r border-gray-200 p-4">
        <DataTree 
          :data="dataTree"
          @select="selectDataSource"
        />
      </aside>
      
      <!-- Main data view -->
      <main class="flex-1 p-4">
        <component 
          :is="currentViewComponent"
          :data="filteredData"
          :loading="isLoading"
          @row-select="selectRow"
        />
      </main>
      
      <!-- Data preview panel -->
      <aside v-if="selectedRow" class="w-80 bg-white border-l border-gray-200 p-4">
        <DataPreview :data="selectedRow" />
      </aside>
    </div>
  </div>
</template>

<script setup>
const searchQuery = ref('')
const viewMode = ref('table')
const selectedRow = ref(null)
const isLoading = ref(false)

const dataTree = ref([
  {
    id: 'pipeline',
    label: 'Pipeline Data',
    children: [
      { id: 'typer-output', label: '80/20 Typer Output' },
      { id: 'turtle-files', label: 'Turtle Files' },
      { id: 'ttl-ontologies', label: 'TTL Ontologies' }
    ]
  },
  {
    id: 'runtime',
    label: 'Runtime Data',
    children: [
      { id: 'bitactor-metrics', label: 'BitActor Metrics' },
      { id: 'ash-resources', label: 'Ash Resources' },
      { id: 'reactor-workflows', label: 'Reactor Workflows' }
    ]
  }
])

const currentData = ref([])
const filteredData = computed(() => {
  if (!searchQuery.value) return currentData.value
  // Implement fuzzy search
  return currentData.value.filter(item => 
    JSON.stringify(item).toLowerCase().includes(searchQuery.value.toLowerCase())
  )
})

const currentViewComponent = computed(() => {
  const components = {
    table: 'DataTable',
    grid: 'DataGrid', 
    chart: 'DataChart'
  }
  return components[viewMode.value] || 'DataTable'
})

const selectDataSource = async (source) => {
  isLoading.value = true
  // Fetch data for selected source
  currentData.value = await fetchDataSource(source.id)
  isLoading.value = false
}

const selectRow = (row) => {
  selectedRow.value = row
}

const applyFilters = (filters) => {
  // Apply advanced filters
}

const refreshData = () => {
  // Refresh current data source
}

const exportData = () => {
  // Export filtered data
}
</script>