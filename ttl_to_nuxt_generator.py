#!/usr/bin/env python3
"""
TTL to Nuxt Generator for CNS Aegis Fabric
Generates Vue 3 components, composables, and types from TTL ontologies
Uses Jinja2 templates for 80/20 optimized code generation
Designed for real-time security dashboards and monitoring
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
import rdflib
from rdflib import Graph, URIRef, Literal, Namespace, RDF, RDFS, OWL
from jinja2 import Environment, FileSystemLoader
import typer
from rich.console import Console
from rich.table import Table
from datetime import datetime

# Define namespaces
CYBER = Namespace("http://cns.ai/ontology/cybersecurity#")
BA = Namespace("http://bitactor.org/ontology#")
SH = Namespace("http://www.w3.org/ns/shacl#")
XSD = Namespace("http://www.w3.org/2001/XMLSchema#")

app = typer.Typer()
console = Console()


class TTLToNuxtGenerator:
    """Generate Nuxt 3 components from TTL ontologies for real-time dashboards"""
    
    def __init__(self, template_dir: str = "templates/nuxt"):
        self.template_dir = Path(template_dir)
        self.template_dir.mkdir(parents=True, exist_ok=True)
        
        # Create Jinja environment
        self.env = Environment(
            loader=FileSystemLoader(str(self.template_dir)),
            trim_blocks=True,
            lstrip_blocks=True
        )
        
        # Add custom filters
        self.env.filters['kebab_case'] = self._kebab_case
        self.env.filters['pascal_case'] = self._pascal_case
        self.env.filters['camel_case'] = self._camel_case
        self.env.filters['component_name'] = self._component_name
        
        self.graph = Graph()
        self.components = []
        self.composables = []
        self.types = []
        
    def _kebab_case(self, name: str) -> str:
        """Convert to kebab-case for file names"""
        import re
        name = name.split('#')[-1].split('/')[-1]
        name = re.sub(r'([a-z0-9])([A-Z])', r'\1-\2', name)
        name = re.sub(r'[^a-zA-Z0-9]', '-', name)
        return name.lower()
    
    def _pascal_case(self, name: str) -> str:
        """Convert to PascalCase for component names"""
        parts = self._kebab_case(name).split('-')
        return ''.join(word.capitalize() for word in parts if word)
    
    def _camel_case(self, name: str) -> str:
        """Convert to camelCase for variables"""
        pascal = self._pascal_case(name)
        return pascal[0].lower() + pascal[1:] if pascal else ''
    
    def _component_name(self, name: str) -> str:
        """Generate component name from class name"""
        clean_name = self._pascal_case(name)
        if not clean_name.endswith('Component'):
            clean_name += 'Component'
        return clean_name
    
    def load_ttl(self, ttl_file: str) -> None:
        """Load TTL ontology"""
        self.graph.parse(ttl_file, format='turtle')
        console.print(f"[green]✅ Loaded {len(self.graph)} triples from {ttl_file}[/green]")
    
    def extract_classes(self) -> List[Dict[str, Any]]:
        """Extract class definitions from TTL"""
        classes = []
        
        # Find all OWL classes
        for cls in self.graph.subjects(RDF.type, OWL.Class):
            if isinstance(cls, URIRef):
                class_name = str(cls).split('#')[-1]
                
                # Skip built-in classes
                if str(cls).startswith('http://www.w3.org/'):
                    continue
                
                # Get class metadata
                label = self.graph.value(cls, RDFS.label, default=class_name)
                comment = self.graph.value(cls, RDFS.comment, default="")
                
                # Get parent classes
                parents = [str(p).split('#')[-1] for p in self.graph.objects(cls, RDFS.subClassOf)]
                
                # Get properties for this class
                properties = self._extract_class_properties(cls)
                
                classes.append({
                    'name': class_name,
                    'label': str(label),
                    'description': str(comment),
                    'uri': str(cls),
                    'parents': parents,
                    'properties': properties,
                    'is_threat': 'Threat' in class_name or 'Attack' in class_name,
                    'is_asset': 'Asset' in class_name,
                    'is_network': 'Network' in class_name
                })
        
        return sorted(classes, key=lambda x: x['name'])
    
    def _extract_class_properties(self, cls: URIRef) -> List[Dict[str, Any]]:
        """Extract properties associated with a class"""
        properties = []
        
        # Find properties that have this class as domain
        for prop in self.graph.subjects(RDFS.domain, cls):
            prop_name = str(prop).split('#')[-1]
            prop_range = self.graph.value(prop, RDFS.range)
            prop_type = self._map_to_typescript_type(prop_range)
            
            properties.append({
                'name': prop_name,
                'type': prop_type,
                'label': str(self.graph.value(prop, RDFS.label, default=prop_name)),
                'required': False  # Could be enhanced with SHACL constraints
            })
        
        return properties
    
    def _map_to_typescript_type(self, rdf_type: Optional[URIRef]) -> str:
        """Map RDF types to TypeScript types"""
        if not rdf_type:
            return 'any'
        
        type_map = {
            str(XSD.string): 'string',
            str(XSD.integer): 'number',
            str(XSD.int): 'number',
            str(XSD.float): 'number',
            str(XSD.double): 'number',
            str(XSD.boolean): 'boolean',
            str(XSD.dateTime): 'Date',
            str(XSD.date): 'Date'
        }
        
        return type_map.get(str(rdf_type), 'string')
    
    def generate_components(self, output_dir: str, classes: List[Dict[str, Any]]) -> None:
        """Generate Vue components for each class"""
        components_dir = Path(output_dir) / 'components' / 'aegis'
        components_dir.mkdir(parents=True, exist_ok=True)
        
        # Group classes by type
        threat_classes = [c for c in classes if c['is_threat']]
        asset_classes = [c for c in classes if c['is_asset']]
        network_classes = [c for c in classes if c['is_network']]
        
        # Generate dashboard components
        self._generate_threat_dashboard(components_dir, threat_classes)
        self._generate_asset_monitor(components_dir, asset_classes)
        self._generate_network_topology(components_dir, network_classes)
        
        # Generate individual class components
        for cls in classes:
            self._generate_class_component(components_dir, cls)
    
    def _generate_threat_dashboard(self, output_dir: Path, threat_classes: List[Dict]) -> None:
        """Generate threat dashboard component"""
        template = self.env.from_string(THREAT_DASHBOARD_TEMPLATE)
        content = template.render(
            threats=threat_classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = output_dir / 'ThreatDashboard.vue'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")
    
    def _generate_asset_monitor(self, output_dir: Path, asset_classes: List[Dict]) -> None:
        """Generate asset monitoring component"""
        template = self.env.from_string(ASSET_MONITOR_TEMPLATE)
        content = template.render(
            assets=asset_classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = output_dir / 'AssetMonitor.vue'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")
    
    def _generate_network_topology(self, output_dir: Path, network_classes: List[Dict]) -> None:
        """Generate network topology component"""
        template = self.env.from_string(NETWORK_TOPOLOGY_TEMPLATE)
        content = template.render(
            networks=network_classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = output_dir / 'NetworkTopology.vue'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")
    
    def _generate_class_component(self, output_dir: Path, cls: Dict) -> None:
        """Generate individual class component"""
        template = self.env.from_string(CLASS_COMPONENT_TEMPLATE)
        content = template.render(
            cls=cls,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = output_dir / f"{cls['name']}.vue"
        output_file.write_text(content)
        
    def generate_composables(self, output_dir: str, classes: List[Dict[str, Any]]) -> None:
        """Generate composables for data fetching and state management"""
        composables_dir = Path(output_dir) / 'composables'
        composables_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate main Aegis composable
        template = self.env.from_string(AEGIS_COMPOSABLE_TEMPLATE)
        content = template.render(
            classes=classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = composables_dir / 'useAegisFabric.ts'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")
    
    def generate_types(self, output_dir: str, classes: List[Dict[str, Any]]) -> None:
        """Generate TypeScript type definitions"""
        types_dir = Path(output_dir) / 'types'
        types_dir.mkdir(parents=True, exist_ok=True)
        
        template = self.env.from_string(TYPES_TEMPLATE)
        content = template.render(
            classes=classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = types_dir / 'aegis.ts'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")
    
    def generate_api_routes(self, output_dir: str, classes: List[Dict[str, Any]]) -> None:
        """Generate Nuxt server API routes"""
        api_dir = Path(output_dir) / 'server' / 'api' / 'aegis'
        api_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate WebSocket handler
        template = self.env.from_string(WEBSOCKET_API_TEMPLATE)
        content = template.render(
            classes=classes,
            generated_at=datetime.now().isoformat()
        )
        
        output_file = api_dir / 'ws.ts'
        output_file.write_text(content)
        console.print(f"[green]✅ Generated {output_file}[/green]")


# Component Templates (stored as strings for simplicity, could be external files)
THREAT_DASHBOARD_TEMPLATE = '''<template>
  <div class="threat-dashboard">
    <h2 class="text-2xl font-bold mb-4">🛡️ Threat Detection Dashboard</h2>
    
    <!-- Real-time threat map -->
    <div class="threat-map mb-6">
      <ClientOnly>
        <ThreatMap 
          :threats="activethreats" 
          :update-interval="100"
          @threat-selected="onThreatSelected" 
        />
      </ClientOnly>
    </div>
    
    <!-- Threat statistics -->
    <div class="grid grid-cols-4 gap-4 mb-6">
      <StatCard
        v-for="stat in threatStats"
        :key="stat.label"
        :label="stat.label"
        :value="stat.value"
        :trend="stat.trend"
        :color="stat.color"
      />
    </div>
    
    <!-- Active threats list -->
    <div class="active-threats">
      <h3 class="text-xl font-semibold mb-3">Active Threats</h3>
      <div class="space-y-2">
        {% for threat in threats %}
        <ThreatCard
          v-for="threat in {{ threat.name | camel_case }}s"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        {% endfor %}
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from TTL ontology on {{ generated_at }}
import { useAegisFabric } from '~/composables/useAegisFabric'
import type { Threat, ThreatStats } from '~/types/aegis'

const { activethreats, threatStats, neutralizeThreat } = useAegisFabric()

const onThreatSelected = (threat: Threat) => {
  console.log('Threat selected:', threat)
  // Handle threat selection
}
</script>

<style scoped>
.threat-dashboard {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.threat-map {
  @apply h-96 bg-gray-800 rounded-lg overflow-hidden;
}
</style>
'''

ASSET_MONITOR_TEMPLATE = '''<template>
  <div class="asset-monitor">
    <h2 class="text-2xl font-bold mb-4">📊 Asset Monitoring</h2>
    
    <!-- Asset health overview -->
    <div class="grid grid-cols-3 gap-4 mb-6">
      {% for asset in assets %}
      <div class="asset-category">
        <h3 class="text-lg font-semibold mb-2">{{ asset.label }}</h3>
        <AssetHealthIndicator
          :health="get{{ asset.name }}Health()"
          :count="{{ asset.name | camel_case }}s.length"
        />
      </div>
      {% endfor %}
    </div>
    
    <!-- Real-time asset grid -->
    <div class="asset-grid">
      <AssetGrid
        :assets="allAssets"
        :update-interval="250"
        @asset-clicked="onAssetClicked"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from TTL ontology on {{ generated_at }}
import { useAegisFabric } from '~/composables/useAegisFabric'
import type { Asset } from '~/types/aegis'

const { 
  {% for asset in assets -%}
  {{ asset.name | camel_case }}s,
  get{{ asset.name }}Health,
  {% endfor -%}
  allAssets 
} = useAegisFabric()

const onAssetClicked = (asset: Asset) => {
  console.log('Asset clicked:', asset)
  // Handle asset click
}
</script>

<style scoped>
.asset-monitor {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.asset-grid {
  @apply grid grid-cols-6 gap-2;
}
</style>
'''

NETWORK_TOPOLOGY_TEMPLATE = '''<template>
  <div class="network-topology">
    <h2 class="text-2xl font-bold mb-4">🌐 Network Topology</h2>
    
    <!-- Interactive network visualization -->
    <div class="network-viz">
      <ClientOnly>
        <NetworkGraph
          :nodes="networkNodes"
          :edges="networkEdges"
          :threats="activeNetworkThreats"
          @node-selected="onNodeSelected"
        />
      </ClientOnly>
    </div>
    
    <!-- Network segments -->
    <div class="mt-6">
      <h3 class="text-xl font-semibold mb-3">Network Segments</h3>
      <div class="grid grid-cols-3 gap-4">
        {% for network in networks %}
        <NetworkSegmentCard
          v-for="segment in {{ network.name | camel_case }}s"
          :key="segment.id"
          :segment="segment"
          :health="getSegmentHealth(segment)"
        />
        {% endfor %}
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
// Generated from TTL ontology on {{ generated_at }}
import { useAegisFabric } from '~/composables/useAegisFabric'
import type { NetworkNode, NetworkSegment } from '~/types/aegis'

const { 
  networkNodes, 
  networkEdges, 
  activeNetworkThreats,
  {% for network in networks -%}
  {{ network.name | camel_case }}s,
  {% endfor -%}
  getSegmentHealth 
} = useAegisFabric()

const onNodeSelected = (node: NetworkNode) => {
  console.log('Network node selected:', node)
  // Handle node selection
}
</script>

<style scoped>
.network-topology {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.network-viz {
  @apply h-96 bg-gray-800 rounded-lg;
}
</style>
'''

CLASS_COMPONENT_TEMPLATE = '''<template>
  <div class="{{ cls.name | kebab_case }}-component">
    <h3 class="text-lg font-semibold mb-2">{{ cls.label }}</h3>
    <p class="text-gray-400 mb-4">{{ cls.description }}</p>
    
    {% if cls.properties %}
    <div class="properties">
      {% for prop in cls.properties %}
      <div class="property mb-2">
        <label class="block text-sm font-medium">{{ prop.label }}</label>
        <span class="text-gray-300">{{ "{{ " }}{{ cls.name | camel_case }}.{{ prop.name }}{{ " }}" }}</span>
      </div>
      {% endfor %}
    </div>
    {% endif %}
  </div>
</template>

<script setup lang="ts">
// Generated from {{ cls.uri }} on {{ generated_at }}
import type { {{ cls.name }} } from '~/types/aegis'

interface Props {
  {{ cls.name | camel_case }}: {{ cls.name }}
}

const props = defineProps<Props>()
</script>

<style scoped>
.{{ cls.name | kebab_case }}-component {
  @apply p-4 bg-gray-800 rounded-lg;
}
</style>
'''

AEGIS_COMPOSABLE_TEMPLATE = '''// Generated from TTL ontology on {{ generated_at }}
import { ref, computed, onMounted, onUnmounted } from 'vue'
import type { Ref } from 'vue'
import type { 
  {% for cls in classes -%}
  {{ cls.name }},
  {% endfor -%}
  ThreatStats,
  NetworkNode,
  NetworkEdge
} from '~/types/aegis'

export const useAegisFabric = () => {
  // WebSocket connection
  const ws = ref<WebSocket | null>(null)
  const connected = ref(false)
  
  // State for each class
  {% for cls in classes %}
  const {{ cls.name | camel_case }}s: Ref<{{ cls.name }}[]> = ref([])
  {% endfor %}
  
  // Computed properties
  const activethreats = computed(() => {
    const threats = []
    {% for cls in classes %}
    {% if cls.is_threat %}
    threats.push(...{{ cls.name | camel_case }}s.value)
    {% endif %}
    {% endfor %}
    return threats
  })
  
  const allAssets = computed(() => {
    const assets = []
    {% for cls in classes %}
    {% if cls.is_asset %}
    assets.push(...{{ cls.name | camel_case }}s.value)
    {% endif %}
    {% endfor %}
    return assets
  })
  
  const threatStats = computed<ThreatStats>(() => ({
    total: activethreats.value.length,
    critical: activethreats.value.filter(t => t.severity === 'critical').length,
    high: activethreats.value.filter(t => t.severity === 'high').length,
    medium: activethreats.value.filter(t => t.severity === 'medium').length,
    low: activethreats.value.filter(t => t.severity === 'low').length,
    neutralized: 0
  }))
  
  // WebSocket connection
  const connect = () => {
    ws.value = new WebSocket(`ws://${window.location.host}/api/aegis/ws`)
    
    ws.value.onopen = () => {
      connected.value = true
      console.log('Connected to Aegis Fabric')
    }
    
    ws.value.onmessage = (event) => {
      const data = JSON.parse(event.data)
      handleMessage(data)
    }
    
    ws.value.onclose = () => {
      connected.value = false
      console.log('Disconnected from Aegis Fabric')
      // Reconnect after 1 second
      setTimeout(connect, 1000)
    }
  }
  
  const handleMessage = (data: any) => {
    switch (data.type) {
      {% for cls in classes %}
      case '{{ cls.name }}Update':
        {{ cls.name | camel_case }}s.value = data.payload
        break
      {% endfor %}
      default:
        console.warn('Unknown message type:', data.type)
    }
  }
  
  // Actions
  const neutralizeThreat = async (threatId: string) => {
    if (ws.value?.readyState === WebSocket.OPEN) {
      ws.value.send(JSON.stringify({
        type: 'neutralizeThreat',
        threatId
      }))
    }
  }
  
  // Health checks
  {% for cls in classes %}
  {% if cls.is_asset %}
  const get{{ cls.name }}Health = () => {
    const assets = {{ cls.name | camel_case }}s.value
    if (assets.length === 0) return 'unknown'
    const healthyCount = assets.filter(a => a.status === 'healthy').length
    const ratio = healthyCount / assets.length
    if (ratio >= 0.9) return 'healthy'
    if (ratio >= 0.7) return 'warning'
    return 'critical'
  }
  {% endif %}
  {% endfor %}
  
  const getSegmentHealth = (segment: any) => {
    // Implement segment health logic
    return 'healthy'
  }
  
  // Network topology
  const networkNodes = computed<NetworkNode[]>(() => {
    // Build network nodes from assets
    return []
  })
  
  const networkEdges = computed<NetworkEdge[]>(() => {
    // Build network edges from relationships
    return []
  })
  
  const activeNetworkThreats = computed(() => {
    return activethreats.value.filter(t => t.targetType === 'network')
  })
  
  // Lifecycle
  onMounted(() => {
    connect()
  })
  
  onUnmounted(() => {
    if (ws.value) {
      ws.value.close()
    }
  })
  
  return {
    // State
    {% for cls in classes -%}
    {{ cls.name | camel_case }}s,
    {% endfor %}
    
    // Computed
    activethreats,
    allAssets,
    threatStats,
    networkNodes,
    networkEdges,
    activeNetworkThreats,
    
    // Actions
    neutralizeThreat,
    
    // Health
    {% for cls in classes %}
    {% if cls.is_asset -%}
    get{{ cls.name }}Health,
    {% endif -%}
    {% endfor -%}
    getSegmentHealth,
    
    // Connection
    connected
  }
}
'''

TYPES_TEMPLATE = '''// Generated TypeScript types from TTL ontology
// Generated on {{ generated_at }}

{% for cls in classes %}
export interface {{ cls.name }} {
  id: string
  uri: string
  label: string
  {% for prop in cls.properties -%}
  {{ prop.name }}: {{ prop.type }}
  {% endfor -%}
  {% if cls.is_threat -%}
  severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
  {% endif -%}
  {% if cls.is_asset -%}
  status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
  {% endif -%}
}

{% endfor %}

export interface ThreatStats {
  total: number
  critical: number
  high: number
  medium: number
  low: number
  neutralized: number
}

export interface NetworkNode {
  id: string
  label: string
  type: string
  x?: number
  y?: number
  status: 'healthy' | 'warning' | 'critical' | 'offline'
}

export interface NetworkEdge {
  id: string
  source: string
  target: string
  type: string
  bandwidth?: number
  latency?: number
}

export interface AegisFabricMessage {
  type: string
  payload: any
  timestamp: number
}
'''

WEBSOCKET_API_TEMPLATE = '''// WebSocket API for Aegis Fabric real-time communication
// Generated on {{ generated_at }}

import type { Peer } from 'crossws'
import { defineWebSocketHandler } from 'h3'

// Import BitActor bridge (will be implemented)
// import { BitActorBridge } from '~/server/utils/bitactor-bridge'

interface AegisClient extends Peer {
  id: string
  subscriptions: Set<string>
}

const clients = new Map<string, AegisClient>()

export default defineWebSocketHandler({
  open(peer) {
    const client = peer as AegisClient
    client.id = Math.random().toString(36).substring(7)
    client.subscriptions = new Set()
    clients.set(client.id, client)
    
    console.log(`Client ${client.id} connected`)
    
    // Send initial state
    sendInitialState(client)
  },
  
  message(peer, message) {
    const client = peer as AegisClient
    const data = JSON.parse(message.text())
    
    console.log(`Message from ${client.id}:`, data)
    
    switch (data.type) {
      case 'subscribe':
        handleSubscribe(client, data.channels)
        break
        
      case 'unsubscribe':
        handleUnsubscribe(client, data.channels)
        break
        
      case 'neutralizeThreat':
        handleNeutralizeThreat(client, data.threatId)
        break
        
      default:
        console.warn('Unknown message type:', data.type)
    }
  },
  
  close(peer) {
    const client = peer as AegisClient
    clients.delete(client.id)
    console.log(`Client ${client.id} disconnected`)
  },
  
  error(peer, error) {
    console.error('WebSocket error:', error)
  }
})

function sendInitialState(client: AegisClient) {
  // Send current state for all entity types
  {% for cls in classes %}
  client.send(JSON.stringify({
    type: '{{ cls.name }}Update',
    payload: getMock{{ cls.name }}s() // TODO: Replace with real data from BitActor
  }))
  {% endfor %}
}

function handleSubscribe(client: AegisClient, channels: string[]) {
  channels.forEach(channel => {
    client.subscriptions.add(channel)
  })
}

function handleUnsubscribe(client: AegisClient, channels: string[]) {
  channels.forEach(channel => {
    client.subscriptions.delete(channel)
  })
}

function handleNeutralizeThreat(client: AegisClient, threatId: string) {
  // TODO: Send neutralization command to BitActor
  console.log(`Neutralizing threat ${threatId}`)
  
  // Broadcast update to all clients
  broadcast({
    type: 'threatNeutralized',
    threatId,
    timestamp: Date.now()
  })
}

function broadcast(data: any) {
  const message = JSON.stringify(data)
  clients.forEach(client => {
    client.send(message)
  })
}

// Mock data generators (replace with BitActor integration)
{% for cls in classes %}
function getMock{{ cls.name }}s() {
  return [] // TODO: Implement
}
{% endfor %}

// Start real-time update loop (100ms intervals for ultra-low latency)
setInterval(() => {
  // TODO: Get updates from BitActor and broadcast to clients
}, 100)
'''


@app.command()
def generate(
    ttl_file: str = typer.Argument(..., help="Path to TTL ontology file"),
    output_dir: str = typer.Argument(..., help="Output directory for Nuxt components"),
    template_dir: str = typer.Option("templates/nuxt", help="Template directory")
):
    """Generate Nuxt 3 components from TTL ontology"""
    
    generator = TTLToNuxtGenerator(template_dir)
    
    # Load TTL
    generator.load_ttl(ttl_file)
    
    # Extract classes
    classes = generator.extract_classes()
    
    # Show summary
    table = Table(title="Extracted Classes")
    table.add_column("Class", style="cyan")
    table.add_column("Type", style="green")
    table.add_column("Properties", style="yellow")
    
    for cls in classes:
        class_type = []
        if cls['is_threat']:
            class_type.append("Threat")
        if cls['is_asset']:
            class_type.append("Asset")
        if cls['is_network']:
            class_type.append("Network")
        
        table.add_row(
            cls['name'],
            ", ".join(class_type) or "Other",
            str(len(cls['properties']))
        )
    
    console.print(table)
    
    # Generate all components
    console.print("\n[bold]Generating Nuxt components...[/bold]")
    generator.generate_components(output_dir, classes)
    generator.generate_composables(output_dir, classes)
    generator.generate_types(output_dir, classes)
    generator.generate_api_routes(output_dir, classes)
    
    console.print(f"\n[green]✅ Successfully generated {len(classes)} components![/green]")
    console.print(f"[yellow]📁 Output directory: {output_dir}[/yellow]")


@app.command()
def create_templates(
    template_dir: str = typer.Option("templates/nuxt", help="Template directory")
):
    """Create default Nuxt template files"""
    
    template_path = Path(template_dir)
    template_path.mkdir(parents=True, exist_ok=True)
    
    # Create template files
    templates = {
        "threat-dashboard.vue.j2": THREAT_DASHBOARD_TEMPLATE,
        "asset-monitor.vue.j2": ASSET_MONITOR_TEMPLATE,
        "network-topology.vue.j2": NETWORK_TOPOLOGY_TEMPLATE,
        "class-component.vue.j2": CLASS_COMPONENT_TEMPLATE,
        "aegis-composable.ts.j2": AEGIS_COMPOSABLE_TEMPLATE,
        "types.ts.j2": TYPES_TEMPLATE,
        "websocket-api.ts.j2": WEBSOCKET_API_TEMPLATE
    }
    
    for filename, content in templates.items():
        file_path = template_path / filename
        file_path.write_text(content)
        console.print(f"[green]✅ Created {file_path}[/green]")
    
    console.print(f"\n[green]✅ Created {len(templates)} template files![/green]")


if __name__ == "__main__":
    app()