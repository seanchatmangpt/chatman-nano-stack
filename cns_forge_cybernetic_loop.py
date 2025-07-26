#!/usr/bin/env python3
"""
CNS Forge Cybernetic Loop Monitor
Real-time TTL stream processing for the 2026 vision
Observes system state and feeds the ecosystem composer
"""

import asyncio
import json
import time
from pathlib import Path
from typing import Dict, List, Any, AsyncGenerator
from dataclasses import dataclass, asdict
from datetime import datetime
from collections import deque
import typer
from rdflib import Graph, URIRef, Namespace
import websockets
import aiofiles
from rich.console import Console
from rich.live import Live
from rich.table import Table
from rich.panel import Panel

app = typer.Typer()
console = Console()

CNS = Namespace("http://cns.io/forge#")
AEGIS = Namespace("http://cns.io/aegis/fabric#")
BITACTOR = Namespace("http://cns.io/bitactor#")

@dataclass
class SystemState:
    """Real-time system state from TTL streams"""
    timestamp: datetime
    component_id: str
    component_type: str
    status: str
    metrics: Dict[str, Any]
    ttl_source: str
    performance_data: Dict[str, float]
    alerts: List[str]

@dataclass
class CyberneticObservation:
    """Structured observation for the Forge"""
    observation_id: str
    timestamp: datetime
    system_states: List[SystemState]
    emergent_properties: Dict[str, Any]
    anomalies: List[str]
    optimization_opportunities: List[str]

class CNSForgeCyberneticLoop:
    """
    The cybernetic heart of the CNS Forge
    Continuously observes, learns, and feeds the ecosystem composer
    """
    
    def __init__(self):
        self.active_observations = {}
        self.system_graph = Graph()
        self.observation_history = deque(maxlen=1000)  # Automatic memory management
        self.websocket_connections = set()
        self.ttl_graph_cache = {}  # Cache for parsed TTL graphs
        
    async def monitor_ttl_streams(self) -> AsyncGenerator[SystemState, None]:
        """Monitor continuous TTL streams from deployed systems"""
        console.print("[bold blue]🔄 Initiating TTL stream monitoring...[/bold blue]")
        
        # Monitor multiple TTL sources
        ttl_sources = [
            "bitactor_otp/src/bitactor_telemetry.ttl",
            "k8s/aegis-fabric-state.ttl", 
            "generated/system_runtime_state.ttl",
            "terraform/infrastructure_state.ttl"
        ]
        
        while True:
            for source in ttl_sources:
                try:
                    # Check if file exists asynchronously
                    source_path = Path(source)
                    if source_path.exists():
                        # Check if we need to reparse (file modification time)
                        current_mtime = source_path.stat().st_mtime
                        
                        if (source not in self.ttl_graph_cache or 
                            self.ttl_graph_cache[source]['mtime'] != current_mtime):
                            
                            # Parse TTL asynchronously
                            g = await self._parse_ttl_async(source)
                            if g:
                                self.ttl_graph_cache[source] = {
                                    'graph': g,
                                    'mtime': current_mtime
                                }
                        
                        # Use cached graph
                        cached_data = self.ttl_graph_cache.get(source)
                        if cached_data:
                            g = cached_data['graph']
                            
                            # Extract current system metrics
                            for subj, pred, obj in g:
                                if pred == CNS.hasMetric:
                                    state = await self.parse_system_state(subj, obj, g, source)
                                    if state:
                                        yield state
                                        
                except Exception as e:
                    console.print(f"[red]Error processing {source}: {e}[/red]")
            
            await asyncio.sleep(0.1)  # 100ms observation cycle
    
    async def _parse_ttl_async(self, source: str) -> Graph:
        """Parse TTL file asynchronously with error handling"""
        try:
            async with aiofiles.open(source, 'r') as f:
                ttl_content = await f.read()
            
            # Parse in executor to avoid blocking
            loop = asyncio.get_event_loop()
            g = Graph()
            await loop.run_in_executor(None, lambda: g.parse(data=ttl_content, format='turtle'))
            return g
            
        except Exception as e:
            console.print(f"[red]Failed to parse TTL {source}: {e}[/red]")
            return None
    
    async def parse_system_state(self, subject, metric_obj, graph, source) -> SystemState:
        """Parse TTL graph into structured system state"""
        try:
            # Extract component details
            component_id = str(subject).split('#')[-1]
            
            # Get component type
            component_type = "unknown"
            for s, p, o in graph.triples((subject, CNS.componentType, None)):
                component_type = str(o)
                break
            
            # Get status
            status = "unknown"
            for s, p, o in graph.triples((subject, CNS.status, None)):
                status = str(o)
                break
            
            # Extract metrics
            metrics = {}
            performance_data = {}
            for s, p, o in graph.triples((metric_obj, None, None)):
                prop_name = str(p).split('#')[-1]
                if prop_name in ['latency_ns', 'throughput_ops', 'memory_mb', 'cpu_percent']:
                    try:
                        performance_data[prop_name] = float(o)
                    except:
                        pass
                else:
                    metrics[prop_name] = str(o)
            
            # Check for alerts
            alerts = []
            for s, p, o in graph.triples((subject, CNS.alert, None)):
                alerts.append(str(o))
            
            return SystemState(
                timestamp=datetime.now(),
                component_id=component_id,
                component_type=component_type,
                status=status,
                metrics=metrics,
                ttl_source=source,
                performance_data=performance_data,
                alerts=alerts
            )
            
        except Exception as e:
            console.print(f"[red]Error parsing system state: {e}[/red]")
            return None
    
    async def analyze_emergent_properties(self, states: List[SystemState]) -> Dict[str, Any]:
        """Analyze emergent properties from system states"""
        if not states:
            return {}
        
        # Calculate system-wide metrics
        total_latency = sum(s.performance_data.get('latency_ns', 0) for s in states)
        avg_latency = total_latency / len(states) if states else 0
        
        total_throughput = sum(s.performance_data.get('throughput_ops', 0) for s in states)
        
        # Count component states
        status_counts = {}
        for state in states:
            status_counts[state.status] = status_counts.get(state.status, 0) + 1
        
        # System health score (0-100)
        healthy_ratio = status_counts.get('healthy', 0) / len(states) if states else 0
        health_score = healthy_ratio * 100
        
        return {
            'avg_latency_ns': avg_latency,
            'total_throughput_ops': total_throughput,
            'component_count': len(states),
            'status_distribution': status_counts,
            'system_health_score': health_score,
            'alert_count': sum(len(s.alerts) for s in states)
        }
    
    async def detect_anomalies(self, states: List[SystemState]) -> List[str]:
        """Detect system anomalies requiring attention"""
        anomalies = []
        
        # Check for performance degradation
        for state in states:
            latency = state.performance_data.get('latency_ns', 0)
            if latency > 100000:  # > 100μs
                anomalies.append(f"High latency in {state.component_id}: {latency}ns")
            
            if state.status in ['failed', 'degraded', 'unhealthy']:
                anomalies.append(f"Component {state.component_id} status: {state.status}")
            
            if state.alerts:
                for alert in state.alerts:
                    anomalies.append(f"Alert from {state.component_id}: {alert}")
        
        return anomalies
    
    async def identify_optimizations(self, states: List[SystemState]) -> List[str]:
        """Identify optimization opportunities"""
        optimizations = []
        
        # Check for underutilized resources
        for state in states:
            cpu = state.performance_data.get('cpu_percent', 0)
            memory = state.performance_data.get('memory_mb', 0)
            
            if cpu < 10:
                optimizations.append(f"Underutilized CPU in {state.component_id}: {cpu}%")
            
            if cpu > 80:
                optimizations.append(f"Scale up {state.component_id} - CPU at {cpu}%")
        
        return optimizations
    
    async def generate_observation(self, states: List[SystemState]) -> CyberneticObservation:
        """Generate structured observation for the Forge"""
        observation_id = f"obs_{int(time.time() * 1000)}"
        
        emergent_properties = await self.analyze_emergent_properties(states)
        anomalies = await self.detect_anomalies(states)
        optimizations = await self.identify_optimizations(states)
        
        observation = CyberneticObservation(
            observation_id=observation_id,
            timestamp=datetime.now(),
            system_states=states,
            emergent_properties=emergent_properties,
            anomalies=anomalies,
            optimization_opportunities=optimizations
        )
        
        # Store observation (deque automatically manages size)
        self.observation_history.append(observation)
        
        return observation
    
    async def stream_to_forge(self, observation: CyberneticObservation):
        """Stream observation to CNS Forge ecosystem composer"""
        observation_data = {
            'type': 'cybernetic_observation',
            'data': asdict(observation),
            'timestamp': observation.timestamp.isoformat()
        }
        
        # Stream to all connected WebSocket clients
        if self.websocket_connections:
            message = json.dumps(observation_data, default=str)
            disconnected = set()
            
            for websocket in self.websocket_connections:
                try:
                    await websocket.send(message)
                except websockets.exceptions.ConnectionClosed:
                    disconnected.add(websocket)
            
            # Remove disconnected clients
            self.websocket_connections -= disconnected
        
        # Also save to file for batch processing
        async with aiofiles.open('cns_forge_observations.jsonl', 'a') as f:
            await f.write(json.dumps(observation_data, default=str) + '\\n')
    
    async def websocket_handler(self, websocket, path):
        """Handle WebSocket connections from Forge components"""
        self.websocket_connections.add(websocket)
        console.print(f"[green]🔌 Forge component connected: {websocket.remote_address}[/green]")
        
        try:
            await websocket.wait_closed()
        finally:
            self.websocket_connections.discard(websocket)
            console.print(f"[yellow]🔌 Forge component disconnected[/yellow]")
    
    async def run_cybernetic_loop(self):
        """Main cybernetic loop - observe, analyze, stream"""
        console.print("[bold green]🌟 CNS Forge Cybernetic Loop ACTIVATED[/bold green]")
        
        # Start WebSocket server for Forge communication
        websocket_server = await websockets.serve(
            self.websocket_handler, 
            "localhost", 
            8083
        )
        
        # Buffer for collecting states
        state_buffer = []
        last_observation_time = time.time()
        
        try:
            async for state in self.monitor_ttl_streams():
                state_buffer.append(state)
                
                # Generate observation every 1 second or when buffer is full
                current_time = time.time()
                if (current_time - last_observation_time >= 1.0) or len(state_buffer) >= 50:
                    if state_buffer:
                        observation = await self.generate_observation(state_buffer)
                        await self.stream_to_forge(observation)
                        
                        # Clear buffer
                        state_buffer = []
                        last_observation_time = current_time
                        
                        # Live display
                        self.display_observation(observation)
        
        except KeyboardInterrupt:
            console.print("\\n[yellow]Shutting down cybernetic loop...[/yellow]")
        finally:
            websocket_server.close()
            await websocket_server.wait_closed()
    
    def display_observation(self, observation: CyberneticObservation):
        """Display current observation in rich format"""
        table = Table(title="CNS Forge Cybernetic Observation")
        table.add_column("Property", style="cyan")
        table.add_column("Value", style="green")
        
        props = observation.emergent_properties
        table.add_row("Observation ID", observation.observation_id)
        table.add_row("Component Count", str(props.get('component_count', 0)))
        table.add_row("Avg Latency", f"{props.get('avg_latency_ns', 0):.0f}ns")
        table.add_row("Total Throughput", f"{props.get('total_throughput_ops', 0):.0f} ops/s")
        table.add_row("Health Score", f"{props.get('system_health_score', 0):.1f}%")
        table.add_row("Anomalies", str(len(observation.anomalies)))
        table.add_row("Optimizations", str(len(observation.optimization_opportunities)))
        
        console.print(table)
        
        if observation.anomalies:
            console.print(Panel("\\n".join(observation.anomalies[:3]), title="🚨 Anomalies", border_style="red"))
        
        if observation.optimization_opportunities:
            console.print(Panel("\\n".join(observation.optimization_opportunities[:3]), title="⚡ Optimizations", border_style="yellow"))

@app.command()
def monitor():
    """Start the CNS Forge cybernetic loop monitor"""
    loop = CNSForgeCyberneticLoop()
    asyncio.run(loop.run_cybernetic_loop())

@app.command()
def status():
    """Show current cybernetic loop status"""
    console.print("[bold blue]CNS Forge Cybernetic Loop Status[/bold blue]")
    
    # Check for recent observations
    if Path('cns_forge_observations.jsonl').exists():
        with open('cns_forge_observations.jsonl', 'r') as f:
            lines = f.readlines()
            console.print(f"Total observations recorded: {len(lines)}")
            if lines:
                last_obs = json.loads(lines[-1])
                console.print(f"Last observation: {last_obs['timestamp']}")
    else:
        console.print("No observations recorded yet")

if __name__ == "__main__":
    app()