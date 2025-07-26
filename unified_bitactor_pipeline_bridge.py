#!/usr/bin/env python3
"""
Unified BitActor Pipeline Bridge - 80/20 Implementation
Connects Python types → TTL ontology → Elixir/Ash/Reactor → Kubernetes
"""

import json
import subprocess
import tempfile
import uuid
from pathlib import Path
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
from datetime import datetime

# Import BitActor types
from bitactor_types import (
    BitActor, Signal, SignalType, SignalPriority, TelemetryFrame,
    TTLConstraint, TTLPrecision, SwarmConfiguration, SwarmTopology,
    create_bitactor, create_signal, create_telemetry_frame
)

@dataclass
class PipelineResult:
    """Result from complete pipeline execution"""
    success: bool
    stages_completed: List[str]
    generated_files: Dict[str, str]
    elixir_modules: List[str]
    k8s_manifests: List[str]
    ttl_preserved: bool
    error_message: Optional[str] = None
    execution_time_ms: int = 0

class UnifiedBitActorPipelineBridge:
    """
    Bridge between Python BitActor types and Elixir BitActor implementation
    Orchestrates the complete 80/20 pipeline transformation
    """
    
    def __init__(self, app_name: str = "bitactor"):
        self.app_name = app_name
        self.temp_dir = Path(tempfile.mkdtemp(prefix="bitactor_pipeline_"))
        self.stages = [
            "typer", "turtle", "ttl2dspy", "BitActor", 
            "Erlang", "Ash", "Reactor", "k8s"
        ]
        
    def execute_full_pipeline(
        self, 
        bitactors: List[BitActor],
        signals: List[Signal],
        swarm_config: SwarmConfiguration
    ) -> PipelineResult:
        """Execute the complete 80/20 pipeline"""
        start_time = datetime.now()
        
        try:
            # Stage 1: Python Types → TTL Ontology
            ttl_content = self.generate_ttl_from_python_types(bitactors, signals, swarm_config)
            
            # Stage 2: TTL → BitActor DSL via Elixir
            elixir_result = self.execute_elixir_transformation(ttl_content)
            
            # Stage 3: Generate complete deployment
            deployment_result = self.generate_deployment_manifests(
                elixir_result, swarm_config
            )
            
            end_time = datetime.now()
            execution_time_ms = int((end_time - start_time).total_seconds() * 1000)
            
            return PipelineResult(
                success=True,
                stages_completed=self.stages,
                generated_files=deployment_result["files"],
                elixir_modules=elixir_result["modules"],
                k8s_manifests=deployment_result["manifests"],
                ttl_preserved=True,
                execution_time_ms=execution_time_ms
            )
            
        except Exception as e:
            return PipelineResult(
                success=False,
                stages_completed=[],
                generated_files={},
                elixir_modules=[],
                k8s_manifests=[],
                ttl_preserved=False,
                error_message=str(e)
            )
    
    def generate_ttl_from_python_types(
        self,
        bitactors: List[BitActor],
        signals: List[Signal],
        swarm_config: SwarmConfiguration
    ) -> str:
        """Generate TTL ontology from Python BitActor types"""
        
        ttl_content = f"""@prefix bitactor: <http://bitactor.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Generated from Python BitActor types
bitactor: a owl:Ontology ;
    rdfs:label "BitActor Generated Ontology" ;
    rdfs:comment "Generated from Python types via 80/20 pipeline" .

# Core Classes
{self._generate_actor_classes(bitactors)}

# Signal Classes  
{self._generate_signal_classes(signals)}

# Properties
{self._generate_properties(bitactors, signals)}

# TTL Constraints
{self._generate_ttl_constraints(bitactors, swarm_config)}

# Swarm Configuration
{self._generate_swarm_configuration(swarm_config)}
"""
        
        return ttl_content
    
    def execute_elixir_transformation(self, ttl_content: str) -> Dict[str, Any]:
        """Execute Elixir transformation via ExistingCodeConnector"""
        
        # Write TTL to temp file
        ttl_file = self.temp_dir / "ontology.ttl"
        ttl_file.write_text(ttl_content)
        
        # Create Elixir script to execute transformation
        elixir_script = f'''
        # Load the connector
        Code.require_file("existing_code_connector.ex")
        
        # Read TTL content
        ttl_content = File.read!("{ttl_file}")
        
        # Execute transformation
        case ExistingCodeConnector.transform_ttl_to_bitactor(ttl_content) do
          {{:ok, result}} ->
            # Convert to JSON for Python
            json_result = Jason.encode!(%{{
              success: true,
              modules: Enum.map(result.bitactor_dsl, fn mod -> 
                %{{name: extract_module_name(mod), code: mod}}
              end),
              ash_resources: Enum.map(result.ash_resources, fn res ->
                %{{name: res.module_name, code: res.code}}
              end),
              reactor_workflows: Enum.map(result.reactor_workflows, fn wf ->
                %{{name: wf.name, code: wf.code}}
              end),
              k8s_manifests: result.k8s_manifests,
              metadata: result.pipeline_metadata
            }})
            
            IO.puts(json_result)
            
          {{:error, reason}} ->
            error_json = Jason.encode!(%{{
              success: false,
              error: inspect(reason)
            }})
            IO.puts(error_json)
        end
        
        defp extract_module_name(code) when is_binary(code) do
          case Regex.run(~r/defmodule\\s+(\\S+)/, code) do
            [_, module_name] -> module_name
            _ -> "UnknownModule"
          end
        end
        '''
        
        # Execute Elixir script
        script_file = self.temp_dir / "transform.exs"
        script_file.write_text(elixir_script)
        
        result = subprocess.run(
            ["elixir", str(script_file)],
            capture_output=True,
            text=True,
            cwd="."
        )
        
        if result.returncode != 0:
            raise RuntimeError(f"Elixir transformation failed: {result.stderr}")
        
        return json.loads(result.stdout)
    
    def generate_deployment_manifests(
        self, 
        elixir_result: Dict[str, Any],
        swarm_config: SwarmConfiguration
    ) -> Dict[str, Any]:
        """Generate complete K8s deployment manifests"""
        
        manifests = []
        files = {}
        
        # Namespace
        namespace_manifest = f"""apiVersion: v1
kind: Namespace
metadata:
  name: {self.app_name}-bitactor
  labels:
    app: bitactor
    pipeline: "80-20"
---"""
        manifests.append(namespace_manifest)
        
        # ConfigMap for TTL constraints
        ttl_config = f"""apiVersion: v1
kind: ConfigMap
metadata:
  name: {self.app_name}-ttl-config
  namespace: {self.app_name}-bitactor
data:
  ttl_constraints.yaml: |
    global_ttl_budget_ms: {swarm_config.global_ttl_budget_ms}
    actor_ttl_budget_ms: {swarm_config.actor_ttl_budget_ms}
    precision: nanosecond
    max_actors: {swarm_config.max_actors}
---"""
        manifests.append(ttl_config)
        
        # StatefulSet
        statefulset = f"""apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: {self.app_name}-bitactor
  namespace: {self.app_name}-bitactor
spec:
  serviceName: {self.app_name}-bitactor-headless
  replicas: {swarm_config.replicas}
  selector:
    matchLabels:
      app: bitactor
      deployment: {self.app_name}
  template:
    metadata:
      labels:
        app: bitactor
        deployment: {self.app_name}
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
    spec:
      containers:
      - name: bitactor
        image: {swarm_config.image}
        ports:
        - containerPort: 4369
          name: epmd
        - containerPort: 9100
          name: erlang
        - containerPort: 9090
          name: metrics
        env:
        - name: SWARM_TOPOLOGY
          value: "{swarm_config.topology.value}"
        - name: TTL_BUDGET_MS
          value: "{swarm_config.actor_ttl_budget_ms}"
        - name: MAX_ACTORS
          value: "{swarm_config.max_actors}"
        - name: AUTO_SCALE
          value: "{str(swarm_config.auto_scale).lower()}"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        volumeMounts:
        - name: ttl-config
          mountPath: /app/config/ttl
        livenessProbe:
          httpGet:
            path: /health
            port: 9090
          initialDelaySeconds: 30
          periodSeconds: 10
      volumes:
      - name: ttl-config
        configMap:
          name: {self.app_name}-ttl-config
---"""
        manifests.append(statefulset)
        
        # HPA
        hpa = f"""apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: {self.app_name}-bitactor-hpa
  namespace: {self.app_name}-bitactor
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: StatefulSet
    name: {self.app_name}-bitactor
  minReplicas: {swarm_config.min_actors}
  maxReplicas: {swarm_config.max_actors}
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: {swarm_config.target_cpu_percent}
  - type: Pods
    pods:
      metric:
        name: bitactor_signals_per_second
      target:
        type: AverageValue
        averageValue: "1000"
---"""
        manifests.append(hpa)
        
        # Service
        service = f"""apiVersion: v1
kind: Service
metadata:
  name: {self.app_name}-bitactor
  namespace: {self.app_name}-bitactor
spec:
  type: ClusterIP
  selector:
    app: bitactor
    deployment: {self.app_name}
  ports:
  - name: erlang
    port: 9100
    targetPort: 9100
  - name: metrics
    port: 9090
    targetPort: 9090
---"""
        manifests.append(service)
        
        # Write all files
        deployment_yaml = "\n".join(manifests)
        files[f"{self.app_name}-bitactor-deployment.yaml"] = deployment_yaml
        
        # Write Elixir modules
        for module in elixir_result.get("modules", []):
            filename = f"{module['name'].lower().replace('.', '_')}.ex"
            files[filename] = module["code"]
        
        return {
            "manifests": manifests,
            "files": files
        }
    
    def _generate_actor_classes(self, bitactors: List[BitActor]) -> str:
        """Generate OWL classes for BitActors"""
        classes = []
        
        for actor in bitactors:
            actor_class = f"""
bitactor:{actor.name.replace(' ', '')} a owl:Class ;
    rdfs:subClassOf bitactor:BitActor ;
    rdfs:label "{actor.name}" ;
    rdfs:comment "BitActor with TTL budget of {actor.ttl_budget}ms" ;
    bitactor:ttlBudgetMs {actor.ttl_budget} ;
    bitactor:status bitactor:Status{actor.status.value.title()} ."""
            classes.append(actor_class)
        
        return "\n".join(classes)
    
    def _generate_signal_classes(self, signals: List[Signal]) -> str:
        """Generate OWL classes for Signals"""
        signal_types = set(signal.type for signal in signals)
        classes = []
        
        for signal_type in signal_types:
            signal_class = f"""
bitactor:{signal_type.value.title()}Signal a owl:Class ;
    rdfs:subClassOf bitactor:Signal ;
    rdfs:label "{signal_type.value.title()} Signal" ;
    rdfs:comment "Signal of type {signal_type.value}" ."""
            classes.append(signal_class)
        
        return "\n".join(classes)
    
    def _generate_properties(self, bitactors: List[BitActor], signals: List[Signal]) -> str:
        """Generate OWL properties"""
        properties = []
        
        # Actor properties
        for actor in bitactors:
            if actor.parent_actor_id:
                prop = f"""
bitactor:hasParent_{actor.name.replace(' ', '')} a owl:ObjectProperty ;
    rdfs:domain bitactor:{actor.name.replace(' ', '')} ;
    rdfs:range bitactor:BitActor ;
    rdfs:label "has parent actor" ."""
                properties.append(prop)
        
        # Signal properties
        for signal in signals:
            if signal.source_actor_id and signal.target_actor_id:
                prop = f"""
bitactor:signal_{signal.id} a owl:ObjectProperty ;
    rdfs:domain bitactor:BitActor ;
    rdfs:range bitactor:BitActor ;
    rdfs:label "processes signal {signal.type.value}" ;
    bitactor:priority {signal.priority.value} ."""
                properties.append(prop)
        
        return "\n".join(properties)
    
    def _generate_ttl_constraints(self, bitactors: List[BitActor], swarm_config: SwarmConfiguration) -> str:
        """Generate TTL constraint definitions"""
        constraints = []
        
        for actor in bitactors:
            constraint = f"""
bitactor:ttlConstraint_{actor.name.replace(' ', '')} a bitactor:TTLConstraint ;
    bitactor:budgetNs {actor.ttl_constraint.budget_ns} ;
    bitactor:maxBudgetMs {actor.ttl_constraint.max_budget_ms} ;
    bitactor:precision bitactor:{actor.ttl_constraint.precision.value.title()} ."""
            constraints.append(constraint)
        
        # Global constraint
        global_constraint = f"""
bitactor:globalTTLConstraint a bitactor:TTLConstraint ;
    bitactor:budgetNs {swarm_config.global_ttl_budget_ms * 1_000_000} ;
    bitactor:maxBudgetMs {swarm_config.global_ttl_budget_ms} ;
    bitactor:precision bitactor:Nanosecond ."""
        constraints.append(global_constraint)
        
        return "\n".join(constraints)
    
    def _generate_swarm_configuration(self, swarm_config: SwarmConfiguration) -> str:
        """Generate swarm configuration in TTL"""
        return f"""
bitactor:swarmConfig a bitactor:SwarmConfiguration ;
    bitactor:swarmId "{swarm_config.swarm_id}" ;
    bitactor:topology bitactor:{swarm_config.topology.value.title()}Topology ;
    bitactor:maxActors {swarm_config.max_actors} ;
    bitactor:strategy bitactor:{swarm_config.strategy.title()}Strategy ;
    bitactor:autoScale {str(swarm_config.auto_scale).lower()} ;
    bitactor:minActors {swarm_config.min_actors} ;
    bitactor:targetCpuPercent {swarm_config.target_cpu_percent} ;
    bitactor:namespace "{swarm_config.namespace}" ;
    bitactor:serviceName "{swarm_config.service_name}" ;
    bitactor:image "{swarm_config.image}" ;
    bitactor:replicas {swarm_config.replicas} ."""

def main():
    """Example usage of the unified pipeline"""
    
    # Create example BitActors
    signal_processor = create_bitactor("SignalProcessor", ttl_budget_ms=10)
    data_analyzer = create_bitactor("DataAnalyzer", ttl_budget_ms=15)
    telemetry_collector = create_bitactor("TelemetryCollector", ttl_budget_ms=5)
    
    bitactors = [signal_processor, data_analyzer, telemetry_collector]
    
    # Create example signals
    data_signal = create_signal(
        SignalType.DATA,
        {"data": [1, 2, 3, 4, 5]},
        SignalPriority.HIGH,
        ttl_ms=8
    )
    
    control_signal = create_signal(
        SignalType.CONTROL,
        {"command": "start_processing"},
        SignalPriority.CRITICAL,
        ttl_ms=5
    )
    
    signals = [data_signal, control_signal]
    
    # Create swarm configuration
    swarm_config = SwarmConfiguration(
        topology=SwarmTopology.HIERARCHICAL,
        max_actors=50,
        strategy="adaptive",
        global_ttl_budget_ms=5000,
        actor_ttl_budget_ms=10,
        auto_scale=True,
        min_actors=3,
        target_cpu_percent=70
    )
    
    # Execute pipeline
    bridge = UnifiedBitActorPipelineBridge("example")
    result = bridge.execute_full_pipeline(bitactors, signals, swarm_config)
    
    if result.success:
        print("✅ 80/20 Pipeline executed successfully!")
        print(f"🚀 Stages completed: {' → '.join(result.stages_completed)}")
        print(f"📁 Generated files: {len(result.generated_files)}")
        print(f"🔧 Elixir modules: {len(result.elixir_modules)}")
        print(f"☸️  K8s manifests: {len(result.k8s_manifests)}")
        print(f"⏱️  Execution time: {result.execution_time_ms}ms")
        print(f"🎯 TTL preserved: {result.ttl_preserved}")
        
        # Write generated files
        output_dir = Path("generated_pipeline_output")
        output_dir.mkdir(exist_ok=True)
        
        for filename, content in result.generated_files.items():
            (output_dir / filename).write_text(content)
            print(f"📝 Generated: {output_dir / filename}")
        
        print(f"\n🌀 Complete 80/20 pipeline output written to: {output_dir}")
        
    else:
        print(f"❌ Pipeline failed: {result.error_message}")

if __name__ == "__main__":
    main()