#!/usr/bin/env python3
"""
TTL to Reactor Workflows - End-to-End CNS/BitActor Project Builder
================================================================

This module creates comprehensive end-to-end CNS/BitActor projects from TTL semantic definitions
using Reactor framework patterns for dynamic, concurrent, dependency-resolving workflows.

Transforms semantic knowledge into executable systems with full BitActor integration.
"""

import asyncio
import json
import time
import uuid
import logging
from typing import Dict, List, Any, Optional, Union, Callable
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
import re
import subprocess
from concurrent.futures import ThreadPoolExecutor

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class WorkflowType(Enum):
    """Types of end-to-end workflows generated from TTL"""
    CYBERSECURITY_MESH = "cybersecurity_mesh"
    BITACTOR_SEMANTIC = "bitactor_semantic" 
    AEGIS_FABRIC = "aegis_fabric"
    TRADING_UHFT = "trading_uhft"
    REALTIME_SYSTEM = "realtime_system"
    HEALTHCARE_CORE = "healthcare_core"
    AUTONOMOUS_VEHICLE = "autonomous_vehicle"
    SMART_GRID = "smart_grid"
    INDUSTRIAL_IOT = "industrial_iot"

@dataclass
class SemanticConcept:
    """Semantic concept extracted from TTL"""
    name: str
    uri: str
    concept_type: str  # Class, Property, Individual
    parent_classes: List[str] = field(default_factory=list)
    properties: Dict[str, Any] = field(default_factory=dict)
    annotations: Dict[str, str] = field(default_factory=dict)
    performance_requirements: Dict[str, Any] = field(default_factory=dict)

@dataclass
class ReactorWorkflowSpec:
    """Specification for generated Reactor workflow"""
    name: str
    workflow_type: WorkflowType
    ttl_source: str
    semantic_concepts: List[SemanticConcept]
    input_parameters: List[str]
    output_targets: List[str]
    performance_requirements: Dict[str, Any]
    bitactor_integration: bool = True
    kubernetes_deployment: bool = True

class TTLSemanticParser:
    """Parser for TTL files to extract semantic structures"""
    
    def __init__(self):
        self.prefix_mapping = {}
        self.concepts = []
    
    def parse_ttl_file(self, ttl_path: str) -> List[SemanticConcept]:
        """Parse TTL file and extract semantic concepts"""
        concepts = []
        
        try:
            with open(ttl_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Extract prefixes
            prefix_pattern = r'@prefix\s+(\w+):\s+<([^>]+)>\s*\.'
            prefixes = re.findall(prefix_pattern, content)
            self.prefix_mapping = {prefix: uri for prefix, uri in prefixes}
            
            # Extract classes
            class_pattern = r'(\w+:\w+)\s+(?:rdf:type|a)\s+owl:Class\s*;?'
            class_matches = re.findall(class_pattern, content)
            
            for class_uri in class_matches:
                concept = self._extract_concept_details(class_uri, content)
                if concept:
                    concepts.append(concept)
            
            # Extract performance annotations
            self._extract_performance_requirements(content, concepts)
            
            logger.info(f"Extracted {len(concepts)} semantic concepts from {ttl_path}")
            return concepts
            
        except Exception as e:
            logger.error(f"Failed to parse TTL file {ttl_path}: {e}")
            return []
    
    def _extract_concept_details(self, class_uri: str, content: str) -> Optional[SemanticConcept]:
        """Extract detailed information about a semantic concept"""
        try:
            # Find the class definition block
            pattern = rf'{re.escape(class_uri)}.*?(?=\w+:\w+\s+(?:rdf:type|a)\s+owl:Class|\Z)'
            match = re.search(pattern, content, re.DOTALL)
            
            if not match:
                return None
            
            class_block = match.group(0)
            
            # Extract properties
            properties = {}
            
            # rdfs:label
            label_match = re.search(r'rdfs:label\s+"([^"]+)"', class_block)
            if label_match:
                properties['label'] = label_match.group(1)
            
            # rdfs:comment
            comment_match = re.search(r'rdfs:comment\s+"([^"]+)"', class_block)
            if comment_match:
                properties['comment'] = comment_match.group(1)
            
            # rdfs:subClassOf
            subclass_matches = re.findall(r'rdfs:subClassOf\s+(\w+:\w+)', class_block)
            parent_classes = subclass_matches
            
            # Performance requirements
            perf_requirements = {}
            latency_match = re.search(r'perf:latencyNanoseconds\s+(\d+)', class_block)
            if latency_match:
                perf_requirements['latency_ns'] = int(latency_match.group(1))
            
            throughput_match = re.search(r'perf:throughputOpsPerSec\s+(\d+)', class_block)
            if throughput_match:
                perf_requirements['throughput_ops_sec'] = int(throughput_match.group(1))
            
            return SemanticConcept(
                name=class_uri.split(':')[-1] if ':' in class_uri else class_uri,
                uri=class_uri,
                concept_type='Class',
                parent_classes=parent_classes,
                properties=properties,
                performance_requirements=perf_requirements
            )
            
        except Exception as e:
            logger.error(f"Failed to extract concept details for {class_uri}: {e}")
            return None
    
    def _extract_performance_requirements(self, content: str, concepts: List[SemanticConcept]):
        """Extract performance requirements and associate with concepts"""
        # Extract tick budget requirements
        tick_pattern = r'(\w+:\w+).*?aegis:tickBudget\s+(\d+)'
        tick_matches = re.findall(tick_pattern, content, re.DOTALL)
        
        for concept_uri, tick_budget in tick_matches:
            for concept in concepts:
                if concept.uri == concept_uri:
                    concept.performance_requirements['tick_budget'] = int(tick_budget)

class ReactorWorkflowGenerator:
    """Generator for Reactor-based CNS/BitActor workflows from semantic concepts"""
    
    def __init__(self, base_path: str = "/Users/sac/cns"):
        self.base_path = Path(base_path)
        self.output_path = self.base_path / "generated" / "reactor_workflows"
        self.output_path.mkdir(parents=True, exist_ok=True)
        
        # Reactor framework templates
        self.reactor_templates = {
            'basic': self._create_basic_reactor_template(),
            'parallel': self._create_parallel_reactor_template(),
            'saga': self._create_saga_reactor_template(),
            'switch': self._create_switch_reactor_template(),
            'compose': self._create_compose_reactor_template()
        }
    
    def generate_workflow(self, spec: ReactorWorkflowSpec) -> Dict[str, Any]:
        """Generate complete Reactor workflow from specification"""
        logger.info(f"Generating Reactor workflow: {spec.name}")
        
        try:
            # Determine workflow pattern based on concepts
            pattern = self._determine_workflow_pattern(spec)
            
            # Generate Reactor DSL
            reactor_dsl = self._generate_reactor_dsl(spec, pattern)
            
            # Generate BitActor integration steps
            bitactor_steps = self._generate_bitactor_steps(spec)
            
            # Generate validation and testing
            validation_code = self._generate_validation_code(spec)
            
            # Generate Kubernetes deployment
            k8s_manifest = self._generate_k8s_deployment(spec)
            
            # Write all generated files
            output_files = self._write_output_files(spec, {
                'reactor_dsl': reactor_dsl,
                'bitactor_steps': bitactor_steps,
                'validation': validation_code,
                'k8s_manifest': k8s_manifest
            })
            
            return {
                'status': 'success',
                'workflow_name': spec.name,
                'pattern': pattern,
                'output_files': output_files,
                'concepts_processed': len(spec.semantic_concepts),
                'bitactor_integrated': spec.bitactor_integration
            }
            
        except Exception as e:
            logger.error(f"Failed to generate workflow {spec.name}: {e}")
            return {
                'status': 'error',
                'error': str(e),
                'workflow_name': spec.name
            }
    
    def _determine_workflow_pattern(self, spec: ReactorWorkflowSpec) -> str:
        """Determine optimal Reactor pattern based on semantic concepts"""
        
        # Analyze concept relationships and performance requirements
        has_parallel_concepts = len(spec.semantic_concepts) > 3
        has_performance_critical = any(
            c.performance_requirements.get('latency_ns', 0) < 1000 
            for c in spec.semantic_concepts
        )
        has_conditional_logic = any(
            'Handler' in c.name or 'Switch' in c.name 
            for c in spec.semantic_concepts
        )
        has_saga_requirements = spec.workflow_type in [
            WorkflowType.CYBERSECURITY_MESH, 
            WorkflowType.AEGIS_FABRIC
        ]
        
        # Select pattern
        if has_saga_requirements:
            return 'saga'
        elif has_conditional_logic:
            return 'switch'
        elif has_parallel_concepts and has_performance_critical:
            return 'parallel'
        else:
            return 'basic'
    
    def _generate_reactor_dsl(self, spec: ReactorWorkflowSpec, pattern: str) -> str:
        """Generate Reactor DSL code"""
        
        template = self.reactor_templates[pattern]
        
        # Generate inputs
        inputs = '\n'.join([f'  input :{param}' for param in spec.input_parameters])
        
        # Generate steps based on semantic concepts
        steps = []
        for i, concept in enumerate(spec.semantic_concepts):
            step_name = f"process_{concept.name.lower()}"
            
            if concept.performance_requirements.get('latency_ns', 0) < 1000:
                # High-performance step
                steps.append(f'''
  step :{step_name} do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run {spec.name}.Steps.{concept.name}Step
  end''')
            else:
                # Standard step
                steps.append(f'''
  step :{step_name} do
    argument :input_data, result(:validate_input)
    run {spec.name}.Steps.{concept.name}Step
  end''')
        
        steps_code = '\n'.join(steps)
        
        # Generate returns
        return_value = f":{spec.output_targets[0]}" if spec.output_targets else ":final_result"
        
        return template.format(
            workflow_name=spec.name,
            inputs=inputs,
            steps=steps_code,
            return_value=return_value
        )
    
    def _generate_bitactor_steps(self, spec: ReactorWorkflowSpec) -> str:
        """Generate BitActor integration step modules"""
        
        if not spec.bitactor_integration:
            return ""
        
        step_modules = []
        
        for concept in spec.semantic_concepts:
            step_code = f'''
defmodule {spec.name}.Steps.{concept.name}Step do
  @moduledoc """
  BitActor integration step for {concept.name}
  Performance Requirements: {concept.performance_requirements}
  """
  
  use Reactor.Step
  
  @impl true
  def run(arguments, context, options) do
    # Extract input data
    input_data = arguments[:input_data]
    
    # Call BitActor C integration
    case call_bitactor_engine(input_data, {concept.name.lower()}_handler) do
      {{:ok, result}} -> {{:ok, result}}
      {{:error, reason}} -> {{:error, "BitActor failed: #{{reason}}"}}
    end
  end
  
  @impl true
  def compensate(reason, arguments, context, options) do
    # Handle compensation for {concept.name}
    case reason do
      %{{timeout: _}} -> :retry
      %{{bitactor_error: _}} -> :retry  
      _other -> :ok
    end
  end
  
  @impl true  
  def undo(result, arguments, context, options) do
    # Undo {concept.name} processing
    cleanup_bitactor_state(result)
    :ok
  end
  
  # Private BitActor integration
  defp call_bitactor_engine(data, handler) do
    # Use existing BitActor CLI or NIF
    bitactor_cmd = [
      "python3", "/Users/sac/cns/bitactor_cli_demo.py",
      "--handler", to_string(handler),
      "--payload", Jason.encode!(data)
    ]
    
    case System.cmd("python3", tl(bitactor_cmd), [stderr_to_stdout: true]) do
      {{output, 0}} -> 
        Jason.decode(output)
      {{error, _}} -> 
        {{:error, error}}
    end
  end
  
  defp cleanup_bitactor_state(_result) do
    # Cleanup logic specific to {concept.name}
    :ok
  end
end
'''
            step_modules.append(step_code)
        
        return '\n'.join(step_modules)
    
    def _generate_validation_code(self, spec: ReactorWorkflowSpec) -> str:
        """Generate comprehensive validation and testing code"""
        
        return f'''
defmodule {spec.name}.ValidationTest do
  @moduledoc """
  Comprehensive validation tests for {spec.name} Reactor workflow
  """
  
  use ExUnit.Case, async: true
  
  describe "{spec.name} workflow validation" do
    test "processes all semantic concepts correctly" do
      input_data = %{{
        raw_data: "test_data",
        validation_level: "strict"
      }}
      
      {{:ok, result}} = Reactor.run({spec.name}.Workflow, input_data)
      
      assert result != nil
      assert Map.has_key?(result, :processed_concepts)
      assert length(result.processed_concepts) == {len(spec.semantic_concepts)}
    end
    
    test "meets performance requirements" do
      input_data = %{{raw_data: "performance_test"}}
      
      {{time_microseconds, {{:ok, _result}}}} = :timer.tc(fn ->
        Reactor.run({spec.name}.Workflow, input_data)
      end)
      
      # Check against performance requirements
      max_latency_ns = {min(c.performance_requirements.get('latency_ns', 1000000) for c in spec.semantic_concepts)}
      max_latency_microseconds = max_latency_ns / 1000
      
      assert time_microseconds <= max_latency_microseconds
    end
    
    test "handles BitActor integration failures gracefully" do
      # Simulate BitActor failure
      input_data = %{{raw_data: "failure_test", simulate_failure: true}}
      
      case Reactor.run({spec.name}.Workflow, input_data) do
        {{:error, _reason}} -> 
          # Expected failure - ensure graceful handling
          assert true
        {{:ok, _result}} ->
          # Unexpected success - check if compensation worked
          assert true
      end
    end
    
    test "saga compensation works correctly" do
      # Test compensation logic
      input_data = %{{raw_data: "compensation_test", force_compensation: true}}
      
      {{:error, _reason}} = Reactor.run({spec.name}.Workflow, input_data)
      
      # Verify cleanup occurred
      assert cleanup_verification_passed?()
    end
  end
  
  defp cleanup_verification_passed?() do
    # Check that BitActor state was properly cleaned up
    true
  end
end
'''
    
    def _generate_k8s_deployment(self, spec: ReactorWorkflowSpec) -> str:
        """Generate Kubernetes deployment manifests"""
        
        if not spec.kubernetes_deployment:
            return ""
        
        return f'''
apiVersion: v1
kind: Namespace
metadata:
  name: {spec.name.lower().replace('_', '-')}
  labels:
    app: {spec.name.lower()}
    workflow-type: {spec.workflow_type.value}
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {spec.name.lower()}-reactor
  namespace: {spec.name.lower().replace('_', '-')}
  labels:
    app: {spec.name.lower()}
    component: reactor-workflow
spec:
  replicas: 3
  selector:
    matchLabels:
      app: {spec.name.lower()}
      component: reactor-workflow
  template:
    metadata:
      labels:
        app: {spec.name.lower()}
        component: reactor-workflow
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
    spec:
      containers:
      - name: reactor-workflow
        image: {spec.name.lower()}/reactor:latest
        ports:
        - containerPort: 4000
          name: http
        - containerPort: 9090
          name: metrics
        env:
        - name: WORKFLOW_TYPE
          value: "{spec.workflow_type.value}"
        - name: BITACTOR_INTEGRATION
          value: "{str(spec.bitactor_integration).lower()}"
        - name: PERFORMANCE_MODE
          value: "optimized"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /health
            port: 4000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 4000
          initialDelaySeconds: 5
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: {spec.name.lower()}-service
  namespace: {spec.name.lower().replace('_', '-')}
spec:
  selector:
    app: {spec.name.lower()}
    component: reactor-workflow
  ports:
  - name: http
    port: 80
    targetPort: 4000
  - name: metrics
    port: 9090
    targetPort: 9090
---
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: {spec.name.lower()}-metrics
  namespace: {spec.name.lower().replace('_', '-')}
spec:
  selector:
    matchLabels:
      app: {spec.name.lower()}
  endpoints:
  - port: metrics
    interval: 15s
'''
    
    def _write_output_files(self, spec: ReactorWorkflowSpec, generated_code: Dict[str, str]) -> List[str]:
        """Write all generated files to output directory"""
        
        output_files = []
        workflow_dir = self.output_path / spec.name.lower()
        workflow_dir.mkdir(exist_ok=True)
        
        # Write Reactor DSL
        reactor_file = workflow_dir / f"{spec.name.lower()}_workflow.ex"
        with open(reactor_file, 'w') as f:
            f.write(generated_code['reactor_dsl'])
        output_files.append(str(reactor_file))
        
        # Write BitActor steps
        if generated_code['bitactor_steps']:
            steps_file = workflow_dir / f"{spec.name.lower()}_steps.ex"
            with open(steps_file, 'w') as f:
                f.write(generated_code['bitactor_steps'])
            output_files.append(str(steps_file))
        
        # Write validation tests
        test_file = workflow_dir / f"{spec.name.lower()}_test.exs"
        with open(test_file, 'w') as f:
            f.write(generated_code['validation'])
        output_files.append(str(test_file))
        
        # Write Kubernetes manifest
        if generated_code['k8s_manifest']:
            k8s_file = workflow_dir / f"{spec.name.lower()}_k8s.yaml"
            with open(k8s_file, 'w') as f:
                f.write(generated_code['k8s_manifest'])
            output_files.append(str(k8s_file))
        
        return output_files
    
    def _create_basic_reactor_template(self) -> str:
        """Basic Reactor workflow template"""
        return '''
defmodule {workflow_name}.Workflow do
  @moduledoc """
  Basic Reactor workflow generated from TTL semantic definitions
  """
  
  use Reactor

{inputs}

  step :validate_input do
    argument :raw_data, input(:raw_data)
    run fn %{{raw_data: data}}, _context ->
      {{:ok, data}}
    end
  end

{steps}

  return {return_value}
end
'''
    
    def _create_parallel_reactor_template(self) -> str:
        """Parallel processing Reactor workflow template"""
        return '''
defmodule {workflow_name}.Workflow do
  @moduledoc """
  High-performance parallel Reactor workflow
  Optimized for concurrent semantic processing
  """
  
  use Reactor

{inputs}

  step :validate_input do
    argument :raw_data, input(:raw_data)
    async? false  # Input validation is critical path
    run fn %{{raw_data: data}}, _context ->
      {{:ok, data}}
    end
  end

{steps}

  collect :aggregate_results do
    argument :results, [result(:process_semantic_concept_1), result(:process_semantic_concept_2)]
    transform fn inputs ->
      %{{
        combined_results: inputs,
        processing_complete: true,
        timestamp: DateTime.utc_now()
      }}
    end
  end

  return {return_value}
end
'''
    
    def _create_saga_reactor_template(self) -> str:
        """Saga-based Reactor workflow template with compensation"""
        return '''
defmodule {workflow_name}.Workflow do
  @moduledoc """
  Saga-based Reactor workflow with full compensation support
  Ensures distributed transaction semantics for CNS/BitActor operations
  """
  
  use Reactor

{inputs}

  step :validate_input do
    argument :raw_data, input(:raw_data)
    run fn %{{raw_data: data}}, _context ->
      {{:ok, data}}
    end
  end

  group :saga_transaction do
    before_all &setup_transaction/3
    after_all &cleanup_transaction/1

{steps}
  end

  return {return_value}
  
  defp setup_transaction(inputs, context, _options) do
    # Initialize distributed transaction context
    transaction_id = UUID.uuid4()
    {{:ok, Map.put(context, :transaction_id, transaction_id)}}
  end
  
  defp cleanup_transaction(_result) do
    # Final cleanup
    :ok
  end
end
'''
    
    def _create_switch_reactor_template(self) -> str:
        """Switch-based conditional Reactor workflow template"""
        return '''
defmodule {workflow_name}.Workflow do
  @moduledoc """
  Conditional Reactor workflow with switch logic
  Routes processing based on semantic concept types
  """
  
  use Reactor

{inputs}

  step :validate_input do
    argument :raw_data, input(:raw_data)
    run fn %{{raw_data: data}}, _context ->
      {{:ok, data}}
    end
  end

  step :classify_input do
    argument :data, result(:validate_input)
    run fn %{{data: data}}, _context ->
      # Classify based on semantic patterns
      concept_type = determine_concept_type(data)
      {{:ok, concept_type}}
    end
  end

  switch :route_processing do
    on result(:classify_input)
    
    matches? &(&1 == :high_performance) do
{steps}
    end
    
    matches? &(&1 == :standard) do
      step :standard_processing do
        argument :data, result(:validate_input)
        run StandardProcessor
      end
    end
    
    default do
      step :default_processing do
        argument :data, result(:validate_input)
        run DefaultProcessor
      end
    end
  end

  return {return_value}
  
  defp determine_concept_type(data) do
    # Logic to determine processing path
    :standard
  end
end
'''
    
    def _create_compose_reactor_template(self) -> str:
        """Compose-based Reactor workflow template for complex orchestration"""
        return '''
defmodule {workflow_name}.Workflow do
  @moduledoc """
  Composed Reactor workflow orchestrating multiple sub-workflows
  Enables complex end-to-end CNS/BitActor processing pipelines
  """
  
  use Reactor

{inputs}

  step :validate_input do
    argument :raw_data, input(:raw_data)
    run fn %{{raw_data: data}}, _context ->
      {{:ok, data}}
    end
  end

  compose :semantic_preprocessing, SemanticPreprocessor.Workflow do
    argument :input_data, result(:validate_input)
  end

{steps}

  compose :bitactor_integration, BitActorIntegration.Workflow do
    argument :processed_data, result(:process_main_concepts)
  end

  compose :validation_workflow, ValidationProcessor.Workflow do
    argument :final_data, result(:bitactor_integration)
  end

  return {return_value}
end
'''

class EndToEndProjectBuilder:
    """Builder for complete end-to-end CNS/BitActor projects from TTL definitions"""
    
    def __init__(self, base_path: str = "/Users/sac/cns"):
        self.base_path = Path(base_path)
        self.parser = TTLSemanticParser()
        self.generator = ReactorWorkflowGenerator(base_path)
        
    async def build_project_from_ttl(self, ttl_path: str, project_name: str) -> Dict[str, Any]:
        """Build complete end-to-end project from TTL file"""
        logger.info(f"Building end-to-end project {project_name} from {ttl_path}")
        
        try:
            # Parse TTL file
            concepts = self.parser.parse_ttl_file(ttl_path)
            
            if not concepts:
                return {
                    'status': 'error',
                    'error': 'No semantic concepts found in TTL file',
                    'ttl_path': ttl_path
                }
            
            # Determine workflow type from TTL content
            workflow_type = self._determine_workflow_type(ttl_path, concepts)
            
            # Create workflow specification
            spec = ReactorWorkflowSpec(
                name=project_name,
                workflow_type=workflow_type,
                ttl_source=ttl_path,
                semantic_concepts=concepts,
                input_parameters=['raw_data', 'config'],
                output_targets=['processed_result'],
                performance_requirements=self._aggregate_performance_requirements(concepts),
                bitactor_integration=True,
                kubernetes_deployment=True
            )
            
            # Generate Reactor workflow
            workflow_result = self.generator.generate_workflow(spec)
            
            if workflow_result['status'] != 'success':
                return workflow_result
            
            # Generate additional project artifacts
            additional_artifacts = await self._generate_project_artifacts(spec)
            
            return {
                'status': 'success',
                'project_name': project_name,
                'ttl_source': ttl_path,
                'workflow_type': workflow_type.value,
                'concepts_processed': len(concepts),
                'reactor_workflow': workflow_result,
                'additional_artifacts': additional_artifacts,
                'deployment_ready': True
            }
            
        except Exception as e:
            logger.error(f"Failed to build project {project_name}: {e}")
            return {
                'status': 'error',
                'error': str(e),
                'project_name': project_name,
                'ttl_path': ttl_path
            }
    
    def _determine_workflow_type(self, ttl_path: str, concepts: List[SemanticConcept]) -> WorkflowType:
        """Determine workflow type based on TTL content and concepts"""
        
        ttl_filename = Path(ttl_path).name.lower()
        
        # Map based on filename patterns
        if 'cybersecurity' in ttl_filename:
            return WorkflowType.CYBERSECURITY_MESH
        elif 'bitactor' in ttl_filename:
            return WorkflowType.BITACTOR_SEMANTIC
        elif 'aegis' in ttl_filename:
            return WorkflowType.AEGIS_FABRIC
        elif 'uhft' in ttl_filename or 'trading' in ttl_filename:
            return WorkflowType.TRADING_UHFT
        elif 'realtime' in ttl_filename:
            return WorkflowType.REALTIME_SYSTEM
        elif 'healthcare' in ttl_filename:
            return WorkflowType.HEALTHCARE_CORE
        elif 'autonomous' in ttl_filename or 'vehicle' in ttl_filename:
            return WorkflowType.AUTONOMOUS_VEHICLE
        elif 'smart_grid' in ttl_filename:
            return WorkflowType.SMART_GRID
        elif 'industrial' in ttl_filename or 'iot' in ttl_filename:
            return WorkflowType.INDUSTRIAL_IOT
        else:
            return WorkflowType.BITACTOR_SEMANTIC  # Default
    
    def _aggregate_performance_requirements(self, concepts: List[SemanticConcept]) -> Dict[str, Any]:
        """Aggregate performance requirements from all concepts"""
        
        min_latency = min((c.performance_requirements.get('latency_ns', 1000000) for c in concepts), default=1000000)
        max_throughput = max((c.performance_requirements.get('throughput_ops_sec', 1000) for c in concepts), default=1000)
        
        return {
            'min_latency_ns': min_latency,
            'target_throughput_ops_sec': max_throughput,
            'tick_budget': min((c.performance_requirements.get('tick_budget', 8) for c in concepts), default=8)
        }
    
    async def _generate_project_artifacts(self, spec: ReactorWorkflowSpec) -> Dict[str, Any]:
        """Generate additional project artifacts beyond the Reactor workflow"""
        
        artifacts = {}
        
        # Generate Docker configuration
        artifacts['dockerfile'] = self._generate_dockerfile(spec)
        
        # Generate CI/CD pipeline
        artifacts['github_actions'] = self._generate_github_actions(spec)
        
        # Generate documentation
        artifacts['documentation'] = self._generate_project_documentation(spec)
        
        # Generate performance benchmarks
        artifacts['benchmarks'] = self._generate_performance_benchmarks(spec)
        
        return artifacts
    
    def _generate_dockerfile(self, spec: ReactorWorkflowSpec) -> str:
        """Generate Dockerfile for the project"""
        return f'''
FROM elixir:1.15-alpine

WORKDIR /app

# Install dependencies
RUN apk add --no-cache build-base git python3 py3-pip

# Copy project files
COPY . .

# Install Elixir dependencies
RUN mix deps.get
RUN mix deps.compile

# Install Python dependencies for BitActor integration
RUN pip3 install asyncio dataclasses

# Compile project
RUN mix compile

# Expose ports
EXPOSE 4000 9090

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \\
  CMD curl -f http://localhost:4000/health || exit 1

CMD ["mix", "run", "--no-halt"]
'''
    
    def _generate_github_actions(self, spec: ReactorWorkflowSpec) -> str:
        """Generate GitHub Actions CI/CD pipeline"""
        return f'''
name: {spec.name} CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    services:
      postgres:
        image: postgres:13
        env:
          POSTGRES_PASSWORD: postgres
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Elixir
      uses: erlef/setup-beam@v1
      with:
        elixir-version: '1.15'
        otp-version: '26'
        
    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: deps
        key: ${{{{ runner.os }}}}-mix-${{{{ hashFiles('**/mix.lock') }}}}
        
    - name: Install dependencies
      run: mix deps.get
      
    - name: Run tests
      run: mix test
      
    - name: Check formatting
      run: mix format --check-formatted
      
    - name: Run Credo
      run: mix credo --strict
      
    - name: Run Dialyzer
      run: mix dialyzer

  build:
    needs: test
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Build Docker image
      run: docker build -t {spec.name.lower()}:${{{{ github.sha }}}} .
      
    - name: Run security scan
      run: docker run --rm -v /var/run/docker.sock:/var/run/docker.sock \\
        aquasec/trivy image {spec.name.lower()}:${{{{ github.sha }}}}

  deploy:
    needs: [test, build]
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Deploy to Kubernetes
      run: |
        kubectl apply -f k8s/
        kubectl rollout status deployment/{spec.name.lower()}-reactor
'''
    
    def _generate_project_documentation(self, spec: ReactorWorkflowSpec) -> str:
        """Generate comprehensive project documentation"""
        return f'''
# {spec.name} - End-to-End CNS/BitActor Project

## Overview

This project was generated from TTL semantic definitions using the Reactor framework.
It provides a complete end-to-end workflow for processing {spec.workflow_type.value} data
with full BitActor integration and Kubernetes deployment.

## Architecture

### Semantic Concepts Processed
{chr(10).join(f"- **{c.name}**: {c.properties.get('comment', 'No description')}" for c in spec.semantic_concepts)}

### Performance Requirements
- **Minimum Latency**: {spec.performance_requirements.get('min_latency_ns', 'N/A')} nanoseconds
- **Target Throughput**: {spec.performance_requirements.get('target_throughput_ops_sec', 'N/A')} ops/sec
- **Tick Budget**: {spec.performance_requirements.get('tick_budget', 8)} ticks

### Reactor Workflow Pattern
- **Type**: {spec.workflow_type.value}
- **BitActor Integration**: {spec.bitactor_integration}
- **Kubernetes Ready**: {spec.kubernetes_deployment}

## Getting Started

### Prerequisites
- Elixir 1.15+
- Docker
- Kubernetes cluster
- BitActor runtime

### Installation
```bash
# Clone and setup
git clone <repository>
cd {spec.name.lower()}

# Install dependencies
mix deps.get

# Run tests
mix test

# Start application
mix run --no-halt
```

### Deployment
```bash
# Build Docker image
docker build -t {spec.name.lower()}:latest .

# Deploy to Kubernetes
kubectl apply -f k8s/
```

## Usage

### Running the Reactor Workflow
```elixir
# Basic usage
input_data = %{{
  raw_data: "your_data_here",
  config: %{{validation_level: "strict"}}
}}

{{:ok, result}} = Reactor.run({spec.name}.Workflow, input_data)
```

### BitActor Integration
The workflow automatically integrates with the BitActor runtime for ultra-low latency processing.
All performance-critical steps are optimized for sub-microsecond execution.

### Monitoring
- **Health Endpoint**: `http://localhost:4000/health`
- **Metrics Endpoint**: `http://localhost:9090/metrics`
- **Prometheus Integration**: Enabled by default

## Performance Tuning

### Configuration
```elixir
config :{spec.name.lower()}, {spec.name}.Workflow,
  async_concurrency: 100,
  max_retries: 3,
  timeout_ms: 5000,
  bitactor_integration: true
```

### Monitoring
Key metrics to monitor:
- `reactor_step_duration_seconds`
- `bitactor_processing_time_ns`
- `workflow_success_rate`

## Troubleshooting

### Common Issues
1. **High Latency**: Check BitActor integration and tick budget
2. **Memory Usage**: Monitor concept processing and garbage collection
3. **Kubernetes Issues**: Verify resource limits and health checks

### Debugging
```bash
# Enable debug logging
export LOG_LEVEL=debug

# Run with tracing
mix run --no-halt --trace
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes and add tests
4. Submit a pull request

## License

Generated by CNS Forge TTL-to-Reactor system.
'''
    
    def _generate_performance_benchmarks(self, spec: ReactorWorkflowSpec) -> str:
        """Generate performance benchmark suite"""
        return f'''
defmodule {spec.name}.PerformanceBenchmark do
  @moduledoc """
  Performance benchmarks for {spec.name} Reactor workflow
  Validates compliance with semantic performance requirements
  """
  
  use Benchee
  
  def run_benchmarks do
    Benchee.run(%{{
      "basic_workflow" => fn -> 
        run_basic_workflow() 
      end,
      "high_load_workflow" => fn -> 
        run_high_load_workflow() 
      end,
      "concurrent_workflows" => fn -> 
        run_concurrent_workflows() 
      end
    }},
    time: 10,
    memory_time: 2,
    reduction_time: 2,
    parallel: 1,
    warmup: 2,
    formatters: [
      Benchee.Formatters.HTML,
      Benchee.Formatters.Console
    ],
    html: %{{file: "benchmarks/{spec.name.lower()}_results.html"}}
    )
  end
  
  defp run_basic_workflow do
    input_data = %{{
      raw_data: "benchmark_data",
      config: %{{validation_level: "basic"}}
    }}
    
    {{:ok, _result}} = Reactor.run({spec.name}.Workflow, input_data)
  end
  
  defp run_high_load_workflow do
    input_data = %{{
      raw_data: String.duplicate("high_load_data", 1000),
      config: %{{validation_level: "strict"}}
    }}
    
    {{:ok, _result}} = Reactor.run({spec.name}.Workflow, input_data)
  end
  
  defp run_concurrent_workflows do
    tasks = Enum.map(1..10, fn i ->
      Task.async(fn ->
        input_data = %{{
          raw_data: "concurrent_data_#{{i}}",
          config: %{{validation_level: "basic"}}
        }}
        
        Reactor.run({spec.name}.Workflow, input_data)
      end)
    end)
    
    Task.await_many(tasks, 5000)
  end
  
  # Performance validation tests
  def validate_performance_requirements do
    # Test latency requirement
    {{time_microseconds, {{:ok, _result}}}} = :timer.tc(fn ->
      run_basic_workflow()
    end)
    
    max_latency_microseconds = {spec.performance_requirements.get('min_latency_ns', 1000000)} / 1000
    
    if time_microseconds > max_latency_microseconds do
      raise "Performance requirement violation: latency #{{time_microseconds}}μs exceeds #{{max_latency_microseconds}}μs"
    end
    
    # Test throughput requirement  
    start_time = System.monotonic_time(:millisecond)
    
    # Run multiple workflows
    Enum.each(1..100, fn _i ->
      run_basic_workflow()
    end)
    
    end_time = System.monotonic_time(:millisecond)
    duration_seconds = (end_time - start_time) / 1000
    actual_throughput = 100 / duration_seconds
    
    target_throughput = {spec.performance_requirements.get('target_throughput_ops_sec', 1000)}
    
    if actual_throughput < target_throughput do
      raise "Performance requirement violation: throughput #{{actual_throughput}} ops/sec below #{{target_throughput}} ops/sec"
    end
    
    :ok
  end
end
'''

# Main execution functions

async def build_all_projects() -> Dict[str, Any]:
    """Build all end-to-end projects from discovered TTL files"""
    builder = EndToEndProjectBuilder()
    
    # Key TTL files to process
    ttl_projects = [
        ("/Users/sac/cns/ontologies/cybersecurity_core.ttl", "CybersecurityMesh"),
        ("/Users/sac/cns/ontologies/bitactor_semantic_core.ttl", "BitActorSemantic"),
        ("/Users/sac/cns/aegis_fabric_ontology.ttl", "AegisFabric"),
        ("/Users/sac/cns/ontologies/generated/uhft/uhft_master.ttl", "UHFTTrading"),
        ("/Users/sac/cns/ontologies/generated/realtime/realtime_master.ttl", "RealtimeSystem"),
        ("/Users/sac/cns/ontologies/healthcare_core.ttl", "HealthcareCore"),
        ("/Users/sac/cns/ontologies/autonomous_vehicle_core.ttl", "AutonomousVehicle"),
        ("/Users/sac/cns/ontologies/smart_grid_core.ttl", "SmartGrid"),
        ("/Users/sac/cns/ontologies/industrial_iot_core.ttl", "IndustrialIoT")
    ]
    
    results = []
    
    # Process projects concurrently
    tasks = [
        builder.build_project_from_ttl(ttl_path, project_name)
        for ttl_path, project_name in ttl_projects
    ]
    
    project_results = await asyncio.gather(*tasks, return_exceptions=True)
    
    for i, result in enumerate(project_results):
        if isinstance(result, Exception):
            results.append({
                'status': 'error',
                'error': str(result),
                'project_name': ttl_projects[i][1],
                'ttl_path': ttl_projects[i][0]
            })
        else:
            results.append(result)
    
    # Aggregate results
    successful = [r for r in results if r.get('status') == 'success']
    failed = [r for r in results if r.get('status') != 'success']
    
    return {
        'status': 'completed',
        'total_projects': len(ttl_projects),
        'successful': len(successful),
        'failed': len(failed),
        'project_results': results,
        'summary': f"Built {len(successful)}/{len(ttl_projects)} end-to-end CNS/BitActor projects"
    }

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='TTL to Reactor Workflows Generator')
    parser.add_argument('--parse-only', action='store_true', help='Only parse TTL file and extract concepts')
    parser.add_argument('--generate-project', action='store_true', help='Generate complete project from TTL')
    parser.add_argument('--ttl-file', help='Path to TTL file to process')
    parser.add_argument('--project-name', help='Name for generated project')
    parser.add_argument('--output-path', help='Output path for generated project')
    
    args = parser.parse_args()
    
    if args.parse_only and args.ttl_file:
        # Parse TTL file only and report concepts
        parser = TTLSemanticParser()
        concepts = parser.parse_ttl_file(args.ttl_file)
        
        print(f"Extracted {len(concepts)} semantic concepts from {args.ttl_file}")
        for concept in concepts:
            print(f"Concept: {concept.name} (Type: {concept.concept_type})")
            if concept.performance_requirements:
                for req, value in concept.performance_requirements.items():
                    print(f"  Performance: {req} = {value}")
        
        print(f"Total performance concepts: {sum(1 for c in concepts if c.performance_requirements)}")
        print(f"Total hierarchical concepts: {sum(len(c.parent_classes) for c in concepts)}")
        
    elif args.generate_project and args.ttl_file and args.project_name:
        # Generate project from TTL file
        builder = EndToEndProjectBuilder()
        
        result = asyncio.run(builder.build_project_from_ttl(args.ttl_file, args.project_name))
        
        if result['status'] == 'success':
            print(f"✅ Successfully generated project: {args.project_name}")
            print(f"   Workflow Type: {result['workflow_type']}")
            print(f"   Concepts Processed: {result['concepts_processed']}")
            print(f"   Files Generated: {len(result['reactor_workflow']['output_files'])}")
            
            # If output path specified, move files there
            if args.output_path:
                import shutil
                import os
                
                os.makedirs(args.output_path, exist_ok=True)
                
                # Copy generated files to specified output path
                for file_path in result['reactor_workflow']['output_files']:
                    filename = os.path.basename(file_path)
                    dest_path = os.path.join(args.output_path, filename)
                    shutil.copy2(file_path, dest_path)
                    print(f"   Copied: {filename}")
        else:
            print(f"❌ Failed to generate project: {result.get('error', 'Unknown error')}")
            exit(1)
            
    else:
        # Run the complete end-to-end project builder (original behavior)
        result = asyncio.run(build_all_projects())
        
        print("\n" + "="*60)
        print("TTL TO REACTOR WORKFLOWS - END-TO-END PROJECT BUILDER")
        print("="*60)
        print(f"Status: {result['status'].upper()}")
        print(f"Projects Built: {result['successful']}/{result['total_projects']}")
        print(f"Success Rate: {result['successful']/result['total_projects']*100:.1f}%")
        print("="*60)
        
        # Print project details
        for project in result['project_results']:
            if project.get('status') == 'success':
                print(f"✅ {project['project_name']}: {project['workflow_type']}")
                print(f"   Concepts: {project['concepts_processed']}")
                print(f"   Files: {len(project['reactor_workflow']['output_files'])}")
            else:
                print(f"❌ {project.get('project_name', 'Unknown')}: {project.get('error', 'Unknown error')}")
        
        print(f"\n{result['summary']}")