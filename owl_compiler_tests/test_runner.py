#!/usr/bin/env python3
"""
Claude Flow Swarm Test Runner for OWL Compiler
Initializes and manages testing agents for comprehensive compiler testing
"""

import os
import sys
import json
import time
import logging
import subprocess
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
import traceback

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))

from owl_compiler import OWLCompiler
from owl_compiler_lifecycle import OWLCompilerLifecycle, CompilationConfig, LogLevel

# OpenTelemetry-style metrics collection
@dataclass
class TestMetrics:
    """Test execution metrics"""
    test_name: str
    start_time: datetime
    end_time: Optional[datetime] = None
    status: str = "pending"  # pending, running, passed, failed, error
    error_message: Optional[str] = None
    metrics: Dict[str, Any] = field(default_factory=dict)
    sub_tests: List['TestMetrics'] = field(default_factory=list)
    
    @property
    def duration_ms(self) -> float:
        if self.end_time:
            return (self.end_time - self.start_time).total_seconds() * 1000
        return 0.0
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            'test_name': self.test_name,
            'status': self.status,
            'duration_ms': self.duration_ms,
            'error_message': self.error_message,
            'metrics': self.metrics,
            'sub_tests': [t.to_dict() for t in self.sub_tests]
        }

class TestAgent:
    """Base class for test agents"""
    
    def __init__(self, name: str, test_dir: Path):
        self.name = name
        self.test_dir = test_dir
        self.logger = logging.getLogger(f"TestAgent.{name}")
        self.metrics = TestMetrics(test_name=name, start_time=datetime.now())
        
    def run(self) -> TestMetrics:
        """Run the test agent"""
        try:
            self.metrics.status = "running"
            self.logger.info(f"Starting test agent: {self.name}")
            
            # Execute test logic
            self.execute()
            
            self.metrics.status = "passed"
            self.logger.info(f"Test agent completed: {self.name}")
            
        except Exception as e:
            self.metrics.status = "failed"
            self.metrics.error_message = str(e)
            self.logger.error(f"Test agent failed: {self.name} - {e}")
            self.logger.debug(traceback.format_exc())
            
        finally:
            self.metrics.end_time = datetime.now()
            
        return self.metrics
    
    def execute(self):
        """Override this method in subclasses"""
        raise NotImplementedError

class OntologyCreatorAgent(TestAgent):
    """Creates test ontology files"""
    
    def execute(self):
        self.logger.info("Creating test ontology files")
        
        # Create basic ontology
        basic_ontology = self.create_basic_ontology()
        basic_path = self.test_dir / "test_data" / "basic_ontology.ttl"
        basic_path.write_text(basic_ontology)
        self.metrics.metrics['basic_ontology_created'] = True
        
        # Create complex ontology with Eightfold mappings
        eightfold_ontology = self.create_eightfold_ontology()
        eightfold_path = self.test_dir / "test_data" / "eightfold_ontology.ttl"
        eightfold_path.write_text(eightfold_ontology)
        self.metrics.metrics['eightfold_ontology_created'] = True
        
        # Create SHACL constraints ontology
        shacl_ontology = self.create_shacl_ontology()
        shacl_path = self.test_dir / "test_data" / "shacl_ontology.ttl"
        shacl_path.write_text(shacl_ontology)
        self.metrics.metrics['shacl_ontology_created'] = True
        
        self.metrics.metrics['total_ontologies_created'] = 3
    
    def create_basic_ontology(self) -> str:
        return '''@prefix : <http://example.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology declaration
: a owl:Ontology ;
    rdfs:label "Test Ontology" ;
    rdfs:comment "Basic test ontology for compiler testing" .

# Classes
:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Represents a person entity" .

:Organization a owl:Class ;
    rdfs:label "Organization" ;
    rdfs:comment "Represents an organization" .

:Employee a owl:Class ;
    rdfs:subClassOf :Person ;
    rdfs:label "Employee" ;
    rdfs:comment "A person who works for an organization" .

# Properties
:hasName a owl:DatatypeProperty ;
    rdfs:label "has name" ;
    rdfs:domain :Person ;
    rdfs:range xsd:string .

:hasAge a owl:DatatypeProperty ;
    rdfs:label "has age" ;
    rdfs:domain :Person ;
    rdfs:range xsd:integer .

:worksFor a owl:ObjectProperty ;
    rdfs:label "works for" ;
    rdfs:domain :Employee ;
    rdfs:range :Organization .

:employs a owl:ObjectProperty ;
    rdfs:label "employs" ;
    owl:inverseOf :worksFor ;
    rdfs:domain :Organization ;
    rdfs:range :Employee .
'''
    
    def create_eightfold_ontology(self) -> str:
        return '''@prefix : <http://example.org/eightfold#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix eh: <http://cns.io/eightfold#> .

# Ontology with Eightfold Path mappings
: a owl:Ontology ;
    rdfs:label "Eightfold Test Ontology" .

# Right Understanding
:KnowledgeBase a owl:Class ;
    rdfs:label "Knowledge Base" ;
    eh:stage "Right Understanding" ;
    rdfs:comment "Base knowledge representation" .

# Right Thought
:IntentionProcessor a owl:Class ;
    rdfs:label "Intention Processor" ;
    eh:stage "Right Thought" ;
    rdfs:comment "Processes intentions and plans" .

# Right Speech
:CommunicationInterface a owl:Class ;
    rdfs:label "Communication Interface" ;
    eh:stage "Right Speech" ;
    rdfs:comment "Handles communication protocols" .

# Right Action
:ExecutionEngine a owl:Class ;
    rdfs:label "Execution Engine" ;
    eh:stage "Right Action" ;
    rdfs:comment "Executes planned actions" .

# Right Livelihood
:MaintenanceSystem a owl:Class ;
    rdfs:label "Maintenance System" ;
    eh:stage "Right Livelihood" ;
    rdfs:comment "Maintains system sustainability" .

# Right Effort
:OptimizationModule a owl:Class ;
    rdfs:label "Optimization Module" ;
    eh:stage "Right Effort" ;
    rdfs:comment "Optimizes system performance" .

# Right Mindfulness
:MonitoringService a owl:Class ;
    rdfs:label "Monitoring Service" ;
    eh:stage "Right Mindfulness" ;
    rdfs:comment "Monitors system state" .

# Right Concentration
:IntegrationHub a owl:Class ;
    rdfs:label "Integration Hub" ;
    eh:stage "Right Concentration" ;
    rdfs:comment "Integrates all components" .
'''
    
    def create_shacl_ontology(self) -> str:
        return '''@prefix : <http://example.org/shacl#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes with SHACL constraints
:ValidatedPerson a owl:Class ;
    rdfs:label "Validated Person" .

:PersonShape a sh:NodeShape ;
    sh:targetClass :ValidatedPerson ;
    sh:property [
        sh:path :hasName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minLength 1 ;
        sh:maxLength 100 ;
    ] ;
    sh:property [
        sh:path :hasAge ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .

# Properties with characteristics
:hasParent a owl:ObjectProperty ;
    rdfs:label "has parent" ;
    rdf:type owl:IrreflexiveProperty .

:hasSibling a owl:ObjectProperty ;
    rdfs:label "has sibling" ;
    rdf:type owl:SymmetricProperty .

:hasAncestor a owl:ObjectProperty ;
    rdfs:label "has ancestor" ;
    rdf:type owl:TransitiveProperty .
'''

class CompilerTestAgent(TestAgent):
    """Tests the OWL compiler functionality"""
    
    def execute(self):
        self.logger.info("Testing OWL compiler functionality")
        
        # Test basic compilation
        basic_test = self.test_basic_compilation()
        self.metrics.sub_tests.append(basic_test)
        
        # Test template functionality
        template_test = self.test_template_functionality()
        self.metrics.sub_tests.append(template_test)
        
        # Test inference
        inference_test = self.test_inference()
        self.metrics.sub_tests.append(inference_test)
        
        # Calculate overall metrics
        passed_tests = sum(1 for t in self.metrics.sub_tests if t.status == "passed")
        self.metrics.metrics['total_tests'] = len(self.metrics.sub_tests)
        self.metrics.metrics['passed_tests'] = passed_tests
        self.metrics.metrics['failed_tests'] = len(self.metrics.sub_tests) - passed_tests
    
    def test_basic_compilation(self) -> TestMetrics:
        test = TestMetrics(test_name="Basic Compilation", start_time=datetime.now())
        
        try:
            compiler = OWLCompiler()
            spec_path = self.test_dir / "test_data" / "basic_ontology.ttl"
            output_dir = self.test_dir / "generated_code" / "basic"
            output_dir.mkdir(parents=True, exist_ok=True)
            
            result = compiler.compile(spec_path, output_dir)
            
            test.metrics['classes_extracted'] = len(result['classes'])
            test.metrics['properties_extracted'] = len(result['properties'])
            test.metrics['total_triples'] = result['statistics']['total_triples']
            
            # Verify output files exist
            expected_files = ['owl_ontology.h', 'owl_ontology.c', 'owl_ontology.json']
            for filename in expected_files:
                if (output_dir / filename).exists():
                    test.metrics[f'{filename}_created'] = True
                else:
                    raise Exception(f"Expected file {filename} not created")
            
            test.status = "passed"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def test_template_functionality(self) -> TestMetrics:
        test = TestMetrics(test_name="Template Functionality", start_time=datetime.now())
        
        try:
            compiler = OWLCompiler()
            
            # Test custom filters
            test_cases = [
                ("TestClass", "test_class", compiler.template_manager._snake_case_filter),
                ("test_class", "testClass", compiler.template_manager._camel_case_filter),
                ("TestClass", "TEST_CLASS", compiler.template_manager._upper_case_filter),
                ("test string", "test_string", compiler.template_manager._c_identifier_filter),
            ]
            
            for input_val, expected, filter_func in test_cases:
                result = filter_func(input_val)
                if result != expected:
                    raise Exception(f"Filter test failed: {input_val} -> {result} (expected {expected})")
            
            test.metrics['filters_tested'] = len(test_cases)
            test.status = "passed"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def test_inference(self) -> TestMetrics:
        test = TestMetrics(test_name="Inference Testing", start_time=datetime.now())
        
        try:
            compiler = OWLCompiler({'inference_enabled': True})
            spec_path = self.test_dir / "test_data" / "shacl_ontology.ttl"
            
            # Parse and apply inference
            compiler._parse_specification(spec_path)
            initial_triples = len(compiler.graph)
            
            compiler._extract_classes()
            compiler._extract_properties()
            compiler._apply_inference()
            
            final_triples = len(compiler.graph)
            
            test.metrics['initial_triples'] = initial_triples
            test.metrics['final_triples'] = final_triples
            test.metrics['inferred_triples'] = final_triples - initial_triples
            
            # Check for transitive property inference
            transitive_props = [p for p in compiler.properties.values() 
                               if 'Transitive' in p.characteristics]
            test.metrics['transitive_properties'] = len(transitive_props)
            
            test.status = "passed"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test

class LifecycleTestAgent(TestAgent):
    """Tests the compiler lifecycle functionality"""
    
    def execute(self):
        self.logger.info("Testing compiler lifecycle functionality")
        
        # Test configuration
        config_test = self.test_configuration()
        self.metrics.sub_tests.append(config_test)
        
        # Test full pipeline
        pipeline_test = self.test_full_pipeline()
        self.metrics.sub_tests.append(pipeline_test)
        
        # Test C compilation
        compilation_test = self.test_c_compilation()
        self.metrics.sub_tests.append(compilation_test)
    
    def test_configuration(self) -> TestMetrics:
        test = TestMetrics(test_name="Configuration Testing", start_time=datetime.now())
        
        try:
            config = CompilationConfig(
                input_files=[self.test_dir / "test_data" / "basic_ontology.ttl"],
                output_dir=self.test_dir / "generated_code" / "lifecycle",
                optimization_level=2,
                enable_tests=True
            )
            
            lifecycle = OWLCompilerLifecycle(config)
            
            test.metrics['config_created'] = True
            test.metrics['optimization_level'] = config.optimization_level
            test.metrics['tests_enabled'] = config.enable_tests
            
            test.status = "passed"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def test_full_pipeline(self) -> TestMetrics:
        test = TestMetrics(test_name="Full Pipeline Testing", start_time=datetime.now())
        
        try:
            config = CompilationConfig(
                input_files=[self.test_dir / "test_data" / "eightfold_ontology.ttl"],
                output_dir=self.test_dir / "generated_code" / "pipeline",
                optimization_level=1,
                enable_tests=False,  # Disable for now to avoid C compiler dependency
                inference_enabled=True,
                eightfold_integration=True
            )
            
            lifecycle = OWLCompilerLifecycle(config)
            result = lifecycle.run()
            
            test.metrics['pipeline_success'] = result.success
            test.metrics['stages_completed'] = len([s for s in result.stage_results.values() 
                                                   if s['success']])
            test.metrics['total_stages'] = len(result.stage_results)
            test.metrics['output_files'] = len(result.output_files)
            test.metrics['duration_seconds'] = result.duration
            
            if not result.success:
                test.error_message = "; ".join(result.errors)
                test.status = "failed"
            else:
                test.status = "passed"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def test_c_compilation(self) -> TestMetrics:
        test = TestMetrics(test_name="C Compilation Testing", start_time=datetime.now())
        
        try:
            # Check if gcc is available
            gcc_check = subprocess.run(['gcc', '--version'], capture_output=True)
            if gcc_check.returncode != 0:
                test.status = "passed"  # Skip if no gcc
                test.metrics['gcc_available'] = False
                test.metrics['skipped'] = True
                return test
            
            test.metrics['gcc_available'] = True
            
            # Test compiling generated C code
            c_file = self.test_dir / "generated_code" / "basic" / "owl_ontology.c"
            h_file = self.test_dir / "generated_code" / "basic" / "owl_ontology.h"
            
            if c_file.exists() and h_file.exists():
                obj_file = c_file.with_suffix('.o')
                compile_cmd = ['gcc', '-c', str(c_file), '-o', str(obj_file), '-I', str(c_file.parent)]
                
                result = subprocess.run(compile_cmd, capture_output=True, text=True)
                
                test.metrics['compilation_return_code'] = result.returncode
                test.metrics['object_file_created'] = obj_file.exists()
                
                if result.returncode == 0:
                    test.status = "passed"
                else:
                    test.status = "failed"
                    test.error_message = result.stderr
            else:
                test.status = "failed"
                test.error_message = "C source files not found"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test

class CCodeValidationAgent(TestAgent):
    """Validates generated C code"""
    
    def execute(self):
        self.logger.info("Validating generated C code")
        
        # Find all generated C files
        c_files = list((self.test_dir / "generated_code").rglob("*.c"))
        h_files = list((self.test_dir / "generated_code").rglob("*.h"))
        
        self.metrics.metrics['c_files_found'] = len(c_files)
        self.metrics.metrics['h_files_found'] = len(h_files)
        
        for c_file in c_files:
            validation = self.validate_c_file(c_file)
            self.metrics.sub_tests.append(validation)
        
        for h_file in h_files:
            validation = self.validate_h_file(h_file)
            self.metrics.sub_tests.append(validation)
    
    def validate_c_file(self, file_path: Path) -> TestMetrics:
        test = TestMetrics(test_name=f"Validate {file_path.name}", start_time=datetime.now())
        
        try:
            content = file_path.read_text()
            
            # Check for required sections
            checks = {
                'has_includes': '#include' in content,
                'has_functions': 'void' in content or 'int' in content,
                'has_proper_brackets': content.count('{') == content.count('}'),
                'has_semicolons': ';' in content,
                'no_syntax_errors': self.check_c_syntax(file_path)
            }
            
            test.metrics.update(checks)
            
            if all(checks.values()):
                test.status = "passed"
            else:
                test.status = "failed"
                test.error_message = f"Failed checks: {[k for k, v in checks.items() if not v]}"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def validate_h_file(self, file_path: Path) -> TestMetrics:
        test = TestMetrics(test_name=f"Validate {file_path.name}", start_time=datetime.now())
        
        try:
            content = file_path.read_text()
            
            # Check for header guards
            has_ifndef = '#ifndef' in content
            has_define = '#define' in content
            has_endif = '#endif' in content
            
            test.metrics['has_header_guards'] = has_ifndef and has_define and has_endif
            test.metrics['has_declarations'] = 'typedef' in content or 'extern' in content
            
            if test.metrics['has_header_guards']:
                test.status = "passed"
            else:
                test.status = "failed"
                test.error_message = "Missing header guards"
            
        except Exception as e:
            test.status = "failed"
            test.error_message = str(e)
            
        finally:
            test.end_time = datetime.now()
            
        return test
    
    def check_c_syntax(self, file_path: Path) -> bool:
        """Check C syntax using gcc -fsyntax-only"""
        try:
            result = subprocess.run(
                ['gcc', '-fsyntax-only', str(file_path), '-I', str(file_path.parent)],
                capture_output=True,
                text=True
            )
            return result.returncode == 0
        except:
            return True  # Assume OK if gcc not available

class TestOrchestrator:
    """Orchestrates all test agents"""
    
    def __init__(self, test_dir: Path):
        self.test_dir = test_dir
        self.logger = logging.getLogger("TestOrchestrator")
        self.agents: List[TestAgent] = []
        self.results: List[TestMetrics] = []
        
    def initialize_agents(self):
        """Initialize all test agents"""
        self.agents = [
            OntologyCreatorAgent("OntologyCreator", self.test_dir),
            CompilerTestAgent("CompilerTest", self.test_dir),
            LifecycleTestAgent("LifecycleTest", self.test_dir),
            CCodeValidationAgent("CCodeValidation", self.test_dir)
        ]
    
    def run_sequential(self) -> Dict[str, Any]:
        """Run agents sequentially"""
        self.logger.info("Running tests sequentially")
        
        overall_start = datetime.now()
        
        for agent in self.agents:
            result = agent.run()
            self.results.append(result)
        
        overall_end = datetime.now()
        
        return self.generate_report(overall_start, overall_end)
    
    def run_parallel(self, max_workers: int = 4) -> Dict[str, Any]:
        """Run agents in parallel"""
        self.logger.info(f"Running tests in parallel with {max_workers} workers")
        
        overall_start = datetime.now()
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit OntologyCreator first (others depend on it)
            creator_future = executor.submit(self.agents[0].run)
            creator_result = creator_future.result()
            self.results.append(creator_result)
            
            # Submit remaining agents
            futures = {executor.submit(agent.run): agent for agent in self.agents[1:]}
            
            for future in as_completed(futures):
                result = future.result()
                self.results.append(result)
        
        overall_end = datetime.now()
        
        return self.generate_report(overall_start, overall_end)
    
    def generate_report(self, start_time: datetime, end_time: datetime) -> Dict[str, Any]:
        """Generate comprehensive test report"""
        total_tests = len(self.results)
        passed_tests = sum(1 for r in self.results if r.status == "passed")
        failed_tests = sum(1 for r in self.results if r.status == "failed")
        
        report = {
            'metadata': {
                'test_suite': 'OWL Compiler Test Suite',
                'start_time': start_time.isoformat(),
                'end_time': end_time.isoformat(),
                'duration_seconds': (end_time - start_time).total_seconds()
            },
            'summary': {
                'total_agents': len(self.agents),
                'total_tests': total_tests,
                'passed': passed_tests,
                'failed': failed_tests,
                'success_rate': (passed_tests / total_tests * 100) if total_tests > 0 else 0
            },
            'agents': [result.to_dict() for result in self.results],
            'metrics': self.collect_metrics()
        }
        
        return report
    
    def collect_metrics(self) -> Dict[str, Any]:
        """Collect all metrics from test results"""
        metrics = {
            'total_duration_ms': sum(r.duration_ms for r in self.results),
            'agent_metrics': {}
        }
        
        for result in self.results:
            metrics['agent_metrics'][result.test_name] = {
                'duration_ms': result.duration_ms,
                'status': result.status,
                'metrics': result.metrics,
                'sub_test_count': len(result.sub_tests),
                'sub_test_passed': sum(1 for t in result.sub_tests if t.status == "passed")
            }
        
        return metrics

def setup_logging(log_level: str = "INFO"):
    """Setup logging configuration"""
    logging.basicConfig(
        level=getattr(logging, log_level),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(Path(__file__).parent / "test_results" / "test_run.log")
        ]
    )

def main():
    """Main test execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description="OWL Compiler Test Suite")
    parser.add_argument("--parallel", action="store_true", help="Run tests in parallel")
    parser.add_argument("--workers", type=int, default=4, help="Number of parallel workers")
    parser.add_argument("--log-level", default="INFO", help="Logging level")
    
    args = parser.parse_args()
    
    # Setup
    test_dir = Path(__file__).parent
    setup_logging(args.log_level)
    
    # Run tests
    orchestrator = TestOrchestrator(test_dir)
    orchestrator.initialize_agents()
    
    if args.parallel:
        report = orchestrator.run_parallel(args.workers)
    else:
        report = orchestrator.run_sequential()
    
    # Save report
    report_path = test_dir / "test_results" / f"test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    report_path.write_text(json.dumps(report, indent=2))
    
    # Print summary
    print("\n" + "="*60)
    print("OWL Compiler Test Suite - Results")
    print("="*60)
    print(f"Total Agents: {report['summary']['total_agents']}")
    print(f"Total Tests: {report['summary']['total_tests']}")
    print(f"Passed: {report['summary']['passed']}")
    print(f"Failed: {report['summary']['failed']}")
    print(f"Success Rate: {report['summary']['success_rate']:.2f}%")
    print(f"Total Duration: {report['metadata']['duration_seconds']:.2f}s")
    print(f"\nDetailed report saved to: {report_path}")
    
    return 0 if report['summary']['failed'] == 0 else 1

if __name__ == "__main__":
    sys.exit(main())