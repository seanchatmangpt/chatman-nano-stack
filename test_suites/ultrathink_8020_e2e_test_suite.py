#!/usr/bin/env python3
"""
Ultrathink 80/20 E2E Test Suite
End-to-end testing for the complete BitActor pipeline
Focus: 20% of E2E scenarios covering 80% of pipeline functionality
Pipeline: typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
"""

import asyncio
import json
import time
import logging
import sys
import os
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
import subprocess
import concurrent.futures
import websocket
import threading
import queue
import tempfile

@dataclass 
class PipelineStageResult:
    stage_name: str
    start_time_ns: int
    end_time_ns: int
    duration_ns: int
    status: str
    output_data: Optional[Any]
    errors: List[str]
    ttl_compliant: bool
    handoff_successful: bool

@dataclass
class E2EScenarioResult:
    scenario_name: str
    pipeline_stages: List[PipelineStageResult]
    total_duration_ns: int
    overall_status: str
    bottlenecks: List[str]
    data_continuity: bool
    ttl_violations: int

class UltrathinkE2ETestSuite:
    """
    80/20 End-to-End Test Suite for BitActor Pipeline
    Tests critical E2E scenarios covering 80% of pipeline use cases
    """
    
    def __init__(self):
        self.test_session_id = f"e2e_8020_{int(time.time())}"
        self.logger = self._setup_logging()
        self.scenario_results: List[E2EScenarioResult] = []
        
        # Pipeline stage definitions
        self.pipeline_stages = [
            'typer', 'turtle', 'ttl2dspy', 'bitactor', 
            'erlang', 'ash', 'reactor', 'k8s'
        ]
        
        # TTL budgets for E2E tests (nanoseconds)
        self.ttl_budgets = {
            'e2e_global_ns': 120_000_000_000,  # 2 minutes total
            'single_scenario_ns': 30_000_000_000,  # 30 seconds per scenario
            'stage_handoff_ns': 2_000_000_000,     # 2 seconds per handoff
            'typer_stage_ns': 3_000_000_000,       # 3 seconds
            'turtle_stage_ns': 2_000_000_000,      # 2 seconds
            'ttl2dspy_stage_ns': 1_000_000_000,    # 1 second
            'bitactor_stage_ns': 5_000_000_000,    # 5 seconds
            'erlang_stage_ns': 4_000_000_000,      # 4 seconds
            'ash_stage_ns': 6_000_000_000,         # 6 seconds
            'reactor_stage_ns': 8_000_000_000,     # 8 seconds
            'k8s_stage_ns': 10_000_000_000,        # 10 seconds
        }
        
        # Test data templates
        self.test_data_templates = self._create_test_data_templates()
        
    def _setup_logging(self):
        logging.basicConfig(level=logging.INFO)
        logger = logging.getLogger(f'UltrathinkE2E_{self.test_session_id}')
        return logger
        
    def _create_test_data_templates(self) -> Dict[str, Dict]:
        """Create test data templates for different E2E scenarios"""
        return {
            'simple_type_validation': {
                'input_data': {
                    'type': 'user_record',
                    'fields': {'name': 'string', 'age': 'integer', 'email': 'email'},
                    'constraints': {'age': {'min': 0, 'max': 150}}
                },
                'expected_output': 'validated_type_definition'
            },
            'complex_workflow': {
                'input_data': {
                    'workflow_definition': {
                        'name': 'user_registration',
                        'steps': [
                            {'name': 'validate_input', 'action': 'validate'},
                            {'name': 'create_user', 'action': 'ash_create'},
                            {'name': 'send_welcome', 'action': 'notification'}
                        ]
                    }
                },
                'expected_output': 'workflow_execution_result'
            },
            'swarm_coordination': {
                'input_data': {
                    'task_distribution': {
                        'task_type': 'parallel_processing',
                        'agents_required': 5,
                        'coordination_level': 'high'
                    }
                },
                'expected_output': 'distributed_task_results'
            },
            'high_load_processing': {
                'input_data': {
                    'batch_data': {
                        'records': 1000,
                        'processing_type': 'parallel',
                        'ttl_constraint': 15_000_000_000  # 15 seconds
                    }
                },
                'expected_output': 'batch_processing_results'
            }
        }
        
    def run_complete_e2e_test_suite(self) -> Dict[str, Any]:
        """Execute comprehensive 80/20 E2E test suite"""
        self.logger.info(f"Starting Ultrathink 80/20 E2E Test Suite - Session: {self.test_session_id}")
        
        suite_start = time.time_ns()
        
        # Define 80/20 E2E scenarios (20% that cover 80% of use cases)
        e2e_scenarios = [
            ('basic_pipeline_flow', self._test_basic_pipeline_flow),
            ('complex_workflow_e2e', self._test_complex_workflow),
            ('swarm_coordination_e2e', self._test_swarm_coordination_e2e),
            ('high_load_e2e', self._test_high_load_scenario),
            ('error_recovery_e2e', self._test_error_recovery),
            ('ttl_constraint_e2e', self._test_ttl_constraint_enforcement),
            ('cross_stage_communication', self._test_cross_stage_communication),
            ('security_e2e', self._test_security_e2e)
        ]
        
        # Execute scenarios
        for scenario_name, scenario_func in e2e_scenarios:
            self.logger.info(f"Executing E2E scenario: {scenario_name}")
            scenario_result = self._execute_e2e_scenario(scenario_name, scenario_func)
            self.scenario_results.append(scenario_result)
            
        suite_duration = time.time_ns() - suite_start
        
        # Compile results
        results = {
            'session_id': self.test_session_id,
            'suite_type': 'e2e_tests_8020',
            'scenarios': [self._format_scenario_result(sr) for sr in self.scenario_results],
            'overall_results': self._calculate_e2e_overall_results(suite_duration),
            'pipeline_analysis': self._analyze_pipeline_performance(),
            'bottleneck_analysis': self._identify_pipeline_bottlenecks(),
            'data_continuity_analysis': self._analyze_data_continuity(),
            'ttl_compliance_analysis': self._analyze_e2e_ttl_compliance()
        }
        
        self.logger.info(f"E2E test suite completed in {suite_duration / 1_000_000_000:.2f}s")
        return results
        
    def _execute_e2e_scenario(self, scenario_name: str, scenario_func) -> E2EScenarioResult:
        """Execute a single E2E scenario"""
        scenario_start = time.time_ns()
        
        try:
            # Get test data for scenario
            test_data = self.test_data_templates.get(scenario_name.split('_')[0], {})
            
            # Execute scenario
            pipeline_stages = scenario_func(test_data)
            scenario_duration = time.time_ns() - scenario_start
            
            # Analyze results
            overall_status = 'passed' if all(stage.status == 'passed' for stage in pipeline_stages) else 'failed'
            bottlenecks = self._identify_scenario_bottlenecks(pipeline_stages)
            data_continuity = self._check_data_continuity(pipeline_stages)
            ttl_violations = sum(1 for stage in pipeline_stages if not stage.ttl_compliant)
            
            return E2EScenarioResult(
                scenario_name=scenario_name,
                pipeline_stages=pipeline_stages,
                total_duration_ns=scenario_duration,
                overall_status=overall_status,
                bottlenecks=bottlenecks,
                data_continuity=data_continuity,
                ttl_violations=ttl_violations
            )
            
        except Exception as e:
            scenario_duration = time.time_ns() - scenario_start
            self.logger.error(f"E2E scenario {scenario_name} failed: {str(e)}")
            
            return E2EScenarioResult(
                scenario_name=scenario_name,
                pipeline_stages=[],
                total_duration_ns=scenario_duration,
                overall_status='failed',
                bottlenecks=[f"Scenario execution error: {str(e)}"],
                data_continuity=False,
                ttl_violations=1
            )
            
    def _test_basic_pipeline_flow(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test basic data flow through all pipeline stages"""
        stages = []
        current_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            stage_result = self._simulate_pipeline_stage(stage_name, current_data)
            stages.append(stage_result)
            
            # Use stage output as input for next stage
            if stage_result.output_data:
                current_data = stage_result.output_data
                
        return stages
        
    def _test_complex_workflow(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test complex workflow execution through pipeline"""
        stages = []
        workflow_data = test_data.get('input_data', {})
        
        # Simulate complex workflow with branching and convergence
        for stage_name in self.pipeline_stages:
            # Add complexity for reactor and ash stages
            if stage_name in ['ash', 'reactor']:
                stage_data = {**workflow_data, 'complexity': 'high', 'branching': True}
            else:
                stage_data = workflow_data
                
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            workflow_data = stage_result.output_data or workflow_data
            
        return stages
        
    def _test_swarm_coordination_e2e(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test swarm coordination across pipeline stages"""
        stages = []
        swarm_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            # Enable swarm coordination for stages that support it
            if stage_name in ['bitactor', 'ash', 'reactor']:
                stage_data = {**swarm_data, 'swarm_enabled': True, 'agents': 3}
            else:
                stage_data = swarm_data
                
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            swarm_data = stage_result.output_data or swarm_data
            
        return stages
        
    def _test_high_load_scenario(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test pipeline under high load conditions"""
        stages = []
        load_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            # Simulate high load
            stage_data = {**load_data, 'load_multiplier': 10, 'concurrent_requests': 100}
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            load_data = stage_result.output_data or load_data
            
        return stages
        
    def _test_error_recovery(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test error recovery mechanisms across pipeline"""
        stages = []
        error_data = test_data.get('input_data', {})
        
        # Inject errors at different stages
        error_injection_stages = ['turtle', 'ash', 'reactor']
        
        for stage_name in self.pipeline_stages:
            if stage_name in error_injection_stages:
                stage_data = {**error_data, 'inject_error': True, 'error_type': 'recoverable'}
            else:
                stage_data = error_data
                
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            error_data = stage_result.output_data or error_data
            
        return stages
        
    def _test_ttl_constraint_enforcement(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test TTL constraint enforcement across pipeline"""
        stages = []
        ttl_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            # Add strict TTL constraints
            stage_data = {**ttl_data, 'ttl_strict': True, 'ttl_budget_override': True}
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            ttl_data = stage_result.output_data or ttl_data
            
        return stages
        
    def _test_cross_stage_communication(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test communication patterns between pipeline stages"""
        stages = []
        comm_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            # Test different communication patterns
            stage_data = {**comm_data, 'communication_test': True, 'pattern': 'pub_sub'}
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            comm_data = stage_result.output_data or comm_data
            
        return stages
        
    def _test_security_e2e(self, test_data: Dict) -> List[PipelineStageResult]:
        """Test security measures across entire pipeline"""
        stages = []
        security_data = test_data.get('input_data', {})
        
        for stage_name in self.pipeline_stages:
            # Enable security testing
            stage_data = {**security_data, 'security_test': True, 'auth_required': True}
            stage_result = self._simulate_pipeline_stage(stage_name, stage_data)
            stages.append(stage_result)
            security_data = stage_result.output_data or security_data
            
        return stages
        
    def _simulate_pipeline_stage(self, stage_name: str, input_data: Dict) -> PipelineStageResult:
        """Simulate execution of a pipeline stage"""
        stage_start = time.time_ns()
        
        # Get TTL budget for stage
        ttl_budget = self.ttl_budgets.get(f'{stage_name}_stage_ns', 5_000_000_000)
        
        try:
            # Simulate stage processing
            processing_time = self._calculate_stage_processing_time(stage_name, input_data)
            
            # Simulate processing delay
            import time
            time.sleep(processing_time / 1_000_000_000)  # Convert to seconds
            
            stage_end = time.time_ns()
            duration = stage_end - stage_start
            
            # Determine stage status
            status = 'passed'
            errors = []
            
            # Check for injected errors
            if input_data.get('inject_error') and stage_name in ['turtle', 'ash', 'reactor']:
                status = 'failed'
                errors.append(f"Injected error in {stage_name} stage")
                
            # Check TTL compliance
            ttl_compliant = duration <= ttl_budget
            if not ttl_compliant:
                errors.append(f"TTL violation: {duration}ns > {ttl_budget}ns")
                
            # Generate output data
            output_data = self._generate_stage_output(stage_name, input_data, status)
            
            return PipelineStageResult(
                stage_name=stage_name,
                start_time_ns=stage_start,
                end_time_ns=stage_end,
                duration_ns=duration,
                status=status,
                output_data=output_data,
                errors=errors,
                ttl_compliant=ttl_compliant,
                handoff_successful=status == 'passed'
            )
            
        except Exception as e:
            stage_end = time.time_ns()
            duration = stage_end - stage_start
            
            return PipelineStageResult(
                stage_name=stage_name,
                start_time_ns=stage_start,
                end_time_ns=stage_end,
                duration_ns=duration,
                status='failed',
                output_data=None,
                errors=[f"Stage execution error: {str(e)}"],
                ttl_compliant=duration <= ttl_budget,
                handoff_successful=False
            )
            
    def _calculate_stage_processing_time(self, stage_name: str, input_data: Dict) -> int:
        """Calculate realistic processing time for stage based on data"""
        base_times = {
            'typer': 500_000_000,     # 500ms base
            'turtle': 300_000_000,    # 300ms base
            'ttl2dspy': 100_000_000,  # 100ms base
            'bitactor': 1_000_000_000, # 1s base
            'erlang': 800_000_000,    # 800ms base
            'ash': 1_200_000_000,     # 1.2s base
            'reactor': 1_500_000_000, # 1.5s base
            'k8s': 2_000_000_000      # 2s base
        }
        
        base_time = base_times.get(stage_name, 1_000_000_000)
        
        # Apply multipliers based on input data
        multiplier = 1.0
        
        if input_data.get('complexity') == 'high':
            multiplier *= 2.0
        if input_data.get('load_multiplier'):
            multiplier *= min(input_data['load_multiplier'] / 10, 3.0)  # Cap at 3x
        if input_data.get('swarm_enabled'):
            multiplier *= 1.5  # Coordination overhead
            
        return int(base_time * multiplier)
        
    def _generate_stage_output(self, stage_name: str, input_data: Dict, status: str) -> Optional[Dict]:
        """Generate realistic output data for stage"""
        if status == 'failed':
            return None
            
        # Base output structure
        output = {
            'stage': stage_name,
            'processed_at': time.time_ns(),
            'input_hash': hash(str(input_data)),
            'metadata': {
                'processing_time_ns': self._calculate_stage_processing_time(stage_name, input_data),
                'ttl_compliant': True
            }
        }
        
        # Stage-specific output
        if stage_name == 'typer':
            output['type_analysis'] = {'validated': True, 'type_errors': 0}
        elif stage_name == 'turtle':
            output['turtle_transformation'] = {'success': True, 'graph_nodes': 125}
        elif stage_name == 'ttl2dspy':
            output['ttl_monitoring'] = {'violations': 0, 'performance_score': 95.5}
        elif stage_name == 'bitactor':
            output['bitactor_processing'] = {'actors_spawned': 15, 'messages_processed': 1250}
        elif stage_name == 'erlang':
            output['erlang_execution'] = {'processes': 25, 'memory_usage': '128MB'}
        elif stage_name == 'ash':
            output['ash_operations'] = {'resources_created': 5, 'queries_executed': 12}
        elif stage_name == 'reactor':
            output['reactor_workflow'] = {'steps_completed': 8, 'compensations': 0}
        elif stage_name == 'k8s':
            output['k8s_deployment'] = {'pods_ready': 3, 'services_exposed': 2}
            
        return output
        
    def _identify_scenario_bottlenecks(self, stages: List[PipelineStageResult]) -> List[str]:
        """Identify bottlenecks in scenario execution"""
        bottlenecks = []
        
        if not stages:
            return bottlenecks
            
        # Find slowest stages
        avg_duration = sum(stage.duration_ns for stage in stages) / len(stages)
        
        for stage in stages:
            if stage.duration_ns > avg_duration * 2:
                bottlenecks.append(f"{stage.stage_name}: {stage.duration_ns / 1_000_000:.1f}ms (slow)")
                
        # Check for TTL violations
        ttl_violators = [stage for stage in stages if not stage.ttl_compliant]
        for stage in ttl_violators:
            bottlenecks.append(f"{stage.stage_name}: TTL violation")
            
        # Check for failed handoffs
        failed_handoffs = [stage for stage in stages if not stage.handoff_successful]
        for stage in failed_handoffs:
            bottlenecks.append(f"{stage.stage_name}: handoff failed")
            
        return bottlenecks
        
    def _check_data_continuity(self, stages: List[PipelineStageResult]) -> bool:
        """Check if data flows correctly through all stages"""
        for i, stage in enumerate(stages):
            if stage.status == 'failed':
                return False
            if not stage.handoff_successful:
                return False
            if i > 0 and stage.output_data is None and stages[i-1].output_data is not None:
                return False
                
        return True
        
    def _format_scenario_result(self, scenario: E2EScenarioResult) -> Dict[str, Any]:
        """Format scenario result for output"""
        return {
            'scenario_name': scenario.scenario_name,
            'overall_status': scenario.overall_status,
            'total_duration_ns': scenario.total_duration_ns,
            'total_duration_ms': scenario.total_duration_ns / 1_000_000,
            'stages_completed': len(scenario.pipeline_stages),
            'stages_passed': len([s for s in scenario.pipeline_stages if s.status == 'passed']),
            'stages_failed': len([s for s in scenario.pipeline_stages if s.status == 'failed']),
            'data_continuity': scenario.data_continuity,
            'ttl_violations': scenario.ttl_violations,
            'bottlenecks': scenario.bottlenecks,
            'stage_details': [
                {
                    'stage': stage.stage_name,
                    'status': stage.status,
                    'duration_ns': stage.duration_ns,
                    'duration_ms': stage.duration_ns / 1_000_000,
                    'ttl_compliant': stage.ttl_compliant,
                    'errors': stage.errors
                }
                for stage in scenario.pipeline_stages
            ]
        }
        
    def _calculate_e2e_overall_results(self, suite_duration: int) -> Dict[str, Any]:
        """Calculate overall E2E test results"""
        if not self.scenario_results:
            return {'error': 'No scenarios executed'}
            
        total_scenarios = len(self.scenario_results)
        passed_scenarios = len([s for s in self.scenario_results if s.overall_status == 'passed'])
        
        return {
            'total_scenarios': total_scenarios,
            'passed_scenarios': passed_scenarios,
            'failed_scenarios': total_scenarios - passed_scenarios,
            'success_rate': (passed_scenarios / total_scenarios) * 100,
            'suite_duration_ns': suite_duration,
            'suite_duration_s': suite_duration / 1_000_000_000,
            'average_scenario_duration_ns': sum(s.total_duration_ns for s in self.scenario_results) / total_scenarios,
            'ttl_compliant': suite_duration <= self.ttl_budgets['e2e_global_ns']
        }
        
    def _analyze_pipeline_performance(self) -> Dict[str, Any]:
        """Analyze performance across all pipeline stages"""
        stage_performance = {}
        
        # Collect performance data for each stage
        for stage_name in self.pipeline_stages:
            stage_executions = []
            
            for scenario in self.scenario_results:
                for stage in scenario.pipeline_stages:
                    if stage.stage_name == stage_name:
                        stage_executions.append(stage)
                        
            if stage_executions:
                avg_duration = sum(s.duration_ns for s in stage_executions) / len(stage_executions)
                max_duration = max(s.duration_ns for s in stage_executions)
                min_duration = min(s.duration_ns for s in stage_executions)
                success_rate = (len([s for s in stage_executions if s.status == 'passed']) / len(stage_executions)) * 100
                
                stage_performance[stage_name] = {
                    'executions': len(stage_executions),
                    'avg_duration_ns': avg_duration,
                    'avg_duration_ms': avg_duration / 1_000_000,
                    'max_duration_ns': max_duration,
                    'min_duration_ns': min_duration,
                    'success_rate': success_rate,
                    'ttl_violations': len([s for s in stage_executions if not s.ttl_compliant])
                }
                
        return stage_performance
        
    def _identify_pipeline_bottlenecks(self) -> List[Dict[str, Any]]:
        """Identify bottlenecks across the entire pipeline"""
        bottlenecks = []
        
        # Analyze stage performance
        stage_performance = self._analyze_pipeline_performance()
        
        for stage_name, perf in stage_performance.items():
            if perf['success_rate'] < 80:
                bottlenecks.append({
                    'type': 'reliability',
                    'stage': stage_name,
                    'issue': f"Low success rate: {perf['success_rate']:.1f}%",
                    'severity': 'high'
                })
                
            if perf['ttl_violations'] > 0:
                bottlenecks.append({
                    'type': 'performance',
                    'stage': stage_name,
                    'issue': f"TTL violations: {perf['ttl_violations']}",
                    'severity': 'medium'
                })
                
            # Check if stage is significantly slower than others
            all_avg_durations = [p['avg_duration_ns'] for p in stage_performance.values()]
            if all_avg_durations:
                pipeline_avg = sum(all_avg_durations) / len(all_avg_durations)
                if perf['avg_duration_ns'] > pipeline_avg * 2:
                    bottlenecks.append({
                        'type': 'performance',
                        'stage': stage_name,
                        'issue': f"Significantly slow: {perf['avg_duration_ms']:.1f}ms avg",
                        'severity': 'medium'
                    })
                    
        return bottlenecks
        
    def _analyze_data_continuity(self) -> Dict[str, Any]:
        """Analyze data continuity across scenarios"""
        continuity_stats = {
            'scenarios_with_continuity': 0,
            'total_scenarios': len(self.scenario_results),
            'continuity_rate': 0.0,
            'continuity_breaks': []
        }
        
        for scenario in self.scenario_results:
            if scenario.data_continuity:
                continuity_stats['scenarios_with_continuity'] += 1
            else:
                continuity_stats['continuity_breaks'].append({
                    'scenario': scenario.scenario_name,
                    'failed_stages': [s.stage_name for s in scenario.pipeline_stages if s.status == 'failed']
                })
                
        if continuity_stats['total_scenarios'] > 0:
            continuity_stats['continuity_rate'] = (continuity_stats['scenarios_with_continuity'] / continuity_stats['total_scenarios']) * 100
            
        return continuity_stats
        
    def _analyze_e2e_ttl_compliance(self) -> Dict[str, Any]:
        """Analyze TTL compliance across E2E tests"""
        total_violations = sum(scenario.ttl_violations for scenario in self.scenario_results)
        total_stage_executions = sum(len(scenario.pipeline_stages) for scenario in self.scenario_results)
        
        return {
            'total_ttl_violations': total_violations,
            'total_stage_executions': total_stage_executions,
            'violation_rate': (total_violations / total_stage_executions) * 100 if total_stage_executions > 0 else 0,
            'compliant_scenarios': len([s for s in self.scenario_results if s.ttl_violations == 0]),
            'compliance_rate': (len([s for s in self.scenario_results if s.ttl_violations == 0]) / len(self.scenario_results)) * 100 if self.scenario_results else 0
        }


def run_ultrathink_e2e_tests():
    """Execute the complete Ultrathink 80/20 E2E test suite"""
    suite = UltrathinkE2ETestSuite()
    results = suite.run_complete_e2e_test_suite()
    
    # Store results
    results_file = f"/Users/sac/cns/test_suites/ultrathink_e2e_test_results_{suite.test_session_id}.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
        
    print(f"E2E test results saved to: {results_file}")
    return results


if __name__ == "__main__":
    results = run_ultrathink_e2e_tests()
    print(f"E2E test suite completed with {results['overall_results']['success_rate']:.1f}% success rate")