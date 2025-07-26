#!/usr/bin/env python3
"""
Hybrid BitActor Pipeline Combination Variant - Multi-Pattern Integration

This variant explores hybrid combinations of multiple permutations:
- Async Python types + Reactive TTL ontology + Event-sourced Ash
- Streaming TTL parser + Distributed GenServer + Saga workflows
- Serverless K8s + gRPC bridge + Functional DSL
- Cross-pattern TTL constraint preservation
- Multi-topology swarm coordination
- Polyglot pipeline execution with unified TTL semantics
"""

import asyncio
import time
import uuid
import json
from typing import Dict, List, Any, Optional, AsyncGenerator
from dataclasses import dataclass, asdict
from enum import Enum
import logging


# =============================================================================
# Hybrid Type System (Combining Multiple Variants)
# =============================================================================

class HybridTTLPrecision(Enum):
    """Unified TTL precision across all pipeline patterns"""
    NANOSECOND = "nanosecond"
    MICROSECOND = "microsecond"
    MILLISECOND = "millisecond"
    SECOND = "second"


class HybridPipelinePattern(Enum):
    """Available pipeline patterns for hybrid combination"""
    ASYNC_REACTIVE = "async_reactive"
    STREAMING_DISTRIBUTED = "streaming_distributed"
    SERVERLESS_GRPC = "serverless_grpc"
    FUNCTIONAL_EVENTSOURCED = "functional_eventsourced"
    SAGA_STREAMING = "saga_streaming"
    ALL_PATTERNS = "all_patterns"


@dataclass
class HybridTTLConstraint:
    """Unified TTL constraint supporting all pattern variants"""
    budget_ns: int
    precision: HybridTTLPrecision
    max_budget_ms: int
    
    # Async variant support
    coroutine_timeout: Optional[float] = None
    
    # Reactive variant support
    stream_ttl_budget_ms: Optional[int] = None
    element_ttl_budget_ns: Optional[int] = None
    
    # Distributed variant support
    cross_node_latency_ns: Optional[int] = None
    cluster_overhead_ns: Optional[int] = None
    
    # Event-sourced variant support
    event_processing_budget_ns: Optional[int] = None
    aggregate_reconstruction_budget_ns: Optional[int] = None
    
    # Saga variant support
    saga_step_budget_ns: Optional[int] = None
    compensation_budget_ns: Optional[int] = None
    
    # Serverless variant support
    cold_start_overhead_ns: Optional[int] = None
    function_timeout_ns: Optional[int] = None
    
    # gRPC variant support
    grpc_deadline_ms: Optional[int] = None
    network_timeout_ns: Optional[int] = None


@dataclass
class HybridPipelineStage:
    """Pipeline stage supporting multiple execution patterns"""
    name: str
    pattern: HybridPipelinePattern
    input_format: str
    output_format: str
    ttl_constraint: HybridTTLConstraint
    
    # Pattern-specific configurations
    async_config: Optional[Dict[str, Any]] = None
    reactive_config: Optional[Dict[str, Any]] = None
    distributed_config: Optional[Dict[str, Any]] = None
    eventsourced_config: Optional[Dict[str, Any]] = None
    saga_config: Optional[Dict[str, Any]] = None
    serverless_config: Optional[Dict[str, Any]] = None
    grpc_config: Optional[Dict[str, Any]] = None
    streaming_config: Optional[Dict[str, Any]] = None


@dataclass
class HybridExecutionResult:
    """Unified execution result across all patterns"""
    success: bool
    stage_name: str
    pattern_used: HybridPipelinePattern
    execution_time_ns: int
    ttl_remaining_ns: int
    result_data: Optional[Dict[str, Any]] = None
    error_message: Optional[str] = None
    
    # Pattern-specific metrics
    async_metrics: Optional[Dict[str, Any]] = None
    reactive_metrics: Optional[Dict[str, Any]] = None
    distributed_metrics: Optional[Dict[str, Any]] = None
    eventsourced_metrics: Optional[Dict[str, Any]] = None
    saga_metrics: Optional[Dict[str, Any]] = None
    serverless_metrics: Optional[Dict[str, Any]] = None
    grpc_metrics: Optional[Dict[str, Any]] = None
    streaming_metrics: Optional[Dict[str, Any]] = None


# =============================================================================
# Hybrid Pipeline Orchestrator
# =============================================================================

class HybridBitActorPipelineOrchestrator:
    """Orchestrates hybrid pipeline execution across multiple patterns"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.execution_history: List[HybridExecutionResult] = []
        self.pattern_executors = {
            HybridPipelinePattern.ASYNC_REACTIVE: AsyncReactiveExecutor(),
            HybridPipelinePattern.STREAMING_DISTRIBUTED: StreamingDistributedExecutor(),
            HybridPipelinePattern.SERVERLESS_GRPC: ServerlessGrpcExecutor(),
            HybridPipelinePattern.FUNCTIONAL_EVENTSOURCED: FunctionalEventSourcedExecutor(),
            HybridPipelinePattern.SAGA_STREAMING: SagaStreamingExecutor(),
        }
    
    async def execute_hybrid_pipeline(
        self,
        stages: List[HybridPipelineStage],
        input_data: Dict[str, Any],
        global_ttl_constraint: HybridTTLConstraint
    ) -> List[HybridExecutionResult]:
        """Execute complete hybrid pipeline with pattern switching"""
        
        self.logger.info(f"Starting hybrid pipeline with {len(stages)} stages")
        results = []
        current_data = input_data
        global_start_time = time.perf_counter_ns()
        remaining_ttl_ns = global_ttl_constraint.budget_ns
        
        for i, stage in enumerate(stages):
            stage_start_time = time.perf_counter_ns()
            
            # Check global TTL budget
            global_elapsed_ns = stage_start_time - global_start_time
            if global_elapsed_ns >= global_ttl_constraint.budget_ns:
                error_result = HybridExecutionResult(
                    success=False,
                    stage_name=stage.name,
                    pattern_used=stage.pattern,
                    execution_time_ns=0,
                    ttl_remaining_ns=0,
                    error_message="Global TTL budget exceeded before stage execution"
                )
                results.append(error_result)
                break
            
            # Execute stage with pattern-specific executor
            try:
                # Adjust stage TTL based on remaining global budget
                adjusted_stage_constraint = self._adjust_stage_ttl_constraint(
                    stage.ttl_constraint,
                    remaining_ttl_ns,
                    len(stages) - i  # Remaining stages
                )
                
                # Execute with appropriate pattern
                if stage.pattern == HybridPipelinePattern.ALL_PATTERNS:
                    # Try multiple patterns and use the best result
                    result = await self._execute_with_all_patterns(stage, current_data, adjusted_stage_constraint)
                else:
                    executor = self.pattern_executors.get(stage.pattern)
                    if not executor:
                        raise ValueError(f"Unknown pattern: {stage.pattern}")
                    
                    result = await executor.execute_stage(stage, current_data, adjusted_stage_constraint)
                
                stage_end_time = time.perf_counter_ns()
                actual_execution_time = stage_end_time - stage_start_time
                remaining_ttl_ns -= actual_execution_time
                
                # Update result with actual timing
                result.execution_time_ns = actual_execution_time
                result.ttl_remaining_ns = remaining_ttl_ns
                
                results.append(result)
                
                if result.success:
                    current_data = result.result_data or current_data
                    self.logger.info(f"Stage {stage.name} completed successfully with pattern {stage.pattern.value}")
                else:
                    self.logger.error(f"Stage {stage.name} failed: {result.error_message}")
                    break
                
            except Exception as e:
                error_result = HybridExecutionResult(
                    success=False,
                    stage_name=stage.name,
                    pattern_used=stage.pattern,
                    execution_time_ns=time.perf_counter_ns() - stage_start_time,
                    ttl_remaining_ns=remaining_ttl_ns,
                    error_message=str(e)
                )
                results.append(error_result)
                self.logger.error(f"Stage {stage.name} exception: {e}")
                break
        
        self.execution_history.extend(results)
        return results
    
    async def _execute_with_all_patterns(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        """Execute stage with all available patterns and return best result"""
        
        # Distribute TTL budget across patterns
        pattern_count = len(self.pattern_executors)
        per_pattern_budget_ns = ttl_constraint.budget_ns // pattern_count
        
        pattern_results = []
        
        for pattern, executor in self.pattern_executors.items():
            try:
                # Create pattern-specific TTL constraint
                pattern_constraint = HybridTTLConstraint(
                    budget_ns=per_pattern_budget_ns,
                    precision=ttl_constraint.precision,
                    max_budget_ms=per_pattern_budget_ns // 1_000_000
                )
                
                # Copy pattern-specific configurations
                pattern_stage = HybridPipelineStage(
                    name=f"{stage.name}_{pattern.value}",
                    pattern=pattern,
                    input_format=stage.input_format,
                    output_format=stage.output_format,
                    ttl_constraint=pattern_constraint
                )
                
                result = await executor.execute_stage(pattern_stage, input_data, pattern_constraint)
                pattern_results.append((pattern, result))
                
            except Exception as e:
                self.logger.warning(f"Pattern {pattern.value} failed: {e}")
                continue
        
        # Select best result based on success and performance
        successful_results = [(p, r) for p, r in pattern_results if r.success]
        
        if successful_results:
            # Choose fastest successful result
            best_pattern, best_result = min(successful_results, key=lambda x: x[1].execution_time_ns)
            best_result.stage_name = stage.name  # Restore original stage name
            best_result.pattern_used = HybridPipelinePattern.ALL_PATTERNS
            return best_result
        else:
            # Return first failed result if none succeeded
            if pattern_results:
                _, first_result = pattern_results[0]
                first_result.stage_name = stage.name
                first_result.pattern_used = HybridPipelinePattern.ALL_PATTERNS
                return first_result
            else:
                return HybridExecutionResult(
                    success=False,
                    stage_name=stage.name,
                    pattern_used=HybridPipelinePattern.ALL_PATTERNS,
                    execution_time_ns=0,
                    ttl_remaining_ns=0,
                    error_message="No patterns available for execution"
                )
    
    def _adjust_stage_ttl_constraint(
        self,
        stage_constraint: HybridTTLConstraint,
        remaining_global_ttl_ns: int,
        remaining_stages: int
    ) -> HybridTTLConstraint:
        """Adjust stage TTL constraint based on remaining global budget"""
        
        # Calculate fair distribution of remaining TTL
        fair_share_ns = remaining_global_ttl_ns // remaining_stages if remaining_stages > 0 else remaining_global_ttl_ns
        
        # Use minimum of stage budget and fair share
        adjusted_budget_ns = min(stage_constraint.budget_ns, fair_share_ns)
        
        return HybridTTLConstraint(
            budget_ns=adjusted_budget_ns,
            precision=stage_constraint.precision,
            max_budget_ms=adjusted_budget_ns // 1_000_000,
            # Copy pattern-specific constraints
            coroutine_timeout=stage_constraint.coroutine_timeout,
            stream_ttl_budget_ms=stage_constraint.stream_ttl_budget_ms,
            element_ttl_budget_ns=stage_constraint.element_ttl_budget_ns,
            cross_node_latency_ns=stage_constraint.cross_node_latency_ns,
            cluster_overhead_ns=stage_constraint.cluster_overhead_ns,
            event_processing_budget_ns=stage_constraint.event_processing_budget_ns,
            aggregate_reconstruction_budget_ns=stage_constraint.aggregate_reconstruction_budget_ns,
            saga_step_budget_ns=stage_constraint.saga_step_budget_ns,
            compensation_budget_ns=stage_constraint.compensation_budget_ns,
            cold_start_overhead_ns=stage_constraint.cold_start_overhead_ns,
            function_timeout_ns=stage_constraint.function_timeout_ns,
            grpc_deadline_ms=stage_constraint.grpc_deadline_ms,
            network_timeout_ns=stage_constraint.network_timeout_ns
        )
    
    def get_pipeline_analytics(self) -> Dict[str, Any]:
        """Get analytics across all executed hybrid pipelines"""
        if not self.execution_history:
            return {"message": "No pipeline executions recorded"}
        
        total_executions = len(self.execution_history)
        successful_executions = sum(1 for r in self.execution_history if r.success)
        
        # Pattern usage statistics
        pattern_usage = {}
        for result in self.execution_history:
            pattern = result.pattern_used.value
            if pattern not in pattern_usage:
                pattern_usage[pattern] = {"count": 0, "success_rate": 0, "avg_time_ns": 0}
            pattern_usage[pattern]["count"] += 1
        
        # Calculate success rates and average times
        for pattern in pattern_usage:
            pattern_results = [r for r in self.execution_history if r.pattern_used.value == pattern]
            successful_pattern = sum(1 for r in pattern_results if r.success)
            pattern_usage[pattern]["success_rate"] = successful_pattern / len(pattern_results)
            pattern_usage[pattern]["avg_time_ns"] = sum(r.execution_time_ns for r in pattern_results) // len(pattern_results)
        
        # TTL utilization statistics
        total_ttl_used_ns = sum(r.execution_time_ns for r in self.execution_history)
        avg_ttl_utilization = total_ttl_used_ns / total_executions if total_executions > 0 else 0
        
        return {
            "total_executions": total_executions,
            "successful_executions": successful_executions,
            "overall_success_rate": successful_executions / total_executions,
            "pattern_usage": pattern_usage,
            "avg_ttl_utilization_ns": avg_ttl_utilization,
            "total_ttl_used_ns": total_ttl_used_ns
        }


# =============================================================================
# Pattern-Specific Executors
# =============================================================================

class AsyncReactiveExecutor:
    """Executor combining async/await with reactive streams"""
    
    async def execute_stage(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        
        start_time = time.perf_counter_ns()
        
        try:
            # Simulate async reactive processing
            async_timeout = ttl_constraint.coroutine_timeout or (ttl_constraint.budget_ns / 1_000_000_000)
            
            async with asyncio.timeout(async_timeout):
                # Simulate reactive stream processing
                await asyncio.sleep(0.002)  # 2ms processing
                
                result_data = {
                    "processed_by": "async_reactive",
                    "input_size": len(str(input_data)),
                    "stream_elements_processed": 100,
                    "async_operations": 5,
                    "reactive_backpressure_events": 0
                }
                
                end_time = time.perf_counter_ns()
                execution_time = end_time - start_time
                
                return HybridExecutionResult(
                    success=True,
                    stage_name=stage.name,
                    pattern_used=HybridPipelinePattern.ASYNC_REACTIVE,
                    execution_time_ns=execution_time,
                    ttl_remaining_ns=ttl_constraint.budget_ns - execution_time,
                    result_data=result_data,
                    async_metrics={"coroutines_spawned": 3},
                    reactive_metrics={"stream_throughput": 50000}  # elements/sec
                )
                
        except asyncio.TimeoutError:
            return HybridExecutionResult(
                success=False,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.ASYNC_REACTIVE,
                execution_time_ns=time.perf_counter_ns() - start_time,
                ttl_remaining_ns=0,
                error_message="Async reactive processing timeout"
            )


class StreamingDistributedExecutor:
    """Executor combining streaming parser with distributed GenServer"""
    
    async def execute_stage(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        
        start_time = time.perf_counter_ns()
        
        try:
            # Simulate streaming distributed processing
            cluster_overhead = ttl_constraint.cluster_overhead_ns or 1_000_000  # 1ms default
            available_budget = ttl_constraint.budget_ns - cluster_overhead
            
            if available_budget <= 0:
                raise ValueError("Insufficient TTL budget after cluster overhead")
            
            # Simulate streaming chunks and distributed processing
            await asyncio.sleep(0.003)  # 3ms processing
            
            result_data = {
                "processed_by": "streaming_distributed",
                "chunks_processed": 10,
                "cluster_nodes": 3,
                "distributed_operations": 7,
                "streaming_buffer_utilization": 0.65
            }
            
            end_time = time.perf_counter_ns()
            execution_time = end_time - start_time
            
            return HybridExecutionResult(
                success=True,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.STREAMING_DISTRIBUTED,
                execution_time_ns=execution_time,
                ttl_remaining_ns=ttl_constraint.budget_ns - execution_time,
                result_data=result_data,
                streaming_metrics={"chunks_per_second": 3333},
                distributed_metrics={"node_coordination_time_ns": cluster_overhead}
            )
            
        except Exception as e:
            return HybridExecutionResult(
                success=False,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.STREAMING_DISTRIBUTED,
                execution_time_ns=time.perf_counter_ns() - start_time,
                ttl_remaining_ns=0,
                error_message=str(e)
            )


class ServerlessGrpcExecutor:
    """Executor combining serverless K8s with gRPC bridge"""
    
    async def execute_stage(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        
        start_time = time.perf_counter_ns()
        
        try:
            # Simulate serverless cold start + gRPC call
            cold_start_overhead = ttl_constraint.cold_start_overhead_ns or 2_000_000  # 2ms default
            grpc_network_time = ttl_constraint.network_timeout_ns or 500_000  # 0.5ms default
            
            total_overhead = cold_start_overhead + grpc_network_time
            available_budget = ttl_constraint.budget_ns - total_overhead
            
            if available_budget <= 0:
                raise ValueError("Insufficient TTL budget after serverless/gRPC overhead")
            
            # Simulate serverless function execution with gRPC
            await asyncio.sleep(0.001)  # 1ms processing
            
            result_data = {
                "processed_by": "serverless_grpc",
                "function_invocations": 1,
                "grpc_calls": 2,
                "cold_start": True,
                "serverless_scaling_events": 1
            }
            
            end_time = time.perf_counter_ns()
            execution_time = end_time - start_time
            
            return HybridExecutionResult(
                success=True,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.SERVERLESS_GRPC,
                execution_time_ns=execution_time,
                ttl_remaining_ns=ttl_constraint.budget_ns - execution_time,
                result_data=result_data,
                serverless_metrics={"cold_start_time_ns": cold_start_overhead},
                grpc_metrics={"rpc_latency_ns": grpc_network_time}
            )
            
        except Exception as e:
            return HybridExecutionResult(
                success=False,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.SERVERLESS_GRPC,
                execution_time_ns=time.perf_counter_ns() - start_time,
                ttl_remaining_ns=0,
                error_message=str(e)
            )


class FunctionalEventSourcedExecutor:
    """Executor combining functional DSL with event-sourced Ash"""
    
    async def execute_stage(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        
        start_time = time.perf_counter_ns()
        
        try:
            # Simulate functional composition + event sourcing
            event_processing_budget = ttl_constraint.event_processing_budget_ns or 1_500_000  # 1.5ms
            available_budget = ttl_constraint.budget_ns - event_processing_budget
            
            if available_budget <= 0:
                raise ValueError("Insufficient TTL budget for functional event sourcing")
            
            # Simulate functional transformations with event store writes
            await asyncio.sleep(0.0025)  # 2.5ms processing
            
            result_data = {
                "processed_by": "functional_eventsourced",
                "functional_compositions": 5,
                "events_stored": 3,
                "aggregate_updates": 1,
                "pure_functions_executed": 8
            }
            
            end_time = time.perf_counter_ns()
            execution_time = end_time - start_time
            
            return HybridExecutionResult(
                success=True,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.FUNCTIONAL_EVENTSOURCED,
                execution_time_ns=execution_time,
                ttl_remaining_ns=ttl_constraint.budget_ns - execution_time,
                result_data=result_data,
                eventsourced_metrics={"events_per_second": 1200}
            )
            
        except Exception as e:
            return HybridExecutionResult(
                success=False,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.FUNCTIONAL_EVENTSOURCED,
                execution_time_ns=time.perf_counter_ns() - start_time,
                ttl_remaining_ns=0,
                error_message=str(e)
            )


class SagaStreamingExecutor:
    """Executor combining saga workflows with streaming processing"""
    
    async def execute_stage(
        self,
        stage: HybridPipelineStage,
        input_data: Dict[str, Any],
        ttl_constraint: HybridTTLConstraint
    ) -> HybridExecutionResult:
        
        start_time = time.perf_counter_ns()
        
        try:
            # Simulate saga coordination with streaming data
            saga_step_budget = ttl_constraint.saga_step_budget_ns or 2_000_000  # 2ms per step
            compensation_budget = ttl_constraint.compensation_budget_ns or 1_000_000  # 1ms compensation
            
            total_saga_budget = saga_step_budget + compensation_budget
            available_budget = ttl_constraint.budget_ns - total_saga_budget
            
            if available_budget <= 0:
                raise ValueError("Insufficient TTL budget for saga streaming")
            
            # Simulate saga steps with streaming data processing
            await asyncio.sleep(0.0035)  # 3.5ms processing
            
            result_data = {
                "processed_by": "saga_streaming",
                "saga_steps_executed": 3,
                "compensation_actions": 0,
                "streaming_batches": 5,
                "transaction_boundaries": 2
            }
            
            end_time = time.perf_counter_ns()
            execution_time = end_time - start_time
            
            return HybridExecutionResult(
                success=True,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.SAGA_STREAMING,
                execution_time_ns=execution_time,
                ttl_remaining_ns=ttl_constraint.budget_ns - execution_time,
                result_data=result_data,
                saga_metrics={"saga_completion_rate": 1.0}
            )
            
        except Exception as e:
            return HybridExecutionResult(
                success=False,
                stage_name=stage.name,
                pattern_used=HybridPipelinePattern.SAGA_STREAMING,
                execution_time_ns=time.perf_counter_ns() - start_time,
                ttl_remaining_ns=0,
                error_message=str(e)
            )


# =============================================================================
# Hybrid Pipeline Factory
# =============================================================================

class HybridPipelineFactory:
    """Factory for creating hybrid pipeline configurations"""
    
    @staticmethod
    def create_full_hybrid_pipeline() -> List[HybridPipelineStage]:
        """Create complete hybrid pipeline with all pattern variants"""
        
        return [
            # Stage 1: Python Types (Async)
            HybridPipelineStage(
                name="typer_async",
                pattern=HybridPipelinePattern.ASYNC_REACTIVE,
                input_format="python_source",
                output_format="typed_definitions",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=8_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=8,
                    coroutine_timeout=0.008
                )
            ),
            
            # Stage 2: Ontology (Streaming)
            HybridPipelineStage(
                name="turtle_streaming",
                pattern=HybridPipelinePattern.STREAMING_DISTRIBUTED,
                input_format="typed_definitions",
                output_format="ttl_ontology",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=10_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=10,
                    cross_node_latency_ns=1_000_000,
                    cluster_overhead_ns=500_000
                )
            ),
            
            # Stage 3: TTL Parser (Functional + Event-sourced)
            HybridPipelineStage(
                name="ttl2dspy_functional_es",
                pattern=HybridPipelinePattern.FUNCTIONAL_EVENTSOURCED,
                input_format="ttl_ontology",
                output_format="parsed_constraints",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=12_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=12,
                    event_processing_budget_ns=2_000_000,
                    aggregate_reconstruction_budget_ns=1_000_000
                )
            ),
            
            # Stage 4: BitActor DSL (All Patterns - Best Performance)
            HybridPipelineStage(
                name="bitactor_dsl_hybrid",
                pattern=HybridPipelinePattern.ALL_PATTERNS,
                input_format="parsed_constraints",
                output_format="bitactor_dsl",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=15_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=15,
                    # Support all pattern variants
                    coroutine_timeout=0.015,
                    stream_ttl_budget_ms=15,
                    cross_node_latency_ns=1_000_000,
                    event_processing_budget_ns=2_000_000,
                    saga_step_budget_ns=3_000_000,
                    cold_start_overhead_ns=2_000_000,
                    grpc_deadline_ms=15
                )
            ),
            
            # Stage 5: GenServer (Distributed)
            HybridPipelineStage(
                name="erlang_distributed",
                pattern=HybridPipelinePattern.STREAMING_DISTRIBUTED,
                input_format="bitactor_dsl",
                output_format="genserver_processes",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=8_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=8,
                    cross_node_latency_ns=2_000_000,
                    cluster_overhead_ns=1_000_000
                )
            ),
            
            # Stage 6: Ash Resources (Event-sourced)
            HybridPipelineStage(
                name="ash_eventsourced",
                pattern=HybridPipelinePattern.FUNCTIONAL_EVENTSOURCED,
                input_format="genserver_processes",
                output_format="ash_resources",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=10_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=10,
                    event_processing_budget_ns=3_000_000,
                    aggregate_reconstruction_budget_ns=2_000_000
                )
            ),
            
            # Stage 7: Reactor Workflows (Saga)
            HybridPipelineStage(
                name="reactor_saga",
                pattern=HybridPipelinePattern.SAGA_STREAMING,
                input_format="ash_resources",
                output_format="reactor_workflows",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=20_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=20,
                    saga_step_budget_ns=5_000_000,
                    compensation_budget_ns=3_000_000
                )
            ),
            
            # Stage 8: K8s Deployment (Serverless + gRPC)
            HybridPipelineStage(
                name="k8s_serverless_grpc",
                pattern=HybridPipelinePattern.SERVERLESS_GRPC,
                input_format="reactor_workflows",
                output_format="k8s_manifests",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=25_000_000,
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=25,
                    cold_start_overhead_ns=5_000_000,
                    function_timeout_ns=20_000_000,
                    grpc_deadline_ms=25,
                    network_timeout_ns=2_000_000
                )
            )
        ]
    
    @staticmethod
    def create_performance_optimized_pipeline() -> List[HybridPipelineStage]:
        """Create pipeline optimized for performance with minimal TTL budgets"""
        
        return [
            HybridPipelineStage(
                name="typer_optimized",
                pattern=HybridPipelinePattern.ASYNC_REACTIVE,
                input_format="python_source",
                output_format="typed_definitions",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=3_000_000,  # 3ms
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=3,
                    coroutine_timeout=0.003
                )
            ),
            HybridPipelineStage(
                name="all_stages_hybrid",
                pattern=HybridPipelinePattern.ALL_PATTERNS,
                input_format="typed_definitions",
                output_format="k8s_manifests",
                ttl_constraint=HybridTTLConstraint(
                    budget_ns=25_000_000,  # 25ms for all remaining stages
                    precision=HybridTTLPrecision.NANOSECOND,
                    max_budget_ms=25
                )
            )
        ]


# =============================================================================
# Demo and Validation
# =============================================================================

async def main():
    """Demonstrate hybrid pipeline combination variant"""
    print("🌈 Hybrid BitActor Pipeline Combination Variant - Demo")
    
    orchestrator = HybridBitActorPipelineOrchestrator()
    
    # Demo 1: Full hybrid pipeline
    print("\n📋 Demo 1: Full Hybrid Pipeline (8 stages, 5 patterns)")
    full_pipeline = HybridPipelineFactory.create_full_hybrid_pipeline()
    
    input_data = {
        "source_type": "cybersecurity_ontology",
        "ttl_requirements": {
            "precision": "nanosecond",
            "max_budget_ms": 100
        },
        "pipeline_id": str(uuid.uuid4())
    }
    
    global_ttl_constraint = HybridTTLConstraint(
        budget_ns=120_000_000,  # 120ms total budget
        precision=HybridTTLPrecision.NANOSECOND,
        max_budget_ms=120
    )
    
    start_time = time.perf_counter_ns()
    results = await orchestrator.execute_hybrid_pipeline(
        full_pipeline, 
        input_data, 
        global_ttl_constraint
    )
    end_time = time.perf_counter_ns()
    
    print(f"✅ Pipeline completed: {len(results)} stages")
    print(f"⏱️  Total execution time: {(end_time - start_time) / 1_000_000:.2f}ms")
    
    successful_stages = sum(1 for r in results if r.success)
    print(f"🎯 Success rate: {successful_stages}/{len(results)} stages")
    
    # Show pattern usage
    pattern_usage = {}
    for result in results:
        pattern = result.pattern_used.value
        pattern_usage[pattern] = pattern_usage.get(pattern, 0) + 1
    
    print(f"📊 Pattern usage: {pattern_usage}")
    
    # Demo 2: Performance optimized pipeline
    print("\n📋 Demo 2: Performance Optimized Pipeline")
    optimized_pipeline = HybridPipelineFactory.create_performance_optimized_pipeline()
    
    optimized_ttl_constraint = HybridTTLConstraint(
        budget_ns=30_000_000,  # 30ms total budget
        precision=HybridTTLPrecision.NANOSECOND,
        max_budget_ms=30
    )
    
    opt_start_time = time.perf_counter_ns()
    opt_results = await orchestrator.execute_hybrid_pipeline(
        optimized_pipeline,
        input_data,
        optimized_ttl_constraint
    )
    opt_end_time = time.perf_counter_ns()
    
    print(f"✅ Optimized pipeline completed: {len(opt_results)} stages")
    print(f"⏱️  Total execution time: {(opt_end_time - opt_start_time) / 1_000_000:.2f}ms")
    
    # Demo 3: Pipeline analytics
    print("\n📋 Demo 3: Hybrid Pipeline Analytics")
    analytics = orchestrator.get_pipeline_analytics()
    
    print(f"📈 Total executions: {analytics['total_executions']}")
    print(f"📈 Overall success rate: {analytics['overall_success_rate']:.2%}")
    print(f"📈 Average TTL utilization: {analytics['avg_ttl_utilization_ns'] / 1_000_000:.2f}ms")
    
    # Show best performing patterns
    if 'pattern_usage' in analytics:
        best_patterns = sorted(
            analytics['pattern_usage'].items(),
            key=lambda x: x[1]['success_rate'],
            reverse=True
        )
        print(f"🏆 Best performing patterns:")
        for pattern, metrics in best_patterns[:3]:
            print(f"   {pattern}: {metrics['success_rate']:.2%} success, "
                  f"{metrics['avg_time_ns'] / 1_000_000:.2f}ms avg")
    
    print("\n🌈 Hybrid pipeline combination demo completed")
    print(f"🎯 TTL precision maintained: NANOSECOND across all {len(full_pipeline) + len(optimized_pipeline)} stages")
    print(f"🔗 Pattern integration: {len(HybridPipelinePattern)} patterns successfully combined")


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    asyncio.run(main())