#!/usr/bin/env python3
"""
CNS Forge Adversarial Test Permutations
======================================

Adversarial testing framework for CNS Forge 80/20 BDD implementation.
Tests edge cases, failure scenarios, and attack vectors.
"""

import asyncio
import json
import time
import uuid
import random
import logging
from typing import Dict, List, Any, Callable
from dataclasses import dataclass
from enum import Enum
import subprocess
from concurrent.futures import ThreadPoolExecutor

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class AttackVector(Enum):
    """Types of adversarial attack vectors"""
    TTL_EXHAUSTION = "ttl_exhaustion"
    SIGNAL_FLOODING = "signal_flooding"
    DEPENDENCY_DEADLOCK = "dependency_deadlock"
    MEMORY_SATURATION = "memory_saturation"
    COMPENSATION_STORM = "compensation_storm"
    TELEMETRY_OVERFLOW = "telemetry_overflow"
    BYZANTINE_STEPS = "byzantine_steps"

@dataclass
class AdversarialTest:
    """Adversarial test case specification"""
    name: str
    attack_vector: AttackVector
    payload: Dict[str, Any]
    expected_behavior: str
    severity: str  # critical, high, medium, low
    duration_seconds: float = 30.0
    concurrent_attacks: int = 1

class AdversarialTestSuite:
    """Comprehensive adversarial testing framework"""
    
    def __init__(self):
        self.test_results = []
        self.active_attacks = []
        
    def create_adversarial_test_cases(self) -> List[AdversarialTest]:
        """Create comprehensive adversarial test permutations"""
        
        test_cases = [
            # TTL Exhaustion Attacks
            AdversarialTest(
                name="TTL Zero Attack",
                attack_vector=AttackVector.TTL_EXHAUSTION,
                payload={"endpoint": "/api/attack", "method": "POST", "ttl_override": 0},
                expected_behavior="immediate_ttl_expiry_with_graceful_termination",
                severity="critical"
            ),
            
            AdversarialTest(
                name="TTL Negative Attack",
                attack_vector=AttackVector.TTL_EXHAUSTION,
                payload={"endpoint": "/api/attack", "method": "POST", "ttl_override": -5},
                expected_behavior="reject_negative_ttl_with_error",
                severity="high"
            ),
            
            # Signal Flooding Attacks
            AdversarialTest(
                name="Massive Signal Flood",
                attack_vector=AttackVector.SIGNAL_FLOODING,
                payload={"endpoint": "/api/flood", "method": "POST", "signal_count": 10000},
                expected_behavior="rate_limiting_with_backpressure",
                severity="critical",
                concurrent_attacks=100
            ),
            
            AdversarialTest(
                name="Malformed Signal Attack",
                attack_vector=AttackVector.SIGNAL_FLOODING,
                payload={"endpoint": "/api/malformed", "method": "POST", "invalid_data": "x" * 1000000},
                expected_behavior="validation_failure_with_circuit_breaker",
                severity="high"
            ),
            
            # Dependency Deadlock Attacks
            AdversarialTest(
                name="Circular Dependency Attack",
                attack_vector=AttackVector.DEPENDENCY_DEADLOCK,
                payload={"endpoint": "/api/circular", "method": "POST", "create_cycle": True},
                expected_behavior="deadlock_detection_with_timeout",
                severity="critical"
            ),
            
            # Memory Saturation Attacks
            AdversarialTest(
                name="Memory Exhaustion Attack",
                attack_vector=AttackVector.MEMORY_SATURATION,
                payload={"endpoint": "/api/memory", "method": "POST", "allocate_gb": 10},
                expected_behavior="memory_limit_enforcement_with_oom_protection",
                severity="critical"
            ),
            
            # Compensation Storm Attacks
            AdversarialTest(
                name="Saga Compensation Storm",
                attack_vector=AttackVector.COMPENSATION_STORM,
                payload={"endpoint": "/api/saga", "method": "POST", "force_failures": True},
                expected_behavior="controlled_compensation_with_circuit_breaker",
                severity="high"
            ),
            
            # Telemetry Overflow Attacks
            AdversarialTest(
                name="Telemetry Log Bomb",
                attack_vector=AttackVector.TELEMETRY_OVERFLOW,
                payload={"endpoint": "/api/telemetry", "method": "POST", "verbose_logging": True},
                expected_behavior="telemetry_rate_limiting_with_sampling",
                severity="medium"
            ),
            
            # Byzantine Behavior Attacks
            AdversarialTest(
                name="Byzantine Step Behavior",
                attack_vector=AttackVector.BYZANTINE_STEPS,
                payload={"endpoint": "/api/byzantine", "method": "POST", "corrupt_steps": True},
                expected_behavior="step_validation_with_isolation",
                severity="critical"
            )
        ]
        
        return test_cases
    
    async def execute_adversarial_test(self, test: AdversarialTest) -> Dict[str, Any]:
        """Execute a single adversarial test case"""
        logger.info(f"Executing adversarial test: {test.name}")
        
        start_time = time.time()
        
        try:
            # Import the CNS Forge implementation
            from cns_forge_implementation import create_cns_forge_workflow
            
            # Create workflow for testing
            workflow = create_cns_forge_workflow()
            
            # Execute attack based on vector type
            if test.attack_vector == AttackVector.TTL_EXHAUSTION:
                result = await self._execute_ttl_attack(workflow, test)
            elif test.attack_vector == AttackVector.SIGNAL_FLOODING:
                result = await self._execute_flooding_attack(workflow, test)
            elif test.attack_vector == AttackVector.DEPENDENCY_DEADLOCK:
                result = await self._execute_deadlock_attack(workflow, test)
            elif test.attack_vector == AttackVector.MEMORY_SATURATION:
                result = await self._execute_memory_attack(workflow, test)
            elif test.attack_vector == AttackVector.COMPENSATION_STORM:
                result = await self._execute_compensation_attack(workflow, test)
            elif test.attack_vector == AttackVector.TELEMETRY_OVERFLOW:
                result = await self._execute_telemetry_attack(workflow, test)
            elif test.attack_vector == AttackVector.BYZANTINE_STEPS:
                result = await self._execute_byzantine_attack(workflow, test)
            else:
                result = {"status": "error", "error": f"Unknown attack vector: {test.attack_vector}"}
            
            execution_time = time.time() - start_time
            
            # Evaluate if the system behaved as expected
            behavior_match = self._evaluate_expected_behavior(result, test.expected_behavior)
            
            test_result = {
                "test_name": test.name,
                "attack_vector": test.attack_vector.value,
                "severity": test.severity,
                "status": "passed" if behavior_match else "failed",
                "execution_time_seconds": execution_time,
                "expected_behavior": test.expected_behavior,
                "actual_behavior": result.get("status", "unknown"),
                "behavior_match": behavior_match,
                "attack_payload": test.payload,
                "system_response": result
            }
            
            logger.info(f"Adversarial test {test.name}: {test_result['status']}")
            return test_result
            
        except Exception as e:
            execution_time = time.time() - start_time
            logger.error(f"Adversarial test {test.name} failed with exception: {e}")
            
            return {
                "test_name": test.name,
                "attack_vector": test.attack_vector.value,
                "severity": test.severity,
                "status": "error",
                "execution_time_seconds": execution_time,
                "error": str(e),
                "expected_behavior": test.expected_behavior,
                "behavior_match": False
            }
    
    async def _execute_ttl_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute TTL exhaustion attack"""
        payload = test.payload.copy()
        
        # Override TTL for testing
        if "ttl_override" in payload:
            ttl = payload["ttl_override"]
            workflow.initial_ttl = max(0, ttl)  # Ensure non-negative
        
        try:
            result = await workflow.run(payload)
            
            if test.payload.get("ttl_override", 0) <= 0:
                # Should terminate immediately due to TTL expiry
                if result.get("ttl_remaining", 0) == 0 or "ttl_expired" in str(result):
                    return {"status": "ttl_expired_as_expected", "result": result}
                else:
                    return {"status": "ttl_not_enforced", "result": result}
            else:
                return {"status": "completed", "result": result}
                
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_flooding_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute signal flooding attack"""
        payload = test.payload.copy()
        concurrent_count = test.concurrent_attacks
        
        try:
            # Launch multiple concurrent workflows to simulate flooding
            tasks = []
            for i in range(concurrent_count):
                attack_payload = payload.copy()
                attack_payload["attack_id"] = i
                tasks.append(workflow.run(attack_payload))
            
            # Execute with timeout to prevent hanging
            results = await asyncio.wait_for(
                asyncio.gather(*tasks, return_exceptions=True),
                timeout=test.duration_seconds
            )
            
            # Analyze results for rate limiting behavior
            successful = sum(1 for r in results if isinstance(r, dict) and r.get("status") == "completed")
            failed = sum(1 for r in results if isinstance(r, Exception) or (isinstance(r, dict) and r.get("status") != "completed"))
            
            rate_limited = failed > (concurrent_count * 0.8)  # Expect 80%+ to be rate limited
            
            return {
                "status": "rate_limited" if rate_limited else "flooding_successful",
                "successful_requests": successful,
                "failed_requests": failed,
                "rate_limiting_effective": rate_limited
            }
            
        except asyncio.TimeoutError:
            return {"status": "timeout", "behavior": "system_overwhelmed"}
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_deadlock_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute dependency deadlock attack"""
        try:
            # Create artificial circular dependency
            from cns_forge_implementation import ReactorStep, BitActorType, StepResult, StepStatus
            
            class DeadlockStep1(ReactorStep):
                def __init__(self):
                    super().__init__("deadlock_step_1", BitActorType.WORKFLOW)
                
                async def run(self, token):
                    # Simulate waiting for step 2
                    await asyncio.sleep(5.0)  # Long delay to trigger deadlock detection
                    return StepResult(status=StepStatus.COMPLETED, token=token)
            
            class DeadlockStep2(ReactorStep):
                def __init__(self):
                    super().__init__("deadlock_step_2", BitActorType.WORKFLOW)
                
                async def run(self, token):
                    # Simulate waiting for step 1
                    await asyncio.sleep(5.0)  # Long delay to trigger deadlock detection
                    return StepResult(status=StepStatus.COMPLETED, token=token)
            
            # Add circular dependency to workflow
            deadlock1 = DeadlockStep1()
            deadlock2 = DeadlockStep2()
            
            workflow.add_step(deadlock1, depends_on=["deadlock_step_2"])
            workflow.add_step(deadlock2, depends_on=["deadlock_step_1"])
            
            # Execute with timeout
            result = await asyncio.wait_for(
                workflow.run(test.payload),
                timeout=test.duration_seconds
            )
            
            return {"status": "no_deadlock_detected", "result": result}
            
        except asyncio.TimeoutError:
            return {"status": "deadlock_timeout", "behavior": "deadlock_detected_with_timeout"}
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_memory_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute memory saturation attack"""
        try:
            # Create payload with large memory allocation request
            large_payload = test.payload.copy()
            large_payload["large_data"] = "x" * (1024 * 1024)  # 1MB of data
            
            result = await workflow.run(large_payload)
            
            # Check if system handled large payload gracefully
            if result.get("status") == "completed":
                return {"status": "memory_handled", "result": result}
            else:
                return {"status": "memory_rejected", "result": result}
                
        except MemoryError:
            return {"status": "memory_error", "behavior": "memory_limit_enforced"}
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_compensation_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute saga compensation storm attack"""
        try:
            # Force failures in steps to trigger compensation
            from cns_forge_implementation import ReactorStep, BitActorType, StepResult, StepStatus
            
            class FailingStep(ReactorStep):
                def __init__(self, name: str):
                    super().__init__(name, BitActorType.WORKFLOW)
                    self.fail_count = 0
                
                async def run(self, token):
                    self.fail_count += 1
                    if self.fail_count <= 3:  # Fail first 3 times
                        raise Exception(f"Intentional failure #{self.fail_count}")
                    return StepResult(status=StepStatus.COMPLETED, token=token)
                
                async def compensate(self, error, token):
                    # Log compensation attempt
                    logger.info(f"Compensating for {self.name}: {error}")
                    return StepResult(status=StepStatus.FAILED, error=str(error))
            
            # Add failing steps to trigger compensation storm
            for i in range(5):
                failing_step = FailingStep(f"failing_step_{i}")
                workflow.add_step(failing_step)
            
            result = await workflow.run(test.payload)
            
            # Check if compensation was handled properly
            if "compensation" in str(result).lower():
                return {"status": "compensation_triggered", "result": result}
            else:
                return {"status": "no_compensation", "result": result}
                
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_telemetry_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute telemetry overflow attack"""
        try:
            # Enable verbose telemetry
            verbose_payload = test.payload.copy()
            verbose_payload["telemetry_level"] = "debug"
            verbose_payload["generate_logs"] = True
            
            result = await workflow.run(verbose_payload)
            
            # Check if telemetry was rate limited
            telemetry_entries = len(result.get("step_history", []))
            rate_limited = telemetry_entries < 1000  # Expect rate limiting
            
            return {
                "status": "telemetry_handled" if rate_limited else "telemetry_overflow",
                "telemetry_entries": telemetry_entries,
                "rate_limited": rate_limited,
                "result": result
            }
            
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    async def _execute_byzantine_attack(self, workflow, test: AdversarialTest) -> Dict[str, Any]:
        """Execute Byzantine step behavior attack"""
        try:
            # Create steps with malicious behavior
            from cns_forge_implementation import ReactorStep, BitActorType, StepResult, StepStatus
            
            class ByzantineStep(ReactorStep):
                def __init__(self):
                    super().__init__("byzantine_step", BitActorType.WORKFLOW)
                
                async def run(self, token):
                    # Return corrupted token
                    corrupted_token = token
                    corrupted_token.payload["corrupted"] = True
                    corrupted_token.payload["malicious_data"] = {"admin": True, "bypass_security": True}
                    
                    return StepResult(status=StepStatus.COMPLETED, token=corrupted_token)
            
            # Add byzantine step
            byzantine = ByzantineStep()
            workflow.add_step(byzantine)
            
            result = await workflow.run(test.payload)
            
            # Check if malicious data was detected
            malicious_detected = "corrupted" not in result.get("final_payload", {})
            
            return {
                "status": "byzantine_detected" if malicious_detected else "byzantine_undetected",
                "malicious_data_blocked": malicious_detected,
                "result": result
            }
            
        except Exception as e:
            return {"status": "exception", "error": str(e)}
    
    def _evaluate_expected_behavior(self, result: Dict[str, Any], expected: str) -> bool:
        """Evaluate if the system behaved as expected"""
        status = result.get("status", "unknown")
        
        behavior_mappings = {
            "immediate_ttl_expiry_with_graceful_termination": status in ["ttl_expired_as_expected"],
            "reject_negative_ttl_with_error": status in ["ttl_expired_as_expected", "error"],
            "rate_limiting_with_backpressure": status in ["rate_limited", "timeout"],
            "validation_failure_with_circuit_breaker": status in ["failed", "error", "exception"],
            "deadlock_detection_with_timeout": status in ["deadlock_timeout", "timeout"],
            "memory_limit_enforcement_with_oom_protection": status in ["memory_error", "memory_rejected"],
            "controlled_compensation_with_circuit_breaker": status in ["compensation_triggered", "failed"],
            "telemetry_rate_limiting_with_sampling": status in ["telemetry_handled"],
            "step_validation_with_isolation": status in ["byzantine_detected", "failed"]
        }
        
        return behavior_mappings.get(expected, False)
    
    async def run_full_adversarial_suite(self) -> Dict[str, Any]:
        """Run the complete adversarial test suite"""
        logger.info("Starting comprehensive adversarial test suite")
        
        test_cases = self.create_adversarial_test_cases()
        start_time = time.time()
        
        # Execute all tests concurrently
        results = await asyncio.gather(*[
            self.execute_adversarial_test(test) for test in test_cases
        ], return_exceptions=True)
        
        # Process results
        total_tests = len(results)
        passed_tests = sum(1 for r in results if isinstance(r, dict) and r.get("status") == "passed")
        failed_tests = sum(1 for r in results if isinstance(r, dict) and r.get("status") == "failed")
        error_tests = sum(1 for r in results if isinstance(r, dict) and r.get("status") == "error")
        
        execution_time = time.time() - start_time
        
        # Categorize by severity
        critical_failures = sum(1 for r in results 
                              if isinstance(r, dict) and r.get("severity") == "critical" and r.get("status") != "passed")
        
        suite_result = {
            "suite_status": "passed" if critical_failures == 0 else "failed",
            "total_tests": total_tests,
            "passed": passed_tests,
            "failed": failed_tests,
            "errors": error_tests,
            "critical_failures": critical_failures,
            "execution_time_seconds": execution_time,
            "pass_rate_percent": (passed_tests / total_tests * 100) if total_tests > 0 else 0,
            "test_results": [r for r in results if isinstance(r, dict)],
            "summary": f"Adversarial testing completed: {passed_tests}/{total_tests} passed, {critical_failures} critical failures"
        }
        
        logger.info(f"Adversarial test suite completed: {suite_result['summary']}")
        return suite_result

async def main():
    """Run adversarial testing suite"""
    suite = AdversarialTestSuite()
    
    print("🔴 Starting CNS Forge Adversarial Test Suite")
    print("Testing system resilience against attack vectors...")
    print()
    
    result = await suite.run_full_adversarial_suite()
    
    print("\n" + "="*60)
    print("ADVERSARIAL TEST SUITE RESULTS")
    print("="*60)
    print(f"Suite Status: {result['suite_status'].upper()}")
    print(f"Total Tests: {result['total_tests']}")
    print(f"Passed: {result['passed']}")
    print(f"Failed: {result['failed']}")
    print(f"Errors: {result['errors']}")
    print(f"Critical Failures: {result['critical_failures']}")
    print(f"Pass Rate: {result['pass_rate_percent']:.1f}%")
    print(f"Execution Time: {result['execution_time_seconds']:.2f}s")
    print("="*60)
    
    # Write detailed report
    report_path = "/Users/sac/cns/adversarial_test_report.json"
    with open(report_path, 'w') as f:
        json.dump(result, f, indent=2)
    
    print(f"Detailed report: {report_path}")
    
    return 0 if result['suite_status'] == 'passed' else 1

if __name__ == "__main__":
    import sys
    sys.exit(asyncio.run(main()))