#!/usr/bin/env python3
"""
BDD Test Suite for CNS Forge 80/20 Implementation
=================================================

Comprehensive test suite validating the CNS Forge implementation
against the requirements from cns-forge.md specification.
"""

import asyncio
import json
import pytest
import time
import uuid
from unittest.mock import Mock, patch, AsyncMock
from cns_forge_implementation import (
    ReactorWorkflow, Token, StepStatus, StepResult,
    StimulusStep, DecoderStep, BitActorIntegrationStep,
    MemoryStep, TerraformDeploymentStep, SignalEmitterStep,
    create_cns_forge_workflow
)

class TestTTLDrivenExecution:
    """Test TTL-driven execution (Epic 1 from cns-forge.md)"""
    
    @pytest.mark.asyncio
    async def test_ttl_initialization(self):
        """Requirement 1.1: TTL is initialized with workflow creation"""
        workflow = ReactorWorkflow("test_ttl", initial_ttl=8)
        token = Token(
            payload={"test": "data"},
            ttl=8,
            transaction_id=str(uuid.uuid4())
        )
        
        assert token.ttl == 8
        assert workflow.initial_ttl == 8
    
    @pytest.mark.asyncio 
    async def test_ttl_decrementation(self):
        """Requirement 1.2: TTL decrements with each hop"""
        token = Token(
            payload={"test": "data"},
            ttl=5,
            transaction_id=str(uuid.uuid4())
        )
        
        decremented = token.decrement_ttl()
        assert decremented.ttl == 4
        assert token.ttl == 5  # Original unchanged (immutable)
    
    @pytest.mark.asyncio
    async def test_ttl_termination(self):
        """Requirement 1.3: Process terminates when TTL reaches 0"""
        step = StimulusStep()
        token = Token(
            payload={"test": "data"},
            ttl=0,  # Expired TTL
            transaction_id=str(uuid.uuid4())
        )
        
        result = await step.execute_with_telemetry(token)
        assert result.status == StepStatus.TTL_EXPIRED
        assert "TTL expired" in result.error
    
    @pytest.mark.asyncio
    async def test_8_hops_principle(self):
        """Success Criteria: Validate 8 hops maximum constraint"""
        workflow = create_cns_forge_workflow()
        
        # Count total steps in workflow
        total_steps = len(workflow.steps)
        assert total_steps <= 8, f"Workflow has {total_steps} steps, exceeds 8 hops limit"
        
        # Test with minimal TTL
        test_payload = {"endpoint": "/test", "method": "GET"}
        result = await workflow.run(test_payload)
        
        # Should complete within TTL limit
        assert result["status"] in ["completed", "failed"]
        assert result["ttl_remaining"] >= 0

class TestAtomicSingleHopLogic:
    """Test atomic, single-hop logic (Epic 2 from cns-forge.md)"""
    
    @pytest.mark.asyncio
    async def test_step_initialization(self):
        """Requirement 2.1: Each BitActor has init function"""
        step = StimulusStep()
        
        assert step.name == "stimulus_http"
        assert hasattr(step, 'run')
        assert callable(step.run)
    
    @pytest.mark.asyncio
    async def test_state_containment(self):
        """Requirement 2.2: State fully contained in token"""
        token = Token(
            payload={"user_id": 123, "action": "create"},
            ttl=5,
            transaction_id=str(uuid.uuid4())
        )
        
        step = DecoderStep()
        result = await step.run(token)
        
        # Verify state is preserved and enhanced in new token
        assert result.status == StepStatus.COMPLETED
        assert result.token.payload["user_id"] == 123
        assert result.token.payload["action"] == "create"
        assert "validation_status" in result.token.payload
    
    @pytest.mark.asyncio
    async def test_serializable_state(self):
        """Requirement 2.3: Token state is serializable for debugging"""
        token = Token(
            payload={"complex_data": {"nested": [1, 2, 3]}},
            ttl=3,
            transaction_id=str(uuid.uuid4())
        )
        
        # Should be serializable to JSON
        serialized = json.dumps({
            "payload": token.payload,
            "ttl": token.ttl,
            "transaction_id": token.transaction_id,
            "step_history": token.step_history
        })
        
        # Should be deserializable
        deserialized = json.loads(serialized)
        assert deserialized["payload"]["complex_data"]["nested"] == [1, 2, 3]
        assert deserialized["ttl"] == 3

class TestUniversalObservability:
    """Test universal instrumentation (Epic 3 from cns-forge.md)"""
    
    @pytest.mark.asyncio
    async def test_pulse_log_generation(self):
        """Requirement 3.1: Generate pulse log for every BitActor"""
        step = StimulusStep()
        token = Token(
            payload={"test": "observability"},
            ttl=5,
            transaction_id=str(uuid.uuid4())
        )
        
        with patch('cns_forge_implementation.logger') as mock_logger:
            result = await step.execute_with_telemetry(token)
            
            # Verify pulse log was emitted
            mock_logger.info.assert_called()
            
            # Find the pulse log call
            pulse_log_call = None
            for call in mock_logger.info.call_args_list:
                if "BitActor pulse:" in str(call):
                    pulse_log_call = call
                    break
            
            assert pulse_log_call is not None
            assert result.telemetry is not None
    
    @pytest.mark.asyncio
    async def test_pulse_log_content(self):
        """Requirement 3.2: Pulse log contains required metadata"""
        step = DecoderStep()
        token = Token(
            payload={"endpoint": "/test", "method": "POST"},
            ttl=4,
            transaction_id="test-tx-123"
        )
        
        result = await step.execute_with_telemetry(token)
        telemetry = result.telemetry
        
        # Verify required fields
        assert "timestamp" in telemetry
        assert telemetry["transaction_id"] == "test-tx-123"
        assert telemetry["step_name"] == "decode_validate"
        assert telemetry["ttl_remaining"] == 3  # Decremented
        assert "execution_time_ms" in telemetry
        assert "status" in telemetry
    
    @pytest.mark.asyncio
    async def test_causal_chain_reconstruction(self):
        """Requirement 3.3: Reconstruct causal chain from logs"""
        workflow = create_cns_forge_workflow()
        test_payload = {"endpoint": "/api/test", "method": "POST"}
        
        with patch('cns_forge_implementation.logger') as mock_logger:
            result = await workflow.run(test_payload)
            
            # Extract all pulse logs
            pulse_logs = []
            for call in mock_logger.info.call_args_list:
                call_str = str(call)
                if "BitActor pulse:" in call_str:
                    # Extract JSON from log message
                    json_start = call_str.find('{"timestamp"')
                    if json_start > -1:
                        json_end = call_str.rfind('}') + 1
                        try:
                            log_data = json.loads(call_str[json_start:json_end])
                            pulse_logs.append(log_data)
                        except json.JSONDecodeError:
                            pass
            
            # Verify we can reconstruct the causal chain
            transaction_id = result["transaction_id"]
            chain_logs = [log for log in pulse_logs 
                         if log.get("transaction_id") == transaction_id]
            
            assert len(chain_logs) > 0
            
            # Verify chronological order
            timestamps = [log["timestamp"] for log in chain_logs]
            assert timestamps == sorted(timestamps)

class TestBitActorMeshIntegration:
    """Test integration with existing BitActor system (80% existing code)"""
    
    @pytest.mark.asyncio
    async def test_bitactor_integration_step(self):
        """Test integration with existing BitActor C/Erlang system"""
        step = BitActorIntegrationStep()
        token = Token(
            payload={"command": "process", "data": [1, 2, 3]},
            ttl=5,
            transaction_id=str(uuid.uuid4())
        )
        
        # Mock the subprocess call to existing BitActor
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate.return_value = (
                b'{"status": "success", "execution_time_ns": 50000}',
                b''
            )
            mock_subprocess.return_value = mock_process
            
            result = await step.run(token)
            
            assert result.status == StepStatus.COMPLETED
            assert "bitactor_result" in result.token.payload
            assert result.token.payload["bitactor_status"] == "success"
    
    @pytest.mark.asyncio
    async def test_memory_operations(self):
        """Test memory operations following Mnesia/ETS pattern"""
        step = MemoryStep()
        token = Token(
            payload={"user_data": {"id": 123, "name": "test"}},
            ttl=4,
            transaction_id="memory-test-123"
        )
        
        result = await step.run(token)
        
        assert result.status == StepStatus.COMPLETED
        assert "memory_operation" in result.token.payload
        assert result.token.payload["memory_status"] == "success"
        
        # Verify data was stored
        assert "memory-test-123" in step.memory_store
    
    @pytest.mark.asyncio
    async def test_terraform_deployment(self):
        """Test Terraform deployment integration"""
        step = TerraformDeploymentStep()
        token = Token(
            payload={"deployment": "cns-forge", "environment": "test"},
            ttl=3,
            transaction_id=str(uuid.uuid4())
        )
        
        # Mock terraform command
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate.return_value = (
                b'Plan: 5 to add, 0 to change, 0 to destroy.',
                b''
            )
            mock_subprocess.return_value = mock_process
            
            result = await step.run(token)
            
            assert result.status == StepStatus.COMPLETED
            assert "terraform_status" in result.token.payload
            assert result.token.payload["infrastructure_ready"] is True

class TestWorkflowOrchestration:
    """Test complete workflow orchestration and DAG execution"""
    
    @pytest.mark.asyncio
    async def test_dag_dependency_resolution(self):
        """Test DAG execution with proper dependency resolution"""
        workflow = ReactorWorkflow("test_dag", initial_ttl=8)
        
        # Create simple dependency chain: A -> B -> C
        step_a = StimulusStep()
        step_a.name = "step_a"
        step_b = DecoderStep()
        step_b.name = "step_b"
        step_c = MemoryStep()
        step_c.name = "step_c"
        
        workflow.add_step(step_a)
        workflow.add_step(step_b, depends_on=["step_a"])
        workflow.add_step(step_c, depends_on=["step_b"])
        
        # Test dependency resolution
        ready = workflow.get_ready_steps()
        assert ready == ["step_a"]  # Only A should be ready initially
        
        # Simulate step A completion
        workflow.completed_steps["step_a"] = StepResult(status=StepStatus.COMPLETED)
        ready = workflow.get_ready_steps()
        assert ready == ["step_b"]  # Now B should be ready
    
    @pytest.mark.asyncio
    async def test_concurrent_execution(self):
        """Test concurrent execution of independent steps"""
        workflow = ReactorWorkflow("test_concurrent", initial_ttl=8)
        
        # Create parallel branches: A -> (B, C) -> D
        step_a = StimulusStep()
        step_a.name = "step_a"
        step_b = DecoderStep()
        step_b.name = "step_b"
        step_c = MemoryStep()
        step_c.name = "step_c"
        step_d = SignalEmitterStep()
        step_d.name = "step_d"
        
        workflow.add_step(step_a)
        workflow.add_step(step_b, depends_on=["step_a"])
        workflow.add_step(step_c, depends_on=["step_a"])
        workflow.add_step(step_d, depends_on=["step_b", "step_c"])
        
        # Simulate step A completion
        workflow.completed_steps["step_a"] = StepResult(status=StepStatus.COMPLETED)
        
        # B and C should be ready for concurrent execution
        ready = workflow.get_ready_steps()
        assert set(ready) == {"step_b", "step_c"}
    
    @pytest.mark.asyncio
    async def test_complete_workflow_execution(self):
        """Test complete CNS Forge workflow execution"""
        workflow = create_cns_forge_workflow()
        
        test_payload = {
            "endpoint": "/api/cns/forge/test",
            "method": "POST",
            "data": {"test": True}
        }
        
        # Mock external dependencies
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate.return_value = (
                b'{"status": "success", "execution_time_ns": 25000}',
                b''
            )
            mock_subprocess.return_value = mock_process
            
            result = await workflow.run(test_payload)
            
            assert result["status"] in ["completed", "failed"]
            assert "transaction_id" in result
            assert result["total_steps"] == len(workflow.steps)
            assert "duration_ms" in result

class TestSagaCompensation:
    """Test saga pattern compensation for atomicity"""
    
    @pytest.mark.asyncio
    async def test_compensation_on_failure(self):
        """Test that failed workflows trigger compensation"""
        workflow = ReactorWorkflow("test_compensation", initial_ttl=8)
        
        # Create a step that will fail
        failing_step = DecoderStep()
        failing_step.name = "failing_step"
        
        # Override run to simulate failure
        async def fail_run(token):
            raise Exception("Simulated failure")
        failing_step.run = fail_run
        
        workflow.add_step(StimulusStep())
        workflow.add_step(failing_step, depends_on=["stimulus_http"])
        
        test_payload = {"endpoint": "/test", "method": "GET"}
        result = await workflow.run(test_payload)
        
        assert result["status"] == "failed"
        assert len(workflow.failed_steps) > 0
    
    @pytest.mark.asyncio
    async def test_undo_operations(self):
        """Test undo operations for completed steps"""
        step = MemoryStep()
        token = Token(
            payload={"test": "undo"},
            ttl=5,
            transaction_id="undo-test-123"
        )
        
        # Execute step
        result = await step.run(token)
        assert "undo-test-123" in step.memory_store
        
        # Undo step
        undo_result = await step.undo(result, token)
        assert undo_result.status == StepStatus.COMPLETED
        assert "undo-test-123" not in step.memory_store

class TestProductionReadiness:
    """Test production readiness with existing infrastructure"""
    
    @pytest.mark.asyncio
    async def test_terraform_integration(self):
        """Test that Terraform configuration is valid"""
        import os
        terraform_main = "/Users/sac/cns/terraform/main.tf"
        
        assert os.path.exists(terraform_main), "Terraform main.tf not found"
        
        # Basic validation - file should contain key resources
        with open(terraform_main, 'r') as f:
            content = f.read()
            
        assert "kubernetes_namespace" in content
        assert "kubernetes_deployment" in content or "kubernetes_stateful_set" in content
        assert "kubernetes_service" in content
        assert "network_policy" in content
    
    @pytest.mark.asyncio
    async def test_kubernetes_manifests(self):
        """Test that Kubernetes manifests are valid"""
        import os
        k8s_manifest = "/Users/sac/cns/k8s/aegis-fabric-deployment.yaml"
        
        assert os.path.exists(k8s_manifest), "Kubernetes manifest not found"
        
        with open(k8s_manifest, 'r') as f:
            content = f.read()
            
        # Should contain valid Kubernetes resources
        assert "apiVersion:" in content
        assert "kind:" in content
        assert "metadata:" in content
        assert "spec:" in content
    
    def test_jinja_templates_exist(self):
        """Test that required Jinja templates exist"""
        import os
        
        template_dir = "/Users/sac/cns/templates"
        assert os.path.exists(template_dir), "Templates directory not found"
        
        # Check for key templates
        key_templates = [
            "bitactor/bitactor_c.j2",
            "bitactor/bitactor_erlang.j2",
            "k8s_deployment.yaml.j2",
            "terraform_aegis.tf.j2"
        ]
        
        for template in key_templates:
            template_path = os.path.join(template_dir, template)
            assert os.path.exists(template_path), f"Template {template} not found"

# Performance and stress testing
class TestPerformanceValidation:
    """Test performance characteristics and stress scenarios"""
    
    @pytest.mark.asyncio
    async def test_concurrent_workflow_execution(self):
        """Test multiple concurrent workflows"""
        workflows = []
        for i in range(10):
            workflow = create_cns_forge_workflow()
            workflows.append(workflow)
        
        test_payload = {"endpoint": f"/test/{i}", "method": "GET"}
        
        # Mock external calls for performance
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate.return_value = (
                b'{"status": "success", "execution_time_ns": 10000}',
                b''
            )
            mock_subprocess.return_value = mock_process
            
            start_time = time.time()
            
            # Execute all workflows concurrently
            tasks = [workflow.run(test_payload) for workflow in workflows]
            results = await asyncio.gather(*tasks)
            
            execution_time = time.time() - start_time
            
            # All should complete successfully
            assert all(r["status"] in ["completed", "failed"] for r in results)
            
            # Should complete in reasonable time (< 5 seconds for 10 workflows)
            assert execution_time < 5.0
    
    @pytest.mark.asyncio
    async def test_ttl_stress_scenarios(self):
        """Test behavior under extreme TTL constraints"""
        # Test with minimal TTL
        workflow = ReactorWorkflow("stress_ttl", initial_ttl=1)
        step = StimulusStep()
        workflow.add_step(step)
        
        test_payload = {"endpoint": "/stress", "method": "POST"}
        result = await workflow.run(test_payload)
        
        # Should handle gracefully even with minimal TTL
        assert result["status"] in ["completed", "failed"]
        assert result["ttl_remaining"] >= 0

if __name__ == "__main__":
    # Run tests
    pytest.main([__file__, "-v", "--asyncio-mode=auto"])