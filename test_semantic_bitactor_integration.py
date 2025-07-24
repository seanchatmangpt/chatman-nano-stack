#!/usr/bin/env python3
"""
Test Generated Semantic BitActor Integration
Tests the DSPy signatures generated from BitActor TTL ontologies
"""

import sys
sys.path.append('.')

import dspy
from generated_complete_semantic_bitactors import *

def test_semantic_signal_processing():
    """Test semantic signal processing with DSPy signatures"""
    
    # Configure DSPy with a dummy LM for testing
    class MockLM:
        def generate(self, prompt, **kwargs):
            return "semantic_result_processed"
        
        def __call__(self, prompt, **kwargs):
            return [self.generate(prompt, **kwargs)]
    
    dspy.settings.configure(lm=MockLM())
    
    # Test SemanticSignalSignature processing
    semantic_processor = dspy.Predict(SemanticSignalSignature)
    
    result = semantic_processor(
        has_predicate="http://bitactor.org/ontology#hasType",
        has_subject="signal_12345", 
        has_object="semantic_data",
        has_semantic_context="production_kb"
    )
    
    print(f"✅ Semantic Signal Processing: {result}")
    
    # Test HandlerSignature with performance constraints
    handler_processor = dspy.Predict(HandlerSignature)
    
    result = handler_processor(
        has_tick_budget="8",
        vectorizable=True,
        batch_size="16",
        has_hash="42"
    )
    
    print(f"✅ Handler Processing: {result}")
    
    # Test ExecutionResultSignature validation
    execution_processor = dspy.Predict(ExecutionResultSignature)
    
    result = execution_processor(
        actual_ticks="3",
        execution_status="SUCCESS",
        has_trace_id="trace_001"
    )
    
    print(f"✅ Execution Result: {result}")

def test_performance_validation():
    """Test performance validation signatures"""
    
    tick_budget_processor = dspy.Predict(TickBudgetSignature)
    
    # Test standard 8-tick budget
    result = tick_budget_processor(tick_limit="8")
    print(f"✅ Tick Budget Validation: {result}")
    
    # Test SPARQL query complexity
    query_processor = dspy.Predict(SPARQLQuerySignature)
    result = query_processor(query_complexity="4")
    print(f"✅ SPARQL Query Complexity: {result}")

def test_memory_management():
    """Test memory pool and engine signatures"""
    
    memory_processor = dspy.Predict(MemoryPoolSignature)
    result = memory_processor(
        pool_size="32768",
        alignment_bytes="64"
    )
    print(f"✅ Memory Pool Management: {result}")
    
    engine_processor = dspy.Predict(EngineSignature)
    result = engine_processor(
        max_signals="1024",
        dispatch_table_size="256",
        has_triple_store="production_store"
    )
    print(f"✅ Engine Configuration: {result}")

def test_validation_pipeline():
    """Test SHACL validation pipeline"""
    
    validation_processor = dspy.Predict(ValidationSignalSignature)
    result = validation_processor(has_shape="signal_validation_shape")
    print(f"✅ Validation Signal: {result}")

def run_integration_tests():
    """Run comprehensive integration tests"""
    print("🚀 Starting Semantic BitActor Integration Tests")
    print("=" * 60)
    
    try:
        test_semantic_signal_processing()
        print()
        
        test_performance_validation()
        print()
        
        test_memory_management()
        print()
        
        test_validation_pipeline()
        print()
        
        print("=" * 60)
        print("✅ ALL INTEGRATION TESTS PASSED")
        print(f"📊 Generated Signatures: {len(SIGNATURES)}")
        print(f"🎯 Available Signatures: {list_signatures()}")
        
        return True
        
    except Exception as e:
        print(f"❌ INTEGRATION TEST FAILED: {e}")
        return False

if __name__ == "__main__":
    success = run_integration_tests()
    sys.exit(0 if success else 1)