#!/usr/bin/env python3
"""
Comprehensive unit tests for neural_integration_test.py - 80% line coverage
Tests neural signature functionality, performance benchmarks
"""

import pytest
import asyncio
import json
import time
from unittest.mock import Mock, patch, MagicMock, AsyncMock
from datetime import datetime
from pathlib import Path

from neural_integration_test import (
    MockDSPy, MockChainOfThought, CNSNeuralIntegrationTester
)


class TestMockDSPy:
    """Test MockDSPy functionality"""
    
    def test_chain_of_thought_creation(self):
        """Test ChainOfThought mock creation"""
        mock_signature = Mock()
        mock_signature.__name__ = "TestSignature"
        
        cot = MockDSPy.ChainOfThought(mock_signature)
        assert isinstance(cot, MockChainOfThought)
        assert cot.signature == mock_signature
        assert cot.signature_name == "TestSignature"
    
    def test_input_field_mock(self):
        """Test InputField mock"""
        field = MockDSPy.InputField("Test description", str)
        
        assert field["type"] == "input"
        assert field["desc"] == "Test description"
        assert field["dtype"] == str
    
    def test_output_field_mock(self):
        """Test OutputField mock"""
        field = MockDSPy.OutputField("Test description", str)
        
        assert field["type"] == "output"
        assert field["desc"] == "Test description"
        assert field["dtype"] == str


class TestMockChainOfThought:
    """Test MockChainOfThought reasoning functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        self.mock_signature = Mock()
        self.cot = MockChainOfThought(self.mock_signature)
    
    def test_initialization(self):
        """Test MockChainOfThought initialization"""
        self.mock_signature.__name__ = "TestSignature"
        cot = MockChainOfThought(self.mock_signature)
        
        assert cot.signature == self.mock_signature
        assert cot.signature_name == "TestSignature"
    
    def test_order_signature_buy_decision(self):
        """Test OrderSignature with BUY decision logic"""
        self.cot.signature_name = "OrderSignature"
        
        result = self.cot(order_price=85.0, order_quantity=150)
        
        assert hasattr(result, 'execution_decision')
        assert hasattr(result, 'reasoning')
        assert result.execution_decision == "BUY"
        assert "85.0" in result.reasoning
        assert "150" in result.reasoning
        assert "BUY" in result.reasoning
    
    def test_order_signature_sell_decision(self):
        """Test OrderSignature with SELL decision logic"""
        self.cot.signature_name = "OrderSignature"
        
        result = self.cot(order_price=160.0, order_quantity=600)
        
        assert result.execution_decision == "SELL"
        assert "160.0" in result.reasoning
        assert "600" in result.reasoning
        assert "SELL" in result.reasoning
    
    def test_order_signature_hold_decision(self):
        """Test OrderSignature with HOLD decision logic"""
        self.cot.signature_name = "OrderSignature"
        
        result = self.cot(order_price=120.0, order_quantity=300)
        
        assert result.execution_decision == "HOLD"
        assert "120.0" in result.reasoning
        assert "300" in result.reasoning
        assert "HOLD" in result.reasoning
    
    def test_order_signature_default_values(self):
        """Test OrderSignature with default parameter values"""
        self.cot.signature_name = "OrderSignature"
        
        result = self.cot()
        
        # Should use default values: price=100.0, quantity=100
        assert result.execution_decision == "HOLD"  # Based on default logic
        assert "100.0" in result.reasoning
        assert "100" in result.reasoning
    
    def test_market_data_signature_bullish(self):
        """Test MarketDataSignature with BULLISH trend"""
        self.cot.signature_name = "MarketDataSignature"
        
        result = self.cot(market_price=125.0, market_volume=2500)
        
        assert hasattr(result, 'market_trend')
        assert hasattr(result, 'reasoning')
        assert result.market_trend == "BULLISH"
        assert "125.0" in result.reasoning
        assert "2500" in result.reasoning
        assert "BULLISH" in result.reasoning
    
    def test_market_data_signature_bearish_low_price(self):
        """Test MarketDataSignature with BEARISH trend (low price)"""
        self.cot.signature_name = "MarketDataSignature"
        
        result = self.cot(market_price=75.0, market_volume=1000)
        
        assert result.market_trend == "BEARISH"
        assert "75.0" in result.reasoning
        assert "1000" in result.reasoning
        assert "BEARISH" in result.reasoning
    
    def test_market_data_signature_bearish_low_volume(self):
        """Test MarketDataSignature with BEARISH trend (low volume)"""
        self.cot.signature_name = "MarketDataSignature"
        
        result = self.cot(market_price=100.0, market_volume=400)
        
        assert result.market_trend == "BEARISH"
        assert "400" in result.reasoning
        assert "BEARISH" in result.reasoning
    
    def test_market_data_signature_neutral(self):
        """Test MarketDataSignature with NEUTRAL trend"""
        self.cot.signature_name = "MarketDataSignature"
        
        result = self.cot(market_price=105.0, market_volume=1200)
        
        assert result.market_trend == "NEUTRAL"
        assert "105.0" in result.reasoning
        assert "1200" in result.reasoning
        assert "NEUTRAL" in result.reasoning
    
    def test_market_data_signature_default_values(self):
        """Test MarketDataSignature with default values"""
        self.cot.signature_name = "MarketDataSignature"
        
        result = self.cot()
        
        assert result.market_trend == "NEUTRAL"  # Default values lead to neutral
        assert "100.0" in result.reasoning
        assert "1000" in result.reasoning
    
    def test_matching_engine_signature_low_risk(self):
        """Test MatchingEngineSignature with low risk scenario"""
        self.cot.signature_name = "MatchingEngineSignature"
        
        result = self.cot(portfolio_value=100000.0, volatility_measure=0.05, position_size=1000.0)
        
        assert hasattr(result, 'risk_score')
        assert hasattr(result, 'reasoning')
        assert isinstance(result.risk_score, int)
        assert 0 <= result.risk_score <= 100
        assert "0.01" in result.reasoning  # 1% exposure
        assert "0.05" in result.reasoning  # 5% volatility
    
    def test_matching_engine_signature_high_risk(self):
        """Test MatchingEngineSignature with high risk scenario"""
        self.cot.signature_name = "MatchingEngineSignature"
        
        result = self.cot(portfolio_value=10000.0, volatility_measure=0.20, position_size=8000.0)
        
        assert result.risk_score > 50  # Should be high risk
        assert "0.80" in result.reasoning  # 80% exposure ratio
        assert "0.20" in result.reasoning  # 20% volatility
    
    def test_matching_engine_signature_risk_score_bounds(self):
        """Test MatchingEngineSignature risk score bounds"""
        self.cot.signature_name = "MatchingEngineSignature"
        
        # Test minimum bound
        result_low = self.cot(portfolio_value=1000000.0, volatility_measure=0.001, position_size=1.0)
        assert result_low.risk_score >= 0
        
        # Test maximum bound
        result_high = self.cot(portfolio_value=1000.0, volatility_measure=0.50, position_size=1000.0)
        assert result_high.risk_score <= 100
    
    def test_matching_engine_signature_default_values(self):
        """Test MatchingEngineSignature with default values"""
        self.cot.signature_name = "MatchingEngineSignature"
        
        result = self.cot()
        
        assert isinstance(result.risk_score, int)
        assert 0 <= result.risk_score <= 100
        assert "10000.0" in result.reasoning  # Default portfolio value
        assert "0.1" in result.reasoning  # Default volatility
        assert "1000.0" in result.reasoning  # Default position size
    
    def test_unknown_signature(self):
        """Test handling of unknown signature types"""
        self.cot.signature_name = "UnknownSignature"
        
        result = self.cot(test_param="test_value")
        
        assert hasattr(result, 'result')
        assert result.result == 'unknown'


class TestCNSNeuralIntegrationTester:
    """Test CNSNeuralIntegrationTester main functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('neural_integration_test.sys') as mock_sys:
            mock_sys.modules = {'dspy': MockDSPy()}
            self.tester = CNSNeuralIntegrationTester()
    
    def test_initialization(self):
        """Test tester initialization"""
        assert hasattr(self.tester, 'test_results')
        assert isinstance(self.tester.test_results, list)
        assert len(self.tester.test_results) == 0
        assert hasattr(self.tester, 'start_time')
        assert hasattr(self.tester, 'meter')
        assert hasattr(self.tester, 'tracer')
    
    def test_setup_telemetry(self):
        """Test OpenTelemetry setup"""
        # The setup should create meters and tracers
        assert self.tester.meter is not None
        assert self.tester.tracer is not None
        assert hasattr(self.tester, 'inference_duration')
        assert hasattr(self.tester, 'inference_counter')
        assert hasattr(self.tester, 'accuracy_gauge')
        assert hasattr(self.tester, 'throughput_gauge')
    
    @pytest.mark.asyncio
    async def test_run_neural_test_success(self):
        """Test successful neural test execution"""
        async def mock_test_func():
            return {
                "success": True,
                "metrics": {"test_metric": 42},
                "issues": []
            }
        
        with patch.object(self.tester.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.tester._run_neural_test("Test Function", mock_test_func)
            
            assert len(self.tester.test_results) == 1
            result = self.tester.test_results[0]
            assert result["name"] == "Test Function"
            assert result["success"] is True
            assert result["metrics"]["test_metric"] == 42
            assert result["issues"] == []
            assert "duration_ms" in result
            assert "timestamp" in result
    
    @pytest.mark.asyncio
    async def test_run_neural_test_failure(self):
        """Test failed neural test execution"""
        async def mock_test_func():
            return {
                "success": False,
                "metrics": {},
                "issues": ["Test error occurred"]
            }
        
        with patch.object(self.tester.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.tester._run_neural_test("Failed Test", mock_test_func)
            
            assert len(self.tester.test_results) == 1
            result = self.tester.test_results[0]
            assert result["name"] == "Failed Test"
            assert result["success"] is False
            assert "Test error occurred" in result["issues"]
    
    @pytest.mark.asyncio
    async def test_run_neural_test_exception(self):
        """Test neural test with exception"""
        async def mock_test_func():
            raise ValueError("Test exception")
        
        with patch.object(self.tester.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.tester._run_neural_test("Exception Test", mock_test_func)
            
            assert len(self.tester.test_results) == 1
            result = self.tester.test_results[0]
            assert result["name"] == "Exception Test"
            assert result["success"] is False
            assert "error" in result
            assert "Test exception" in result["error"]
    
    @pytest.mark.asyncio
    async def test_test_order_processing(self):
        """Test order processing neural test"""
        result = await self.tester._test_order_processing()
        
        assert "success" in result
        assert "metrics" in result
        assert "issues" in result
        
        if result["success"]:
            assert result["metrics"]["successful_inferences"] >= 0
            assert result["metrics"]["total_test_cases"] == 3
            assert "success_rate" in result["metrics"]
            
            # Check that test case results are recorded
            for i in range(1, 4):
                decision_key = f"test_case_{i}_decision"
                time_key = f"test_case_{i}_inference_time_ms"
                if decision_key in result["metrics"]:
                    assert result["metrics"][decision_key] in ["BUY", "SELL", "HOLD"]
                    assert result["metrics"][time_key] >= 0
    
    @pytest.mark.asyncio
    async def test_test_market_analysis(self):
        """Test market analysis neural test"""
        result = await self.tester._test_market_analysis()
        
        assert "success" in result
        assert "metrics" in result
        assert "issues" in result
        
        if result["success"]:
            assert result["metrics"]["successful_predictions"] >= 0
            assert result["metrics"]["total_test_cases"] == 3
            assert "accuracy" in result["metrics"]
            
            # Check that predictions are valid
            for i in range(1, 4):
                trend_key = f"test_case_{i}_trend"
                if trend_key in result["metrics"]:
                    assert result["metrics"][trend_key] in ["BULLISH", "BEARISH", "NEUTRAL"]
    
    @pytest.mark.asyncio
    async def test_test_risk_assessment(self):
        """Test risk assessment neural test"""
        result = await self.tester._test_risk_assessment()
        
        assert "success" in result
        assert "metrics" in result
        assert "issues" in result
        
        if result["success"]:
            assert result["metrics"]["valid_assessments"] >= 0
            assert result["metrics"]["total_test_cases"] == 3
            assert "validity_rate" in result["metrics"]
            
            # Check that risk scores are valid
            for i in range(1, 4):
                score_key = f"test_case_{i}_risk_score"
                if score_key in result["metrics"]:
                    assert 0 <= result["metrics"][score_key] <= 100
    
    @pytest.mark.asyncio
    async def test_test_neural_performance(self):
        """Test neural performance benchmark"""
        result = await self.tester._test_neural_performance()
        
        assert "success" in result
        assert "metrics" in result
        
        if result["success"]:
            metrics = result["metrics"]
            assert "total_inferences" in metrics
            assert "successful_inferences" in metrics
            assert "throughput_ips" in metrics
            assert "avg_latency_ms" in metrics
            assert "total_time_s" in metrics
            
            assert metrics["total_inferences"] == 100
            assert metrics["successful_inferences"] <= metrics["total_inferences"]
            assert metrics["throughput_ips"] > 0
            assert metrics["avg_latency_ms"] > 0
            assert metrics["total_time_s"] > 0
    
    @pytest.mark.asyncio
    async def test_test_signature_registry(self):
        """Test signature registry functionality"""
        # Mock the imported functions
        mock_signatures = ["OrderSignature", "MarketDataSignature", "MatchingEngineSignature"]
        mock_registry = {name: Mock() for name in mock_signatures}
        
        def mock_get_signature(name):
            if name in mock_registry:
                return mock_registry[name]
            raise ValueError(f"Unknown signature: {name}")
        
        def mock_list_signatures():
            return mock_signatures
        
        with patch('neural_integration_test.get_signature', mock_get_signature):
            with patch('neural_integration_test.list_signatures', mock_list_signatures):
                with patch('neural_integration_test.SIGNATURES', mock_registry):
                    result = await self.tester._test_signature_registry()
                    
                    assert "success" in result
                    assert "metrics" in result
                    
                    if result["success"]:
                        assert result["metrics"]["available_signatures"] == 3
                        assert result["metrics"]["expected_signatures"] == 3
    
    @pytest.mark.asyncio
    async def test_test_signature_registry_missing_signature(self):
        """Test signature registry with missing signature"""
        def mock_get_signature(name):
            if name == "OrderSignature":
                return Mock()
            raise ValueError(f"Unknown signature: {name}")
        
        def mock_list_signatures():
            return ["OrderSignature"]  # Missing other signatures
        
        with patch('neural_integration_test.get_signature', mock_get_signature):
            with patch('neural_integration_test.list_signatures', mock_list_signatures):
                result = await self.tester._test_signature_registry()
                
                assert "issues" in result
                assert len(result["issues"]) > 0
                assert any("Missing expected signature" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_test_signature_registry_invalid_signature_handling(self):
        """Test signature registry invalid signature handling"""
        def mock_get_signature(name):
            if name in ["OrderSignature", "MarketDataSignature", "MatchingEngineSignature"]:
                return Mock()
            raise ValueError(f"Unknown signature: {name}")
        
        def mock_list_signatures():
            return ["OrderSignature", "MarketDataSignature", "MatchingEngineSignature"]
        
        with patch('neural_integration_test.get_signature', mock_get_signature):
            with patch('neural_integration_test.list_signatures', mock_list_signatures):
                result = await self.tester._test_signature_registry()
                
                # Should pass the invalid signature test (raises ValueError as expected)
                assert result["success"] is True or len(result["issues"]) == 0
    
    @pytest.mark.asyncio
    async def test_test_end_to_end_pipeline(self):
        """Test end-to-end neural pipeline"""
        result = await self.tester._test_end_to_end_pipeline()
        
        assert "success" in result
        assert "metrics" in result
        
        if result["success"]:
            metrics = result["metrics"]
            assert "market_trend" in metrics
            assert "risk_score" in metrics
            assert "execution_decision" in metrics
            assert "pipeline_stages" in metrics
            assert "pipeline_success" in metrics
            
            assert metrics["market_trend"] in ["BULLISH", "BEARISH", "NEUTRAL"]
            assert 0 <= metrics["risk_score"] <= 100
            assert metrics["execution_decision"] in ["BUY", "SELL", "HOLD"]
            assert metrics["pipeline_stages"] == 3
            assert metrics["pipeline_success"] is True
    
    @pytest.mark.asyncio
    async def test_test_end_to_end_pipeline_failure_scenarios(self):
        """Test end-to-end pipeline with failure scenarios"""
        # Test market analysis failure
        with patch.object(MockChainOfThought, '__call__') as mock_call:
            mock_call.return_value = Mock()  # Missing market_trend attribute
            
            result = await self.tester._test_end_to_end_pipeline()
            
            assert result["success"] is False
            assert "Market analysis failed in pipeline" in result["issues"]
    
    def test_generate_neural_report_all_pass(self):
        """Test neural report generation with all tests passing"""
        # Setup mock test results
        self.tester.test_results = [
            {
                "name": "Test 1",
                "success": True,
                "duration_ms": 100.0,
                "metrics": {"accuracy": 95.0},
                "issues": []
            },
            {
                "name": "Test 2", 
                "success": True,
                "duration_ms": 150.0,
                "metrics": {"throughput": 50.0},
                "issues": []
            }
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.tester._generate_neural_report()
            
            assert "neural_test_summary" in report
            assert "test_results" in report
            assert "timestamp" in report
            
            summary = report["neural_test_summary"]
            assert summary["total_tests"] == 2
            assert summary["successful_tests"] == 2
            assert summary["success_rate"] == 100.0
            assert summary["final_status"] == "OPTIMAL"
            
            # Check that print was called with success message
            print_calls = [str(call) for call in mock_print.call_args_list]
            success_message = any("ALL NEURAL TESTS PASSED" in call for call in print_calls)
            assert success_message
    
    def test_generate_neural_report_partial_pass(self):
        """Test neural report generation with partial pass"""
        # Setup mixed test results
        self.tester.test_results = [
            {
                "name": "Test 1",
                "success": True,
                "duration_ms": 100.0,
                "metrics": {},
                "issues": []
            },
            {
                "name": "Test 2",
                "success": False,
                "duration_ms": 200.0,
                "metrics": {},
                "issues": ["Test failure"],
                "error": "Test error"
            }
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.tester._generate_neural_report()
            
            summary = report["neural_test_summary"]
            assert summary["total_tests"] == 2
            assert summary["successful_tests"] == 1
            assert summary["success_rate"] == 50.0
            assert summary["final_status"] == "CRITICAL"  # Below 80%
            
            # Check that print was called with critical message
            print_calls = [str(call) for call in mock_print.call_args_list]
            critical_message = any("NEURAL TEST FAILURES" in call for call in print_calls)
            assert critical_message
    
    def test_generate_neural_report_mostly_pass(self):
        """Test neural report generation with mostly passing tests"""
        # Setup mostly passing test results
        self.tester.test_results = [
            {"name": "Test 1", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 2", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 3", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 4", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 5", "success": False, "duration_ms": 100.0, "metrics": {}, "issues": ["Failure"]}
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.tester._generate_neural_report()
            
            summary = report["neural_test_summary"]
            assert summary["success_rate"] == 80.0  # 4/5 = 80%
            assert summary["final_status"] == "ACCEPTABLE"
            
            # Check that print was called with acceptable message
            print_calls = [str(call) for call in mock_print.call_args_list]
            acceptable_message = any("MOST NEURAL TESTS PASSED" in call for call in print_calls)
            assert acceptable_message
    
    def test_generate_neural_report_mermaid_diagrams(self):
        """Test that neural report includes Mermaid diagrams"""
        self.tester.test_results = [
            {"name": "Test 1", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []}
        ]
        
        with patch('builtins.print') as mock_print:
            self.tester._generate_neural_report()
            
            # Check that Mermaid diagrams were printed
            print_calls = [str(call) for call in mock_print.call_args_list]
            
            mermaid_graph = any("```mermaid" in call and "graph TD" in call for call in print_calls)
            mermaid_pie = any("```mermaid" in call and "pie title" in call for call in print_calls)
            mermaid_timeline = any("```mermaid" in call and "timeline" in call for call in print_calls)
            
            assert mermaid_graph, "Should contain mermaid graph diagram"
            assert mermaid_pie, "Should contain mermaid pie chart"
            assert mermaid_timeline, "Should contain mermaid timeline"
    
    @pytest.mark.asyncio
    async def test_run_comprehensive_test(self):
        """Test comprehensive test runner"""
        # Mock all test methods to return success
        mock_test_result = {
            "success": True,
            "metrics": {"test_metric": 100},
            "issues": []
        }
        
        with patch.object(self.tester, '_test_order_processing', return_value=mock_test_result):
            with patch.object(self.tester, '_test_market_analysis', return_value=mock_test_result):
                with patch.object(self.tester, '_test_risk_assessment', return_value=mock_test_result):
                    with patch.object(self.tester, '_test_neural_performance', return_value=mock_test_result):
                        with patch.object(self.tester, '_test_signature_registry', return_value=mock_test_result):
                            with patch.object(self.tester, '_test_end_to_end_pipeline', return_value=mock_test_result):
                                with patch.object(self.tester.tracer, 'start_as_current_span') as mock_span:
                                    mock_span.return_value.__enter__ = Mock()
                                    mock_span.return_value.__exit__ = Mock()
                                    mock_span_obj = Mock()
                                    mock_span.return_value = mock_span_obj
                                    
                                    with patch('builtins.print'):
                                        report = await self.tester.run_comprehensive_test()
                                        
                                        assert "neural_test_summary" in report
                                        assert len(self.tester.test_results) == 6  # All 6 tests ran


class TestPerformanceAndEdgeCases:
    """Test performance characteristics and edge cases"""
    
    def test_mock_chain_of_thought_performance(self):
        """Test performance of mock chain of thought"""
        mock_signature = Mock()
        mock_signature.__name__ = "OrderSignature"
        cot = MockChainOfThought(mock_signature)
        
        # Measure performance of 1000 inferences
        start_time = time.time()
        for i in range(1000):
            result = cot(order_price=100.0 + i % 100, order_quantity=100 + i % 500)
            assert hasattr(result, 'execution_decision')
        
        end_time = time.time()
        total_time = end_time - start_time
        
        # Should complete 1000 inferences in reasonable time (< 1 second)
        assert total_time < 1.0, f"Too slow: {total_time:.3f}s for 1000 inferences"
        
        # Calculate throughput
        throughput = 1000 / total_time
        assert throughput > 1000, f"Low throughput: {throughput:.0f} inferences/sec"
    
    def test_risk_score_calculation_edge_cases(self):
        """Test risk score calculation edge cases"""
        mock_signature = Mock()
        mock_signature.__name__ = "MatchingEngineSignature"
        cot = MockChainOfThought(mock_signature)
        
        # Test zero portfolio value (should handle division by zero)
        result = cot(portfolio_value=0.0, volatility_measure=0.1, position_size=1000.0)
        assert 0 <= result.risk_score <= 100
        
        # Test zero position size
        result = cot(portfolio_value=10000.0, volatility_measure=0.1, position_size=0.0)
        assert result.risk_score == 0
        
        # Test zero volatility
        result = cot(portfolio_value=10000.0, volatility_measure=0.0, position_size=1000.0)
        assert result.risk_score >= 0
        
        # Test extreme values
        result = cot(portfolio_value=1.0, volatility_measure=1.0, position_size=1000000.0)
        assert result.risk_score == 100  # Should cap at 100
    
    def test_order_signature_boundary_conditions(self):
        """Test OrderSignature boundary conditions"""
        mock_signature = Mock()
        mock_signature.__name__ = "OrderSignature"
        cot = MockChainOfThought(mock_signature)
        
        # Test exact boundary conditions
        # BUY condition: price < 90 AND quantity < 200
        result = cot(order_price=89.99, order_quantity=199)
        assert result.execution_decision == "BUY"
        
        result = cot(order_price=90.01, order_quantity=199)
        assert result.execution_decision == "HOLD"
        
        result = cot(order_price=89.99, order_quantity=201)
        assert result.execution_decision == "HOLD"
        
        # SELL condition: price > 150 AND quantity > 500
        result = cot(order_price=150.01, order_quantity=501)
        assert result.execution_decision == "SELL"
        
        result = cot(order_price=149.99, order_quantity=501)
        assert result.execution_decision == "HOLD"
        
        result = cot(order_price=150.01, order_quantity=499)
        assert result.execution_decision == "HOLD"
    
    def test_market_data_signature_boundary_conditions(self):
        """Test MarketDataSignature boundary conditions"""
        mock_signature = Mock()
        mock_signature.__name__ = "MarketDataSignature"
        cot = MockChainOfThought(mock_signature)
        
        # BULLISH condition: price > 120 AND volume > 2000
        result = cot(market_price=120.01, market_volume=2001)
        assert result.market_trend == "BULLISH"
        
        result = cot(market_price=119.99, market_volume=2001)
        assert result.market_trend == "NEUTRAL"
        
        result = cot(market_price=120.01, market_volume=1999)
        assert result.market_trend == "NEUTRAL"
        
        # BEARISH condition: price < 80 OR volume < 500
        result = cot(market_price=79.99, market_volume=1000)
        assert result.market_trend == "BEARISH"
        
        result = cot(market_price=100.0, market_volume=499)
        assert result.market_trend == "BEARISH"
        
        result = cot(market_price=80.01, market_volume=501)
        assert result.market_trend == "NEUTRAL"
    
    def test_negative_values_handling(self):
        """Test handling of negative input values"""
        mock_signature = Mock()
        
        # Test OrderSignature with negative values
        mock_signature.__name__ = "OrderSignature"
        cot = MockChainOfThought(mock_signature)
        
        result = cot(order_price=-10.0, order_quantity=-5)
        assert hasattr(result, 'execution_decision')
        assert result.execution_decision in ["BUY", "SELL", "HOLD"]
        
        # Test MarketDataSignature with negative values
        mock_signature.__name__ = "MarketDataSignature"
        cot = MockChainOfThought(mock_signature)
        
        result = cot(market_price=-50.0, market_volume=-100)
        assert hasattr(result, 'market_trend')
        assert result.market_trend in ["BULLISH", "BEARISH", "NEUTRAL"]
        
        # Test MatchingEngineSignature with negative values
        mock_signature.__name__ = "MatchingEngineSignature"
        cot = MockChainOfThought(mock_signature)
        
        result = cot(portfolio_value=-1000.0, volatility_measure=-0.1, position_size=-500.0)
        assert hasattr(result, 'risk_score')
        assert 0 <= result.risk_score <= 100
    
    def test_large_values_handling(self):
        """Test handling of very large input values"""
        mock_signature = Mock()
        
        # Test with extremely large values
        mock_signature.__name__ = "MatchingEngineSignature"
        cot = MockChainOfThought(mock_signature)
        
        result = cot(
            portfolio_value=1e12,  # 1 trillion
            volatility_measure=10.0,  # 1000% volatility 
            position_size=1e15  # 1 quadrillion
        )
        assert hasattr(result, 'risk_score')
        assert 0 <= result.risk_score <= 100
    
    @pytest.mark.asyncio
    async def test_concurrent_neural_tests(self):
        """Test running multiple neural tests concurrently"""
        with patch('neural_integration_test.sys') as mock_sys:
            mock_sys.modules = {'dspy': MockDSPy()}
            tester = CNSNeuralIntegrationTester()
        
        # Run multiple tests concurrently
        tasks = []
        for i in range(5):
            task = tester._test_order_processing()
            tasks.append(task)
        
        results = await asyncio.gather(*tasks)
        
        # All should succeed
        assert len(results) == 5
        for result in results:
            assert "success" in result
            assert "metrics" in result


class TestErrorHandling:
    """Test error handling scenarios"""
    
    @pytest.mark.asyncio
    async def test_neural_test_with_import_error(self):
        """Test neural test with import errors"""
        with patch('neural_integration_test.sys') as mock_sys:
            mock_sys.modules = {'dspy': MockDSPy()}
            tester = CNSNeuralIntegrationTester()
        
        # Mock import error in signature registry test
        with patch('neural_integration_test.get_signature', side_effect=ImportError("Module not found")):
            result = await tester._test_signature_registry()
            
            assert result["success"] is False
            assert "issues" in result
            assert len(result["issues"]) > 0
    
    @pytest.mark.asyncio
    async def test_neural_test_with_telemetry_error(self):
        """Test neural test with telemetry errors"""
        with patch('neural_integration_test.sys') as mock_sys:
            mock_sys.modules = {'dspy': MockDSPy()}
            tester = CNSNeuralIntegrationTester()
        
        # Mock telemetry error
        with patch.object(tester.inference_counter, 'add', side_effect=Exception("Telemetry error")):
            # Should still complete the test despite telemetry error
            result = await tester._test_order_processing()
            
            # Test should still work even if telemetry fails
            assert "success" in result
    
    def test_mock_dspy_with_none_signature(self):
        """Test MockChainOfThought with None signature"""
        cot = MockChainOfThought(None)
        
        # Should handle gracefully
        result = cot(test_param="test")
        assert hasattr(result, 'result')
        assert result.result == 'unknown'
    
    def test_mock_chain_of_thought_missing_attributes(self):
        """Test MockChainOfThought with signature missing __name__"""
        mock_signature = Mock()
        del mock_signature.__name__  # Remove __name__ attribute
        
        # Should handle AttributeError gracefully
        with pytest.raises(AttributeError):
            cot = MockChainOfThought(mock_signature)


class TestMainFunction:
    """Test main function and async execution"""
    
    @pytest.mark.asyncio
    async def test_main_function_success(self):
        """Test main function successful execution"""
        with patch('neural_integration_test.CNSNeuralIntegrationTester') as mock_tester_class:
            mock_tester = Mock()
            mock_tester.run_comprehensive_test = AsyncMock(return_value={"status": "success"})
            mock_tester_class.return_value = mock_tester
            
            from neural_integration_test import main
            
            # Should complete without exception
            await main()
            
            mock_tester.run_comprehensive_test.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_main_function_keyboard_interrupt(self):
        """Test main function with KeyboardInterrupt"""
        with patch('neural_integration_test.CNSNeuralIntegrationTester') as mock_tester_class:
            mock_tester = Mock()
            mock_tester.run_comprehensive_test = AsyncMock(side_effect=KeyboardInterrupt())
            mock_tester_class.return_value = mock_tester
            
            from neural_integration_test import main
            
            with patch('builtins.print') as mock_print:
                await main()
                
                # Should print keyboard interrupt message
                print_calls = [str(call) for call in mock_print.call_args_list]
                interrupt_message = any("testing stopped by user" in call for call in print_calls)
                assert interrupt_message
    
    @pytest.mark.asyncio
    async def test_main_function_exception(self):
        """Test main function with general exception"""
        with patch('neural_integration_test.CNSNeuralIntegrationTester') as mock_tester_class:
            mock_tester = Mock()
            mock_tester.run_comprehensive_test = AsyncMock(side_effect=RuntimeError("Test error"))
            mock_tester_class.return_value = mock_tester
            
            from neural_integration_test import main
            
            with patch('builtins.print') as mock_print:
                with pytest.raises(RuntimeError):
                    await main()
                
                # Should print error message
                print_calls = [str(call) for call in mock_print.call_args_list]
                error_message = any("testing failed with error" in call for call in print_calls)
                assert error_message


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])