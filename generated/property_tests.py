import hypothesis
from hypothesis import given, strategies as st
import unittest

class PropertyBasedTests(unittest.TestCase):
    
    @given(st.integers(min_value=0, max_value=1000000))
    def test_tick_count_increases(self, initial_count):
        """Property: tick count always increases"""
        service = Service()
        service.tick_count = initial_count
        
        old_count = service.tick_count
        service.tick()
        
        assert service.tick_count > old_count
    
    @given(st.binary(min_size=0, max_size=10000))
    def test_input_validation(self, data):
        """Property: service handles any binary input safely"""
        service = Service()
        
        try:
            result = service.process_input(data)
            # Should either process successfully or raise specific exception
            assert result is not None or isinstance(result, ValidationError)
        except ValidationError:
            # Expected for invalid input
            pass
        except Exception as e:
            # Unexpected exception
            pytest.fail(f"Unexpected exception: {e}")
    
    @given(st.lists(st.integers(min_value=1, max_value=100), min_size=1, max_size=1000))
    def test_performance_scaling(self, request_counts):
        """Property: performance scales linearly"""
        service = Service()
        
        times = []
        for count in request_counts:
            start = time.perf_counter()
            for _ in range(count):
                service.process_request()
            elapsed = time.perf_counter() - start
            times.append(elapsed / count)
        
        # Average time per request should be consistent
        avg_time = sum(times) / len(times)
        for t in times:
            assert abs(t - avg_time) / avg_time < 0.2  # Within 20%

if __name__ == '__main__':
    unittest.main()
