#!/usr/bin/env python3
import unittest
import subprocess
import time

class TestCnsFabricator(unittest.TestCase):
    
    def setUp(self):
        self.service_name = "cns_fabricator"
        
    def test_ttl_compliance(self):
        """Test 8-tick TTL compliance"""
        start = time.perf_counter_ns()
        result = subprocess.run([
            f"./bin/cns_fabricator_test",
            "--workflow", "full",
            "--ttl", "8"
        ], capture_output=True)
        end = time.perf_counter_ns()
        
        self.assertEqual(result.returncode, 0)
        elapsed_ms = (end - start) / 1_000_000
        self.assertLess(elapsed_ms, 1.0, "Exceeded 1ms execution time")
    
    def test_workflow_steps(self):
        """Test individual workflow steps"""
        for step in ['sensor_data_collection', 'anomaly_detection', 'predictive_maintenance', 'resource_optimization', 'quality_control']:
            with self.subTest(step=step):
                result = subprocess.run([
                    f"./bin/cns_fabricator_test",
                    "--step", step
                ], capture_output=True)
                self.assertEqual(result.returncode, 0)

if __name__ == "__main__":
    unittest.main()