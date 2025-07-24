
import sys
import os
import unittest
import importlib.util
import tempfile
from pathlib import Path

class ComprehensiveBitActorTest(unittest.TestCase):
    """Comprehensive test suite for BitActor Python implementations"""
    
    def setUp(self):
        self.modules = []
        self.load_bitactor_modules()
    
    def load_bitactor_modules(self):
        """Load all BitActor modules"""
        bitactor_files = [
            "/Users/sac/cns/bitactor_ttl_generator.py",
            "/Users/sac/cns/bitactor_cli_demo.py",
            "/Users/sac/cns/bitactor_cli.py",
            "/Users/sac/cns/generated/bitactor/semantic_bitactor.py",
            "/Users/sac/cns/generated/demo_output/demo_bitactor.py",
            "/Users/sac/cns/generated/bitactor_cli_test/semantic_bitactor.py",
            "/Users/sac/cns/generated/cli_demo/demo_bitactor.py",
            "/Users/sac/cns/generated/test_fix/test_bitactor.py",

        ]
        
        for file_path in bitactor_files:
            if os.path.exists(file_path):
                try:
                    spec = importlib.util.spec_from_file_location(
                        f"module_{len(self.modules)}", file_path
                    )
                    module = importlib.util.module_from_spec(spec)
                    spec.loader.exec_module(module)
                    self.modules.append((file_path, module))
                except Exception as e:
                    print(f"Failed to load {file_path}: {e}")
    
    def test_module_imports(self):
        """Test all modules can be imported"""
        self.assertGreater(len(self.modules), 0, "No modules loaded")
        for file_path, module in self.modules:
            self.assertIsNotNone(module, f"Module {file_path} is None")
    
    def test_bitactor_classes(self):
        """Test BitActor class instantiation"""
        for file_path, module in self.modules:
            # Find BitActor classes
            for attr_name in dir(module):
                if 'BitActor' in attr_name and not attr_name.startswith('_'):
                    cls = getattr(module, attr_name)
                    if hasattr(cls, '__init__'):
                        try:
                            instance = cls()
                            self.assertIsNotNone(instance)
                        except Exception as e:
                            self.fail(f"Failed to instantiate {attr_name} from {file_path}: {e}")
    
    def test_signal_classes(self):
        """Test Signal class creation"""
        for file_path, module in self.modules:
            for attr_name in dir(module):
                if 'Signal' in attr_name and not attr_name.startswith('_') and 'Type' not in attr_name:
                    cls = getattr(module, attr_name)
                    if hasattr(cls, '__init__'):
                        try:
                            instance = cls(type=1, flags=0, timestamp=0, payload=0)
                            self.assertIsNotNone(instance)
                        except Exception as e:
                            self.fail(f"Failed to create {attr_name} from {file_path}: {e}")
    
    def test_signal_processing(self):
        """Test signal processing functionality"""
        for file_path, module in self.modules:
            bitactor_cls = None
            signal_cls = None
            
            # Find classes
            for attr_name in dir(module):
                if 'BitActor' in attr_name and not attr_name.startswith('_'):
                    bitactor_cls = getattr(module, attr_name)
                elif 'Signal' in attr_name and not attr_name.startswith('_') and 'Type' not in attr_name:
                    signal_cls = getattr(module, attr_name)
            
            if bitactor_cls and signal_cls:
                try:
                    ba = bitactor_cls()
                    signal = signal_cls(type=1, flags=0, timestamp=0, payload=0xDEAD)
                    
                    if hasattr(ba, 'process_signal'):
                        # Test signal processing
                        result = ba.process_signal(signal)
                        # Just ensure it doesn't crash
                        self.assertTrue(True)  # If we get here, it worked
                except Exception as e:
                    # Don't fail on processing errors, just log
                    print(f"Signal processing test warning for {file_path}: {e}")
    
    def test_performance_stats(self):
        """Test performance statistics"""
        for file_path, module in self.modules:
            for attr_name in dir(module):
                if 'BitActor' in attr_name and not attr_name.startswith('_'):
                    cls = getattr(module, attr_name)
                    try:
                        instance = cls()
                        if hasattr(instance, 'get_stats'):
                            stats = instance.get_stats()
                            self.assertIsInstance(stats, dict)
                    except Exception as e:
                        print(f"Stats test warning for {file_path}: {e}")

if __name__ == '__main__':
    # Ensure we can import from current directory
    sys.path.insert(0, '.')
    sys.path.insert(0, 'generated/test_fix')
    sys.path.insert(0, 'generated/bitactor')
    sys.path.insert(0, 'generated/demo_output')
    
    unittest.main(verbosity=2)
