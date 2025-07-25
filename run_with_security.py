#!/usr/bin/env python3
"""
Wrapper script to run any Python module with security patches applied
"""

import sys
import subprocess
import os

# First apply security patches to the current process
import security_patches_8020
security_patches_8020.install_security_patches()

# Add a marker to environment to indicate patches are loaded
os.environ['CNS_SECURITY_PATCHES_LOADED'] = '1'

# Create a wrapper script that applies patches before running target
wrapper_content = '''
import sys
import os

# Check if patches already loaded
if not os.environ.get('CNS_SECURITY_PATCHES_LOADED'):
    import security_patches_8020
    security_patches_8020.install_security_patches()
    os.environ['CNS_SECURITY_PATCHES_LOADED'] = '1'

# Now run the actual module
if len(sys.argv) > 1:
    module_name = sys.argv[1]
    sys.argv = sys.argv[1:]  # Shift arguments
    
    # Import and run the module
    if module_name.endswith('.py'):
        module_name = module_name[:-3]
    
    module = __import__(module_name)
    if hasattr(module, 'main'):
        module.main()
'''

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python run_with_security.py <script_to_run> [args...]")
        sys.exit(1)
    
    script_to_run = sys.argv[1]
    args = sys.argv[2:]
    
    # If running adversarial tests, patch them specially
    if 'adversarial' in script_to_run:
        print("🔒 Running adversarial tests with security patches...")
        
        # Read the original test file
        with open(script_to_run, 'r') as f:
            original_content = f.read()
        
        # Create a patched version that imports security patches
        patched_content = """#!/usr/bin/env python3
# AUTO-PATCHED VERSION WITH SECURITY
import security_patches_8020
security_patches_8020.install_security_patches()

""" + original_content
        
        # Write to temporary file
        import tempfile
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as tmp:
            tmp.write(patched_content)
            tmp_path = tmp.name
        
        try:
            # Run the patched version
            result = subprocess.run([sys.executable, tmp_path] + args)
            sys.exit(result.returncode)
        finally:
            os.unlink(tmp_path)
    else:
        # For other scripts, just ensure patches are in environment
        result = subprocess.run([sys.executable, script_to_run] + args)
        sys.exit(result.returncode)