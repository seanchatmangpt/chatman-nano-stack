#!/usr/bin/env python3
"""
📦 CNS Forge CLI Tools Installer
Installs all 80/20 CLI tools and makes them available system-wide
"""

import os
import sys
import subprocess
from pathlib import Path
import shutil

def main():
    print("📦 Installing CNS Forge 80/20 CLI Tools")
    print("=" * 50)
    
    # Check Python and pip
    print("🐍 Checking Python environment...")
    python_version = sys.version_info
    if python_version < (3, 8):
        print("❌ Python 3.8+ required")
        sys.exit(1)
    print(f"✅ Python {python_version.major}.{python_version.minor}")
    
    # Install dependencies
    print("\n📦 Installing dependencies...")
    try:
        subprocess.run([
            sys.executable, "-m", "pip", "install", "-r", "cli_requirements.txt"
        ], check=True)
        print("✅ Dependencies installed")
    except subprocess.CalledProcessError:
        print("❌ Failed to install dependencies")
        sys.exit(1)
    
    # CLI tools to install
    cli_tools = {
        "cns": "cns_forge_cli.py",
        "bitactor": "bitactor_80_20_cli.py", 
        "semantic": "semantic_80_20_cli.py"
    }
    
    # Get installation directory
    user_bin = Path.home() / ".local" / "bin"
    user_bin.mkdir(parents=True, exist_ok=True)
    
    # Add to PATH if needed
    shell_rc = Path.home() / ".bashrc"
    if not shell_rc.exists():
        shell_rc = Path.home() / ".zshrc"
    
    path_line = f'export PATH="$HOME/.local/bin:$PATH"'
    
    if shell_rc.exists():
        with open(shell_rc) as f:
            content = f.read()
        
        if path_line not in content:
            print(f"\n🔧 Adding {user_bin} to PATH in {shell_rc}")
            with open(shell_rc, "a") as f:
                f.write(f"\n# CNS Forge CLI Tools\n{path_line}\n")
    
    # Install each CLI tool
    print(f"\n🛠️ Installing CLI tools to {user_bin}")
    
    for command_name, script_file in cli_tools.items():
        if not Path(script_file).exists():
            print(f"❌ Script not found: {script_file}")
            continue
        
        # Copy to user bin
        dest = user_bin / command_name
        shutil.copy2(script_file, dest)
        
        # Make executable
        dest.chmod(0o755)
        
        print(f"✅ Installed: {command_name} -> {dest}")
    
    # Create symlinks for convenience
    print("\n🔗 Creating convenience symlinks...")
    symlinks = {
        "cns-forge": "cns",
        "cns-bitactor": "bitactor",
        "cns-semantic": "semantic"
    }
    
    for link_name, target in symlinks.items():
        link_path = user_bin / link_name
        target_path = user_bin / target
        
        if target_path.exists():
            if link_path.exists():
                link_path.unlink()
            link_path.symlink_to(target)
            print(f"✅ {link_name} -> {target}")
    
    # Test installations
    print("\n🧪 Testing installations...")
    
    # Add user_bin to current PATH for testing
    os.environ["PATH"] = f"{user_bin}:{os.environ.get('PATH', '')}"
    
    for command_name in cli_tools.keys():
        try:
            result = subprocess.run([command_name, "--help"], 
                                  capture_output=True, timeout=5)
            if result.returncode == 0:
                print(f"✅ {command_name} working")
            else:
                print(f"⚠️ {command_name} may have issues")
        except (subprocess.TimeoutExpired, FileNotFoundError):
            print(f"❌ {command_name} not working")
    
    print("\n🎉 Installation Complete!")
    print("\nAvailable commands:")
    print("  cns          - Main CNS Forge CLI")
    print("  bitactor     - BitActor management")
    print("  semantic     - TTL/semantic processing")
    print("\nUsage examples:")
    print("  cns status")
    print("  cns forge start")
    print("  bitactor build --profile release")
    print("  semantic generate my_ontology.ttl --output-type dspy")
    print("\n💡 Restart your shell or run:")
    print(f"  export PATH=\"{user_bin}:$PATH\"")

if __name__ == "__main__":
    main()