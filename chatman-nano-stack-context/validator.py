#!/usr/bin/env python3
"""
CNS-CDCS v8.0 Project Health Validator
Autonomous validation and healing for CNS v8.0
"""

import os
import json
from pathlib import Path
from datetime import datetime

class CNSValidator:
    def __init__(self, project_root="/Users/sac/cns"):
        self.root = Path(project_root)
        self.context_dir = self.root / "chatman-nano-stack-context"
        self.health_score = 0
        
    def validate_structure(self):
        """Validate core CNS v8.0 directory structure"""
        required_dirs = [
            "ir", "codegen", "substrate/include", "substrate/src",
            "pragmatic/include", "pragmatic/src", "gatekeeper/include", 
            "gatekeeper/src", "artifacts", "chatman-nano-stack-context"
        ]
        
        missing = []
        for dir_path in required_dirs:
            if not (self.root / dir_path).exists():
                missing.append(dir_path)
                
        return len(missing) == 0, missing
    
    def validate_context(self):
        """Validate CNS context management files"""
        required_files = [
            "current.link", "session_recovery.spr", "cns_context.md"
        ]
        
        missing = []
        for file_name in required_files:
            if not (self.context_dir / file_name).exists():
                missing.append(file_name)
                
        return len(missing) == 0, missing
    
    def health_check(self):
        """Comprehensive health assessment"""
        results = {
            "timestamp": datetime.now().isoformat(),
            "structure_valid": False,
            "context_valid": False,
            "health_score": 0,
            "healing_required": []
        }
        
        # Validate structure
        struct_ok, missing_dirs = self.validate_structure()
        results["structure_valid"] = struct_ok
        if not struct_ok:
            results["healing_required"].extend(f"mkdir {d}" for d in missing_dirs)
            
        # Validate context
        ctx_ok, missing_files = self.validate_context()
        results["context_valid"] = ctx_ok
        if not ctx_ok:
            results["healing_required"].extend(f"create {f}" for f in missing_files)
            
        # Calculate health score
        results["health_score"] = (struct_ok * 50) + (ctx_ok * 50)
        
        return results
    
    def auto_heal(self):
        """Autonomous healing operations"""
        health = self.health_check()
        
        if health["health_score"] == 100:
            return {"status": "HEALTHY", "actions": []}
            
        actions = []
        
        # Auto-create missing directories
        if not health["structure_valid"]:
            required_dirs = [
                "ir", "codegen", "substrate/include", "substrate/src",
                "pragmatic/include", "pragmatic/src", "gatekeeper/include", 
                "gatekeeper/src", "artifacts"
            ]
            for dir_path in required_dirs:
                full_path = self.root / dir_path
                if not full_path.exists():
                    full_path.mkdir(parents=True, exist_ok=True)
                    actions.append(f"Created: {dir_path}")
        
        return {"status": "HEALED", "actions": actions}

if __name__ == "__main__":
    validator = CNSValidator()
    health = validator.health_check()
    
    print("🔧 CNS v8.0 Health Assessment")
    print(f"📊 Health Score: {health['health_score']}/100")
    print(f"🏗️  Structure: {'✅' if health['structure_valid'] else '❌'}")
    print(f"🧠 Context: {'✅' if health['context_valid'] else '❌'}")
    
    if health["healing_required"]:
        print("\n🔄 Auto-healing...")
        heal_result = validator.auto_heal()
        print(f"Status: {heal_result['status']}")
        for action in heal_result["actions"]:
            print(f"  • {action}")
