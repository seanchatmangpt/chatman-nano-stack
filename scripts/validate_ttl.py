#!/usr/bin/env python3
"""
TTL Ontology Validation Script - Called by Ash.Reactor Orchestration
Part of Artificial Hyper Intelligence 80/20 Strategy
"""

import sys
import os
from pathlib import Path

def validate_ttl_ontology(ontology_path):
    """Validate TTL ontology and extract metadata"""
    
    if not os.path.exists(ontology_path):
        print(f"ERROR: Ontology file not found: {ontology_path}")
        return False
    
    try:
        with open(ontology_path, 'r') as f:
            content = f.read()
        
        # Count classes and properties
        classes = content.count('rdf:type owl:Class')
        properties = content.count('rdf:type owl:DatatypeProperty') + content.count('rdf:type owl:ObjectProperty')
        lines = len(content.split('\n'))
        
        # Basic validation
        has_prefix = '@prefix' in content
        has_ontology = 'owl:Ontology' in content
        
        print(f"TTL Validation Results:")
        print(f"File: {ontology_path}")
        print(f"Lines: {lines}")
        print(f"{classes} classes found")
        print(f"{properties} properties found")
        print(f"Has prefixes: {has_prefix}")
        print(f"Has ontology declaration: {has_ontology}")
        
        if classes > 0 and properties > 0 and has_prefix and has_ontology:
            print("✅ TTL ontology validation PASSED")
            return True
        else:
            print("❌ TTL ontology validation FAILED")
            return False
            
    except Exception as e:
        print(f"ERROR: Failed to validate TTL: {str(e)}")
        return False

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 validate_ttl.py <ontology_path>")
        sys.exit(1)
    
    ontology_path = sys.argv[1]
    success = validate_ttl_ontology(ontology_path)
    sys.exit(0 if success else 1)