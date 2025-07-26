#!/usr/bin/env python3
"""
UltraThink to 80/20 Connector
Bridges semantic analysis with Pareto optimization
"""

import json
import sys
import argparse
from typing import Dict, List, Any
from dataclasses import dataclass, asdict
import re


@dataclass
class SemanticType:
    """Represents a semantic type extracted from domain description"""
    name: str
    description: str
    attributes: List[str]
    relationships: List[str]
    importance_hints: List[str]


@dataclass
class SemanticRelationship:
    """Represents a relationship between semantic types"""
    source: str
    target: str
    predicate: str
    cardinality: str = "one-to-many"
    required: bool = False


class UltraThinkAnalyzer:
    """
    Analyzes domain descriptions and extracts semantic structure
    for 80/20 optimization
    """
    
    def __init__(self):
        self.type_patterns = [
            r"(\w+):\s*(?:Represents?|Denotes?|Is)\s+(.+?)(?:\.|$)",
            r"(?:Entity|Component|Type|Class):\s*(\w+)\s*-\s*(.+?)(?:\.|$)",
            r"(\w+)\s*-\s*(.+?)(?:\.|$)"
        ]
        
        self.relationship_patterns = [
            r"(\w+)\s+(?:has|contains|includes)\s+(\w+)",
            r"(\w+)\s+(?:triggers?|causes?|leads to)\s+(\w+)",
            r"(\w+)\s+(?:detects?|monitors?|tracks?)\s+(\w+)",
            r"(\w+)\s+(?:protects?|secures?|guards?)\s+(\w+)",
            r"(\w+)\s+(?:exploits?|targets?|attacks?)\s+(\w+)",
            r"(\w+)\s+(?:references?|points to|links to)\s+(\w+)",
            r"(\w+)\s+(?:belongs to|is part of|is in)\s+(\w+)"
        ]
        
        self.importance_keywords = [
            "critical", "essential", "primary", "main", "core",
            "key", "important", "fundamental", "central", "vital"
        ]
    
    def analyze(self, domain_description: str) -> Dict[str, Any]:
        """
        Analyze domain description and extract semantic model
        """
        # Extract types
        types = self._extract_types(domain_description)
        
        # Extract relationships
        relationships = self._extract_relationships(domain_description, types)
        
        # Add importance scoring hints
        self._add_importance_hints(types, domain_description)
        
        # Convert to format expected by 80/20 optimizer
        return {
            "types": [self._type_to_dict(t) for t in types],
            "relationships": [asdict(r) for r in relationships],
            "metadata": {
                "source": "ultrathink",
                "analysis_method": "semantic_extraction"
            }
        }
    
    def _extract_types(self, text: str) -> List[SemanticType]:
        """Extract semantic types from text"""
        types = []
        seen_names = set()
        
        for pattern in self.type_patterns:
            matches = re.finditer(pattern, text, re.MULTILINE | re.IGNORECASE)
            
            for match in matches:
                name = match.group(1).strip()
                description = match.group(2).strip() if len(match.groups()) > 1 else ""
                
                # Normalize name
                name = name.replace(" ", "")
                
                if name and name not in seen_names:
                    seen_names.add(name)
                    
                    # Extract potential attributes from description
                    attributes = self._extract_attributes(description)
                    
                    types.append(SemanticType(
                        name=name,
                        description=description,
                        attributes=attributes,
                        relationships=[],
                        importance_hints=[]
                    ))
        
        return types
    
    def _extract_relationships(self, text: str, types: List[SemanticType]) -> List[SemanticRelationship]:
        """Extract relationships between types"""
        relationships = []
        type_names = {t.name.lower() for t in types}
        
        for pattern in self.relationship_patterns:
            matches = re.finditer(pattern, text, re.IGNORECASE)
            
            for match in matches:
                source = match.group(1).strip().replace(" ", "")
                target = match.group(2).strip().replace(" ", "")
                
                # Check if both types exist
                if source.lower() in type_names and target.lower() in type_names:
                    # Determine predicate from pattern
                    predicate = self._determine_predicate(pattern)
                    
                    relationships.append(SemanticRelationship(
                        source=source,
                        target=target,
                        predicate=predicate,
                        required="should" in text or "must" in text
                    ))
                    
                    # Update type relationships
                    for t in types:
                        if t.name == source:
                            t.relationships.append(f"{predicate} {target}")
        
        return relationships
    
    def _extract_attributes(self, description: str) -> List[str]:
        """Extract potential attributes from description"""
        attributes = []
        
        # Look for attribute patterns
        attr_patterns = [
            r"with\s+(?:properties?|attributes?|fields?)?\s*:?\s*([^.]+)",
            r"including\s+([^.]+)",
            r"such as\s+([^.]+)"
        ]
        
        for pattern in attr_patterns:
            match = re.search(pattern, description, re.IGNORECASE)
            if match:
                attr_text = match.group(1)
                # Split by common delimiters
                attrs = re.split(r"[,;]|\s+and\s+", attr_text)
                attributes.extend([a.strip() for a in attrs if a.strip()])
        
        return attributes
    
    def _add_importance_hints(self, types: List[SemanticType], text: str):
        """Add importance hints based on context"""
        for t in types:
            # Check if type name appears near importance keywords
            for keyword in self.importance_keywords:
                pattern = rf"{keyword}\s+\w*\s*{t.name}"
                if re.search(pattern, text, re.IGNORECASE):
                    t.importance_hints.append(keyword)
            
            # Check frequency of mentions
            mentions = len(re.findall(t.name, text, re.IGNORECASE))
            if mentions > 3:
                t.importance_hints.append(f"high_frequency_{mentions}")
    
    def _type_to_dict(self, t: SemanticType) -> Dict[str, Any]:
        """Convert SemanticType to dictionary format for 80/20 optimizer"""
        return {
            "name": t.name,
            "uri": f"http://cns.io/ultrathink#{t.name}",
            "attributes": t.attributes,
            "constraints": [],  # Could be extracted from description
            "relationships": t.relationships,
            "usage_score": len(t.importance_hints) * 0.1  # Initial score based on hints
        }
    
    def _determine_predicate(self, pattern: str) -> str:
        """Determine predicate name from regex pattern"""
        if "has" in pattern or "contains" in pattern:
            return "has"
        elif "triggers" in pattern or "causes" in pattern:
            return "triggers"
        elif "detects" in pattern or "monitors" in pattern:
            return "monitors"
        elif "protects" in pattern:
            return "protects"
        elif "exploits" in pattern or "attacks" in pattern:
            return "exploits"
        elif "references" in pattern:
            return "references"
        elif "belongs" in pattern:
            return "belongsTo"
        else:
            return "relatedTo"


def main():
    parser = argparse.ArgumentParser(description="UltraThink to 80/20 Connector")
    parser.add_argument("--input", type=str, required=True, 
                       help="Domain description text")
    parser.add_argument("--input-file", type=str,
                       help="Read input from file instead of command line")
    parser.add_argument("--output", type=str,
                       help="Output file (default: stdout)")
    parser.add_argument("--pretty", action="store_true",
                       help="Pretty print JSON output")
    
    args = parser.parse_args()
    
    # Get input text
    if args.input_file:
        with open(args.input_file, 'r') as f:
            input_text = f.read()
    else:
        input_text = args.input
    
    # Analyze
    analyzer = UltraThinkAnalyzer()
    result = analyzer.analyze(input_text)
    
    # Output
    output_json = json.dumps(result, indent=2 if args.pretty else None)
    
    if args.output:
        with open(args.output, 'w') as f:
            f.write(output_json)
    else:
        print(output_json)


if __name__ == "__main__":
    main()