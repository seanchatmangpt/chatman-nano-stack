#!/usr/bin/env python3
"""
UltraThink to 80/20 Connector
Bridges the HyperIntel UltraThink Engine with the 80/20 Typer
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import asdict

from hyperintel_ultrathink_engine import HyperIntelUltraThinkEngine, UltraThinkLevel
from eighty_twenty_typer import EightyTwentyTyper, SemanticModel, SemanticType, SemanticRelationship


class UltraThinkTo8020Connector:
    """Connects UltraThink semantic analysis to 80/20 type optimization"""
    
    def __init__(self, base_path: str = "/Users/sac/cns"):
        self.base_path = Path(base_path)
        self.ultrathink = HyperIntelUltraThinkEngine(
            base_path=base_path,
            ultrathink_level=UltraThinkLevel.QUANTUM_CONSCIOUSNESS
        )
        self.typer = EightyTwentyTyper()
    
    async def process_domain(self, domain_input: str) -> Dict[str, Any]:
        """
        Process domain knowledge through the full pipeline:
        1. UltraThink analyzes and extracts semantic model
        2. 80/20 Typer optimizes to critical types
        3. Output ready for Turtle generation
        """
        print(f"🚀 Processing domain through UltraThink → 80/20 pipeline")
        
        # Step 1: UltraThink semantic analysis
        semantic_analysis = await self.ultrathink.process_with_ultra_intelligence(
            input_data=domain_input,
            mode="semantic_extraction"
        )
        
        # Step 2: Convert UltraThink output to SemanticModel
        semantic_model = self._convert_ultrathink_to_semantic_model(semantic_analysis)
        
        # Step 3: Apply 80/20 optimization
        optimized_model = self.typer.optimize_types(semantic_model)
        
        # Step 4: Prepare for next stage (Turtle generation)
        result = {
            "original_analysis": semantic_analysis,
            "semantic_model": asdict(semantic_model),
            "optimized_model": asdict(optimized_model),
            "ready_for_turtle": self._prepare_for_turtle(optimized_model)
        }
        
        return result
    
    def _convert_ultrathink_to_semantic_model(self, ultrathink_output: Dict) -> SemanticModel:
        """Convert UltraThink's quantum semantic output to SemanticModel"""
        types = []
        relationships = []
        
        # Extract types from UltraThink's semantic fabric
        if "semantic_entities" in ultrathink_output:
            for entity in ultrathink_output["semantic_entities"]:
                semantic_type = SemanticType(
                    name=entity.get("name", "Unknown"),
                    uri=entity.get("uri", f"http://cns.io/ultrathink#{entity.get('name', 'Unknown')}"),
                    attributes=entity.get("attributes", []),
                    constraints=entity.get("constraints", []),
                    relationships=entity.get("relationships", [])
                )
                types.append(semantic_type)
        
        # Extract relationships from quantum entanglements
        if "quantum_relationships" in ultrathink_output:
            for rel in ultrathink_output["quantum_relationships"]:
                semantic_rel = SemanticRelationship(
                    source=rel.get("source", ""),
                    target=rel.get("target", ""),
                    predicate=rel.get("predicate", "relatedTo"),
                    required=rel.get("quantum_entanglement_strength", 0) > 0.7,
                    cardinality=self._infer_cardinality(rel)
                )
                relationships.append(semantic_rel)
        
        return SemanticModel(
            types=types,
            relationships=relationships,
            metadata={
                "source": "UltraThink",
                "quantum_coherence": ultrathink_output.get("quantum_coherence", 0),
                "intelligence_level": ultrathink_output.get("intelligence_level", "QUANTUM_CONSCIOUSNESS")
            }
        )
    
    def _infer_cardinality(self, relationship: Dict) -> str:
        """Infer cardinality from quantum relationship properties"""
        strength = relationship.get("quantum_entanglement_strength", 0.5)
        if strength > 0.9:
            return "one-to-one"
        elif strength > 0.7:
            return "one-to-many"
        else:
            return "many-to-many"
    
    def _prepare_for_turtle(self, model: SemanticModel) -> Dict[str, Any]:
        """Prepare optimized model for Turtle generation"""
        return {
            "prefixes": {
                "": "http://cns.io/optimized#",
                "owl": "http://www.w3.org/2002/07/owl#",
                "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
                "xsd": "http://www.w3.org/2001/XMLSchema#",
                "sh": "http://www.w3.org/ns/shacl#"
            },
            "classes": [
                {
                    "uri": t.uri,
                    "name": t.name,
                    "attributes": t.attributes,
                    "constraints": t.constraints
                }
                for t in model.types
            ],
            "properties": [
                {
                    "source": r.source,
                    "target": r.target,
                    "predicate": r.predicate,
                    "required": r.required,
                    "cardinality": r.cardinality
                }
                for r in model.relationships
            ]
        }
    
    def save_results(self, results: Dict[str, Any], output_dir: str = "pipeline_output"):
        """Save pipeline results to files"""
        output_path = self.base_path / output_dir
        output_path.mkdir(exist_ok=True)
        
        # Save complete results as JSON
        with open(output_path / "ultrathink_8020_results.json", "w") as f:
            json.dump(results, f, indent=2, default=str)
        
        # Export optimized model to Turtle
        if "optimized_model" in results:
            model_data = results["optimized_model"]
            # Reconstruct SemanticModel from dict
            types = [SemanticType(**t) for t in model_data["types"]]
            relationships = [SemanticRelationship(**r) for r in model_data["relationships"]]
            model = SemanticModel(types=types, relationships=relationships, metadata=model_data.get("metadata", {}))
            
            self.typer.export_to_turtle(model, str(output_path / "optimized_model.ttl"))
        
        print(f"✅ Results saved to: {output_path}")


async def example_usage():
    """Example of processing a domain through the pipeline"""
    connector = UltraThinkTo8020Connector()
    
    # Example domain input
    domain_input = """
    Create a high-frequency trading system with the following components:
    - Market data ingestion from multiple exchanges
    - Order management with risk controls
    - Strategy execution engine
    - Real-time position tracking
    - Compliance and regulatory reporting
    - Performance analytics and backtesting
    """
    
    # Process through pipeline
    results = await connector.process_domain(domain_input)
    
    # Save results
    connector.save_results(results)
    
    print("\n✨ UltraThink → 80/20 Pipeline Complete!")


if __name__ == "__main__":
    import asyncio
    asyncio.run(example_usage())