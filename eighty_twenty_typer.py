#!/usr/bin/env python3
"""
80/20 Typer - Pareto Principle Type System Optimizer

Implements the 80/20 rule to identify the 20% of types and relationships
that provide 80% of the value in a semantic model.
"""

import json
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple, Optional
from pathlib import Path
import networkx as nx
import numpy as np


@dataclass
class SemanticType:
    """Represents a type in the semantic model"""
    name: str
    uri: str
    attributes: List[str] = field(default_factory=list)
    constraints: List[str] = field(default_factory=list)
    relationships: List[str] = field(default_factory=list)
    usage_score: float = 0.0


@dataclass
class SemanticRelationship:
    """Represents a relationship between types"""
    source: str
    target: str
    predicate: str
    required: bool = False
    cardinality: str = "one-to-many"


@dataclass
class SemanticModel:
    """Complete semantic model"""
    types: List[SemanticType]
    relationships: List[SemanticRelationship]
    metadata: Dict = field(default_factory=dict)


class EightyTwentyTyper:
    """
    Optimizes type systems using the Pareto Principle.
    Identifies the critical 20% that provides 80% of value.
    """
    
    def __init__(self):
        self.importance_weights = {
            'reference_count': 0.3,
            'relationship_density': 0.3,
            'attribute_richness': 0.2,
            'constraint_complexity': 0.1,
            'centrality_score': 0.1
        }
    
    def optimize_types(self, semantic_model: SemanticModel) -> SemanticModel:
        """
        Main optimization function - applies 80/20 principle
        """
        print("🎯 Starting 80/20 type optimization...")
        
        # Build graph representation for analysis
        graph = self._build_type_graph(semantic_model)
        
        # Calculate importance scores
        scores = self._calculate_importance_scores(semantic_model, graph)
        
        # Rank types by importance
        ranked_types = self._rank_types(semantic_model.types, scores)
        
        # Select critical 20%
        critical_types = self._select_critical_twenty_percent(ranked_types, semantic_model)
        
        # Optimize relationships
        optimized_model = self._optimize_relationships(critical_types, semantic_model)
        
        # Report results
        self._report_optimization_results(semantic_model, optimized_model)
        
        return optimized_model
    
    def _build_type_graph(self, model: SemanticModel) -> nx.DiGraph:
        """Build a directed graph of type relationships"""
        graph = nx.DiGraph()
        
        # Add nodes
        for t in model.types:
            graph.add_node(t.name, type=t)
        
        # Add edges
        for rel in model.relationships:
            if rel.source in graph and rel.target in graph:
                graph.add_edge(
                    rel.source, 
                    rel.target,
                    predicate=rel.predicate,
                    required=rel.required
                )
        
        return graph
    
    def _calculate_importance_scores(self, model: SemanticModel, graph: nx.DiGraph) -> Dict[str, float]:
        """Calculate multi-factor importance scores"""
        scores = {}
        
        # Reference count - how often each type is referenced
        ref_counts = self._count_references(model)
        
        # Relationship density - how connected each type is
        rel_density = self._calculate_relationship_density(model)
        
        # Attribute richness - types with more attributes
        attr_richness = self._measure_attribute_richness(model)
        
        # Constraint complexity - types with complex constraints
        const_complexity = self._analyze_constraint_complexity(model)
        
        # Graph centrality - PageRank-style importance
        centrality = nx.pagerank(graph) if len(graph) > 0 else {}
        
        # Combine scores
        for type_obj in model.types:
            name = type_obj.name
            
            # Normalize each metric (handle empty collections)
            ref_max = max(ref_counts.values()) if ref_counts.values() else 1
            rel_max = max(rel_density.values()) if rel_density.values() else 1
            attr_max = max(attr_richness.values()) if attr_richness.values() else 1
            const_max = max(const_complexity.values()) if const_complexity.values() else 1
            cent_max = max(centrality.values()) if centrality.values() else 1
            
            ref_score = ref_counts.get(name, 0) / ref_max
            rel_score = rel_density.get(name, 0) / rel_max
            attr_score = attr_richness.get(name, 0) / attr_max
            const_score = const_complexity.get(name, 0) / const_max
            cent_score = centrality.get(name, 0) / cent_max
            
            # Weighted combination
            total_score = (
                ref_score * self.importance_weights['reference_count'] +
                rel_score * self.importance_weights['relationship_density'] +
                attr_score * self.importance_weights['attribute_richness'] +
                const_score * self.importance_weights['constraint_complexity'] +
                cent_score * self.importance_weights['centrality_score']
            )
            
            scores[name] = total_score
            type_obj.usage_score = total_score
        
        return scores
    
    def _count_references(self, model: SemanticModel) -> Dict[str, int]:
        """Count how many times each type is referenced"""
        counts = {}
        
        for rel in model.relationships:
            counts[rel.source] = counts.get(rel.source, 0) + 1
            counts[rel.target] = counts.get(rel.target, 0) + 1
        
        return counts
    
    def _calculate_relationship_density(self, model: SemanticModel) -> Dict[str, int]:
        """Calculate relationship density per type"""
        density = {}
        
        for t in model.types:
            rel_count = sum(1 for rel in model.relationships 
                          if rel.source == t.name or rel.target == t.name)
            density[t.name] = rel_count
        
        return density
    
    def _measure_attribute_richness(self, model: SemanticModel) -> Dict[str, int]:
        """Measure attribute count per type"""
        return {t.name: len(t.attributes) for t in model.types}
    
    def _analyze_constraint_complexity(self, model: SemanticModel) -> Dict[str, int]:
        """Analyze constraint complexity"""
        return {t.name: len(t.constraints) for t in model.types}
    
    def _rank_types(self, types: List[SemanticType], scores: Dict[str, float]) -> List[SemanticType]:
        """Rank types by importance score"""
        return sorted(types, key=lambda t: scores.get(t.name, 0), reverse=True)
    
    def _select_critical_twenty_percent(self, ranked_types: List[SemanticType], 
                                      model: SemanticModel) -> List[SemanticType]:
        """Select the critical 20% plus mandatory dependencies"""
        # Calculate 20% (minimum 1)
        twenty_percent_count = max(1, int(len(ranked_types) * 0.2))
        
        # Start with top 20%
        critical_types = set(ranked_types[:twenty_percent_count])
        critical_names = {t.name for t in critical_types}
        
        # Add mandatory dependencies
        added = True
        while added:
            added = False
            for rel in model.relationships:
                if rel.required and rel.source in critical_names and rel.target not in critical_names:
                    # Find and add the target type
                    target_type = next((t for t in model.types if t.name == rel.target), None)
                    if target_type:
                        critical_types.add(target_type)
                        critical_names.add(target_type.name)
                        added = True
        
        return list(critical_types)
    
    def _optimize_relationships(self, critical_types: List[SemanticType], 
                              model: SemanticModel) -> SemanticModel:
        """Create optimized model with only critical types and their relationships"""
        critical_names = {t.name for t in critical_types}
        
        # Filter relationships
        optimized_relationships = [
            rel for rel in model.relationships
            if rel.source in critical_names and rel.target in critical_names
        ]
        
        return SemanticModel(
            types=critical_types,
            relationships=optimized_relationships,
            metadata={
                **model.metadata,
                'optimization': {
                    'method': '80/20 Pareto Principle',
                    'original_type_count': len(model.types),
                    'optimized_type_count': len(critical_types),
                    'reduction_percentage': round((1 - len(critical_types) / len(model.types)) * 100, 2)
                }
            }
        )
    
    def _report_optimization_results(self, original: SemanticModel, optimized: SemanticModel):
        """Report optimization results"""
        print(f"\n📊 80/20 Optimization Results:")
        print(f"   Original types: {len(original.types)}")
        print(f"   Optimized types: {len(optimized.types)} ({len(optimized.types)/len(original.types)*100:.1f}%)")
        print(f"   Types removed: {len(original.types) - len(optimized.types)}")
        print(f"   Original relationships: {len(original.relationships)}")
        print(f"   Optimized relationships: {len(optimized.relationships)}")
        
        print(f"\n🏆 Top 5 Critical Types:")
        for i, t in enumerate(optimized.types[:5], 1):
            print(f"   {i}. {t.name} (score: {t.usage_score:.3f})")
    
    def export_to_turtle(self, model: SemanticModel, output_path: str):
        """Export optimized model to Turtle format"""
        ttl_content = ["@prefix : <http://cns.io/optimized#> ."]
        ttl_content.append("@prefix owl: <http://www.w3.org/2002/07/owl#> .")
        ttl_content.append("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .")
        ttl_content.append("")
        
        # Add types as classes
        for t in model.types:
            ttl_content.append(f":{t.name} a owl:Class ;")
            ttl_content.append(f'    rdfs:label "{t.name}" ;')
            if t.attributes:
                ttl_content.append(f'    rdfs:comment "Attributes: {", ".join(t.attributes)}" .')
            else:
                ttl_content.append("    .")
            ttl_content.append("")
        
        # Add relationships as properties
        for rel in model.relationships:
            prop_name = f"{rel.predicate}_{rel.source}_to_{rel.target}"
            ttl_content.append(f":{prop_name} a owl:ObjectProperty ;")
            ttl_content.append(f"    rdfs:domain :{rel.source} ;")
            ttl_content.append(f"    rdfs:range :{rel.target} .")
            ttl_content.append("")
        
        # Write to file
        Path(output_path).write_text("\n".join(ttl_content))
        print(f"\n✅ Exported optimized model to: {output_path}")


def main():
    """Example usage"""
    # Create example semantic model
    types = [
        SemanticType("User", "http://example.com/User", 
                    ["id", "name", "email"], ["unique_email"]),
        SemanticType("Order", "http://example.com/Order",
                    ["id", "total", "status"], ["positive_total"]),
        SemanticType("Product", "http://example.com/Product",
                    ["id", "name", "price"], ["positive_price"]),
        SemanticType("OrderItem", "http://example.com/OrderItem",
                    ["quantity", "price"], []),
        SemanticType("Category", "http://example.com/Category",
                    ["name"], []),
        SemanticType("Tag", "http://example.com/Tag", ["name"], []),
        SemanticType("Review", "http://example.com/Review",
                    ["rating", "comment"], ["rating_range"]),
    ]
    
    relationships = [
        SemanticRelationship("User", "Order", "places", required=True),
        SemanticRelationship("Order", "OrderItem", "contains", required=True),
        SemanticRelationship("OrderItem", "Product", "references", required=True),
        SemanticRelationship("Product", "Category", "belongsTo"),
        SemanticRelationship("Product", "Tag", "hasTag"),
        SemanticRelationship("Product", "Review", "hasReview"),
        SemanticRelationship("User", "Review", "writes"),
    ]
    
    model = SemanticModel(types=types, relationships=relationships)
    
    # Optimize
    typer = EightyTwentyTyper()
    optimized = typer.optimize_types(model)
    
    # Export
    typer.export_to_turtle(optimized, "optimized_model.ttl")


if __name__ == "__main__":
    main()