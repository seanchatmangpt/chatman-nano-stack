"""
TTL/SHACL Reachability Analysis
Determines which ontology triples are reachable for compilation
"""
from typing import Set, Dict, List, Tuple, Optional
import rdflib
from rdflib import Graph, URIRef, Literal, BNode
from rdflib.namespace import RDF, RDFS, OWL, SH, XSD
import hashlib

class ReachabilityAnalyzer:
    """Analyze TTL/SHACL graphs for reachable execution paths"""
    
    def __init__(self):
        self.graph = Graph()
        self.shacl_graph = Graph()
        self.reachable_triples: Set[Tuple] = set()
        self.execution_paths: Dict[str, List[Tuple]] = {}
        self.coverage_stats = {
            'total_triples': 0,
            'reachable_triples': 0,
            'coverage_percent': 0.0
        }
    
    def load_ttl(self, ttl_file: str):
        """Load TTL ontology file"""
        self.graph.parse(ttl_file, format='turtle')
        self.coverage_stats['total_triples'] = len(self.graph)
    
    def load_shacl(self, shacl_file: str):
        """Load SHACL constraints file"""
        self.shacl_graph.parse(shacl_file, format='turtle')
    
    def analyze_reachability(self) -> Dict[str, Set[Tuple]]:
        """Analyze which triples are reachable from entry points"""
        # Find entry points (shapes with triggers)
        entry_points = self._find_entry_points()
        
        # Trace execution paths from each entry
        for entry in entry_points:
            path = self._trace_path(entry)
            self.execution_paths[str(entry)] = path
            self.reachable_triples.update(path)
        
        # Calculate coverage
        self.coverage_stats['reachable_triples'] = len(self.reachable_triples)
        self.coverage_stats['coverage_percent'] = (
            len(self.reachable_triples) / len(self.graph) * 100
            if len(self.graph) > 0 else 0
        )
        
        return self.execution_paths
    
    def _find_entry_points(self) -> List[URIRef]:
        """Find SHACL shapes that can trigger execution"""
        entries = []
        
        # Look for shapes with sh:targetClass or sh:targetNode
        for shape in self.shacl_graph.subjects(RDF.type, SH.NodeShape):
            if (shape, SH.targetClass, None) in self.shacl_graph:
                entries.append(shape)
            elif (shape, SH.targetNode, None) in self.shacl_graph:
                entries.append(shape)
        
        # Also look for property shapes with paths
        for shape in self.shacl_graph.subjects(RDF.type, SH.PropertyShape):
            if (shape, SH.path, None) in self.shacl_graph:
                entries.append(shape)
        
        return entries
    
    def _trace_path(self, start: URIRef, visited: Optional[Set] = None) -> List[Tuple]:
        """Trace execution path from a starting point"""
        if visited is None:
            visited = set()
        
        if start in visited:
            return []
        
        visited.add(start)
        path = []
        
        # Get all triples with this subject
        for p, o in self.graph.predicate_objects(start):
            path.append((start, p, o))
            
            # Recursively trace if object is a resource
            if isinstance(o, URIRef):
                path.extend(self._trace_path(o, visited))
        
        # Also check SHACL constraints
        for p, o in self.shacl_graph.predicate_objects(start):
            if p in [SH.property, SH.path, SH.targetClass]:
                path.append((start, p, o))
                if isinstance(o, URIRef):
                    path.extend(self._trace_path(o, visited))
        
        return path
    
    def get_reachable_classes(self) -> Set[URIRef]:
        """Get all reachable classes"""
        classes = set()
        for s, p, o in self.reachable_triples:
            if p == RDF.type and isinstance(o, URIRef):
                classes.add(o)
            elif p == RDFS.subClassOf and isinstance(s, URIRef):
                classes.add(s)
        return classes
    
    def get_reachable_properties(self) -> Set[URIRef]:
        """Get all reachable properties"""
        properties = set()
        for s, p, o in self.reachable_triples:
            if isinstance(p, URIRef) and p not in [RDF.type, RDFS.subClassOf]:
                properties.add(p)
        return properties
    
    def prune_graph(self) -> Graph:
        """Create pruned graph with only reachable triples"""
        pruned = Graph()
        
        # Copy namespaces
        for prefix, ns in self.graph.namespaces():
            pruned.bind(prefix, ns)
        
        # Add only reachable triples
        for triple in self.reachable_triples:
            pruned.add(triple)
        
        return pruned
    
    def generate_hash(self) -> str:
        """Generate hash of reachable triples for verification"""
        # Sort triples for deterministic hashing
        sorted_triples = sorted(
            [(str(s), str(p), str(o)) for s, p, o in self.reachable_triples]
        )
        
        # Create hash
        hasher = hashlib.blake2b(digest_size=32)
        for s, p, o in sorted_triples:
            hasher.update(f"{s}|{p}|{o}\n".encode('utf-8'))
        
        return hasher.hexdigest()
    
    def report(self) -> str:
        """Generate reachability report"""
        lines = [
            "Reachability Analysis Report",
            "=" * 40,
            f"Total triples: {self.coverage_stats['total_triples']}",
            f"Reachable triples: {self.coverage_stats['reachable_triples']}",
            f"Coverage: {self.coverage_stats['coverage_percent']:.1f}%",
            "",
            f"Entry points: {len(self.execution_paths)}",
            f"Reachable classes: {len(self.get_reachable_classes())}",
            f"Reachable properties: {len(self.get_reachable_properties())}",
            "",
            "Execution Paths:"
        ]
        
        for entry, path in self.execution_paths.items():
            lines.append(f"\n  From {entry}:")
            lines.append(f"    Path length: {len(path)} triples")
        
        return '\n'.join(lines)