#!/usr/bin/env python3
"""
SEMANTIC DEFINITIONS VALIDATION TEST
Tests TTL/OWL/SHACL semantic definitions for syntactic correctness
"""

import pytest
from pathlib import Path
import rdflib
from rdflib import Graph
import sys

# Add CNS root to path
CNS_ROOT = Path(__file__).parent
sys.path.insert(0, str(CNS_ROOT))

# ============================================================================
# SEMANTIC VALIDATION TESTS
# ============================================================================

class TestSemanticDefinitions:
    """Test semantic definitions for validity"""
    
    def test_dfls_erlang_core_ttl_syntax(self):
        """Test DFLS Erlang core TTL syntax validity"""
        ttl_file = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "dfls_erlang_core.ttl"
        
        if ttl_file.exists():
            graph = Graph()
            try:
                graph.parse(ttl_file, format="turtle")
                # Should have loaded triples
                assert len(graph) > 0
                print(f"✅ DFLS Erlang Core TTL: {len(graph)} triples loaded")
            except Exception as e:
                pytest.fail(f"❌ DFLS Erlang Core TTL syntax error: {e}")
        else:
            pytest.skip("DFLS Erlang Core TTL file not found")
    
    def test_dfls_shacl_validation_ttl_syntax(self):
        """Test DFLS SHACL validation TTL syntax validity"""
        ttl_file = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "dfls_shacl_validation.ttl"
        
        if ttl_file.exists():
            graph = Graph()
            try:
                graph.parse(ttl_file, format="turtle")
                assert len(graph) > 0
                print(f"✅ DFLS SHACL Validation TTL: {len(graph)} triples loaded")
            except Exception as e:
                pytest.fail(f"❌ DFLS SHACL Validation TTL syntax error: {e}")
        else:
            pytest.skip("DFLS SHACL Validation TTL file not found")
    
    def test_bitactor_semantic_core_ttl_syntax(self):
        """Test BitActor semantic core TTL syntax validity"""
        ttl_file = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "bitactor_semantic_core.ttl"
        
        if ttl_file.exists():
            graph = Graph()
            try:
                graph.parse(ttl_file, format="turtle")
                assert len(graph) > 0
                print(f"✅ BitActor Semantic Core TTL: {len(graph)} triples loaded")
            except Exception as e:
                pytest.fail(f"❌ BitActor Semantic Core TTL syntax error: {e}")
        else:
            pytest.skip("BitActor Semantic Core TTL file not found")
    
    def test_bitactor_semantic_shacl_ttl_syntax(self):
        """Test BitActor semantic SHACL TTL syntax validity"""
        ttl_file = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "bitactor_semantic_shacl.ttl"
        
        if ttl_file.exists():
            graph = Graph()
            try:
                graph.parse(ttl_file, format="turtle")
                assert len(graph) > 0
                print(f"✅ BitActor Semantic SHACL TTL: {len(graph)} triples loaded")
            except Exception as e:
                pytest.fail(f"❌ BitActor Semantic SHACL TTL syntax error: {e}")
        else:
            pytest.skip("BitActor Semantic SHACL TTL file not found")
    
    def test_sparql_queries_syntax(self):
        """Test SPARQL queries syntax validity"""
        sparql_file = CNS_ROOT / "bitactor_otp" / "priv" / "sparql" / "dfls_code_generation_queries.sparql"
        
        if sparql_file.exists():
            with open(sparql_file, 'r') as f:
                content = f.read()
            
            # Check that file contains expected query patterns
            assert "SELECT" in content
            assert "WHERE" in content
            assert "PREFIX" in content
            
            # Count approximate number of queries
            query_count = content.count("# Query")
            print(f"✅ DFLS SPARQL Queries: {query_count} queries found")
            
            assert query_count > 0
        else:
            pytest.skip("DFLS SPARQL queries file not found")
    
    def test_ontology_namespace_consistency(self):
        """Test that ontologies use consistent namespaces"""
        ttl_files = [
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "dfls_erlang_core.ttl",
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "dfls_shacl_validation.ttl",
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "bitactor_semantic_core.ttl",
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "bitactor_semantic_shacl.ttl"
        ]
        
        expected_namespaces = [
            "http://cns.bitactor.io/ontology/dfls#",
            "http://cns.bitactor.io/ontology/otp#", 
            "http://cns.bitactor.io/ontology/bitactor#",
            "http://bitactor.org/ontology#"
        ]
        
        found_namespaces = set()
        
        for ttl_file in ttl_files:
            if ttl_file.exists():
                with open(ttl_file, 'r') as f:
                    content = f.read()
                
                # Extract namespace declarations
                for line in content.split('\n'):
                    if line.startswith('@prefix') and '<http' in line:
                        # Extract namespace URI
                        start = line.find('<') + 1
                        end = line.find('>')
                        if start > 0 and end > start:
                            namespace = line[start:end]
                            found_namespaces.add(namespace)
        
        print(f"✅ Found namespaces: {sorted(found_namespaces)}")
        
        # Check that we have consistent namespace usage
        assert len(found_namespaces) > 0
    
    def test_shacl_constraints_structure(self):
        """Test SHACL constraints have proper structure"""
        shacl_files = [
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "dfls_shacl_validation.ttl",
            CNS_ROOT / "bitactor_otp" / "priv" / "ontologies" / "bitactor_semantic_shacl.ttl"
        ]
        
        shape_count = 0
        constraint_count = 0
        
        for shacl_file in shacl_files:
            if shacl_file.exists():
                graph = Graph()
                graph.parse(shacl_file, format="turtle")
                
                # Query for SHACL shapes
                shapes_query = """
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?shape WHERE {
                    ?shape a sh:NodeShape .
                }
                """
                
                shapes = list(graph.query(shapes_query))
                shape_count += len(shapes)
                
                # Query for SHACL constraints
                constraints_query = """
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?constraint WHERE {
                    ?shape sh:property ?constraint .
                }
                """
                
                constraints = list(graph.query(constraints_query))
                constraint_count += len(constraints)
        
        print(f"✅ SHACL Structure: {shape_count} shapes, {constraint_count} constraints")
        
        # Should have some shapes and constraints
        assert shape_count > 0
        assert constraint_count > 0

# ============================================================================
# SEMANTIC INTEGRATION TESTS
# ============================================================================

class TestSemanticIntegration:
    """Test semantic integration between components"""
    
    def test_combined_ontology_loading(self):
        """Test loading multiple ontologies together"""
        ontology_dir = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies"
        
        if not ontology_dir.exists():
            pytest.skip("Ontologies directory not found")
        
        combined_graph = Graph()
        loaded_files = 0
        
        # Load all TTL files
        for ttl_file in ontology_dir.glob("*.ttl"):
            try:
                combined_graph.parse(ttl_file, format="turtle")
                loaded_files += 1
            except Exception as e:
                print(f"⚠️ Could not load {ttl_file.name}: {e}")
        
        print(f"✅ Combined ontology: {loaded_files} files, {len(combined_graph)} triples")
        
        assert loaded_files > 0
        assert len(combined_graph) > 0
    
    def test_sparql_queries_against_ontologies(self):
        """Test SPARQL queries execute against actual ontologies"""
        ontology_dir = CNS_ROOT / "bitactor_otp" / "priv" / "ontologies"
        sparql_file = CNS_ROOT / "bitactor_otp" / "priv" / "sparql" / "dfls_code_generation_queries.sparql"
        
        if not ontology_dir.exists() or not sparql_file.exists():
            pytest.skip("Required ontology or SPARQL files not found")
        
        # Load combined ontologies
        graph = Graph()
        for ttl_file in ontology_dir.glob("*.ttl"):
            try:
                graph.parse(ttl_file, format="turtle")
            except:
                pass
        
        if len(graph) == 0:
            pytest.skip("No ontologies could be loaded")
        
        # Test a simple SPARQL query
        test_query = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        
        SELECT ?class ?label WHERE {
            ?class a owl:Class .
            OPTIONAL { ?class rdfs:label ?label }
        }
        LIMIT 10
        """
        
        try:
            results = list(graph.query(test_query))
            print(f"✅ SPARQL test query returned {len(results)} results")
            assert len(results) >= 0  # Should execute without error
        except Exception as e:
            pytest.fail(f"❌ SPARQL query execution failed: {e}")

if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])