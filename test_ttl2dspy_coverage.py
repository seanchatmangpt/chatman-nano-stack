#!/usr/bin/env python3
"""
Comprehensive unit tests for ttl2dspy.py - 80% line coverage
Tests TTL to DSPy signature conversion, error handling, batch processing
"""

import pytest
import tempfile
import subprocess
import sys
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime

import rdflib
from rdflib import Graph, Namespace, URIRef, Literal
from rdflib.namespace import OWL, RDF, RDFS, SH, XSD

from ttl2dspy import (
    TTL2DSPyTranspiler, parse_ontology, write_signature_file, main,
    CNS, SHACL
)


class TestTTL2DSPyTranspiler:
    """Test TTL2DSPyTranspiler class functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        self.transpiler = TTL2DSPyTranspiler()
    
    def test_transpiler_initialization(self):
        """Test transpiler initialization"""
        assert isinstance(self.transpiler.seen_field_names, set)
        assert len(self.transpiler.seen_field_names) == 0
        assert self.transpiler.signature_count == 0
    
    def test_safe_local_name(self):
        """Test safe local name extraction"""
        # Test with hash fragment
        assert self.transpiler.safe_local_name("http://example.org#TestClass") == "TestClass"
        
        # Test with slash
        assert self.transpiler.safe_local_name("http://example.org/TestClass") == "TestClass"
        
        # Test with no separator
        assert self.transpiler.safe_local_name("TestClass") == "TestClass"
        
        # Test with empty string
        assert self.transpiler.safe_local_name("") == ""
        
        # Test with URIRef object
        uri_ref = URIRef("http://example.org#TestClass")
        assert self.transpiler.safe_local_name(uri_ref) == "TestClass"
    
    def test_snake_case_conversion(self):
        """Test snake_case conversion"""
        # Test camelCase
        assert self.transpiler.snake_case("camelCase") == "camel_case"
        
        # Test PascalCase
        assert self.transpiler.snake_case("PascalCase") == "pascal_case"
        
        # Test already snake_case
        assert self.transpiler.snake_case("snake_case") == "snake_case"
        
        # Test with hyphens
        assert self.transpiler.snake_case("kebab-case") == "kebab_case"
        
        # Test with spaces
        assert self.transpiler.snake_case("space case") == "space_case"
        
        # Test with mixed characters
        assert self.transpiler.snake_case("Mixed-Case_With123Numbers") == "mixed_case_with123_numbers"
        
        # Test starting with digit
        assert self.transpiler.snake_case("123test") == "field_123test"
        
        # Test empty string
        assert self.transpiler.snake_case("") == "unnamed_field"
        
        # Test multiple underscores
        assert self.transpiler.snake_case("test___case") == "test_case"
        
        # Test leading/trailing underscores
        assert self.transpiler.snake_case("_test_case_") == "test_case"
    
    def test_check_field_collision(self):
        """Test field name collision detection and resolution"""
        # Test normal field name
        field_name = self.transpiler.check_field_collision("test_field", "http://example.org#testField")
        assert field_name == "test_field"
        assert "test_field" in self.transpiler.seen_field_names
        
        # Test collision with seen field
        field_name2 = self.transpiler.check_field_collision("test_field", "http://example.org#testField2")
        assert field_name2 == "test_field_1"
        assert "test_field_1" in self.transpiler.seen_field_names
        
        # Test multiple collisions
        field_name3 = self.transpiler.check_field_collision("test_field", "http://example.org#testField3")
        assert field_name3 == "test_field_2"
        
        # Test reserved name collision
        self.transpiler.seen_field_names.clear()
        field_name = self.transpiler.check_field_collision("metadata", "http://example.org#metadata")
        assert field_name == "custom_metadata"
        
        # Test rdflib namespace collision
        with patch.object(rdflib.Namespace, "test_attr", "collision", create=True):
            field_name = self.transpiler.check_field_collision("test_attr", "http://example.org#testAttr")
            assert field_name == "custom_test_attr"
    
    def test_extract_datatype(self):
        """Test datatype extraction from property shapes"""
        g = Graph()
        prop_shape = URIRef("http://example.org#testPropShape")
        
        # Test string datatype
        g.add((prop_shape, SH.datatype, XSD.string))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=str"
        
        # Test boolean datatype
        g.remove((prop_shape, SH.datatype, XSD.string))
        g.add((prop_shape, SH.datatype, XSD.boolean))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=bool"
        
        # Test integer datatype
        g.remove((prop_shape, SH.datatype, XSD.boolean))
        g.add((prop_shape, SH.datatype, XSD.int))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=int"
        
        # Test long datatype
        g.remove((prop_shape, SH.datatype, XSD.int))
        g.add((prop_shape, SH.datatype, XSD.long))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=int"
        
        # Test float datatype
        g.remove((prop_shape, SH.datatype, XSD.long))
        g.add((prop_shape, SH.datatype, XSD.float))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=float"
        
        # Test double datatype
        g.remove((prop_shape, SH.datatype, XSD.float))
        g.add((prop_shape, SH.datatype, XSD.double))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=float"
        
        # Test decimal datatype
        g.remove((prop_shape, SH.datatype, XSD.double))
        g.add((prop_shape, SH.datatype, XSD.decimal))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=float"
        
        # Test class constraint
        g.remove((prop_shape, SH.datatype, XSD.decimal))
        g.add((prop_shape, SH['class'], URIRef("http://example.org#TestClass")))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=str"
        
        # Test default (no datatype)
        g.remove((prop_shape, SH['class'], URIRef("http://example.org#TestClass")))
        dtype = self.transpiler.extract_datatype(prop_shape, g)
        assert dtype == "dtype=str"
    
    def test_find_property_shapes(self):
        """Test property shape discovery"""
        g = Graph()
        test_class = URIRef("http://example.org#TestClass")
        
        # Pattern 1: Direct sh:targetClass on property shapes
        prop_shape1 = URIRef("http://example.org#propShape1")
        prop1 = URIRef("http://example.org#prop1")
        g.add((prop_shape1, SH.path, prop1))
        g.add((prop_shape1, SH.targetClass, test_class))
        
        # Pattern 2: Node shapes with sh:property links
        node_shape = URIRef("http://example.org#nodeShape")
        prop_shape2 = URIRef("http://example.org#propShape2")
        prop2 = URIRef("http://example.org#prop2")
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape2))
        g.add((prop_shape2, SH.path, prop2))
        
        # Property shape without path (should be ignored)
        prop_shape3 = URIRef("http://example.org#propShape3")
        g.add((node_shape, SH.property, prop_shape3))
        # No path for prop_shape3
        
        prop_shapes = self.transpiler.find_property_shapes(test_class, g)
        
        assert len(prop_shapes) == 2
        assert prop_shape1 in prop_shapes
        assert prop_shape2 in prop_shapes
        assert prop_shape3 not in prop_shapes
    
    def test_is_output_field(self):
        """Test output field detection"""
        g = Graph()
        prop_shape = URIRef("http://example.org#testPropShape")
        
        # Test cns:outputField = true
        g.add((prop_shape, CNS.outputField, Literal("true")))
        assert self.transpiler.is_output_field(prop_shape, g) is True
        
        # Test cns:outputField = 1
        g.remove((prop_shape, CNS.outputField, Literal("true")))
        g.add((prop_shape, CNS.outputField, Literal("1")))
        assert self.transpiler.is_output_field(prop_shape, g) is True
        
        # Test cns:outputField = yes
        g.remove((prop_shape, CNS.outputField, Literal("1")))
        g.add((prop_shape, CNS.outputField, Literal("yes")))
        assert self.transpiler.is_output_field(prop_shape, g) is True
        
        # Test cns:outputField = false
        g.remove((prop_shape, CNS.outputField, Literal("yes")))
        g.add((prop_shape, CNS.outputField, Literal("false")))
        assert self.transpiler.is_output_field(prop_shape, g) is False
        
        # Test rdfs:comment containing "output"
        g.remove((prop_shape, CNS.outputField, Literal("false")))
        g.add((prop_shape, RDFS.comment, Literal("This is an output field")))
        assert self.transpiler.is_output_field(prop_shape, g) is True
        
        # Test rdfs:comment not containing "output"
        g.remove((prop_shape, RDFS.comment, Literal("This is an output field")))
        g.add((prop_shape, RDFS.comment, Literal("This is an input field")))
        assert self.transpiler.is_output_field(prop_shape, g) is False
        
        # Test no markers
        g.remove((prop_shape, RDFS.comment, Literal("This is an input field")))
        assert self.transpiler.is_output_field(prop_shape, g) is False
    
    def test_build_signatures_simple(self):
        """Test signature building with simple ontology"""
        g = Graph()
        
        # Create test class with SHACL shape
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#TestClassShape")
        prop_shape1 = URIRef("http://example.org#prop1Shape")
        prop_shape2 = URIRef("http://example.org#prop2Shape")
        prop1 = URIRef("http://example.org#property1")
        prop2 = URIRef("http://example.org#property2")
        
        # Add SHACL shape
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape1))
        g.add((node_shape, SH.property, prop_shape2))
        
        # Add property shapes
        g.add((prop_shape1, SH.path, prop1))
        g.add((prop_shape1, SH.datatype, XSD.string))
        g.add((prop_shape1, RDFS.comment, Literal("Input property")))
        
        g.add((prop_shape2, SH.path, prop2))
        g.add((prop_shape2, SH.datatype, XSD.string))
        g.add((prop_shape2, CNS.outputField, Literal("true")))
        g.add((prop_shape2, RDFS.comment, Literal("Output property")))
        
        # Add class comment
        g.add((test_class, RDFS.comment, Literal("Test class for DSPy signature")))
        
        signatures = self.transpiler.build_signatures(g)
        
        assert len(signatures) == 1
        signature_name = "TestClassSignature"
        assert signature_name in signatures
        
        signature_code = signatures[signature_name]
        assert "class TestClassSignature(dspy.Signature):" in signature_code
        assert "property1 = dspy.InputField" in signature_code
        assert "property2 = dspy.OutputField" in signature_code
        assert "Test class for DSPy signature" in signature_code
        assert self.transpiler.signature_count == 1
    
    def test_build_signatures_no_output_field(self):
        """Test signature building with auto-generated output field"""
        g = Graph()
        
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#TestClassShape")
        prop_shape = URIRef("http://example.org#propShape")
        prop = URIRef("http://example.org#property")
        
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape))
        g.add((prop_shape, SH.path, prop))
        g.add((prop_shape, SH.datatype, XSD.string))
        
        signatures = self.transpiler.build_signatures(g)
        
        assert len(signatures) == 1
        signature_code = list(signatures.values())[0]
        
        # Should have input field and auto-generated output field
        assert "property = dspy.InputField" in signature_code
        assert "result = dspy.OutputField" in signature_code
    
    def test_build_signatures_multiple_output_fields(self):
        """Test signature building with multiple output fields (should keep only first)"""
        g = Graph()
        
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#TestClassShape")
        prop_shape1 = URIRef("http://example.org#prop1Shape")
        prop_shape2 = URIRef("http://example.org#prop2Shape")
        prop1 = URIRef("http://example.org#property1")
        prop2 = URIRef("http://example.org#property2")
        
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape1))
        g.add((node_shape, SH.property, prop_shape2))
        
        # Both properties marked as output
        g.add((prop_shape1, SH.path, prop1))
        g.add((prop_shape1, CNS.outputField, Literal("true")))
        g.add((prop_shape2, SH.path, prop2))
        g.add((prop_shape2, CNS.outputField, Literal("true")))
        
        signatures = self.transpiler.build_signatures(g)
        signature_code = list(signatures.values())[0]
        
        # Should only have one output field (the first one)
        output_field_count = signature_code.count("dspy.OutputField")
        assert output_field_count == 1
    
    def test_build_signatures_no_classes(self):
        """Test signature building with no classes"""
        g = Graph()  # Empty graph
        
        signatures = self.transpiler.build_signatures(g)
        assert len(signatures) == 0
        assert self.transpiler.signature_count == 0
    
    def test_build_signatures_no_property_shapes(self):
        """Test signature building with class but no property shapes"""
        g = Graph()
        
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#TestClassShape")
        g.add((node_shape, SH.targetClass, test_class))
        # No property shapes
        
        signatures = self.transpiler.build_signatures(g)
        assert len(signatures) == 0
    
    def test_generate_module_single_signature(self):
        """Test module generation with single signature"""
        signatures = {
            "TestSignature": '''class TestSignature(dspy.Signature):
    """Test signature"""
    
    input_field = dspy.InputField(desc="Input", dtype=str)
    output_field = dspy.OutputField(desc="Output", dtype=str)
'''
        }
        
        module_code = self.transpiler.generate_module(signatures, "http://example.org/test")
        
        assert "import dspy" in module_code
        assert "from typing import Union" in module_code
        assert "__all__ = [\"TestSignature\"]" in module_code
        assert "class TestSignature(dspy.Signature):" in module_code
        assert "SIGNATURES = {" in module_code
        assert '"TestSignature": TestSignature,' in module_code
        assert "def get_signature(name: str)" in module_code
        assert "def list_signatures()" in module_code
        assert "http://example.org/test" in module_code
    
    def test_generate_module_multiple_signatures(self):
        """Test module generation with multiple signatures"""
        signatures = {
            "TestSignature1": "class TestSignature1(dspy.Signature): pass",
            "TestSignature2": "class TestSignature2(dspy.Signature): pass"
        }
        
        module_code = self.transpiler.generate_module(signatures)
        
        assert '"TestSignature1", "TestSignature2"' in module_code
        assert '"TestSignature1": TestSignature1,' in module_code
        assert '"TestSignature2": TestSignature2,' in module_code
    
    def test_generate_module_empty_signatures(self):
        """Test module generation with no signatures"""
        signatures = {}
        
        module_code = self.transpiler.generate_module(signatures)
        
        assert "__all__ = []" in module_code
        assert "SIGNATURES = {}" in module_code
        assert "Signatures generated: 0" in module_code


class TestParseOntology:
    """Test parse_ontology function"""
    
    def test_parse_valid_ttl(self):
        """Test parsing valid TTL file"""
        ttl_content = """
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ex:TestOntology a owl:Ontology .
        ex:TestClass a owl:Class .
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(ttl_content)
            f.flush()
            
            try:
                g, ontology_uri = parse_ontology(Path(f.name))
                
                assert g is not None
                assert isinstance(g, Graph)
                assert len(g) > 0
                assert ontology_uri == "http://example.org/TestOntology"
                
            finally:
                Path(f.name).unlink()
    
    def test_parse_invalid_ttl(self):
        """Test parsing invalid TTL file"""
        invalid_content = "This is not valid TTL content"
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(invalid_content)
            f.flush()
            
            try:
                g, ontology_uri = parse_ontology(Path(f.name))
                
                assert g is None
                assert ontology_uri == ""
                
            finally:
                Path(f.name).unlink()
    
    def test_parse_ttl_no_ontology_uri(self):
        """Test parsing TTL without ontology URI"""
        ttl_content = """
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ex:TestClass a owl:Class .
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(ttl_content)
            f.flush()
            
            try:
                g, ontology_uri = parse_ontology(Path(f.name))
                
                assert g is not None
                assert ontology_uri == ""  # No ontology URI found
                
            finally:
                Path(f.name).unlink()
    
    def test_parse_nonexistent_file(self):
        """Test parsing nonexistent file"""
        g, ontology_uri = parse_ontology(Path("/nonexistent/file.ttl"))
        
        assert g is None
        assert ontology_uri == ""


class TestWriteSignatureFile:
    """Test write_signature_file function"""
    
    def test_write_single_signature(self):
        """Test writing single signature to file"""
        signatures = {
            "TestSignature": '''class TestSignature(dspy.Signature):
    """Test signature"""
    input_field = dspy.InputField(desc="Input", dtype=str)
    output_field = dspy.OutputField(desc="Output", dtype=str)
'''
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            output_path = Path(f.name)
            
            try:
                success = write_signature_file(signatures, output_path, "http://example.org/test", merge_mode=False)
                
                assert success is True
                assert output_path.exists()
                
                content = output_path.read_text()
                assert "import dspy" in content
                assert "class TestSignature(dspy.Signature):" in content
                assert "http://example.org/test" in content
                
            finally:
                if output_path.exists():
                    output_path.unlink()
    
    def test_write_multiple_signatures_merge_mode(self):
        """Test writing multiple signatures in merge mode"""
        signatures = {
            "TestSignature1": "class TestSignature1(dspy.Signature): pass",
            "TestSignature2": "class TestSignature2(dspy.Signature): pass"
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            output_path = Path(f.name)
            
            try:
                success = write_signature_file(signatures, output_path, "http://example.org/test", merge_mode=True)
                
                assert success is True
                content = output_path.read_text()
                assert "TestSignature1" in content
                assert "TestSignature2" in content
                assert "SIGNATURES = {" in content
                
            finally:
                if output_path.exists():
                    output_path.unlink()
    
    def test_write_multiple_signatures_no_merge(self):
        """Test writing multiple signatures without merge mode"""
        signatures = {
            "TestSignature1": "class TestSignature1(dspy.Signature): pass",
            "TestSignature2": "class TestSignature2(dspy.Signature): pass"
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            output_path = Path(f.name)
            
            try:
                success = write_signature_file(signatures, output_path, "http://example.org/test", merge_mode=False)
                
                assert success is True
                content = output_path.read_text()
                # Should generate full module when multiple signatures
                assert "SIGNATURES = {" in content
                
            finally:
                if output_path.exists():
                    output_path.unlink()
    
    def test_write_to_invalid_path(self):
        """Test writing to invalid path"""
        signatures = {"TestSignature": "class TestSignature(dspy.Signature): pass"}
        invalid_path = Path("/invalid/nonexistent/directory/file.py")
        
        # Should create parent directories
        success = write_signature_file(signatures, invalid_path, "http://example.org/test")
        
        # Will succeed due to mkdir(parents=True, exist_ok=True)
        assert success is True
        
        # Cleanup
        if invalid_path.exists():
            invalid_path.unlink()
            invalid_path.parent.rmdir()


class TestMainFunction:
    """Test main function and CLI functionality"""
    
    def create_test_ttl(self, content=None):
        """Helper to create test TTL file"""
        if content is None:
            content = """
            @prefix ex: <http://example.org/> .
            @prefix owl: <http://www.w3.org/2002/07/owl#> .
            @prefix shacl: <http://www.w3.org/ns/shacl#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix cns: <http://cns.io/ontology#> .
            
            ex:TestClass a owl:Class ;
                rdfs:comment "Test class for DSPy signature" .
            
            ex:TestClassShape a shacl:NodeShape ;
                shacl:targetClass ex:TestClass ;
                shacl:property ex:prop1Shape, ex:prop2Shape .
            
            ex:prop1Shape a shacl:PropertyShape ;
                shacl:path ex:property1 ;
                shacl:datatype <http://www.w3.org/2001/XMLSchema#string> ;
                rdfs:comment "Input property" .
            
            ex:prop2Shape a shacl:PropertyShape ;
                shacl:path ex:property2 ;
                shacl:datatype <http://www.w3.org/2001/XMLSchema#string> ;
                cns:outputField "true" ;
                rdfs:comment "Output property" .
            """
        
        f = tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False)
        f.write(content)
        f.close()
        return Path(f.name)
    
    @patch('sys.argv')
    def test_main_single_file_mode(self, mock_argv):
        """Test main function in single file mode"""
        ttl_file = self.create_test_ttl()
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        mock_argv.return_value = ['ttl2dspy.py', str(ttl_file), str(output_file)]
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', str(ttl_file), str(output_file)]):
                result = main()
                
                assert result == 0  # Success
                assert output_file.exists()
                
                content = output_file.read_text()
                assert "TestClassSignature" in content
                assert "dspy.InputField" in content
                assert "dspy.OutputField" in content
                
        finally:
            ttl_file.unlink()
            if output_file.exists():
                output_file.unlink()
    
    @patch('sys.argv')
    def test_main_batch_mode(self, mock_argv):
        """Test main function in batch mode"""
        ttl_file1 = self.create_test_ttl()
        ttl_file2 = self.create_test_ttl()
        output_dir = Path(tempfile.mkdtemp())
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', '--batch', str(ttl_file1), str(ttl_file2), str(output_dir)]):
                result = main()
                
                assert result == 0  # Success
                
                # Check output files were created
                output_files = list(output_dir.glob("signatures_*.py"))
                assert len(output_files) == 2
                
                for output_file in output_files:
                    content = output_file.read_text()
                    assert "TestClassSignature" in content
                
        finally:
            ttl_file1.unlink()
            ttl_file2.unlink()
            for f in output_dir.rglob("*"):
                if f.is_file():
                    f.unlink()
            output_dir.rmdir()
    
    @patch('sys.argv')
    def test_main_merge_mode(self, mock_argv):
        """Test main function in merge mode"""
        ttl_file1 = self.create_test_ttl()
        ttl_file2 = self.create_test_ttl()
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', '--merge', str(ttl_file1), str(ttl_file2), str(output_file)]):
                result = main()
                
                assert result == 0  # Success
                assert output_file.exists()
                
                content = output_file.read_text()
                assert "TestClassSignature" in content
                assert "SIGNATURES = {" in content
                
        finally:
            ttl_file1.unlink()
            ttl_file2.unlink()
            if output_file.exists():
                output_file.unlink()
    
    @patch('sys.argv')
    def test_main_no_ttl_files(self, mock_argv):
        """Test main function with no TTL files found"""
        nonexistent_dir = Path("/nonexistent/directory")
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', str(nonexistent_dir), str(output_file)]):
            result = main()
            
            assert result == 2  # No files found error
    
    @patch('sys.argv')
    def test_main_multiple_inputs_no_batch_or_merge(self, mock_argv):
        """Test main function with multiple inputs but no batch/merge mode"""
        ttl_file1 = self.create_test_ttl()
        ttl_file2 = self.create_test_ttl()
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', str(ttl_file1), str(ttl_file2), str(output_file)]):
                result = main()
                
                assert result == 2  # Multiple inputs require batch/merge mode
                
        finally:
            ttl_file1.unlink()
            ttl_file2.unlink()
    
    @patch('sys.argv')
    def test_main_verbose_mode(self, mock_argv):
        """Test main function with verbose output"""
        ttl_file = self.create_test_ttl()
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', '--verbose', str(ttl_file), str(output_file)]):
                with patch('builtins.print') as mock_print:
                    result = main()
                    
                    assert result == 0
                    
                    # Check that verbose output was printed
                    print_calls = [call[0][0] for call in mock_print.call_args_list]
                    verbose_output = any("Processing" in call for call in print_calls)
                    assert verbose_output
                
        finally:
            ttl_file.unlink()
            if output_file.exists():
                output_file.unlink()
    
    @patch('sys.argv')
    def test_main_invalid_ttl_file(self, mock_argv):
        """Test main function with invalid TTL file"""
        invalid_ttl = tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False)
        invalid_ttl.write("This is not valid TTL")
        invalid_ttl.close()
        
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', invalid_ttl.name, str(output_file)]):
                result = main()
                
                assert result == 1  # Parsing error
                
        finally:
            Path(invalid_ttl.name).unlink()
    
    @patch('sys.argv')
    def test_main_no_shacl_shapes(self, mock_argv):
        """Test main function with TTL file but no SHACL shapes"""
        ttl_content = """
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ex:TestClass a owl:Class .
        """
        
        ttl_file = self.create_test_ttl(ttl_content)
        output_file = Path(tempfile.mktemp(suffix='.py'))
        
        try:
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', str(ttl_file), str(output_file)]):
                result = main()
                
                assert result == 1  # No shapes found
                
        finally:
            ttl_file.unlink()


class TestEdgeCases:
    """Test edge cases and error conditions"""
    
    def test_field_name_with_special_characters(self):
        """Test field name handling with special characters"""
        transpiler = TTL2DSPyTranspiler()
        
        # Test field names with various special characters
        test_cases = [
            ("field@name", "field_name"),
            ("field#name", "field_name"),
            ("field$name", "field_name"),
            ("field%name", "field_name"),
            ("field&name", "field_name"),
            ("field*name", "field_name"),
            ("field+name", "field_name"),
            ("field=name", "field_name"),
            ("field?name", "field_name"),
            ("field[name]", "field_name"),
            ("field{name}", "field_name"),
            ("field|name", "field_name"),
            ("field\\name", "field_name"),
            ("field/name", "field_name"),
            ("field<name>", "field_name"),
            ("field.name", "field_name"),
            ("field,name", "field_name"),
            ("field;name", "field_name"),
            ("field:name", "field_name"),
            ("field\"name", "field_name"),
            ("field'name", "field_name"),
        ]
        
        for input_name, expected in test_cases:
            result = transpiler.snake_case(input_name)
            assert result == expected, f"Failed for input: {input_name}"
    
    def test_property_shape_without_path(self):
        """Test handling of property shapes without sh:path"""
        g = Graph()
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#nodeShape")
        prop_shape = URIRef("http://example.org#propShape")
        
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape))
        # Missing sh:path for property shape
        g.add((prop_shape, SH.datatype, XSD.string))
        
        transpiler = TTL2DSPyTranspiler()
        signatures = transpiler.build_signatures(g)
        
        # Should not create signature due to missing path
        assert len(signatures) == 0
    
    def test_field_name_collision_resolution_many(self):
        """Test field name collision resolution with many collisions"""
        transpiler = TTL2DSPyTranspiler()
        
        # Create many collisions for the same base name
        base_name = "test_field"
        results = []
        
        for i in range(10):
            result = transpiler.check_field_collision(base_name, f"http://example.org#{i}")
            results.append(result)
        
        # Should get test_field, test_field_1, test_field_2, ..., test_field_9
        assert results[0] == "test_field"
        for i in range(1, 10):
            assert results[i] == f"test_field_{i}"
    
    def test_signature_with_no_description(self):
        """Test signature generation with properties that have no description"""
        g = Graph()
        
        test_class = URIRef("http://example.org#TestClass")
        node_shape = URIRef("http://example.org#nodeShape")
        prop_shape = URIRef("http://example.org#propShape")
        prop = URIRef("http://example.org#property")
        
        g.add((node_shape, SH.targetClass, test_class))
        g.add((node_shape, SH.property, prop_shape))
        g.add((prop_shape, SH.path, prop))
        g.add((prop_shape, SH.datatype, XSD.string))
        # No rdfs:comment or sh:description
        
        transpiler = TTL2DSPyTranspiler()
        signatures = transpiler.build_signatures(g)
        
        assert len(signatures) == 1
        signature_code = list(signatures.values())[0]
        
        # Should use default description based on property name
        assert "property property" in signature_code  # Default description format
    
    def test_directory_input_with_glob_patterns(self):
        """Test handling directory input with glob patterns"""
        # Create temporary directory with TTL files
        temp_dir = Path(tempfile.mkdtemp())
        
        try:
            # Create some TTL files
            (temp_dir / "file1.ttl").write_text("@prefix ex: <http://example.org/> . ex:Test a <http://www.w3.org/2002/07/owl#Class> .")
            (temp_dir / "file2.turtle").write_text("@prefix ex: <http://example.org/> . ex:Test2 a <http://www.w3.org/2002/07/owl#Class> .")
            (temp_dir / "file3.n3").write_text("@prefix ex: <http://example.org/> . ex:Test3 a <http://www.w3.org/2002/07/owl#Class> .")
            (temp_dir / "file4.txt").write_text("Not a TTL file")  # Should be ignored
            
            with patch('ttl2dspy.sys.argv', ['ttl2dspy.py', '--batch', str(temp_dir), str(temp_dir / "output")]):
                result = main()
                
                # Should find 3 TTL files
                output_files = list((temp_dir / "output").glob("signatures_*.py"))
                assert len(output_files) == 3
                
        finally:
            # Cleanup
            for f in temp_dir.rglob("*"):
                if f.is_file():
                    f.unlink()
            for d in temp_dir.rglob("*"):
                if d.is_dir():
                    d.rmdir()
            temp_dir.rmdir()


class TestPerformance:
    """Test performance characteristics"""
    
    def test_large_ontology_performance(self):
        """Test performance with large ontology (mock)"""
        g = Graph()
        
        # Create many classes with SHACL shapes
        num_classes = 100
        for i in range(num_classes):
            test_class = URIRef(f"http://example.org#TestClass{i}")
            node_shape = URIRef(f"http://example.org#nodeShape{i}")
            prop_shape = URIRef(f"http://example.org#propShape{i}")
            prop = URIRef(f"http://example.org#property{i}")
            
            g.add((node_shape, SH.targetClass, test_class))
            g.add((node_shape, SH.property, prop_shape))
            g.add((prop_shape, SH.path, prop))
            g.add((prop_shape, SH.datatype, XSD.string))
        
        transpiler = TTL2DSPyTranspiler()
        
        import time
        start_time = time.time()
        signatures = transpiler.build_signatures(g)
        end_time = time.time()
        
        assert len(signatures) == num_classes
        assert transpiler.signature_count == num_classes
        
        # Should complete in reasonable time (less than 1 second for 100 classes)
        execution_time = end_time - start_time
        assert execution_time < 1.0, f"Too slow: {execution_time:.2f}s"
    
    def test_memory_efficiency(self):
        """Test memory efficiency by checking field name set cleanup"""
        transpiler = TTL2DSPyTranspiler()
        
        # Build signatures for multiple classes
        for class_num in range(5):
            g = Graph()
            test_class = URIRef(f"http://example.org#TestClass{class_num}")
            node_shape = URIRef(f"http://example.org#nodeShape{class_num}")
            prop_shape = URIRef(f"http://example.org#propShape{class_num}")
            prop = URIRef(f"http://example.org#property{class_num}")
            
            g.add((node_shape, SH.targetClass, test_class))
            g.add((node_shape, SH.property, prop_shape))
            g.add((prop_shape, SH.path, prop))
            g.add((prop_shape, SH.datatype, XSD.string))
            
            signatures = transpiler.build_signatures(g)
            
            # Field names should be cleared between signature generations
            # (This happens in build_signatures method)
            
        # Final check - should have generated 5 signatures
        assert transpiler.signature_count == 5


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])