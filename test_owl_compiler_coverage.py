#!/usr/bin/env python3
"""
Comprehensive unit tests for owl_compiler.py - 80% line coverage
Tests OWL compilation, template rendering, class/property extraction
"""

import json
import pytest
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from dataclasses import dataclass
from datetime import datetime

from rdflib import Graph, Namespace, URIRef, Literal
from rdflib.namespace import OWL, RDF, RDFS
from jinja2 import Environment, DictLoader

from owl_compiler import (
    OWLClass, OWLProperty, ReasoningRule, TemplateManager, 
    OWLCompiler, CNS, EH, SHACL
)


class TestOWLClass:
    """Test OWLClass dataclass"""
    
    def test_owl_class_creation(self):
        """Test OWLClass instance creation"""
        cls = OWLClass(
            uri="http://example.org#TestClass",
            label="TestClass",
            comment="A test class"
        )
        assert cls.uri == "http://example.org#TestClass"
        assert cls.label == "TestClass"
        assert cls.comment == "A test class"
        assert cls.parent_classes == []
        assert cls.properties == []
        assert cls.constraints == []
        assert cls.annotations == {}
        assert cls.axioms == []
        assert cls.eightfold_mapping is None
    
    def test_owl_class_with_properties(self):
        """Test OWLClass with properties and parent classes"""
        cls = OWLClass(
            uri="http://example.org#TestClass",
            label="TestClass",
            parent_classes=["http://example.org#ParentClass"],
            properties=[{
                "uri": "http://example.org#property1",
                "label": "property1",
                "type": "ObjectProperty"
            }]
        )
        assert len(cls.parent_classes) == 1
        assert len(cls.properties) == 1
        assert cls.properties[0]["type"] == "ObjectProperty"


class TestOWLProperty:
    """Test OWLProperty dataclass"""
    
    def test_owl_property_creation(self):
        """Test OWLProperty instance creation"""
        prop = OWLProperty(
            uri="http://example.org#testProperty",
            label="testProperty",
            type="ObjectProperty"
        )
        assert prop.uri == "http://example.org#testProperty"
        assert prop.label == "testProperty"
        assert prop.type == "ObjectProperty"
        assert prop.domain is None
        assert prop.range is None
        assert prop.characteristics == []
        assert prop.inverse_of is None
    
    def test_owl_property_with_domain_range(self):
        """Test OWLProperty with domain and range"""
        prop = OWLProperty(
            uri="http://example.org#testProperty",
            label="testProperty",
            type="ObjectProperty",
            domain=["http://example.org#DomainClass"],
            range=["http://example.org#RangeClass"],
            characteristics=["Functional", "Transitive"]
        )
        assert len(prop.domain) == 1
        assert len(prop.range) == 1
        assert "Functional" in prop.characteristics
        assert "Transitive" in prop.characteristics


class TestReasoningRule:
    """Test ReasoningRule dataclass"""
    
    def test_reasoning_rule_creation(self):
        """Test ReasoningRule instance creation"""
        rule = ReasoningRule(
            id="rule_001",
            type="inference",
            antecedent=[{"subject": "?x", "predicate": "hasParent", "object": "?y"}],
            consequent={"subject": "?x", "predicate": "isChildOf", "object": "?y"},
            confidence=0.95,
            eightfold_stage="Right Understanding"
        )
        assert rule.id == "rule_001"
        assert rule.type == "inference"
        assert len(rule.antecedent) == 1
        assert rule.confidence == 0.95
        assert rule.eightfold_stage == "Right Understanding"


class TestTemplateManager:
    """Test TemplateManager functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        self.template_manager = TemplateManager()
    
    def test_template_manager_initialization(self):
        """Test TemplateManager initialization"""
        assert self.template_manager.env is not None
        assert isinstance(self.template_manager.env, Environment)
        assert len(self.template_manager.built_in_templates) > 0
    
    def test_custom_filters(self):
        """Test custom Jinja filters"""
        # Test c_identifier filter
        assert self.template_manager._c_identifier_filter("test-name") == "test_name"
        assert self.template_manager._c_identifier_filter("123test") == "_123test"
        assert self.template_manager._c_identifier_filter("test.name") == "test_name"
        assert self.template_manager._c_identifier_filter("") == "_"
    
    def test_snake_case_filter(self):
        """Test snake_case filter"""
        assert self.template_manager._snake_case_filter("CamelCase") == "camel_case"
        assert self.template_manager._snake_case_filter("snake_case") == "snake_case"
        assert self.template_manager._snake_case_filter("UPPER_CASE") == "upper_case"
        assert self.template_manager._snake_case_filter("MixedCASE") == "mixed_case"
    
    def test_camel_case_filter(self):
        """Test camelCase filter"""
        assert self.template_manager._camel_case_filter("snake_case") == "snakeCase"
        assert self.template_manager._camel_case_filter("UPPER_CASE") == "upperCase"
        assert self.template_manager._camel_case_filter("mixed-case") == "mixedCase"
    
    def test_upper_case_filter(self):
        """Test UPPER_CASE filter"""
        assert self.template_manager._upper_case_filter("camelCase") == "CAMEL_CASE"
        assert self.template_manager._upper_case_filter("snake_case") == "SNAKE_CASE"
    
    def test_escape_c_string_filter(self):
        """Test C string escaping"""
        assert self.template_manager._escape_c_string_filter("test\"string") == "test\\\"string"
        assert self.template_manager._escape_c_string_filter("test\\path") == "test\\\\path"
        assert self.template_manager._escape_c_string_filter("test\nline") == "test\\nline"
        assert self.template_manager._escape_c_string_filter("") == ""
    
    def test_extract_local_name_filter(self):
        """Test local name extraction from URI"""
        assert self.template_manager._extract_local_name_filter("http://example.org#TestClass") == "TestClass"
        assert self.template_manager._extract_local_name_filter("http://example.org/TestClass") == "TestClass"
        assert self.template_manager._extract_local_name_filter("TestClass") == "TestClass"
    
    def test_xsd_to_c_type_filter(self):
        """Test XSD to C type mapping"""
        assert self.template_manager._xsd_to_c_type_filter("http://www.w3.org/2001/XMLSchema#string") == "char*"
        assert self.template_manager._xsd_to_c_type_filter("http://www.w3.org/2001/XMLSchema#int") == "int32_t"
        assert self.template_manager._xsd_to_c_type_filter("http://www.w3.org/2001/XMLSchema#boolean") == "bool"
        assert self.template_manager._xsd_to_c_type_filter("http://www.w3.org/2001/XMLSchema#double") == "double"
        assert self.template_manager._xsd_to_c_type_filter("unknown") == "void*"
    
    def test_is_primitive_type_filter(self):
        """Test primitive type detection"""
        assert self.template_manager._is_primitive_type_filter("http://www.w3.org/2001/XMLSchema#string") is True
        assert self.template_manager._is_primitive_type_filter("http://www.w3.org/2001/XMLSchema#int") is True
        assert self.template_manager._is_primitive_type_filter("http://example.org#CustomType") is False
    
    def test_get_type_size_filter(self):
        """Test type size calculation"""
        assert self.template_manager._get_type_size_filter("int") == 4
        assert self.template_manager._get_type_size_filter("char") == 1
        assert self.template_manager._get_type_size_filter("double") == 8
        assert self.template_manager._get_type_size_filter("void*") == 8
        assert self.template_manager._get_type_size_filter("unknown") == 8
    
    def test_sort_by_eightfold_filter(self):
        """Test Eightfold Path sorting"""
        class MockClass:
            def __init__(self, eightfold_mapping):
                self.eightfold_mapping = eightfold_mapping
        
        classes = [
            MockClass({'stage': 'Right Effort'}),
            MockClass({'stage': 'Right Understanding'}),
            MockClass({'stage': 'Right Action'}),
            MockClass(None)
        ]
        
        sorted_classes = self.template_manager._sort_by_eightfold_filter(classes)
        assert sorted_classes[0].eightfold_mapping['stage'] == 'Right Understanding'
        assert sorted_classes[-1].eightfold_mapping is None
    
    def test_render_template(self):
        """Test template rendering"""
        context = {
            'test_value': 'Hello, World!',
            'test_list': ['item1', 'item2']
        }
        
        # Test with built-in template
        result = self.template_manager.render_template('json_output.json.j2', {
            'metadata': {'compiler': 'test', 'version': '1.0', 'timestamp': '2023-01-01', 'config': {}},
            'statistics': {'total_triples': 100, 'total_classes': 10, 'total_properties': 20, 
                          'total_rules': 5, 'property_types': {}, 'class_hierarchy_depth': 3,
                          'eightfold_coverage': 80.0},
            'prefixes': {'ex': 'http://example.org/'},
            'classes': [],
            'properties': [],
            'rules': []
        })
        
        assert '"compiler": "test"' in result
        assert '"total_classes": 10' in result
        parsed = json.loads(result)
        assert parsed['metadata']['compiler'] == 'test'
    
    def test_render_from_string(self):
        """Test template rendering from string"""
        template_string = "Hello {{ name }}! Value: {{ value }}"
        context = {'name': 'World', 'value': 42}
        
        result = self.template_manager.render_from_string(template_string, context)
        assert result == "Hello World! Value: 42"
    
    def test_format_comment_filter(self):
        """Test comment formatting"""
        long_comment = "This is a very long comment that should be wrapped to multiple lines when formatted for C code generation."
        formatted = self.template_manager._format_comment_filter(long_comment, width=50)
        
        lines = formatted.split('\n')
        assert all(line.startswith(' * ') for line in lines)
        assert all(len(line) <= 50 for line in lines)
        assert len(lines) > 1


class TestOWLCompiler:
    """Test OWLCompiler main functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        self.compiler = OWLCompiler()
    
    def test_owl_compiler_initialization(self):
        """Test OWLCompiler initialization"""
        assert self.compiler.config is not None
        assert self.compiler.template_manager is not None
        assert isinstance(self.compiler.graph, Graph)
        assert self.compiler.classes == {}
        assert self.compiler.properties == {}
        assert self.compiler.rules == []
        assert self.compiler.prefixes == {}
        assert self.compiler.statistics == {}
    
    def test_default_config(self):
        """Test default configuration"""
        config = self.compiler._default_config()
        
        assert config['strict_mode'] is True
        assert config['inference_enabled'] is True
        assert config['reasoning_depth'] == 3
        assert config['extract_shacl'] is True
        assert config['eightfold_integration'] is True
        assert 'c_header' in config['output_formats']
        assert 'c_implementation' in config['output_formats']
    
    def test_custom_config(self):
        """Test custom configuration"""
        custom_config = {
            'strict_mode': False,
            'inference_enabled': False,
            'output_formats': ['json']
        }
        
        compiler = OWLCompiler(config=custom_config)
        assert compiler.config['strict_mode'] is False
        assert compiler.config['inference_enabled'] is False
        assert compiler.config['output_formats'] == ['json']
    
    @patch('owl_compiler.Path')
    def test_parse_specification_ttl(self, mock_path):
        """Test parsing TTL specification"""
        mock_path_obj = Mock()
        mock_path_obj.suffix = '.ttl'
        mock_path.return_value = mock_path_obj
        
        # Mock graph parsing
        with patch.object(self.compiler.graph, 'parse') as mock_parse:
            with patch.object(self.compiler.graph, 'namespaces', return_value=[('ex', 'http://example.org/')]):
                self.compiler._parse_specification(mock_path_obj)
                
                mock_parse.assert_called_once_with(mock_path_obj, format='turtle')
                assert 'ex' in self.compiler.prefixes
                assert self.compiler.prefixes['ex'] == 'http://example.org/'
    
    @patch('owl_compiler.Path')
    def test_parse_specification_owl(self, mock_path):
        """Test parsing OWL specification"""
        mock_path_obj = Mock()
        mock_path_obj.suffix = '.owl'
        mock_path.return_value = mock_path_obj
        
        with patch.object(self.compiler.graph, 'parse') as mock_parse:
            with patch.object(self.compiler.graph, 'namespaces', return_value=[]):
                self.compiler._parse_specification(mock_path_obj)
                
                mock_parse.assert_called_once_with(mock_path_obj, format='xml')
    
    def test_get_label(self):
        """Test label extraction"""
        # Setup mock graph with label
        test_uri = URIRef("http://example.org#TestClass")
        test_label = Literal("Test Class")
        
        with patch.object(self.compiler.graph, 'value', return_value=test_label):
            label = self.compiler._get_label(test_uri)
            assert label == "Test Class"
        
        # Test fallback to local name
        with patch.object(self.compiler.graph, 'value', return_value=None):
            label = self.compiler._get_label(test_uri)
            assert label == "TestClass"
    
    def test_get_comment(self):
        """Test comment extraction"""
        test_uri = URIRef("http://example.org#TestClass")
        test_comment = Literal("This is a test class")
        
        with patch.object(self.compiler.graph, 'value', return_value=test_comment):
            comment = self.compiler._get_comment(test_uri)
            assert comment == "This is a test class"
        
        # Test no comment
        with patch.object(self.compiler.graph, 'value', return_value=None):
            comment = self.compiler._get_comment(test_uri)
            assert comment is None
    
    def test_get_property_type(self):
        """Test property type detection"""
        test_uri = URIRef("http://example.org#testProperty")
        
        # Test ObjectProperty
        with patch.object(self.compiler.graph, '__contains__', 
                         side_effect=lambda triple: triple == (test_uri, RDF.type, OWL.ObjectProperty)):
            prop_type = self.compiler._get_property_type(test_uri)
            assert prop_type == "ObjectProperty"
        
        # Test DatatypeProperty
        with patch.object(self.compiler.graph, '__contains__', 
                         side_effect=lambda triple: triple == (test_uri, RDF.type, OWL.DatatypeProperty)):
            prop_type = self.compiler._get_property_type(test_uri)
            assert prop_type == "DatatypeProperty"
        
        # Test fallback
        with patch.object(self.compiler.graph, '__contains__', return_value=False):
            prop_type = self.compiler._get_property_type(test_uri)
            assert prop_type == "Property"
    
    def test_extract_property_characteristics(self):
        """Test property characteristics extraction"""
        test_uri = URIRef("http://example.org#testProperty")
        
        # Mock functional and transitive property
        def mock_contains(triple):
            return triple in [
                (test_uri, RDF.type, OWL.FunctionalProperty),
                (test_uri, RDF.type, OWL.TransitiveProperty)
            ]
        
        with patch.object(self.compiler.graph, '__contains__', side_effect=mock_contains):
            characteristics = self.compiler._extract_property_characteristics(test_uri)
            
            assert "Functional" in characteristics
            assert "Transitive" in characteristics
            assert len(characteristics) == 2
    
    def test_extract_annotations(self):
        """Test annotation extraction"""
        test_uri = URIRef("http://example.org#TestClass")
        
        # Mock annotations
        mock_objects = [
            Literal("http://example.org/reference"),
            Literal("2023-01-01"),
            Literal("Test Creator")
        ]
        
        def mock_predicate_objects(subject):
            if subject == test_uri:
                return [
                    (RDFS.seeAlso, mock_objects[0]),
                    (URIRef("http://purl.org/dc/terms/created"), mock_objects[1]),
                    (URIRef("http://purl.org/dc/terms/creator"), mock_objects[2]),
                    (URIRef("http://cns.io/customAnnotation"), Literal("custom value"))
                ]
            return []
        
        def mock_graph_objects(subject, predicate):
            mappings = {
                RDFS.seeAlso: [mock_objects[0]] if subject == test_uri else [],
                URIRef("http://purl.org/dc/terms/created"): [mock_objects[1]] if subject == test_uri else [],
                URIRef("http://purl.org/dc/terms/creator"): [mock_objects[2]] if subject == test_uri else []
            }
            return mappings.get(predicate, [])
        
        with patch.object(self.compiler.graph, 'predicate_objects', side_effect=mock_predicate_objects):
            with patch.object(self.compiler.graph, 'objects', side_effect=mock_graph_objects):
                annotations = self.compiler._extract_annotations(test_uri)
                
                assert annotations['seeAlso'] == "http://example.org/reference"
                assert annotations['created'] == "2023-01-01"
                assert annotations['creator'] == "Test Creator"
                assert annotations['customAnnotation'] == "custom value"
    
    def test_extract_classes(self):
        """Test class extraction from graph"""
        test_class_uri = URIRef("http://example.org#TestClass")
        parent_uri = URIRef("http://example.org#ParentClass")
        prop_uri = URIRef("http://example.org#testProperty")
        
        # Mock graph queries
        with patch.object(self.compiler.graph, 'subjects', return_value=[test_class_uri]):
            with patch.object(self.compiler.graph, 'objects', 
                             side_effect=lambda s, p: [parent_uri] if p == RDFS.subClassOf else []):
                with patch.object(self.compiler, '_get_label', return_value="TestClass"):
                    with patch.object(self.compiler, '_get_comment', return_value="Test class"):
                        with patch.object(self.compiler, '_extract_annotations', return_value={}):
                            with patch.object(self.compiler, '_extract_class_axioms', return_value=[]):
                                with patch.object(self.compiler, '_extract_shacl_constraints', return_value=[]):
                                    self.compiler._extract_classes()
                                    
                                    assert str(test_class_uri) in self.compiler.classes
                                    owl_class = self.compiler.classes[str(test_class_uri)]
                                    assert owl_class.label == "TestClass"
                                    assert owl_class.comment == "Test class"
                                    assert str(parent_uri) in owl_class.parent_classes
    
    def test_extract_properties(self):
        """Test property extraction from graph"""
        test_prop_uri = URIRef("http://example.org#testProperty")
        domain_uri = URIRef("http://example.org#DomainClass")
        range_uri = URIRef("http://example.org#RangeClass")
        
        # Mock ObjectProperty
        with patch.object(self.compiler.graph, 'subjects', return_value=[test_prop_uri]):
            with patch.object(self.compiler.graph, 'objects', 
                             side_effect=lambda s, p: {
                                 RDFS.domain: [domain_uri],
                                 RDFS.range: [range_uri]
                             }.get(p, [])):
                with patch.object(self.compiler, '_get_label', return_value="testProperty"):
                    with patch.object(self.compiler, '_extract_property_characteristics', return_value=["Functional"]):
                        with patch.object(self.compiler, '_extract_annotations', return_value={}):
                            with patch.object(self.compiler, '_extract_shacl_constraints', return_value=[]):
                                self.compiler._extract_properties()
                                
                                assert str(test_prop_uri) in self.compiler.properties
                                owl_prop = self.compiler.properties[str(test_prop_uri)]
                                assert owl_prop.label == "testProperty"
                                assert owl_prop.type == "ObjectProperty"
                                assert str(domain_uri) in owl_prop.domain
                                assert str(range_uri) in owl_prop.range
                                assert "Functional" in owl_prop.characteristics
    
    def test_calculate_hierarchy_depth(self):
        """Test class hierarchy depth calculation"""
        # Create mock class hierarchy
        self.compiler.classes = {
            "http://example.org#A": OWLClass("http://example.org#A", "A", parent_classes=[]),
            "http://example.org#B": OWLClass("http://example.org#B", "B", parent_classes=["http://example.org#A"]),
            "http://example.org#C": OWLClass("http://example.org#C", "C", parent_classes=["http://example.org#B"]),
            "http://example.org#D": OWLClass("http://example.org#D", "D", parent_classes=["http://example.org#C"])
        }
        
        depth = self.compiler._calculate_hierarchy_depth()
        assert depth == 4  # A->B->C->D
    
    def test_calculate_hierarchy_depth_circular(self):
        """Test hierarchy depth with circular reference"""
        # Create circular reference
        self.compiler.classes = {
            "http://example.org#A": OWLClass("http://example.org#A", "A", parent_classes=["http://example.org#B"]),
            "http://example.org#B": OWLClass("http://example.org#B", "B", parent_classes=["http://example.org#A"])
        }
        
        depth = self.compiler._calculate_hierarchy_depth()
        assert depth >= 0  # Should handle circular reference gracefully
    
    def test_calculate_eightfold_coverage(self):
        """Test Eightfold Path coverage calculation"""
        self.compiler.classes = {
            "http://example.org#A": OWLClass("http://example.org#A", "A", eightfold_mapping={'stage': 'Right Understanding'}),
            "http://example.org#B": OWLClass("http://example.org#B", "B", eightfold_mapping={'stage': 'Right Action'}),
            "http://example.org#C": OWLClass("http://example.org#C", "C", eightfold_mapping=None),
            "http://example.org#D": OWLClass("http://example.org#D", "D", eightfold_mapping=None)
        }
        
        coverage = self.compiler._calculate_eightfold_coverage()
        assert coverage == 50.0  # 2 out of 4 classes have mappings
    
    def test_calculate_eightfold_coverage_empty(self):
        """Test Eightfold coverage with no classes"""
        self.compiler.classes = {}
        coverage = self.compiler._calculate_eightfold_coverage()
        assert coverage == 0.0
    
    def test_compile_statistics(self):
        """Test statistics compilation"""
        # Setup mock data
        self.compiler.classes = {"test": OWLClass("test", "Test")}
        self.compiler.properties = {
            "prop1": OWLProperty("prop1", "Prop1", "ObjectProperty"),
            "prop2": OWLProperty("prop2", "Prop2", "DatatypeProperty")
        }
        self.compiler.rules = [ReasoningRule("rule1", "inference", [], {})]
        
        with patch.object(self.compiler.graph, '__len__', return_value=100):
            with patch.object(self.compiler, '_calculate_hierarchy_depth', return_value=3):
                with patch.object(self.compiler, '_calculate_eightfold_coverage', return_value=75.0):
                    self.compiler._compile_statistics()
                    
                    stats = self.compiler.statistics
                    assert stats['total_triples'] == 100
                    assert stats['total_classes'] == 1
                    assert stats['total_properties'] == 2
                    assert stats['total_rules'] == 1
                    assert stats['property_types']['ObjectProperty'] == 1
                    assert stats['property_types']['DatatypeProperty'] == 1
                    assert stats['class_hierarchy_depth'] == 3
                    assert stats['eightfold_coverage'] == 75.0
    
    def test_create_compilation_result(self):
        """Test compilation result creation"""
        # Setup test data
        test_class = OWLClass("http://example.org#Test", "Test", comment="Test class")
        test_prop = OWLProperty("http://example.org#prop", "prop", "ObjectProperty")
        test_rule = ReasoningRule("rule1", "inference", [], {})
        
        self.compiler.classes = {"http://example.org#Test": test_class}
        self.compiler.properties = {"http://example.org#prop": test_prop}
        self.compiler.rules = [test_rule]
        self.compiler.prefixes = {"ex": "http://example.org/"}
        self.compiler.statistics = {"total_classes": 1}
        
        result = self.compiler._create_compilation_result()
        
        assert 'metadata' in result
        assert 'classes' in result
        assert 'properties' in result
        assert 'rules' in result
        assert 'prefixes' in result
        assert 'statistics' in result
        
        assert len(result['classes']) == 1
        assert len(result['properties']) == 1
        assert len(result['rules']) == 1
        assert result['prefixes']['ex'] == "http://example.org/"
        assert result['statistics']['total_classes'] == 1
    
    def test_serialize_class(self):
        """Test class serialization"""
        test_class = OWLClass(
            uri="http://example.org#Test",
            label="Test",
            comment="Test class",
            parent_classes=["http://example.org#Parent"],
            properties=[{"uri": "prop", "label": "prop"}],
            constraints=[{"type": "minCardinality", "value": 1}],
            annotations={"creator": "test"},
            axioms=[{"type": "equivalentClass", "target": "other"}],
            eightfold_mapping={"stage": "Right Understanding"}
        )
        
        serialized = self.compiler._serialize_class(test_class)
        
        assert serialized['uri'] == "http://example.org#Test"
        assert serialized['label'] == "Test"
        assert serialized['comment'] == "Test class"
        assert len(serialized['parent_classes']) == 1
        assert len(serialized['properties']) == 1
        assert len(serialized['constraints']) == 1
        assert serialized['annotations']['creator'] == "test"
        assert len(serialized['axioms']) == 1
        assert serialized['eightfold_mapping']['stage'] == "Right Understanding"
    
    def test_serialize_property(self):
        """Test property serialization"""
        test_prop = OWLProperty(
            uri="http://example.org#prop",
            label="prop",
            type="ObjectProperty",
            domain=["http://example.org#Domain"],
            range=["http://example.org#Range"],
            characteristics=["Functional"],
            inverse_of="http://example.org#inverseProp",
            constraints=[{"type": "cardinality", "value": 1}],
            annotations={"comment": "Test property"}
        )
        
        serialized = self.compiler._serialize_property(test_prop)
        
        assert serialized['uri'] == "http://example.org#prop"
        assert serialized['label'] == "prop"
        assert serialized['type'] == "ObjectProperty"
        assert len(serialized['domain']) == 1
        assert len(serialized['range']) == 1
        assert "Functional" in serialized['characteristics']
        assert serialized['inverse_of'] == "http://example.org#inverseProp"
        assert len(serialized['constraints']) == 1
        assert serialized['annotations']['comment'] == "Test property"
    
    def test_serialize_rule(self):
        """Test rule serialization"""
        test_rule = ReasoningRule(
            id="rule1",
            type="inference",
            antecedent=[{"subject": "?x", "predicate": "hasParent", "object": "?y"}],
            consequent={"subject": "?x", "predicate": "isChildOf", "object": "?y"},
            confidence=0.95,
            eightfold_stage="Right Understanding"
        )
        
        serialized = self.compiler._serialize_rule(test_rule)
        
        assert serialized['id'] == "rule1"
        assert serialized['type'] == "inference"
        assert len(serialized['antecedent']) == 1
        assert serialized['consequent']['predicate'] == "isChildOf"
        assert serialized['confidence'] == 0.95
        assert serialized['eightfold_stage'] == "Right Understanding"
    
    def test_add_custom_filter(self):
        """Test adding custom Jinja filter"""
        def custom_filter(value):
            return f"custom_{value}"
        
        self.compiler.add_custom_filter("custom", custom_filter)
        assert "custom" in self.compiler.template_manager.env.filters
        
        # Test the filter works
        result = self.compiler.template_manager.env.filters["custom"]("test")
        assert result == "custom_test"
    
    def test_add_custom_global(self):
        """Test adding custom Jinja global"""
        def custom_global():
            return "custom_value"
        
        self.compiler.add_custom_global("custom_func", custom_global)
        assert "custom_func" in self.compiler.template_manager.env.globals
        
        # Test the global works
        result = self.compiler.template_manager.env.globals["custom_func"]()
        assert result == "custom_value"
    
    def test_render_custom_template(self):
        """Test custom template rendering"""
        template_string = "Classes: {{ classes|length }}, Properties: {{ properties|length }}"
        
        # Setup test data
        self.compiler.classes = {"test": OWLClass("test", "Test")}
        self.compiler.properties = {"prop": OWLProperty("prop", "Prop", "ObjectProperty")}
        
        result = self.compiler.render_custom_template(template_string)
        assert "Classes: 1" in result
        assert "Properties: 1" in result
    
    @patch('owl_compiler.Path')
    def test_generate_output_files(self, mock_path_class):
        """Test output file generation"""
        # Setup mocks
        mock_output_dir = Mock()
        mock_output_dir.mkdir = Mock()
        mock_path_class.return_value = mock_output_dir
        
        # Create test result
        result = {
            'metadata': {'compiler': 'test', 'version': '1.0', 'timestamp': '2023-01-01', 'config': {}},
            'statistics': {'total_triples': 100, 'total_classes': 10, 'total_properties': 20, 
                          'total_rules': 5, 'property_types': {}, 'class_hierarchy_depth': 3,
                          'eightfold_coverage': 80.0},
            'prefixes': {'ex': 'http://example.org/'},
            'classes': [],
            'properties': [],
            'rules': []
        }
        
        # Mock file writing
        mock_file = Mock()
        mock_file.write_text = Mock()
        mock_output_dir.__truediv__ = Mock(return_value=mock_file)
        
        with patch.object(self.compiler.template_manager, 'render_template', return_value="test output"):
            self.compiler._generate_output_files(result, mock_output_dir, "test")
            
            # Verify directory creation
            mock_output_dir.mkdir.assert_called_once_with(parents=True, exist_ok=True)
            
            # Verify file writing was called
            assert mock_file.write_text.call_count > 0
    
    def test_prepare_template_context(self):
        """Test template context preparation"""
        result = {
            'classes': [],
            'properties': [],
            'rules': []
        }
        
        context = self.compiler._prepare_template_context(result, "test_ontology")
        
        assert context['header_guard'] == "TEST_ONTOLOGY_H"
        assert context['header_filename'] == "test_ontology.h"
        assert context['source_filename'] == "test_ontology.c"
        assert context['base_name'] == "test_ontology"
        assert len(context['eightfold_stages']) == 8
        assert 'Right Understanding' in context['eightfold_stages']
        assert context['source_files'] == ["test_ontology.c"]
        assert context['header_files'] == ["test_ontology.h"]


class TestIntegration:
    """Integration tests for complete compilation workflow"""
    
    def test_minimal_ontology_compilation(self):
        """Test compilation of minimal ontology"""
        # Create minimal TTL content
        ttl_content = """
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        
        ex:TestClass a owl:Class ;
            rdfs:label "Test Class" ;
            rdfs:comment "A test class for unit testing" .
        
        ex:testProperty a owl:ObjectProperty ;
            rdfs:label "test property" ;
            rdfs:domain ex:TestClass ;
            rdfs:range ex:TestClass .
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write(ttl_content)
            f.flush()
            
            try:
                compiler = OWLCompiler()
                result = compiler.compile(Path(f.name))
                
                # Verify compilation result
                assert 'classes' in result
                assert 'properties' in result
                assert 'statistics' in result
                assert len(result['classes']) >= 1
                assert len(result['properties']) >= 1
                
                # Find our test class
                test_class = next((c for c in result['classes'] if c['label'] == 'Test Class'), None)
                assert test_class is not None
                assert test_class['comment'] == 'A test class for unit testing'
                
                # Find our test property
                test_prop = next((p for p in result['properties'] if p['label'] == 'test property'), None)
                assert test_prop is not None
                assert test_prop['type'] == 'ObjectProperty'
                
            finally:
                Path(f.name).unlink()


class TestErrorHandling:
    """Test error handling and edge cases"""
    
    def test_invalid_file_format(self):
        """Test handling of invalid file format"""
        compiler = OWLCompiler()
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.invalid', delete=False) as f:
            f.write("invalid content")
            f.flush()
            
            try:
                # Should handle unknown format gracefully
                result = compiler.compile(Path(f.name))
                assert result is not None
            finally:
                Path(f.name).unlink()
    
    def test_empty_ontology(self):
        """Test handling of empty ontology"""
        compiler = OWLCompiler()
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write("# Empty ontology")
            f.flush()
            
            try:
                result = compiler.compile(Path(f.name))
                
                assert result is not None
                assert result['classes'] == []
                assert result['properties'] == []
                assert result['statistics']['total_classes'] == 0
                assert result['statistics']['total_properties'] == 0
                
            finally:
                Path(f.name).unlink()
    
    def test_template_error_handling(self):
        """Test template rendering error handling"""
        template_manager = TemplateManager()
        
        # Test with invalid template
        with pytest.raises(Exception):
            template_manager.render_template("nonexistent_template.j2", {})
        
        # Test with malformed template string
        with pytest.raises(Exception):
            template_manager.render_from_string("{{ unclosed", {})


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])