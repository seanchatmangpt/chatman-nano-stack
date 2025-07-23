#!/usr/bin/env python3
"""
OWL AOT Compiler - Semantic Processing Component with Jinja Templating
Processes OWL/TTL ontologies for the AOT pipeline using Jinja2 templates
"""

import os
import re
import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Set
from dataclasses import dataclass, field
from datetime import datetime
import rdflib
from rdflib import Graph, Namespace, RDF, RDFS, OWL, URIRef, Literal, BNode
from rdflib.namespace import SKOS, DCTERMS, XSD
import json
from jinja2 import Environment, FileSystemLoader, Template, DictLoader

logger = logging.getLogger(__name__)

# Define namespaces
CNS = Namespace("http://cns.io/ontology#")
EH = Namespace("http://cns.io/eightfold#")
SHACL = Namespace("http://www.w3.org/ns/shacl#")

@dataclass
class OWLClass:
    """Represents an OWL class with its properties and constraints"""
    uri: str
    label: str
    comment: Optional[str] = None
    parent_classes: List[str] = field(default_factory=list)
    properties: List[Dict[str, Any]] = field(default_factory=list)
    constraints: List[Dict[str, Any]] = field(default_factory=list)
    annotations: Dict[str, Any] = field(default_factory=dict)
    axioms: List[Dict[str, Any]] = field(default_factory=list)
    eightfold_mapping: Optional[Dict[str, Any]] = None

@dataclass
class OWLProperty:
    """Represents an OWL property"""
    uri: str
    label: str
    type: str  # ObjectProperty, DatatypeProperty, AnnotationProperty
    domain: Optional[List[str]] = None
    range: Optional[List[str]] = None
    characteristics: List[str] = field(default_factory=list)  # Functional, InverseFunctional, Transitive, etc.
    inverse_of: Optional[str] = None
    constraints: List[Dict[str, Any]] = field(default_factory=list)
    annotations: Dict[str, Any] = field(default_factory=dict)

@dataclass
class ReasoningRule:
    """Represents a reasoning rule extracted from the ontology"""
    id: str
    type: str  # inference, constraint, axiom
    antecedent: List[Dict[str, Any]]
    consequent: Dict[str, Any]
    confidence: float = 1.0
    eightfold_stage: Optional[str] = None

class TemplateManager:
    """Manages Jinja2 templates for code generation"""
    
    def __init__(self, template_dir: Optional[Path] = None):
        self.template_dir = template_dir or Path(__file__).parent / "templates"
        self.env = self._setup_environment()
        self.built_in_templates = self._get_builtin_templates()
    
    def _setup_environment(self) -> Environment:
        """Setup Jinja2 environment with custom filters"""
        if self.template_dir.exists():
            loader = FileSystemLoader(str(self.template_dir))
        else:
            loader = DictLoader(self._get_builtin_templates())
        
        env = Environment(
            loader=loader,
            trim_blocks=True,
            lstrip_blocks=True,
            keep_trailing_newline=True
        )
        
        # Add custom filters
        env.filters.update({
            'c_identifier': self._c_identifier_filter,
            'camel_case': self._camel_case_filter,
            'snake_case': self._snake_case_filter,
            'upper_case': self._upper_case_filter,
            'escape_c_string': self._escape_c_string_filter,
            'format_comment': self._format_comment_filter,
            'extract_local_name': self._extract_local_name_filter,
            'get_type_size': self._get_type_size_filter,
            'is_primitive_type': self._is_primitive_type_filter,
            'xsd_to_c_type': self._xsd_to_c_type_filter,
            'sort_by_eightfold': self._sort_by_eightfold_filter
        })
        
        # Add custom functions
        env.globals.update({
            'now': datetime.now,
            'enumerate': enumerate,
            'zip': zip,
            'len': len,
            'max': max,
            'min': min,
            'sum': sum,
            'sorted': sorted
        })
        
        return env
    
    def _get_builtin_templates(self) -> Dict[str, str]:
        """Return built-in templates as dictionary"""
        return {
            'c_header.h.j2': '''/*
 * Generated OWL C Header
 * Timestamp: {{ now().isoformat() }}
 * Compiler: {{ metadata.compiler }} {{ metadata.version }}
 */

#ifndef {{ header_guard }}
#define {{ header_guard }}

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Ontology Metadata */
#define ONTOLOGY_VERSION "{{ metadata.version }}"
#define ONTOLOGY_TIMESTAMP "{{ metadata.timestamp }}"
#define TOTAL_CLASSES {{ statistics.total_classes }}
#define TOTAL_PROPERTIES {{ statistics.total_properties }}
#define TOTAL_RULES {{ statistics.total_rules }}

/* Namespace Prefixes */
{% for prefix, namespace in prefixes.items() %}
#define PREFIX_{{ prefix|upper }}_URI "{{ namespace }}"
{% endfor %}

/* Forward Declarations */
{% for class in classes %}
typedef struct {{ class.label|c_identifier }}_s {{ class.label|c_identifier }}_t;
{% endfor %}

/* Eightfold Path Stages */
typedef enum {
{% for stage in eightfold_stages %}
    EIGHTFOLD_{{ stage|upper_case }},
{% endfor %}
    EIGHTFOLD_STAGE_COUNT
} eightfold_stage_t;

/* Property Types */
typedef enum {
    PROPERTY_TYPE_OBJECT,
    PROPERTY_TYPE_DATATYPE,
    PROPERTY_TYPE_ANNOTATION
} property_type_t;

/* Property Characteristics */
typedef enum {
    PROP_FUNCTIONAL = 1 << 0,
    PROP_INVERSE_FUNCTIONAL = 1 << 1,
    PROP_TRANSITIVE = 1 << 2,
    PROP_SYMMETRIC = 1 << 3,
    PROP_ASYMMETRIC = 1 << 4,
    PROP_REFLEXIVE = 1 << 5,
    PROP_IRREFLEXIVE = 1 << 6
} property_characteristics_t;

/* Base OWL Object */
typedef struct {
    const char* uri;
    const char* label;
    const char* comment;
    eightfold_stage_t eightfold_stage;
    uint32_t type_id;
    void* instance_data;
} owl_object_t;

/* Property Descriptor */
typedef struct {
    const char* uri;
    const char* label;
    property_type_t type;
    property_characteristics_t characteristics;
    const char** domain_classes;
    const char** range_classes;
    const char* inverse_property;
    size_t domain_count;
    size_t range_count;
} property_descriptor_t;

/* Class Definitions */
{% for class in classes %}
/* {{ class.comment|format_comment if class.comment else "Class: " + class.label }} */
struct {{ class.label|c_identifier }}_s {
    owl_object_t base;
    
    /* Parent class data */
    {% for parent in class.parent_classes %}
    {{ parent|extract_local_name|c_identifier }}_t* {{ parent|extract_local_name|snake_case }}_parent;
    {% endfor %}
    
    /* Properties */
    {% for prop in class.properties %}
    {% if prop.range %}
    {% for range_type in prop.range %}
    {% if range_type|is_primitive_type %}
    {{ range_type|xsd_to_c_type }} {{ prop.label|snake_case }};
    {% else %}
    {{ range_type|extract_local_name|c_identifier }}_t* {{ prop.label|snake_case }};
    {% endif %}
    {% endfor %}
    {% else %}
    void* {{ prop.label|snake_case }};
    {% endif %}
    {% endfor %}
    
    /* Constraint validation flags */
    {% for constraint in class.constraints %}
    bool constraint_{{ loop.index0 }}_valid;
    {% endfor %}
    
    /* Optimization hints */
    {% if class.annotations.optimization_hints %}
    struct {
        {% for hint in class.annotations.optimization_hints %}
        bool {{ hint.type|snake_case }}_enabled;
        {% endfor %}
    } optimization;
    {% endif %}
};

/* Constructor/Destructor for {{ class.label }} */
{{ class.label|c_identifier }}_t* {{ class.label|snake_case }}_create(void);
void {{ class.label|snake_case }}_destroy({{ class.label|c_identifier }}_t* obj);
bool {{ class.label|snake_case }}_validate(const {{ class.label|c_identifier }}_t* obj);

{% endfor %}

/* Property Descriptors Array */
extern const property_descriptor_t g_property_descriptors[{{ properties|length }}];

/* Class Registry */
typedef struct {
    const char* uri;
    const char* label;
    size_t instance_size;
    eightfold_stage_t eightfold_stage;
    const char** parent_classes;
    size_t parent_count;
    owl_object_t* (*constructor)(void);
    void (*destructor)(owl_object_t*);
    bool (*validator)(const owl_object_t*);
} class_descriptor_t;

extern const class_descriptor_t g_class_descriptors[{{ classes|length }}];

/* Reasoning Engine */
typedef struct {
    const char* id;
    const char* type;
    float confidence;
    eightfold_stage_t stage;
    bool (*apply_rule)(owl_object_t* subject);
} reasoning_rule_t;

extern const reasoning_rule_t g_reasoning_rules[{{ rules|length }}];

/* API Functions */
owl_object_t* owl_create_instance(const char* class_uri);
void owl_destroy_instance(owl_object_t* obj);
bool owl_validate_instance(const owl_object_t* obj);
const property_descriptor_t* owl_get_property(const char* property_uri);
const class_descriptor_t* owl_get_class(const char* class_uri);
bool owl_apply_reasoning(owl_object_t* obj);
eightfold_stage_t owl_get_eightfold_stage(const char* class_uri);

/* Eightfold Path Integration */
typedef struct {
    eightfold_stage_t stage;
    owl_object_t** instances;
    size_t instance_count;
    size_t capacity;
} eightfold_context_t;

eightfold_context_t* eightfold_create_context(void);
void eightfold_destroy_context(eightfold_context_t* ctx);
bool eightfold_add_instance(eightfold_context_t* ctx, owl_object_t* obj);
owl_object_t** eightfold_get_stage_instances(eightfold_context_t* ctx, eightfold_stage_t stage);
bool eightfold_execute_stage(eightfold_context_t* ctx, eightfold_stage_t stage);

#ifdef __cplusplus
}
#endif

#endif /* {{ header_guard }} */
''',

            'c_implementation.c.j2': '''/*
 * Generated OWL C Implementation
 * Timestamp: {{ now().isoformat() }}
 * Compiler: {{ metadata.compiler }} {{ metadata.version }}
 */

#include "{{ header_filename }}"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Property Descriptors */
const property_descriptor_t g_property_descriptors[{{ properties|length }}] = {
{% for prop in properties %}
    {
        .uri = "{{ prop.uri }}",
        .label = "{{ prop.label }}",
        .type = PROPERTY_TYPE_{{ 'OBJECT' if prop.type == 'ObjectProperty' else 'DATATYPE' if prop.type == 'DatatypeProperty' else 'ANNOTATION' }},
        .characteristics = {% if prop.characteristics %}{{ prop.characteristics|join(' | PROP_') }}{% else %}0{% endif %},
        .domain_classes = (const char*[]){
            {% for domain in prop.domain or [] %}
            "{{ domain }}",
            {% endfor %}
            NULL
        },
        .range_classes = (const char*[]){
            {% for range in prop.range or [] %}
            "{{ range }}",
            {% endfor %}
            NULL
        },
        .inverse_property = {% if prop.inverse_of %}"{{ prop.inverse_of }}"{% else %}NULL{% endif %},
        .domain_count = {{ (prop.domain or [])|length }},
        .range_count = {{ (prop.range or [])|length }}
    }{{ "," if not loop.last }}
{% endfor %}
};

/* Forward Declarations for Constructors/Destructors */
{% for class in classes %}
owl_object_t* {{ class.label|snake_case }}_constructor(void);
void {{ class.label|snake_case }}_destructor(owl_object_t* obj);
bool {{ class.label|snake_case }}_validator(const owl_object_t* obj);
{% endfor %}

/* Class Descriptors */
const class_descriptor_t g_class_descriptors[{{ classes|length }}] = {
{% for class in classes %}
    {
        .uri = "{{ class.uri }}",
        .label = "{{ class.label }}",
        .instance_size = sizeof({{ class.label|c_identifier }}_t),
        .eightfold_stage = {% if class.eightfold_mapping %}EIGHTFOLD_{{ class.eightfold_mapping.stage|upper_case }}{% else %}EIGHTFOLD_STAGE_COUNT{% endif %},
        .parent_classes = (const char*[]){
            {% for parent in class.parent_classes %}
            "{{ parent }}",
            {% endfor %}
            NULL
        },
        .parent_count = {{ class.parent_classes|length }},
        .constructor = {{ class.label|snake_case }}_constructor,
        .destructor = {{ class.label|snake_case }}_destructor,
        .validator = {{ class.label|snake_case }}_validator
    }{{ "," if not loop.last }}
{% endfor %}
};

/* Reasoning Rules */
const reasoning_rule_t g_reasoning_rules[{{ rules|length }}] = {
{% for rule in rules %}
    {
        .id = "{{ rule.id }}",
        .type = "{{ rule.type }}",
        .confidence = {{ rule.confidence }},
        .stage = {% if rule.eightfold_stage %}EIGHTFOLD_{{ rule.eightfold_stage|upper_case }}{% else %}EIGHTFOLD_STAGE_COUNT{% endif %},
        .apply_rule = NULL  /* TODO: Implement rule application */
    }{{ "," if not loop.last }}
{% endfor %}
};

/* Class Implementations */
{% for class in classes %}

/* {{ class.label }} Implementation */
{{ class.label|c_identifier }}_t* {{ class.label|snake_case }}_create(void) {
    {{ class.label|c_identifier }}_t* obj = calloc(1, sizeof({{ class.label|c_identifier }}_t));
    if (!obj) return NULL;
    
    /* Initialize base object */
    obj->base.uri = "{{ class.uri }}";
    obj->base.label = "{{ class.label }}";
    obj->base.comment = {% if class.comment %}"{{ class.comment|escape_c_string }}"{% else %}NULL{% endif %};
    obj->base.eightfold_stage = {% if class.eightfold_mapping %}EIGHTFOLD_{{ class.eightfold_mapping.stage|upper_case }}{% else %}EIGHTFOLD_STAGE_COUNT{% endif %};
    obj->base.type_id = {{ loop.index0 }};
    obj->base.instance_data = obj;
    
    /* Initialize constraints as valid by default */
    {% for constraint in class.constraints %}
    obj->constraint_{{ loop.index0 }}_valid = true;
    {% endfor %}
    
    /* Initialize optimization hints */
    {% if class.annotations.optimization_hints %}
    {% for hint in class.annotations.optimization_hints %}
    obj->optimization.{{ hint.type|snake_case }}_enabled = true;
    {% endfor %}
    {% endif %}
    
    return obj;
}

void {{ class.label|snake_case }}_destroy({{ class.label|c_identifier }}_t* obj) {
    if (!obj) return;
    
    /* Clean up parent references */
    {% for parent in class.parent_classes %}
    if (obj->{{ parent|extract_local_name|snake_case }}_parent) {
        {{ parent|extract_local_name|snake_case }}_destroy(obj->{{ parent|extract_local_name|snake_case }}_parent);
    }
    {% endfor %}
    
    /* Clean up property objects */
    {% for prop in class.properties %}
    {% if prop.range %}
    {% for range_type in prop.range %}
    {% if not range_type|is_primitive_type %}
    if (obj->{{ prop.label|snake_case }}) {
        {{ range_type|extract_local_name|snake_case }}_destroy(obj->{{ prop.label|snake_case }});
    }
    {% endif %}
    {% endfor %}
    {% endif %}
    {% endfor %}
    
    free(obj);
}

bool {{ class.label|snake_case }}_validate(const {{ class.label|c_identifier }}_t* obj) {
    if (!obj) return false;
    
    /* Validate constraints */
    {% for constraint in class.constraints %}
    if (!obj->constraint_{{ loop.index0 }}_valid) {
        return false;
    }
    {% endfor %}
    
    /* Validate parent objects */
    {% for parent in class.parent_classes %}
    if (obj->{{ parent|extract_local_name|snake_case }}_parent && 
        !{{ parent|extract_local_name|snake_case }}_validate(obj->{{ parent|extract_local_name|snake_case }}_parent)) {
        return false;
    }
    {% endfor %}
    
    return true;
}

owl_object_t* {{ class.label|snake_case }}_constructor(void) {
    return (owl_object_t*){{ class.label|snake_case }}_create();
}

void {{ class.label|snake_case }}_destructor(owl_object_t* obj) {
    {{ class.label|snake_case }}_destroy(({{ class.label|c_identifier }}_t*)obj);
}

bool {{ class.label|snake_case }}_validator(const owl_object_t* obj) {
    return {{ class.label|snake_case }}_validate((const {{ class.label|c_identifier }}_t*)obj);
}

{% endfor %}

/* API Implementation */
owl_object_t* owl_create_instance(const char* class_uri) {
    for (size_t i = 0; i < {{ classes|length }}; i++) {
        if (strcmp(g_class_descriptors[i].uri, class_uri) == 0) {
            return g_class_descriptors[i].constructor();
        }
    }
    return NULL;
}

void owl_destroy_instance(owl_object_t* obj) {
    if (!obj) return;
    
    for (size_t i = 0; i < {{ classes|length }}; i++) {
        if (g_class_descriptors[i].instance_size == obj->type_id) {
            g_class_descriptors[i].destructor(obj);
            return;
        }
    }
}

bool owl_validate_instance(const owl_object_t* obj) {
    if (!obj) return false;
    
    for (size_t i = 0; i < {{ classes|length }}; i++) {
        if (g_class_descriptors[i].instance_size == obj->type_id) {
            return g_class_descriptors[i].validator(obj);
        }
    }
    return false;
}

const property_descriptor_t* owl_get_property(const char* property_uri) {
    for (size_t i = 0; i < {{ properties|length }}; i++) {
        if (strcmp(g_property_descriptors[i].uri, property_uri) == 0) {
            return &g_property_descriptors[i];
        }
    }
    return NULL;
}

const class_descriptor_t* owl_get_class(const char* class_uri) {
    for (size_t i = 0; i < {{ classes|length }}; i++) {
        if (strcmp(g_class_descriptors[i].uri, class_uri) == 0) {
            return &g_class_descriptors[i];
        }
    }
    return NULL;
}

bool owl_apply_reasoning(owl_object_t* obj) {
    if (!obj) return false;
    
    bool applied = false;
    for (size_t i = 0; i < {{ rules|length }}; i++) {
        if (g_reasoning_rules[i].apply_rule && g_reasoning_rules[i].apply_rule(obj)) {
            applied = true;
        }
    }
    return applied;
}

eightfold_stage_t owl_get_eightfold_stage(const char* class_uri) {
    const class_descriptor_t* desc = owl_get_class(class_uri);
    return desc ? desc->eightfold_stage : EIGHTFOLD_STAGE_COUNT;
}

/* Eightfold Path Implementation */
eightfold_context_t* eightfold_create_context(void) {
    eightfold_context_t* ctx = calloc(1, sizeof(eightfold_context_t));
    if (!ctx) return NULL;
    
    ctx->capacity = 100;  /* Initial capacity */
    ctx->instances = calloc(ctx->capacity, sizeof(owl_object_t*));
    if (!ctx->instances) {
        free(ctx);
        return NULL;
    }
    
    return ctx;
}

void eightfold_destroy_context(eightfold_context_t* ctx) {
    if (!ctx) return;
    
    /* Clean up instances */
    for (size_t i = 0; i < ctx->instance_count; i++) {
        owl_destroy_instance(ctx->instances[i]);
    }
    
    free(ctx->instances);
    free(ctx);
}

bool eightfold_add_instance(eightfold_context_t* ctx, owl_object_t* obj) {
    if (!ctx || !obj) return false;
    
    /* Resize if needed */
    if (ctx->instance_count >= ctx->capacity) {
        size_t new_capacity = ctx->capacity * 2;
        owl_object_t** new_instances = realloc(ctx->instances, 
                                               new_capacity * sizeof(owl_object_t*));
        if (!new_instances) return false;
        
        ctx->instances = new_instances;
        ctx->capacity = new_capacity;
    }
    
    ctx->instances[ctx->instance_count++] = obj;
    return true;
}

owl_object_t** eightfold_get_stage_instances(eightfold_context_t* ctx, eightfold_stage_t stage) {
    if (!ctx) return NULL;
    
    /* Count instances for this stage */
    size_t count = 0;
    for (size_t i = 0; i < ctx->instance_count; i++) {
        if (ctx->instances[i]->eightfold_stage == stage) {
            count++;
        }
    }
    
    if (count == 0) return NULL;
    
    /* Allocate result array */
    owl_object_t** result = calloc(count + 1, sizeof(owl_object_t*));
    if (!result) return NULL;
    
    /* Fill result array */
    size_t idx = 0;
    for (size_t i = 0; i < ctx->instance_count; i++) {
        if (ctx->instances[i]->eightfold_stage == stage) {
            result[idx++] = ctx->instances[i];
        }
    }
    
    return result;
}

bool eightfold_execute_stage(eightfold_context_t* ctx, eightfold_stage_t stage) {
    if (!ctx) return false;
    
    /* Execute all instances for this stage */
    bool success = true;
    for (size_t i = 0; i < ctx->instance_count; i++) {
        if (ctx->instances[i]->eightfold_stage == stage) {
            /* Apply reasoning and validation */
            if (!owl_apply_reasoning(ctx->instances[i]) || 
                !owl_validate_instance(ctx->instances[i])) {
                success = false;
            }
        }
    }
    
    return success;
}
''',

            'json_output.json.j2': '''{
    "metadata": {
        "compiler": "{{ metadata.compiler }}",
        "version": "{{ metadata.version }}",
        "timestamp": "{{ metadata.timestamp }}",
        "config": {{ metadata.config|tojson }}
    },
    "statistics": {
        "total_triples": {{ statistics.total_triples }},
        "total_classes": {{ statistics.total_classes }},
        "total_properties": {{ statistics.total_properties }},
        "total_rules": {{ statistics.total_rules }},
        "property_types": {{ statistics.property_types|tojson }},
        "class_hierarchy_depth": {{ statistics.class_hierarchy_depth }},
        "eightfold_coverage": {{ statistics.eightfold_coverage }}
    },
    "prefixes": {{ prefixes|tojson }},
    "classes": [
        {% for class in classes %}
        {
            "uri": "{{ class.uri }}",
            "label": "{{ class.label }}",
            "comment": {% if class.comment %}"{{ class.comment|escape_c_string }}"{% else %}null{% endif %},
            "parent_classes": {{ class.parent_classes|tojson }},
            "properties": {{ class.properties|tojson }},
            "constraints": {{ class.constraints|tojson }},
            "annotations": {{ class.annotations|tojson }},
            "axioms": {{ class.axioms|tojson }},
            "eightfold_mapping": {% if class.eightfold_mapping %}{{ class.eightfold_mapping|tojson }}{% else %}null{% endif %}
        }{{ "," if not loop.last }}
        {% endfor %}
    ],
    "properties": [
        {% for prop in properties %}
        {
            "uri": "{{ prop.uri }}",
            "label": "{{ prop.label }}",
            "type": "{{ prop.type }}",
            "domain": {% if prop.domain %}{{ prop.domain|tojson }}{% else %}null{% endif %},
            "range": {% if prop.range %}{{ prop.range|tojson }}{% else %}null{% endif %},
            "characteristics": {{ prop.characteristics|tojson }},
            "inverse_of": {% if prop.inverse_of %}"{{ prop.inverse_of }}"{% else %}null{% endif %},
            "constraints": {{ prop.constraints|tojson }},
            "annotations": {{ prop.annotations|tojson }}
        }{{ "," if not loop.last }}
        {% endfor %}
    ],
    "rules": [
        {% for rule in rules %}
        {
            "id": "{{ rule.id }}",
            "type": "{{ rule.type }}",
            "antecedent": {{ rule.antecedent|tojson }},
            "consequent": {{ rule.consequent|tojson }},
            "confidence": {{ rule.confidence }},
            "eightfold_stage": {% if rule.eightfold_stage %}"{{ rule.eightfold_stage }}"{% else %}null{% endif %}
        }{{ "," if not loop.last }}
        {% endfor %}
    ]
}
''',

            'makefile.j2': '''# Generated Makefile for OWL AOT Compilation
# Timestamp: {{ now().isoformat() }}

CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -O2 -g
LDFLAGS = 
INCLUDES = -I.

# Source files
SOURCES = {{ source_files|join(' ') }}
OBJECTS = $(SOURCES:.c=.o)
HEADERS = {{ header_files|join(' ') }}

# Target
TARGET = owl_ontology

# Build rules
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/
	cp $(HEADERS) /usr/local/include/

uninstall:
	rm -f /usr/local/bin/$(TARGET)
	rm -f /usr/local/include/owl_ontology.h

# Testing
test: $(TARGET)
	./$(TARGET) --self-test

# Documentation
docs:
	doxygen Doxyfile

.PHONY: all clean install uninstall test docs

# Dependencies
{% for source in source_files %}
{{ source.replace('.c', '.o') }}: {{ source }} $(HEADERS)
{% endfor %}
'''
        }
    
    def render_template(self, template_name: str, context: Dict[str, Any]) -> str:
        """Render a template with the given context"""
        template = self.env.get_template(template_name)
        return template.render(context)
    
    def render_from_string(self, template_string: str, context: Dict[str, Any]) -> str:
        """Render a template from string with the given context"""
        template = self.env.from_string(template_string)
        return template.render(context)
    
    # Custom filters
    def _c_identifier_filter(self, value: str) -> str:
        """Convert to valid C identifier"""
        # Remove invalid characters and replace with underscore
        result = re.sub(r'[^a-zA-Z0-9_]', '_', str(value))
        # Ensure it doesn't start with a number
        if result and result[0].isdigit():
            result = '_' + result
        return result or '_'
    
    def _camel_case_filter(self, value: str) -> str:
        """Convert to camelCase"""
        words = re.split(r'[^a-zA-Z0-9]', str(value))
        if not words:
            return ''
        return words[0].lower() + ''.join(word.capitalize() for word in words[1:])
    
    def _snake_case_filter(self, value: str) -> str:
        """Convert to snake_case"""
        # Convert camelCase to snake_case
        s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', str(value))
        s2 = re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1)
        # Replace non-alphanumeric with underscore and convert to lowercase
        result = re.sub(r'[^a-zA-Z0-9]', '_', s2).lower()
        # Remove multiple underscores
        result = re.sub(r'_+', '_', result)
        return result.strip('_')
    
    def _upper_case_filter(self, value: str) -> str:
        """Convert to UPPER_CASE"""
        return self._snake_case_filter(value).upper()
    
    def _escape_c_string_filter(self, value: str) -> str:
        """Escape string for C"""
        if not value:
            return ''
        return value.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')
    
    def _format_comment_filter(self, value: str, width: int = 80) -> str:
        """Format comment for C code"""
        if not value:
            return ''
        
        lines = []
        words = value.split()
        current_line = ' * '
        
        for word in words:
            if len(current_line + word) > width - 3:
                lines.append(current_line.rstrip())
                current_line = ' * ' + word + ' '
            else:
                current_line += word + ' '
        
        if current_line.strip() != '*':
            lines.append(current_line.rstrip())
        
        return '\n'.join(lines)
    
    def _extract_local_name_filter(self, uri: str) -> str:
        """Extract local name from URI"""
        if '#' in uri:
            return uri.split('#')[-1]
        elif '/' in uri:
            return uri.split('/')[-1]
        return uri
    
    def _get_type_size_filter(self, type_name: str) -> int:
        """Get size of C type"""
        type_sizes = {
            'int': 4,
            'float': 4,
            'double': 8,
            'char': 1,
            'bool': 1,
            'uint8_t': 1,
            'uint16_t': 2,
            'uint32_t': 4,
            'uint64_t': 8,
            'int8_t': 1,
            'int16_t': 2,
            'int32_t': 4,
            'int64_t': 8,
            'void*': 8  # Assume 64-bit pointers
        }
        return type_sizes.get(type_name, 8)  # Default to pointer size
    
    def _is_primitive_type_filter(self, type_uri: str) -> bool:
        """Check if type is primitive"""
        primitive_types = {
            'http://www.w3.org/2001/XMLSchema#string',
            'http://www.w3.org/2001/XMLSchema#int',
            'http://www.w3.org/2001/XMLSchema#integer',
            'http://www.w3.org/2001/XMLSchema#float',
            'http://www.w3.org/2001/XMLSchema#double',
            'http://www.w3.org/2001/XMLSchema#boolean',
            'http://www.w3.org/2001/XMLSchema#dateTime',
            'http://www.w3.org/2001/XMLSchema#date',
            'http://www.w3.org/2001/XMLSchema#decimal'
        }
        return type_uri in primitive_types
    
    def _xsd_to_c_type_filter(self, type_uri: str) -> str:
        """Map XSD types to C types"""
        type_mapping = {
            'http://www.w3.org/2001/XMLSchema#string': 'char*',
            'http://www.w3.org/2001/XMLSchema#int': 'int32_t',
            'http://www.w3.org/2001/XMLSchema#integer': 'int32_t',
            'http://www.w3.org/2001/XMLSchema#float': 'float',
            'http://www.w3.org/2001/XMLSchema#double': 'double',
            'http://www.w3.org/2001/XMLSchema#boolean': 'bool',
            'http://www.w3.org/2001/XMLSchema#dateTime': 'time_t',
            'http://www.w3.org/2001/XMLSchema#date': 'time_t',
            'http://www.w3.org/2001/XMLSchema#decimal': 'double'
        }
        return type_mapping.get(type_uri, 'void*')
    
    def _sort_by_eightfold_filter(self, classes: List[Any]) -> List[Any]:
        """Sort classes by Eightfold Path stage"""
        eightfold_order = {
            'Right Understanding': 0,
            'Right Thought': 1,
            'Right Speech': 2,
            'Right Action': 3,
            'Right Livelihood': 4,
            'Right Effort': 5,
            'Right Mindfulness': 6,
            'Right Concentration': 7
        }
        
        def sort_key(cls):
            if hasattr(cls, 'eightfold_mapping') and cls.eightfold_mapping:
                stage = cls.eightfold_mapping.get('stage')
                return eightfold_order.get(stage, 999)
            return 999
        
        return sorted(classes, key=sort_key)

class OWLCompiler:
    """OWL/TTL Compiler for AOT Pipeline with Jinja2 templating"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None, template_dir: Optional[Path] = None):
        self.config = config or self._default_config()
        self.template_manager = TemplateManager(template_dir)
        self.graph = Graph()
        self.classes: Dict[str, OWLClass] = {}
        self.properties: Dict[str, OWLProperty] = {}
        self.rules: List[ReasoningRule] = []
        self.prefixes: Dict[str, str] = {}
        self.statistics: Dict[str, Any] = {}
        
        # Bind common namespaces
        self.graph.bind("cns", CNS)
        self.graph.bind("eh", EH)
        self.graph.bind("shacl", SHACL)
        self.graph.bind("owl", OWL)
        self.graph.bind("rdfs", RDFS)
        self.graph.bind("skos", SKOS)
        self.graph.bind("dcterms", DCTERMS)
    
    def _default_config(self) -> Dict[str, Any]:
        """Default configuration for OWL compilation"""
        return {
            'strict_mode': True,
            'inference_enabled': True,
            'reasoning_depth': 3,
            'extract_shacl': True,
            'eightfold_integration': True,
            'optimization_hints': True,
            'output_formats': ['c_header', 'c_implementation', 'json'],
            'template_customization': True
        }
    
    def compile(self, spec_path: Path, output_dir: Optional[Path] = None) -> Dict[str, Any]:
        """Compile OWL/TTL specification into structured representation"""
        logger.info(f"Compiling OWL specification: {spec_path}")
        
        # Parse the RDF graph
        self._parse_specification(spec_path)
        
        # Extract ontology components
        self._extract_classes()
        self._extract_properties()
        self._extract_rules()
        self._extract_eightfold_mappings()
        
        # Apply inference if enabled
        if self.config['inference_enabled']:
            self._apply_inference()
        
        # Generate optimization hints
        if self.config['optimization_hints']:
            self._generate_optimization_hints()
        
        # Compile statistics
        self._compile_statistics()
        
        # Create compilation result
        result = self._create_compilation_result()
        
        # Generate output files if output directory specified
        if output_dir:
            self._generate_output_files(result, output_dir, spec_path.stem)
        
        return result
    
    def _generate_output_files(self, result: Dict[str, Any], output_dir: Path, base_name: str) -> None:
        """Generate output files using Jinja templates"""
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Prepare template context
        context = self._prepare_template_context(result, base_name)
        
        # Generate files based on configured output formats
        for output_format in self.config.get('output_formats', []):
            self._generate_format_output(output_format, context, output_dir, base_name)
    
    def _prepare_template_context(self, result: Dict[str, Any], base_name: str) -> Dict[str, Any]:
        """Prepare context for template rendering"""
        context = result.copy()
        
        # Add template-specific variables
        context.update({
            'header_guard': f"{base_name.upper()}_H",
            'header_filename': f"{base_name}.h",
            'source_filename': f"{base_name}.c",
            'base_name': base_name,
            'eightfold_stages': [
                'Right Understanding',
                'Right Thought', 
                'Right Speech',
                'Right Action',
                'Right Livelihood',
                'Right Effort',
                'Right Mindfulness',
                'Right Concentration'
            ],
            'source_files': [f"{base_name}.c"],
            'header_files': [f"{base_name}.h"]
        })
        
        return context
    
    def _generate_format_output(self, output_format: str, context: Dict[str, Any], 
                              output_dir: Path, base_name: str) -> None:
        """Generate output for specific format"""
        format_mappings = {
            'c_header': ('c_header.h.j2', f"{base_name}.h"),
            'c_implementation': ('c_implementation.c.j2', f"{base_name}.c"),
            'json': ('json_output.json.j2', f"{base_name}.json"),
            'makefile': ('makefile.j2', 'Makefile')
        }
        
        if output_format not in format_mappings:
            logger.warning(f"Unknown output format: {output_format}")
            return
        
        template_name, output_filename = format_mappings[output_format]
        
        # Render template
        output_content = self.template_manager.render_template(template_name, context)
        
        # Write to file
        output_path = output_dir / output_filename
        output_path.write_text(output_content, encoding='utf-8')
        
        logger.info(f"Generated {output_format} output: {output_path}")
    
    def render_custom_template(self, template_string: str, context: Optional[Dict[str, Any]] = None) -> str:
        """Render a custom template string"""
        template_context = context or {}
        template_context.update(self._create_compilation_result())
        return self.template_manager.render_from_string(template_string, template_context)
    
    def add_custom_filter(self, name: str, filter_func) -> None:
        """Add custom Jinja filter"""
        self.template_manager.env.filters[name] = filter_func
    
    def add_custom_global(self, name: str, global_func) -> None:
        """Add custom Jinja global function"""
        self.template_manager.env.globals[name] = global_func
    
    # [Rest of the original methods remain the same, copying from the original implementation]
    def _parse_specification(self, spec_path: Path) -> None:
        """Parse RDF specification file"""
        format_map = {
            '.ttl': 'turtle',
            '.n3': 'n3',
            '.owl': 'xml',
            '.rdf': 'xml',
            '.jsonld': 'json-ld'
        }
        
        file_format = format_map.get(spec_path.suffix.lower(), 'turtle')
        self.graph.parse(spec_path, format=file_format)
        
        logger.info(f"Parsed {len(self.graph)} triples from {spec_path}")
        
        for prefix, namespace in self.graph.namespaces():
            self.prefixes[prefix] = str(namespace)
    
    def _extract_classes(self) -> None:
        """Extract OWL classes from the graph"""
        for class_uri in self.graph.subjects(RDF.type, OWL.Class):
            if isinstance(class_uri, BNode):
                continue
                
            owl_class = OWLClass(
                uri=str(class_uri),
                label=self._get_label(class_uri),
                comment=self._get_comment(class_uri)
            )
            
            # Extract parent classes
            for parent in self.graph.objects(class_uri, RDFS.subClassOf):
                if isinstance(parent, URIRef):
                    owl_class.parent_classes.append(str(parent))
            
            # Extract class properties (via domain)
            for prop_uri in self.graph.subjects(RDFS.domain, class_uri):
                prop_info = {
                    'uri': str(prop_uri),
                    'label': self._get_label(prop_uri),
                    'type': self._get_property_type(prop_uri)
                }
                
                ranges = list(self.graph.objects(prop_uri, RDFS.range))
                if ranges:
                    prop_info['range'] = [str(r) for r in ranges if isinstance(r, URIRef)]
                
                owl_class.properties.append(prop_info)
            
            if self.config['extract_shacl']:
                owl_class.constraints = self._extract_shacl_constraints(class_uri)
            
            owl_class.annotations = self._extract_annotations(class_uri)
            owl_class.axioms = self._extract_class_axioms(class_uri)
            
            self.classes[str(class_uri)] = owl_class
    
    def _extract_properties(self) -> None:
        """Extract OWL properties from the graph"""
        property_types = [
            (OWL.ObjectProperty, 'ObjectProperty'),
            (OWL.DatatypeProperty, 'DatatypeProperty'),
            (OWL.AnnotationProperty, 'AnnotationProperty')
        ]
        
        for prop_type_uri, prop_type_name in property_types:
            for prop_uri in self.graph.subjects(RDF.type, prop_type_uri):
                if isinstance(prop_uri, BNode):
                    continue
                
                owl_prop = OWLProperty(
                    uri=str(prop_uri),
                    label=self._get_label(prop_uri),
                    type=prop_type_name
                )
                
                domains = list(self.graph.objects(prop_uri, RDFS.domain))
                if domains:
                    owl_prop.domain = [str(d) for d in domains if isinstance(d, URIRef)]
                
                ranges = list(self.graph.objects(prop_uri, RDFS.range))
                if ranges:
                    owl_prop.range = [str(r) for r in ranges if isinstance(r, URIRef)]
                
                owl_prop.characteristics = self._extract_property_characteristics(prop_uri)
                
                inverse = list(self.graph.objects(prop_uri, OWL.inverseOf))
                if inverse:
                    owl_prop.inverse_of = str(inverse[0])
                
                if self.config['extract_shacl']:
                    owl_prop.constraints = self._extract_shacl_constraints(prop_uri)
                
                owl_prop.annotations = self._extract_annotations(prop_uri)
                
                self.properties[str(prop_uri)] = owl_prop
    
    def _extract_rules(self) -> None:
        """Extract reasoning rules from the ontology"""
        rule_id = 0
        
        for rule_uri in self.graph.subjects(RDF.type, URIRef("http://www.w3.org/2003/11/swrl#Imp")):
            rule = self._extract_swrl_rule(rule_uri, rule_id)
            if rule:
                self.rules.append(rule)
                rule_id += 1
        
        for restriction in self.graph.subjects(RDF.type, OWL.Restriction):
            rule = self._extract_restriction_rule(restriction, rule_id)
            if rule:
                self.rules.append(rule)
                rule_id += 1
        
        for prop_uri, prop in self.properties.items():
            if 'Transitive' in prop.characteristics:
                rule = ReasoningRule(
                    id=f"rule_{rule_id}",
                    type="inference",
                    antecedent=[
                        {"subject": "?x", "predicate": prop_uri, "object": "?y"},
                        {"subject": "?y", "predicate": prop_uri, "object": "?z"}
                    ],
                    consequent={"subject": "?x", "predicate": prop_uri, "object": "?z"},
                    confidence=1.0
                )
                self.rules.append(rule)
                rule_id += 1
    
    def _extract_eightfold_mappings(self) -> None:
        """Extract Eightfold Path mappings from the ontology"""
        if not self.config['eightfold_integration']:
            return
        
        eightfold_stages = {
            'Right Understanding': ['Knowledge', 'Comprehension', 'Insight'],
            'Right Thought': ['Intention', 'Planning', 'Design'],
            'Right Speech': ['Communication', 'Expression', 'Declaration'],
            'Right Action': ['Implementation', 'Execution', 'Operation'],
            'Right Livelihood': ['Sustainability', 'Maintenance', 'Support'],
            'Right Effort': ['Optimization', 'Improvement', 'Enhancement'],
            'Right Mindfulness': ['Monitoring', 'Awareness', 'Observation'],
            'Right Concentration': ['Focus', 'Integration', 'Synthesis']
        }
        
        for class_uri, owl_class in self.classes.items():
            eightfold_anno = self.graph.value(URIRef(class_uri), EH.stage)
            if eightfold_anno:
                owl_class.eightfold_mapping = {
                    'stage': str(eightfold_anno),
                    'explicit': True
                }
            else:
                for stage, keywords in eightfold_stages.items():
                    if any(keyword.lower() in owl_class.label.lower() for keyword in keywords):
                        owl_class.eightfold_mapping = {
                            'stage': stage,
                            'explicit': False,
                            'confidence': 0.8
                        }
                        break
    
    def _apply_inference(self) -> None:
        """Apply OWL inference rules to expand the knowledge base"""
        logger.info("Applying OWL inference...")
        self._apply_rdfs_inference()
        self._apply_owl_inference()
        
        for rule in self.rules:
            if rule.type == "inference":
                self._apply_rule(rule)
    
    def _apply_rdfs_inference(self) -> None:
        """Apply RDFS inference rules"""
        changes = True
        while changes:
            changes = False
            for s, p, o in self.graph.triples((None, RDFS.subClassOf, None)):
                for o2 in self.graph.objects(o, RDFS.subClassOf):
                    if not (s, RDFS.subClassOf, o2) in self.graph:
                        self.graph.add((s, RDFS.subClassOf, o2))
                        changes = True
        
        for prop, domain in self.graph.subject_objects(RDFS.domain):
            for s, o in self.graph.subject_objects(prop):
                if not (s, RDF.type, domain) in self.graph:
                    self.graph.add((s, RDF.type, domain))
        
        for prop, range_class in self.graph.subject_objects(RDFS.range):
            for s, o in self.graph.subject_objects(prop):
                if isinstance(o, URIRef) and not (o, RDF.type, range_class) in self.graph:
                    self.graph.add((o, RDF.type, range_class))
    
    def _apply_owl_inference(self) -> None:
        """Apply OWL inference rules"""
        for prop1, prop2 in self.graph.subject_objects(OWL.inverseOf):
            for s, o in self.graph.subject_objects(prop1):
                if not (o, prop2, s) in self.graph:
                    self.graph.add((o, prop2, s))
        
        for prop in self.graph.subjects(RDF.type, OWL.SymmetricProperty):
            for s, o in self.graph.subject_objects(prop):
                if not (o, prop, s) in self.graph:
                    self.graph.add((o, prop, s))
        
        for prop in self.graph.subjects(RDF.type, OWL.TransitiveProperty):
            changes = True
            while changes:
                changes = False
                for s, o1 in self.graph.subject_objects(prop):
                    for o2 in self.graph.objects(o1, prop):
                        if not (s, prop, o2) in self.graph:
                            self.graph.add((s, prop, o2))
                            changes = True
    
    def _generate_optimization_hints(self) -> None:
        """Generate optimization hints for the C code generator"""
        for class_uri, owl_class in self.classes.items():
            hints = []
            
            property_count = len(owl_class.properties)
            if property_count > 10:
                hints.append({
                    'type': 'cache_optimization',
                    'reason': 'high_property_count',
                    'suggestion': 'use_property_index'
                })
            
            if owl_class.parent_classes:
                hints.append({
                    'type': 'memory_layout',
                    'reason': 'inheritance_hierarchy',
                    'suggestion': 'vtable_optimization'
                })
            
            if owl_class.eightfold_mapping:
                stage = owl_class.eightfold_mapping.get('stage')
                if stage in ['Right Action', 'Right Effort']:
                    hints.append({
                        'type': 'performance_critical',
                        'reason': f'eightfold_stage_{stage}',
                        'suggestion': 'hot_path_optimization'
                    })
            
            if hints:
                owl_class.annotations['optimization_hints'] = hints
    
    def _compile_statistics(self) -> None:
        """Compile statistics about the ontology"""
        self.statistics = {
            'total_triples': len(self.graph),
            'total_classes': len(self.classes),
            'total_properties': len(self.properties),
            'total_rules': len(self.rules),
            'property_types': {},
            'class_hierarchy_depth': self._calculate_hierarchy_depth(),
            'eightfold_coverage': self._calculate_eightfold_coverage()
        }
        
        for prop in self.properties.values():
            prop_type = prop.type
            self.statistics['property_types'][prop_type] = \
                self.statistics['property_types'].get(prop_type, 0) + 1
    
    def _calculate_hierarchy_depth(self) -> int:
        """Calculate the maximum depth of the class hierarchy"""
        max_depth = 0
        
        def get_depth(class_uri: str, visited: Set[str] = None) -> int:
            if visited is None:
                visited = set()
            
            if class_uri in visited:
                return 0
            
            visited.add(class_uri)
            
            if class_uri not in self.classes:
                return 0
            
            parent_depths = []
            for parent in self.classes[class_uri].parent_classes:
                parent_depths.append(get_depth(parent, visited.copy()))
            
            return 1 + max(parent_depths) if parent_depths else 1
        
        for class_uri in self.classes:
            depth = get_depth(class_uri)
            max_depth = max(max_depth, depth)
        
        return max_depth
    
    def _calculate_eightfold_coverage(self) -> float:
        """Calculate the percentage of classes with Eightfold mappings"""
        if not self.classes:
            return 0.0
        
        mapped_count = sum(1 for cls in self.classes.values() 
                          if cls.eightfold_mapping is not None)
        
        return (mapped_count / len(self.classes)) * 100
    
    def _create_compilation_result(self) -> Dict[str, Any]:
        """Create the final compilation result"""
        return {
            'prefixes': self.prefixes,
            'classes': [self._serialize_class(cls) for cls in self.classes.values()],
            'properties': [self._serialize_property(prop) for prop in self.properties.values()],
            'rules': [self._serialize_rule(rule) for rule in self.rules],
            'statistics': self.statistics,
            'metadata': {
                'compiler': 'OWL AOT Compiler with Jinja',
                'version': '1.0.0',
                'timestamp': datetime.now().isoformat(),
                'config': self.config
            }
        }
    
    def _serialize_class(self, owl_class: OWLClass) -> Dict[str, Any]:
        """Serialize OWLClass to dictionary"""
        return {
            'uri': owl_class.uri,
            'label': owl_class.label,
            'comment': owl_class.comment,
            'parent_classes': owl_class.parent_classes,
            'properties': owl_class.properties,
            'constraints': owl_class.constraints,
            'annotations': owl_class.annotations,
            'axioms': owl_class.axioms,
            'eightfold_mapping': owl_class.eightfold_mapping
        }
    
    def _serialize_property(self, owl_prop: OWLProperty) -> Dict[str, Any]:
        """Serialize OWLProperty to dictionary"""
        return {
            'uri': owl_prop.uri,
            'label': owl_prop.label,
            'type': owl_prop.type,
            'domain': owl_prop.domain,
            'range': owl_prop.range,
            'characteristics': owl_prop.characteristics,
            'inverse_of': owl_prop.inverse_of,
            'constraints': owl_prop.constraints,
            'annotations': owl_prop.annotations
        }
    
    def _serialize_rule(self, rule: ReasoningRule) -> Dict[str, Any]:
        """Serialize ReasoningRule to dictionary"""
        return {
            'id': rule.id,
            'type': rule.type,
            'antecedent': rule.antecedent,
            'consequent': rule.consequent,
            'confidence': rule.confidence,
            'eightfold_stage': rule.eightfold_stage
        }
    
    # Helper methods (keeping the same implementations as original)
    def _get_label(self, subject: URIRef) -> str:
        """Get the label for a subject"""
        label = self.graph.value(subject, RDFS.label)
        if label:
            return str(label)
        
        label = self.graph.value(subject, SKOS.prefLabel)
        if label:
            return str(label)
        
        return str(subject).split('#')[-1].split('/')[-1]
    
    def _get_comment(self, subject: URIRef) -> Optional[str]:
        """Get the comment for a subject"""
        comment = self.graph.value(subject, RDFS.comment)
        if comment:
            return str(comment)
        
        comment = self.graph.value(subject, SKOS.definition)
        if comment:
            return str(comment)
        
        return None
    
    def _get_property_type(self, prop_uri: URIRef) -> str:
        """Determine the type of a property"""
        if (prop_uri, RDF.type, OWL.ObjectProperty) in self.graph:
            return "ObjectProperty"
        elif (prop_uri, RDF.type, OWL.DatatypeProperty) in self.graph:
            return "DatatypeProperty"
        elif (prop_uri, RDF.type, OWL.AnnotationProperty) in self.graph:
            return "AnnotationProperty"
        else:
            return "Property"
    
    def _extract_property_characteristics(self, prop_uri: URIRef) -> List[str]:
        """Extract property characteristics"""
        characteristics = []
        
        char_types = [
            (OWL.FunctionalProperty, 'Functional'),
            (OWL.InverseFunctionalProperty, 'InverseFunctional'),
            (OWL.TransitiveProperty, 'Transitive'),
            (OWL.SymmetricProperty, 'Symmetric'),
            (OWL.AsymmetricProperty, 'Asymmetric'),
            (OWL.ReflexiveProperty, 'Reflexive'),
            (OWL.IrreflexiveProperty, 'Irreflexive')
        ]
        
        for char_type, char_name in char_types:
            if (prop_uri, RDF.type, char_type) in self.graph:
                characteristics.append(char_name)
        
        return characteristics
    
    def _extract_shacl_constraints(self, subject: URIRef) -> List[Dict[str, Any]]:
        """Extract SHACL constraints for a subject"""
        constraints = []
        
        for shape in self.graph.subjects(SHACL.targetClass, subject):
            constraint = {
                'shape': str(shape),
                'target': str(subject),
                'properties': []
            }
            
            for prop_shape in self.graph.objects(shape, SHACL.property):
                prop_constraint = {}
                
                path = self.graph.value(prop_shape, SHACL.path)
                if path:
                    prop_constraint['path'] = str(path)
                
                min_count = self.graph.value(prop_shape, SHACL.minCount)
                if min_count:
                    prop_constraint['minCount'] = int(min_count)
                
                max_count = self.graph.value(prop_shape, SHACL.maxCount)
                if max_count:
                    prop_constraint['maxCount'] = int(max_count)
                
                datatype = self.graph.value(prop_shape, SHACL.datatype)
                if datatype:
                    prop_constraint['datatype'] = str(datatype)
                
                pattern = self.graph.value(prop_shape, SHACL.pattern)
                if pattern:
                    prop_constraint['pattern'] = str(pattern)
                
                constraint['properties'].append(prop_constraint)
            
            constraints.append(constraint)
        
        return constraints
    
    def _extract_annotations(self, subject: URIRef) -> Dict[str, Any]:
        """Extract all annotations for a subject"""
        annotations = {}
        
        anno_props = [
            (RDFS.seeAlso, 'seeAlso'),
            (RDFS.isDefinedBy, 'isDefinedBy'),
            (DCTERMS.created, 'created'),
            (DCTERMS.modified, 'modified'),
            (DCTERMS.creator, 'creator'),
            (DCTERMS.description, 'description'),
            (SKOS.note, 'note'),
            (SKOS.example, 'example')
        ]
        
        for prop, key in anno_props:
            values = list(self.graph.objects(subject, prop))
            if values:
                if len(values) == 1:
                    annotations[key] = str(values[0])
                else:
                    annotations[key] = [str(v) for v in values]
        
        for p, o in self.graph.predicate_objects(subject):
            if isinstance(p, URIRef) and str(p).startswith('http://cns.io/'):
                # Custom annotations
                key = str(p).split('#')[-1].split('/')[-1]
                annotations[key] = str(o)
        
        return annotations
    
    def _extract_swrl_rule(self, rule_uri: URIRef, rule_id: int) -> Optional[ReasoningRule]:
        """Extract SWRL rule"""
        # Simplified implementation - would need full SWRL parsing
        return None
    
    def _extract_restriction_rule(self, restriction: URIRef, rule_id: int) -> Optional[ReasoningRule]:
        """Extract OWL restriction as rule"""
        # Simplified implementation
        return None
    
    def _apply_rule(self, rule: ReasoningRule) -> None:
        """Apply a single reasoning rule"""
        # Simplified implementation
        pass
    
    def _extract_class_axioms(self, class_uri: URIRef) -> List[Dict[str, Any]]:
        """Extract axioms for a class"""
        axioms = []
        
        # Extract equivalent classes
        for equiv in self.graph.objects(class_uri, OWL.equivalentClass):
            axioms.append({
                'type': 'equivalentClass',
                'target': str(equiv)
            })
        
        # Extract disjoint classes
        for disjoint in self.graph.objects(class_uri, OWL.disjointWith):
            axioms.append({
                'type': 'disjointWith',
                'target': str(disjoint)
            })
        
        return axioms

# Main entry point
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="OWL AOT Compiler")
    parser.add_argument("spec", type=Path, help="OWL/TTL specification file")
    parser.add_argument("--output", "-o", type=Path, help="Output directory")
    parser.add_argument("--template-dir", type=Path, help="Custom template directory")
    args = parser.parse_args()
    
    compiler = OWLCompiler(template_dir=args.template_dir)
    result = compiler.compile(args.spec, args.output)
    print(f"Compilation complete. Generated {len(result['classes'])} classes and {len(result['properties'])} properties.")