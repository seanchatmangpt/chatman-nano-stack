#!/usr/bin/env python3
"""
SHACL AOT Compiler - Template-based Code Generation
Compiles SHACL shapes into optimized C validation code using Jinja2
"""

import os
import re
import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Set, Union
from dataclasses import dataclass, field
from datetime import datetime
import rdflib
from rdflib import Graph, Namespace, RDF, RDFS, OWL, URIRef, Literal, BNode
from rdflib.namespace import XSD
import json
import jinja2
from jinja2 import Environment, FileSystemLoader, Template, meta

logger = logging.getLogger(__name__)

# Define namespaces
SHACL = Namespace("http://www.w3.org/ns/shacl#")
CNS = Namespace("http://cns.io/ontology#")
EH = Namespace("http://cns.io/eightfold#")

@dataclass
class SHACLConstraint:
    """Represents a SHACL constraint with validation logic"""
    id: str
    type: str  # minCount, maxCount, datatype, pattern, etc.
    target: str
    property_path: Optional[str] = None
    value: Optional[Union[str, int, float, bool]] = None
    severity: str = "sh:Violation"
    message: Optional[str] = None
    eightfold_stage: Optional[str] = None
    optimization_hints: List[str] = field(default_factory=list)
    template_vars: Dict[str, Any] = field(default_factory=dict)

@dataclass
class SHACLShape:
    """Represents a SHACL shape with its constraints"""
    uri: str
    target_class: Optional[str] = None
    target_node: Optional[str] = None
    closed: bool = False
    constraints: List[SHACLConstraint] = field(default_factory=list)
    property_shapes: List['SHACLPropertyShape'] = field(default_factory=list)
    deactivated: bool = False
    severity: str = "sh:Violation"
    eightfold_mapping: Optional[Dict[str, Any]] = None

@dataclass
class SHACLPropertyShape:
    """Represents a SHACL property shape"""
    path: str
    name: Optional[str] = None
    description: Optional[str] = None
    constraints: List[SHACLConstraint] = field(default_factory=list)
    optional: bool = True
    group: Optional[str] = None

@dataclass
class ValidationFunction:
    """Represents a generated validation function"""
    name: str
    code: str
    constraints: List[str]
    dependencies: List[str] = field(default_factory=list)
    performance_level: str = "standard"  # fast, standard, comprehensive

class SHACLCompiler:
    """SHACL Compiler with Jinja2 template generation"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or self._default_config()
        self.graph = Graph()
        self.shapes: Dict[str, SHACLShape] = {}
        self.constraints: List[SHACLConstraint] = []
        self.validation_functions: List[ValidationFunction] = []
        self.template_env: Optional[Environment] = None
        self.templates: Dict[str, Template] = {}
        self.statistics: Dict[str, Any] = {}
        
        # Bind namespaces
        self.graph.bind("sh", SHACL)
        self.graph.bind("cns", CNS)
        self.graph.bind("eh", EH)
        self.graph.bind("xsd", XSD)
        
        # Initialize template environment
        self._init_templates()
    
    def _default_config(self) -> Dict[str, Any]:
        """Default configuration for SHACL compilation"""
        return {
            'template_dir': 'templates',
            'output_format': 'c',
            'optimization_level': 'O2',
            'eightfold_integration': True,
            'generate_benchmarks': True,
            'inline_simple_checks': True,
            'use_lookup_tables': True,
            'memory_pool_size': 1024 * 1024,  # 1MB
            'max_validation_depth': 10
        }
    
    def _init_templates(self) -> None:
        """Initialize Jinja2 template environment"""
        template_dir = Path(__file__).parent / self.config.get('template_dir', 'templates')
        
        if template_dir.exists():
            self.template_env = Environment(
                loader=FileSystemLoader(str(template_dir)),
                trim_blocks=True,
                lstrip_blocks=True,
                keep_trailing_newline=True
            )
        else:
            # Use string templates if directory doesn't exist
            self.template_env = Environment(loader=jinja2.DictLoader({}))
        
        # Add custom filters and functions
        self._register_template_functions()
        
        # Load built-in templates
        self._load_builtin_templates()
    
    def _register_template_functions(self) -> None:
        """Register custom Jinja2 filters and functions"""
        
        def c_escape(text: str) -> str:
            """Escape text for C string literals"""
            return text.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
        
        def c_identifier(name: str) -> str:
            """Convert name to valid C identifier"""
            # Remove namespace prefixes
            name = name.split('#')[-1].split('/')[-1]
            # Replace invalid characters
            name = re.sub(r'[^a-zA-Z0-9_]', '_', name)
            # Ensure it starts with letter or underscore
            if name and name[0].isdigit():
                name = '_' + name
            return name or '_unnamed'
        
        def c_type_mapping(xsd_type: str) -> str:
            """Map XSD types to C types"""
            type_map = {
                str(XSD.string): 'char*',
                str(XSD.int): 'int',
                str(XSD.integer): 'long',
                str(XSD.long): 'long long',
                str(XSD.float): 'float',
                str(XSD.double): 'double',
                str(XSD.boolean): 'bool',
                str(XSD.dateTime): 'time_t',
                str(XSD.date): 'time_t',
                str(XSD.anyURI): 'char*'
            }
            return type_map.get(xsd_type, 'void*')
        
        def optimize_constraint(constraint_type: str, value: Any) -> str:
            """Generate optimized constraint check"""
            if constraint_type == 'minCount' and value == 1:
                return 'REQUIRED_CHECK'
            elif constraint_type == 'maxCount' and value == 1:
                return 'SINGLE_VALUE_CHECK'
            elif constraint_type == 'datatype':
                return f'TYPE_CHECK_{c_identifier(str(value))}'
            return 'GENERIC_CHECK'
        
        def eightfold_optimization(stage: str) -> str:
            """Generate Eightfold-specific optimizations"""
            stage_opts = {
                'Right Understanding': 'UNDERSTANDING_FAST_PATH',
                'Right Action': 'ACTION_HOT_PATH',
                'Right Effort': 'EFFORT_INLINE_CHECK',
                'Right Concentration': 'CONCENTRATION_BATCH_CHECK'
            }
            return stage_opts.get(stage, 'STANDARD_PATH')
        
        # Register filters
        self.template_env.filters['c_escape'] = c_escape
        self.template_env.filters['c_identifier'] = c_identifier
        self.template_env.filters['c_type'] = c_type_mapping
        self.template_env.filters['optimize'] = optimize_constraint
        self.template_env.filters['eightfold_opt'] = eightfold_optimization
        
        # Register global functions
        self.template_env.globals['now'] = datetime.now
        self.template_env.globals['len'] = len
        self.template_env.globals['enumerate'] = enumerate
    
    def _load_builtin_templates(self) -> None:
        """Load built-in template definitions"""
        
        # C header template
        c_header_template = """
/*
 * Generated SHACL Validation Code
 * Compiler: SHACL AOT Compiler v1.0
 * Generated: {{ now().isoformat() }}
 * Optimization: {{ config.optimization_level }}
 */

#ifndef SHACL_VALIDATION_H
#define SHACL_VALIDATION_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Validation result codes
typedef enum {
    SHACL_VALID = 0,
    SHACL_VIOLATION = 1,
    SHACL_ERROR = 2
} shacl_result_t;

// Validation context
typedef struct {
    void* data_graph;
    void* shapes_graph;
    uint32_t max_depth;
    bool fast_mode;
    {% if config.eightfold_integration %}
    uint8_t eightfold_stage;
    {% endif %}
} shacl_context_t;

// Validation report
typedef struct shacl_violation {
    char* focus_node;
    char* property_path;
    char* constraint_component;
    char* message;
    struct shacl_violation* next;
} shacl_violation_t;

typedef struct {
    bool conforms;
    uint32_t violation_count;
    shacl_violation_t* violations;
} shacl_report_t;

// Function declarations
{% for func in validation_functions %}
shacl_result_t {{ func.name }}(shacl_context_t* ctx, const void* node);
{% endfor %}

// Main validation function
shacl_report_t* validate_graph(shacl_context_t* ctx);

// Utility functions
void shacl_report_free(shacl_report_t* report);
void shacl_add_violation(shacl_report_t* report, const char* focus_node, 
                        const char* property_path, const char* message);

#endif // SHACL_VALIDATION_H
        """
        
        # C implementation template
        c_impl_template = """
/*
 * SHACL Validation Implementation
 * Generated: {{ now().isoformat() }}
 */

#include "shacl_validation.h"
#include "runtime_support.h"
#include <stdio.h>
#include <assert.h>

{% if config.use_lookup_tables %}
// Lookup tables for fast validation
static const char* datatype_names[] = {
    {% for constraint in constraints %}
    {% if constraint.type == 'datatype' %}
    "{{ constraint.value }}",
    {% endif %}
    {% endfor %}
    NULL
};
{% endif %}

{% for func in validation_functions %}
/**
 * Validate {{ func.name }}
 * Constraints: {{ func.constraints | join(', ') }}
 * Performance: {{ func.performance_level }}
 */
shacl_result_t {{ func.name }}(shacl_context_t* ctx, const void* node) {
{{ func.code | indent(4, true) }}
}

{% endfor %}

// Main validation entry point
shacl_report_t* validate_graph(shacl_context_t* ctx) {
    shacl_report_t* report = calloc(1, sizeof(shacl_report_t));
    if (!report) return NULL;
    
    report->conforms = true;
    
    {% for shape in shapes.values() %}
    {% if shape.target_class %}
    // Validate {{ shape.target_class | c_identifier }}
    {% for constraint in shape.constraints %}
    if (validate_{{ constraint.id | c_identifier }}(ctx, node) != SHACL_VALID) {
        report->conforms = false;
        shacl_add_violation(report, node_id, "{{ constraint.property_path or '' }}", 
                           "{{ constraint.message or 'Constraint violation' | c_escape }}");
    }
    {% endfor %}
    {% endif %}
    {% endfor %}
    
    return report;
}

// Utility implementations
void shacl_report_free(shacl_report_t* report) {
    if (!report) return;
    
    shacl_violation_t* violation = report->violations;
    while (violation) {
        shacl_violation_t* next = violation->next;
        free(violation->focus_node);
        free(violation->property_path);
        free(violation->constraint_component);
        free(violation->message);
        free(violation);
        violation = next;
    }
    
    free(report);
}

void shacl_add_violation(shacl_report_t* report, const char* focus_node,
                        const char* property_path, const char* message) {
    shacl_violation_t* violation = calloc(1, sizeof(shacl_violation_t));
    if (!violation) return;
    
    violation->focus_node = strdup(focus_node);
    violation->property_path = strdup(property_path);
    violation->message = strdup(message);
    
    // Add to linked list
    violation->next = report->violations;
    report->violations = violation;
    report->violation_count++;
}
        """
        
        # Constraint-specific templates
        constraint_templates = {
            'minCount': """
    uint32_t count = 0;
    // Count property values
    {% if config.inline_simple_checks and constraint.value == 1 %}
    if (!has_property(node, "{{ constraint.property_path }}")) {
        return SHACL_VIOLATION;
    }
    {% else %}
    count = count_property_values(node, "{{ constraint.property_path }}");
    if (count < {{ constraint.value }}) {
        return SHACL_VIOLATION;
    }
    {% endif %}
    return SHACL_VALID;
            """,
            
            'maxCount': """
    uint32_t count = count_property_values(node, "{{ constraint.property_path }}");
    {% if config.inline_simple_checks and constraint.value == 1 %}
    if (count > 1) {
        return SHACL_VIOLATION;
    }
    {% else %}
    if (count > {{ constraint.value }}) {
        return SHACL_VIOLATION;
    }
    {% endif %}
    return SHACL_VALID;
            """,
            
            'datatype': """
    {% if config.use_lookup_tables %}
    return validate_datatype_fast(node, "{{ constraint.property_path }}", 
                                 "{{ constraint.value }}") ? SHACL_VALID : SHACL_VIOLATION;
    {% else %}
    if (!check_datatype(node, "{{ constraint.property_path }}", "{{ constraint.value }}")) {
        return SHACL_VIOLATION;
    }
    return SHACL_VALID;
    {% endif %}
            """,
            
            'pattern': """
    if (!match_pattern(get_property_value(node, "{{ constraint.property_path }}"), 
                      "{{ constraint.value | c_escape }}")) {
        return SHACL_VIOLATION;
    }
    return SHACL_VALID;
            """,
            
            'class': """
    if (!is_instance_of(node, "{{ constraint.value }}")) {
        return SHACL_VIOLATION;
    }
    return SHACL_VALID;
            """
        }
        
        # Store templates
        self.templates = {
            'c_header': self.template_env.from_string(c_header_template),
            'c_impl': self.template_env.from_string(c_impl_template),
            **{f'constraint_{k}': self.template_env.from_string(v) 
               for k, v in constraint_templates.items()}
        }
    
    def compile(self, spec_path: Path) -> Dict[str, Any]:
        """Compile SHACL specification into validation code"""
        logger.info(f"Compiling SHACL specification: {spec_path}")
        
        # Parse the SHACL graph
        self._parse_specification(spec_path)
        
        # Extract SHACL components
        self._extract_shapes()
        self._extract_constraints()
        self._extract_eightfold_mappings()
        
        # Generate validation functions
        self._generate_validation_functions()
        
        # Generate code using templates
        generated_code = self._generate_code()
        
        # Compile statistics
        self._compile_statistics()
        
        return {
            'generated_code': generated_code,
            'shapes': [self._serialize_shape(shape) for shape in self.shapes.values()],
            'constraints': [self._serialize_constraint(c) for c in self.constraints],
            'validation_functions': [self._serialize_function(f) for f in self.validation_functions],
            'statistics': self.statistics,
            'metadata': {
                'compiler': 'SHACL AOT Compiler',
                'version': '1.0.0',
                'timestamp': datetime.now().isoformat(),
                'config': self.config
            }
        }
    
    def _parse_specification(self, spec_path: Path) -> None:
        """Parse SHACL specification file"""
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
    
    def _extract_shapes(self) -> None:
        """Extract SHACL shapes from the graph"""
        for shape_uri in self.graph.subjects(RDF.type, SHACL.NodeShape):
            if isinstance(shape_uri, BNode):
                continue
            
            shape = SHACLShape(uri=str(shape_uri))
            
            # Target class
            target_class = self.graph.value(shape_uri, SHACL.targetClass)
            if target_class:
                shape.target_class = str(target_class)
            
            # Target node
            target_node = self.graph.value(shape_uri, SHACL.targetNode)
            if target_node:
                shape.target_node = str(target_node)
            
            # Closed shape
            closed = self.graph.value(shape_uri, SHACL.closed)
            if closed:
                shape.closed = bool(closed)
            
            # Deactivated
            deactivated = self.graph.value(shape_uri, SHACL.deactivated)
            if deactivated:
                shape.deactivated = bool(deactivated)
            
            # Severity
            severity = self.graph.value(shape_uri, SHACL.severity)
            if severity:
                shape.severity = str(severity)
            
            # Property shapes
            for prop_shape_uri in self.graph.objects(shape_uri, SHACL.property):
                prop_shape = self._extract_property_shape(prop_shape_uri)
                if prop_shape:
                    shape.property_shapes.append(prop_shape)
            
            self.shapes[str(shape_uri)] = shape
    
    def _extract_property_shape(self, prop_shape_uri: URIRef) -> Optional[SHACLPropertyShape]:
        """Extract a SHACL property shape"""
        path = self.graph.value(prop_shape_uri, SHACL.path)
        if not path:
            return None
        
        prop_shape = SHACLPropertyShape(path=str(path))
        
        # Name
        name = self.graph.value(prop_shape_uri, SHACL.name)
        if name:
            prop_shape.name = str(name)
        
        # Description
        desc = self.graph.value(prop_shape_uri, SHACL.description)
        if desc:
            prop_shape.description = str(desc)
        
        # Group
        group = self.graph.value(prop_shape_uri, SHACL.group)
        if group:
            prop_shape.group = str(group)
        
        # Optional (check for minCount)
        min_count = self.graph.value(prop_shape_uri, SHACL.minCount)
        if min_count and int(min_count) > 0:
            prop_shape.optional = False
        
        return prop_shape
    
    def _extract_constraints(self) -> None:
        """Extract SHACL constraints from all shapes"""
        constraint_id = 0
        
        # Core constraint components
        constraint_types = [
            (SHACL.minCount, 'minCount'),
            (SHACL.maxCount, 'maxCount'),
            (SHACL.minLength, 'minLength'),
            (SHACL.maxLength, 'maxLength'),
            (SHACL.pattern, 'pattern'),
            (SHACL.datatype, 'datatype'),
            (SHACL['class'], 'class'),
            (SHACL.nodeKind, 'nodeKind'),
            (SHACL.minInclusive, 'minInclusive'),
            (SHACL.maxInclusive, 'maxInclusive'),
            (SHACL.minExclusive, 'minExclusive'),
            (SHACL.maxExclusive, 'maxExclusive')
        ]
        
        for subject in self.graph.subjects():
            if isinstance(subject, BNode):
                continue
            
            for constraint_prop, constraint_type in constraint_types:
                for value in self.graph.objects(subject, constraint_prop):
                    constraint = SHACLConstraint(
                        id=f"constraint_{constraint_id}",
                        type=constraint_type,
                        target=str(subject),
                        value=self._convert_value(value)
                    )
                    
                    # Property path
                    path = self.graph.value(subject, SHACL.path)
                    if path:
                        constraint.property_path = str(path)
                    
                    # Message
                    message = self.graph.value(subject, SHACL.message)
                    if message:
                        constraint.message = str(message)
                    
                    # Severity
                    severity = self.graph.value(subject, SHACL.severity)
                    if severity:
                        constraint.severity = str(severity)
                    
                    # Optimization hints
                    constraint.optimization_hints = self._generate_constraint_optimization_hints(constraint)
                    
                    # Template variables
                    constraint.template_vars = self._generate_template_vars(constraint)
                    
                    self.constraints.append(constraint)
                    constraint_id += 1
                    
                    # Add to parent shape
                    if str(subject) in self.shapes:
                        self.shapes[str(subject)].constraints.append(constraint)
    
    def _extract_eightfold_mappings(self) -> None:
        """Extract Eightfold Path mappings for optimization"""
        if not self.config['eightfold_integration']:
            return
        
        for shape_uri, shape in self.shapes.items():
            # Check for explicit Eightfold annotation
            eightfold_stage = self.graph.value(URIRef(shape_uri), EH.stage)
            if eightfold_stage:
                shape.eightfold_mapping = {
                    'stage': str(eightfold_stage),
                    'explicit': True
                }
                
                # Apply stage-specific optimizations to constraints
                for constraint in shape.constraints:
                    constraint.eightfold_stage = str(eightfold_stage)
                    self._apply_eightfold_optimization(constraint)
    
    def _apply_eightfold_optimization(self, constraint: SHACLConstraint) -> None:
        """Apply Eightfold-specific optimizations to constraints"""
        stage = constraint.eightfold_stage
        
        if stage == 'Right Understanding':
            constraint.optimization_hints.append('UNDERSTANDING_FAST_PATH')
        elif stage == 'Right Action':
            constraint.optimization_hints.append('ACTION_HOT_PATH')
        elif stage == 'Right Effort':
            constraint.optimization_hints.append('EFFORT_INLINE_CHECK')
        elif stage == 'Right Concentration':
            constraint.optimization_hints.append('CONCENTRATION_BATCH_CHECK')
    
    def _generate_validation_functions(self) -> None:
        """Generate validation functions for each constraint"""
        for constraint in self.constraints:
            func_name = f"validate_{constraint.id}"
            
            # Determine performance level
            performance_level = self._determine_performance_level(constraint)
            
            # Generate function code
            code = self._generate_constraint_code(constraint)
            
            # Determine dependencies
            dependencies = self._determine_dependencies(constraint)
            
            validation_func = ValidationFunction(
                name=func_name,
                code=code,
                constraints=[constraint.type],
                dependencies=dependencies,
                performance_level=performance_level
            )
            
            self.validation_functions.append(validation_func)
    
    def _generate_constraint_code(self, constraint: SHACLConstraint) -> str:
        """Generate C code for a specific constraint"""
        template_name = f"constraint_{constraint.type}"
        
        if template_name in self.templates:
            template = self.templates[template_name]
        else:
            # Generic template
            template = self.template_env.from_string("""
    // Generic constraint check for {{ constraint.type }}
    if (!check_constraint(node, "{{ constraint.property_path }}", 
                         "{{ constraint.type }}", "{{ constraint.value }}")) {
        return SHACL_VIOLATION;
    }
    return SHACL_VALID;
            """)
        
        return template.render(
            constraint=constraint,
            config=self.config
        ).strip()
    
    def _generate_code(self) -> Dict[str, str]:
        """Generate all validation code using templates"""
        generated_code = {}
        
        # Generate header file
        header_code = self.templates['c_header'].render(
            validation_functions=self.validation_functions,
            config=self.config
        )
        generated_code['shacl_validation.h'] = header_code
        
        # Generate implementation file
        impl_code = self.templates['c_impl'].render(
            validation_functions=self.validation_functions,
            shapes=self.shapes,
            constraints=self.constraints,
            config=self.config
        )
        generated_code['shacl_validation.c'] = impl_code
        
        # Generate benchmark code if requested
        if self.config['generate_benchmarks']:
            benchmark_code = self._generate_benchmark_code()
            generated_code['shacl_benchmark.c'] = benchmark_code
        
        return generated_code
    
    def _generate_benchmark_code(self) -> str:
        """Generate benchmark code for validation functions"""
        benchmark_template = """
/*
 * SHACL Validation Benchmarks
 * Generated: {{ now().isoformat() }}
 */

#include "shacl_validation.h"
#include <time.h>
#include <stdio.h>

#define BENCHMARK_ITERATIONS 1000000

typedef struct {
    const char* name;
    shacl_result_t (*func)(shacl_context_t*, const void*);
} benchmark_func_t;

static benchmark_func_t benchmark_funcs[] = {
    {% for func in validation_functions %}
    {"{{ func.name }}", {{ func.name }}},
    {% endfor %}
    {NULL, NULL}
};

void run_benchmarks(void) {
    shacl_context_t ctx = {0};
    void* test_node = create_test_node();
    
    printf("SHACL Validation Benchmarks\\n");
    printf("============================\\n");
    
    for (int i = 0; benchmark_funcs[i].name; i++) {
        clock_t start = clock();
        
        for (int j = 0; j < BENCHMARK_ITERATIONS; j++) {
            benchmark_funcs[i].func(&ctx, test_node);
        }
        
        clock_t end = clock();
        double elapsed = ((double)(end - start)) / CLOCKS_PER_SEC;
        double ns_per_call = (elapsed * 1000000000.0) / BENCHMARK_ITERATIONS;
        
        printf("%-30s: %8.2f ns/call\\n", benchmark_funcs[i].name, ns_per_call);
    }
    
    free_test_node(test_node);
}

int main(void) {
    run_benchmarks();
    return 0;
}
        """
        
        template = self.template_env.from_string(benchmark_template)
        return template.render(validation_functions=self.validation_functions)
    
    def _compile_statistics(self) -> None:
        """Compile statistics about the SHACL shapes"""
        self.statistics = {
            'total_shapes': len(self.shapes),
            'total_constraints': len(self.constraints),
            'total_validation_functions': len(self.validation_functions),
            'constraint_types': {},
            'optimization_coverage': 0,
            'eightfold_coverage': 0
        }
        
        # Count constraint types
        for constraint in self.constraints:
            ctype = constraint.type
            self.statistics['constraint_types'][ctype] = \
                self.statistics['constraint_types'].get(ctype, 0) + 1
        
        # Calculate optimization coverage
        optimized_count = sum(1 for c in self.constraints if c.optimization_hints)
        if self.constraints:
            self.statistics['optimization_coverage'] = \
                (optimized_count / len(self.constraints)) * 100
        
        # Calculate Eightfold coverage
        eightfold_count = sum(1 for s in self.shapes.values() if s.eightfold_mapping)
        if self.shapes:
            self.statistics['eightfold_coverage'] = \
                (eightfold_count / len(self.shapes)) * 100
    
    # Helper methods
    def _convert_value(self, value: Union[URIRef, Literal, BNode]) -> Union[str, int, float, bool]:
        """Convert RDF value to Python value"""
        if isinstance(value, Literal):
            datatype = value.datatype
            if datatype == XSD.integer or datatype == XSD.int:
                return int(value)
            elif datatype == XSD.float or datatype == XSD.double:
                return float(value)
            elif datatype == XSD.boolean:
                return bool(value)
            else:
                return str(value)
        else:
            return str(value)
    
    def _generate_constraint_optimization_hints(self, constraint: SHACLConstraint) -> List[str]:
        """Generate optimization hints for a constraint"""
        hints = []
        
        if constraint.type in ['minCount', 'maxCount'] and constraint.value == 1:
            hints.append('SIMPLE_CARDINALITY')
        
        if constraint.type == 'datatype' and self.config['use_lookup_tables']:
            hints.append('LOOKUP_TABLE_CANDIDATE')
        
        if constraint.type in ['pattern'] and self.config['inline_simple_checks']:
            hints.append('INLINE_CANDIDATE')
        
        return hints
    
    def _generate_template_vars(self, constraint: SHACLConstraint) -> Dict[str, Any]:
        """Generate template variables for a constraint"""
        return {
            'constraint_id': constraint.id,
            'optimization_level': self.config['optimization_level'],
            'inline_simple': self.config['inline_simple_checks'],
            'use_lookup': self.config['use_lookup_tables']
        }
    
    def _determine_performance_level(self, constraint: SHACLConstraint) -> str:
        """Determine performance level for constraint validation"""
        if constraint.eightfold_stage in ['Right Action', 'Right Effort']:
            return 'fast'
        elif 'SIMPLE_CARDINALITY' in constraint.optimization_hints:
            return 'fast'
        elif constraint.type in ['pattern', 'class']:
            return 'comprehensive'
        else:
            return 'standard'
    
    def _determine_dependencies(self, constraint: SHACLConstraint) -> List[str]:
        """Determine dependencies for a constraint"""
        deps = []
        
        if constraint.type == 'datatype':
            deps.append('datatype_validation')
        
        if constraint.type == 'pattern':
            deps.append('regex_engine')
        
        if constraint.type == 'class':
            deps.append('inference_engine')
        
        return deps
    
    def _serialize_shape(self, shape: SHACLShape) -> Dict[str, Any]:
        """Serialize SHACLShape to dictionary"""
        return {
            'uri': shape.uri,
            'target_class': shape.target_class,
            'target_node': shape.target_node,
            'closed': shape.closed,
            'deactivated': shape.deactivated,
            'severity': shape.severity,
            'constraint_count': len(shape.constraints),
            'property_shape_count': len(shape.property_shapes),
            'eightfold_mapping': shape.eightfold_mapping
        }
    
    def _serialize_constraint(self, constraint: SHACLConstraint) -> Dict[str, Any]:
        """Serialize SHACLConstraint to dictionary"""
        return {
            'id': constraint.id,
            'type': constraint.type,
            'target': constraint.target,
            'property_path': constraint.property_path,
            'value': constraint.value,
            'severity': constraint.severity,
            'message': constraint.message,
            'eightfold_stage': constraint.eightfold_stage,
            'optimization_hints': constraint.optimization_hints
        }
    
    def _serialize_function(self, func: ValidationFunction) -> Dict[str, Any]:
        """Serialize ValidationFunction to dictionary"""
        return {
            'name': func.name,
            'code_length': len(func.code),
            'constraints': func.constraints,
            'dependencies': func.dependencies,
            'performance_level': func.performance_level
        }


def main():
    """Test the SHACL compiler"""
    import argparse
    
    parser = argparse.ArgumentParser(description="SHACL AOT Compiler")
    parser.add_argument("input", type=Path, help="Input SHACL file")
    parser.add_argument("--output-dir", type=Path, help="Output directory")
    parser.add_argument("--optimization", choices=['O0', 'O1', 'O2', 'O3'], 
                       default='O2', help="Optimization level")
    parser.add_argument("--template-dir", type=Path, help="Template directory")
    
    args = parser.parse_args()
    
    # Create compiler
    config = {
        'optimization_level': args.optimization,
        'template_dir': str(args.template_dir) if args.template_dir else 'templates'
    }
    compiler = SHACLCompiler(config)
    
    # Compile
    result = compiler.compile(args.input)
    
    # Output generated code
    output_dir = args.output_dir or Path.cwd()
    output_dir.mkdir(exist_ok=True)
    
    for filename, code in result['generated_code'].items():
        output_file = output_dir / filename
        output_file.write_text(code)
        print(f"Generated: {output_file}")
    
    # Output compilation result
    result_file = output_dir / "compilation_result.json"
    result_file.write_text(json.dumps(result, indent=2, default=str))
    print(f"Compilation result: {result_file}")
    
    # Print statistics
    print(f"\nCompilation Statistics:")
    for key, value in result['statistics'].items():
        print(f"  {key}: {value}")


if __name__ == "__main__":
    main()