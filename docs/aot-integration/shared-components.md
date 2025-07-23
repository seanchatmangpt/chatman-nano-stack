# Shared Components Catalog

## Overview

This document catalogs all components that are duplicated across the AOT compilers and can be extracted into shared modules.

## 1. Template Filters

### Identical Filters (95% overlap)

```python
# Currently duplicated in both owl_compiler.py and shacl_compiler.py

def c_identifier_filter(value: str) -> str:
    """Convert to valid C identifier"""
    result = re.sub(r'[^a-zA-Z0-9_]', '_', str(value))
    if result and result[0].isdigit():
        result = '_' + result
    return result or '_'

def snake_case_filter(value: str) -> str:
    """Convert to snake_case"""
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', str(value))
    s2 = re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1)
    return s2.lower()

def camel_case_filter(value: str) -> str:
    """Convert to camelCase"""
    words = re.split(r'[^a-zA-Z0-9]', str(value))
    if not words:
        return ''
    return words[0].lower() + ''.join(word.capitalize() for word in words[1:])

def upper_case_filter(value: str) -> str:
    """Convert to UPPER_CASE"""
    return snake_case_filter(value).upper()

def extract_local_name_filter(uri: str) -> str:
    """Extract local name from URI"""
    if '#' in uri:
        return uri.split('#')[-1]
    elif '/' in uri:
        return uri.split('/')[-1]
    return uri

def escape_c_string_filter(value: str) -> str:
    """Escape string for C code"""
    if not value:
        return ''
    return value.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')

def xsd_to_c_type_filter(type_uri: str) -> str:
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

def is_primitive_type_filter(type_uri: str) -> bool:
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
```

### Additional Shared Filters

```python
def format_comment_filter(value: str, width: int = 80) -> str:
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

def get_type_size_filter(type_name: str) -> int:
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
        'void*': 8  # Assume 64-bit pointers
    }
    return type_sizes.get(type_name, 8)
```

## 2. Template Patterns

### Common C Header Pattern

```jinja2
/*
 * Generated {{ format }} C Header
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

/* Metadata */
#define VERSION "{{ metadata.version }}"
#define TIMESTAMP "{{ metadata.timestamp }}"

/* Forward declarations */
{% for type in types %}
typedef struct {{ type.name|c_identifier }}_s {{ type.name|c_identifier }}_t;
{% endfor %}

/* Structures */
{% for type in types %}
struct {{ type.name|c_identifier }}_s {
    /* Base object */
    base_object_t base;
    
    /* Properties */
    {% for prop in type.properties %}
    {{ prop.type|xsd_to_c_type }} {{ prop.name|snake_case }};
    {% endfor %}
};
{% endfor %}

#ifdef __cplusplus
}
#endif

#endif /* {{ header_guard }} */
```

### Common C Implementation Pattern

```jinja2
/*
 * Generated {{ format }} C Implementation
 * Timestamp: {{ now().isoformat() }}
 * Compiler: {{ metadata.compiler }} {{ metadata.version }}
 */

#include "{{ header_file }}"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

{% for type in types %}
/* {{ type.name }} Implementation */

{{ type.name|c_identifier }}_t* {{ type.name|snake_case }}_create(void) {
    {{ type.name|c_identifier }}_t* obj = calloc(1, sizeof({{ type.name|c_identifier }}_t));
    if (!obj) return NULL;
    
    /* Initialize base */
    obj->base.type_id = {{ loop.index0 }};
    obj->base.ref_count = 1;
    
    return obj;
}

void {{ type.name|snake_case }}_destroy({{ type.name|c_identifier }}_t* obj) {
    if (!obj) return;
    
    /* Clean up properties */
    {% for prop in type.properties %}
    {% if prop.type == 'string' %}
    free(obj->{{ prop.name|snake_case }});
    {% endif %}
    {% endfor %}
    
    free(obj);
}
{% endfor %}
```

## 3. Configuration Structures

### Common Configuration Schema

```python
@dataclass
class CompilerConfig:
    """Shared compiler configuration"""
    # Input/Output
    input_files: List[Path]
    output_dir: Path
    output_formats: List[str] = field(default_factory=lambda: ['c_header', 'c_implementation'])
    
    # Compilation options
    optimization_level: int = 2
    enable_inference: bool = True
    strict_mode: bool = False
    
    # Template options
    template_dir: Optional[Path] = None
    custom_filters: Dict[str, Callable] = field(default_factory=dict)
    
    # C generation options
    generate_comments: bool = True
    include_guards: bool = True
    c_standard: str = "c11"
    
    # Performance options
    enable_caching: bool = True
    parallel_compilation: bool = True
    max_workers: int = 4
```

## 4. Base Classes

### Abstract Compiler Base

```python
from abc import ABC, abstractmethod

class BaseAOTCompiler(ABC):
    """Base class for all AOT compilers"""
    
    def __init__(self, config: CompilerConfig):
        self.config = config
        self.graph = Graph()
        self.statistics = CompilationStatistics()
        self.template_manager = TemplateManager(config)
        
    @abstractmethod
    def parse(self, input_path: Path) -> None:
        """Parse input file into RDF graph"""
        pass
        
    @abstractmethod
    def extract(self) -> Dict[str, Any]:
        """Extract semantic elements from graph"""
        pass
        
    @abstractmethod
    def transform(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply compiler-specific transformations"""
        pass
        
    def generate(self, data: Dict[str, Any]) -> Dict[str, str]:
        """Generate code using templates"""
        outputs = {}
        
        for format in self.config.output_formats:
            template = self.template_manager.get_template(format)
            outputs[format] = template.render(**data)
            
        return outputs
    
    def compile(self) -> CompilationResult:
        """Complete compilation pipeline"""
        try:
            # Parse input
            for input_file in self.config.input_files:
                self.parse(input_file)
            
            # Extract semantic elements
            data = self.extract()
            
            # Transform data
            data = self.transform(data)
            
            # Generate code
            outputs = self.generate(data)
            
            # Write outputs
            self._write_outputs(outputs)
            
            return CompilationResult(success=True, outputs=outputs)
            
        except Exception as e:
            return CompilationResult(success=False, error=str(e))
```

## 5. Shared Data Structures

### Common Statistics

```python
@dataclass
class CompilationStatistics:
    """Shared compilation statistics"""
    # Parsing stats
    total_triples: int = 0
    parse_time_ms: float = 0.0
    
    # Extraction stats
    total_classes: int = 0
    total_properties: int = 0
    total_constraints: int = 0
    extraction_time_ms: float = 0.0
    
    # Generation stats
    files_generated: int = 0
    total_lines: int = 0
    generation_time_ms: float = 0.0
    
    # Compilation stats
    compilation_success: bool = False
    compilation_time_ms: float = 0.0
    binary_size_bytes: int = 0
```

### Common Result Types

```python
@dataclass
class CompilationResult:
    """Result of compilation"""
    success: bool
    outputs: Dict[str, str] = field(default_factory=dict)
    error: Optional[str] = None
    statistics: Optional[CompilationStatistics] = None
    warnings: List[str] = field(default_factory=list)

@dataclass 
class ValidationResult:
    """Result of validation"""
    valid: bool
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
```

## 6. Shared Utilities

### Path Handling

```python
def ensure_output_dir(output_dir: Path) -> None:
    """Ensure output directory exists"""
    output_dir.mkdir(parents=True, exist_ok=True)

def get_output_path(output_dir: Path, base_name: str, extension: str) -> Path:
    """Get standardized output path"""
    return output_dir / f"{base_name}.{extension}"

def get_header_guard(file_path: Path) -> str:
    """Generate header guard from file path"""
    name = file_path.stem.upper()
    return f"{name}_H"
```

### Error Handling

```python
class CompilationError(Exception):
    """Base compilation error"""
    pass

class ParseError(CompilationError):
    """Error during parsing"""
    pass

class ExtractionError(CompilationError):
    """Error during extraction"""
    pass

class GenerationError(CompilationError):
    """Error during code generation"""
    pass
```

## 7. Lifecycle Components

### Shared Stage Definitions

```python
class CompilationStage(Enum):
    """Unified compilation stages"""
    INITIALIZATION = "initialization"
    PARSING = "parsing"
    EXTRACTION = "extraction"
    TRANSFORMATION = "transformation"
    OPTIMIZATION = "optimization"
    GENERATION = "generation"
    COMPILATION = "compilation"
    VALIDATION = "validation"
    PACKAGING = "packaging"
```

### Stage Executor

```python
class StageExecutor:
    """Executes compilation stages"""
    
    def __init__(self, callbacks: Dict[str, Callable] = None):
        self.callbacks = callbacks or {}
        self.stage_times = {}
        
    def execute_stage(self, stage: CompilationStage, func: Callable, *args, **kwargs):
        """Execute a compilation stage with timing and callbacks"""
        stage_name = stage.value
        
        # Pre-stage callback
        if 'pre_stage' in self.callbacks:
            self.callbacks['pre_stage'](stage_name)
            
        start_time = time.time()
        
        try:
            result = func(*args, **kwargs)
            elapsed = (time.time() - start_time) * 1000
            self.stage_times[stage_name] = elapsed
            
            # Success callback
            if 'post_stage' in self.callbacks:
                self.callbacks['post_stage'](stage_name, True, elapsed)
                
            return result
            
        except Exception as e:
            elapsed = (time.time() - start_time) * 1000
            
            # Failure callback
            if 'post_stage' in self.callbacks:
                self.callbacks['post_stage'](stage_name, False, elapsed)
                
            raise
```

## Summary

These shared components represent approximately **2,500 lines of code** that can be extracted into a common framework, reducing duplication and improving maintainability across all AOT compilers in the system.