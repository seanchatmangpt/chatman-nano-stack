# Unified AOT Framework API Design

## Overview

This document describes the API design for the unified AOT compilation framework, providing a consistent interface for all semantic format compilers.

## Core API

### Basic Usage

```python
from aot_framework import compile_semantic

# Simple compilation
result = compile_semantic(
    input_files=["ontology.ttl", "shapes.ttl"],
    output_dir="./generated",
    compiler="auto"  # Auto-detect format
)

if result.success:
    print(f"Generated: {result.outputs}")
else:
    print(f"Error: {result.error}")
```

### Advanced Usage

```python
from aot_framework import AOTConfig, UnifiedCompiler

# Configure compilation
config = AOTConfig(
    input_files=["ontology.ttl", "shapes.ttl"],
    output_dir="./generated",
    compiler_type="combined",  # Combined OWL+SHACL
    output_formats=["c_header", "c_implementation", "rust", "python"],
    optimization_level=3,
    enable_eightfold=True,
    c_standard="c11"
)

# Create compiler
compiler = UnifiedCompiler(config)

# Add custom templates
compiler.add_template("custom_header", """
/* My Custom Header Template */
{{ content }}
""")

# Add custom filters
compiler.add_filter("my_filter", lambda x: x.upper())

# Compile with callbacks
result = compiler.compile(
    on_stage=lambda stage, status: print(f"{stage}: {status}"),
    on_progress=lambda percent: print(f"Progress: {percent}%")
)
```

## Configuration API

### AOTConfig Class

```python
@dataclass
class AOTConfig:
    """Unified configuration for all AOT compilers"""
    
    # Required
    input_files: List[Path]
    output_dir: Path
    
    # Compiler Selection
    compiler_type: str = "auto"  # auto|owl|shacl|combined|custom
    
    # Output Options
    output_formats: List[str] = field(default_factory=lambda: ["c_header", "c_implementation"])
    output_prefix: Optional[str] = None
    
    # Compilation Options
    optimization_level: int = 2  # 0-3
    enable_inference: bool = True
    strict_mode: bool = False
    validate_output: bool = True
    
    # Language Options
    c_standard: str = "c11"  # c99|c11|c17
    cpp_compatible: bool = True
    generate_comments: bool = True
    
    # Feature Flags
    enable_eightfold: bool = True
    enable_statistics: bool = True
    enable_caching: bool = True
    
    # Performance
    parallel_compilation: bool = True
    max_workers: Optional[int] = None
    memory_limit_mb: Optional[int] = None
    
    # Templates
    template_dir: Optional[Path] = None
    custom_templates: Dict[str, str] = field(default_factory=dict)
    custom_filters: Dict[str, Callable] = field(default_factory=dict)
    
    # Validation
    run_tests: bool = False
    test_timeout_seconds: int = 30
    
    # Logging
    log_level: str = "INFO"
    log_file: Optional[Path] = None
```

## Compiler API

### UnifiedCompiler Class

```python
class UnifiedCompiler:
    """Main compiler interface"""
    
    def __init__(self, config: AOTConfig):
        """Initialize compiler with configuration"""
        
    def compile(self, 
                on_stage: Optional[Callable[[str, str], None]] = None,
                on_progress: Optional[Callable[[float], None]] = None) -> CompilationResult:
        """Execute compilation with optional callbacks"""
        
    def add_template(self, name: str, template: str) -> None:
        """Add a custom template"""
        
    def add_filter(self, name: str, filter_func: Callable) -> None:
        """Add a custom Jinja2 filter"""
        
    def add_plugin(self, plugin: CompilerPlugin) -> None:
        """Add a compiler plugin"""
        
    def validate_input(self) -> ValidationResult:
        """Validate input files before compilation"""
        
    def get_statistics(self) -> CompilationStatistics:
        """Get compilation statistics"""
```

### CompilationResult Class

```python
@dataclass
class CompilationResult:
    """Result of compilation"""
    success: bool
    outputs: Dict[str, Path] = field(default_factory=dict)
    error: Optional[str] = None
    warnings: List[str] = field(default_factory=list)
    statistics: Optional[CompilationStatistics] = None
    
    @property
    def c_header(self) -> Optional[Path]:
        """Get path to generated C header"""
        return self.outputs.get('c_header')
        
    @property
    def c_implementation(self) -> Optional[Path]:
        """Get path to generated C implementation"""
        return self.outputs.get('c_implementation')
```

## Plugin API

### Creating Custom Compilers

```python
from aot_framework import CompilerPlugin, register_compiler

@register_compiler("spin")
class SPINCompiler(CompilerPlugin):
    """SPIN rules to C compiler"""
    
    def get_name(self) -> str:
        return "SPIN Compiler"
        
    def can_handle(self, file_path: Path) -> bool:
        """Check if this compiler can handle the file"""
        return file_path.suffix in ['.spin', '.ttl']
        
    def parse(self, file_path: Path) -> Graph:
        """Parse SPIN rules"""
        # Implementation
        
    def extract(self, graph: Graph) -> Dict[str, Any]:
        """Extract SPIN rules"""
        # Implementation
        
    def get_templates(self) -> Dict[str, str]:
        """Provide SPIN-specific templates"""
        return {
            'spin_rules.c': self.load_template('spin_rules.c.j2')
        }
```

### Using Plugins

```python
from aot_framework import UnifiedCompiler
from my_plugins import SPINCompiler

# Register plugin
compiler = UnifiedCompiler(config)
compiler.add_plugin(SPINCompiler())

# Now handles SPIN files automatically
result = compiler.compile()
```

## Template API

### Custom Template Context

```python
# All templates receive this context
context = {
    # Metadata
    'timestamp': '2024-01-23 10:30:00',
    'generator': 'AOT Framework',
    'version': '1.0.0',
    
    # Configuration
    'config': config,
    
    # Extracted data
    'classes': [...],      # OWL classes
    'properties': [...],   # OWL properties
    'shapes': [...],       # SHACL shapes
    'constraints': [...],  # SHACL constraints
    
    # Statistics
    'statistics': {
        'total_classes': 10,
        'total_properties': 15,
        'total_constraints': 20
    },
    
    # Helpers
    'has_owl': True,
    'has_shacl': True,
    'prefix': 'MY_ONTOLOGY'
}
```

### Template Inheritance

```jinja2
{# base_template.j2 #}
/* Generated Code */
{% block includes %}
#include <stdint.h>
#include <stdbool.h>
{% endblock %}

{% block content %}
{% endblock %}

{# custom_template.j2 #}
{% extends "base_template.j2" %}

{% block includes %}
{{ super() }}
#include "my_custom.h"
{% endblock %}

{% block content %}
/* My custom implementation */
{% endblock %}
```

## Async API

### Asynchronous Compilation

```python
import asyncio
from aot_framework import compile_semantic_async

async def compile_multiple():
    """Compile multiple ontologies concurrently"""
    
    tasks = []
    for i in range(10):
        task = compile_semantic_async(
            input_files=[f"ontology_{i}.ttl"],
            output_dir=f"./output_{i}"
        )
        tasks.append(task)
    
    results = await asyncio.gather(*tasks)
    
    for i, result in enumerate(results):
        print(f"Ontology {i}: {'Success' if result.success else 'Failed'}")

# Run
asyncio.run(compile_multiple())
```

## CLI API

### Command Line Interface

```bash
# Basic usage
aot-compile ontology.ttl -o ./generated

# Combined compilation
aot-compile ontology.ttl shapes.ttl -o ./generated --compiler combined

# With options
aot-compile ontology.ttl \
    --output ./generated \
    --formats c_header,c_implementation,rust \
    --optimization 3 \
    --parallel \
    --validate

# Using config file
aot-compile --config compilation.yaml

# Watch mode
aot-compile ontology.ttl --watch --output ./generated
```

### Configuration File

```yaml
# compilation.yaml
input_files:
  - ontology.ttl
  - shapes.ttl

output_dir: ./generated

compiler_type: combined

output_formats:
  - c_header
  - c_implementation
  - rust
  - python

optimization_level: 3

features:
  eightfold: true
  inference: true
  validation: true

c_options:
  standard: c11
  cpp_compatible: true
  
templates:
  custom_header: |
    /* My Custom Header */
    {{ content }}
```

## Error Handling

### Exception Hierarchy

```python
class AOTError(Exception):
    """Base exception for AOT framework"""

class ConfigurationError(AOTError):
    """Invalid configuration"""

class ParseError(AOTError):
    """Error parsing input files"""

class CompilationError(AOTError):
    """Error during compilation"""

class TemplateError(AOTError):
    """Error in template processing"""

class ValidationError(AOTError):
    """Validation failed"""
```

### Error Handling Example

```python
from aot_framework import compile_semantic, AOTError

try:
    result = compile_semantic(
        input_files=["ontology.ttl"],
        output_dir="./generated"
    )
except ParseError as e:
    print(f"Failed to parse input: {e}")
except TemplateError as e:
    print(f"Template error: {e}")
except CompilationError as e:
    print(f"Compilation failed: {e}")
except AOTError as e:
    print(f"General error: {e}")
```

## Integration Examples

### Django Integration

```python
# django_app/ontology.py
from django.conf import settings
from aot_framework import compile_semantic

def compile_ontology(ontology_file):
    """Compile ontology for Django app"""
    
    result = compile_semantic(
        input_files=[ontology_file],
        output_dir=settings.GENERATED_CODE_DIR,
        compiler="owl",
        output_formats=["python"]  # Generate Python bindings
    )
    
    if result.success:
        # Import generated module
        import_module(f"{settings.GENERATED_CODE_DIR}.ontology")
    
    return result
```

### FastAPI Integration

```python
# fastapi_app/compile_endpoint.py
from fastapi import FastAPI, UploadFile
from aot_framework import compile_semantic_async

app = FastAPI()

@app.post("/compile")
async def compile_ontology(file: UploadFile):
    """Compile uploaded ontology"""
    
    # Save uploaded file
    temp_path = f"/tmp/{file.filename}"
    with open(temp_path, "wb") as f:
        f.write(await file.read())
    
    # Compile
    result = await compile_semantic_async(
        input_files=[temp_path],
        output_dir="/tmp/generated"
    )
    
    return {
        "success": result.success,
        "outputs": list(result.outputs.keys()),
        "statistics": result.statistics
    }
```

### CI/CD Integration

```yaml
# .github/workflows/compile.yml
name: Compile Ontologies

on: [push]

jobs:
  compile:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Setup Python
      uses: actions/setup-python@v2
      with:
        python-version: '3.9'
    
    - name: Install AOT Framework
      run: pip install aot-framework
    
    - name: Compile Ontologies
      run: |
        aot-compile ontologies/*.ttl \
          --output ./generated \
          --formats c_header,c_implementation \
          --validate
    
    - name: Test Generated Code
      run: |
        gcc -O2 generated/*.c -o test_ontology
        ./test_ontology
```

## Best Practices

### 1. Configuration Management

```python
# config.py
from aot_framework import AOTConfig

def get_development_config():
    return AOTConfig(
        input_files=["dev/ontology.ttl"],
        output_dir="./generated",
        optimization_level=0,
        generate_comments=True
    )

def get_production_config():
    return AOTConfig(
        input_files=["prod/ontology.ttl"],
        output_dir="./dist",
        optimization_level=3,
        generate_comments=False,
        validate_output=True
    )
```

### 2. Error Recovery

```python
def compile_with_retry(config, max_retries=3):
    """Compile with automatic retry"""
    
    for attempt in range(max_retries):
        try:
            result = UnifiedCompiler(config).compile()
            if result.success:
                return result
        except Exception as e:
            if attempt == max_retries - 1:
                raise
            time.sleep(2 ** attempt)  # Exponential backoff
```

### 3. Progress Monitoring

```python
def compile_with_progress():
    """Compile with progress bar"""
    
    from tqdm import tqdm
    
    pbar = tqdm(total=100, desc="Compiling")
    
    def update_progress(percent):
        pbar.update(percent - pbar.n)
    
    compiler = UnifiedCompiler(config)
    result = compiler.compile(on_progress=update_progress)
    
    pbar.close()
    return result
```

## Migration Guide

### From owl_compiler.py

```python
# Old way
from owl_compiler import OWLCompiler
compiler = OWLCompiler()
compiler.compile("ontology.ttl", output_dir="./out")

# New way
from aot_framework import compile_semantic
result = compile_semantic(
    input_files=["ontology.ttl"],
    output_dir="./out",
    compiler="owl"
)
```

### From shacl_compiler.py

```python
# Old way
from shacl_compiler import SHACLCompiler
compiler = SHACLCompiler()
compiler.compile("shapes.ttl")

# New way
from aot_framework import compile_semantic
result = compile_semantic(
    input_files=["shapes.ttl"],
    output_dir="./out",
    compiler="shacl"
)
```

## Summary

The unified AOT framework API provides:

1. **Simple, intuitive interface** for basic usage
2. **Powerful configuration** for advanced scenarios
3. **Extensible plugin system** for custom formats
4. **Async support** for high-performance applications
5. **CLI interface** for command-line usage
6. **Comprehensive error handling**
7. **Easy integration** with existing frameworks

This design ensures the framework is both easy to use for simple cases and flexible enough for complex requirements.