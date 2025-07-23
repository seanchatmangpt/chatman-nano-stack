# AOT Compiler Integration Plan

## Overview

This document provides a detailed, phased approach to integrating the four AOT compilation systems into a unified framework.

## Integration Goals

1. **Eliminate code duplication** (~40% reduction)
2. **Maintain backward compatibility** with existing APIs
3. **Enable new capabilities** (combined compilation)
4. **Improve performance** through shared optimization
5. **Simplify maintenance** with single codebase

## Phase 1: Foundation (Week 1)

### 1.1 Create Framework Structure

```bash
# Create the new framework
mkdir -p aot_framework/{core,compilers,runtime,templates,tests}

# Create package files
touch aot_framework/__init__.py
touch aot_framework/core/__init__.py
touch aot_framework/compilers/__init__.py
```

### 1.2 Extract Common Filters

**File**: `aot_framework/core/filters.py`

```python
"""Common Jinja2 filters for all AOT compilers"""

import re
from typing import Dict, Callable

def c_identifier(value: str) -> str:
    """Convert to valid C identifier"""
    result = re.sub(r'[^a-zA-Z0-9_]', '_', str(value))
    if result and result[0].isdigit():
        result = '_' + result
    return result or '_'

# ... (all other filters from shared-components.md)

# Export all filters
COMMON_FILTERS: Dict[str, Callable] = {
    'c_identifier': c_identifier,
    'snake_case': snake_case,
    'camel_case': camel_case,
    'upper_case': upper_case,
    'xsd_to_c_type': xsd_to_c_type,
    'extract_local_name': extract_local_name,
    'escape_c_string': escape_c_string,
    'is_primitive_type': is_primitive_type,
    'format_comment': format_comment,
    'get_type_size': get_type_size,
}
```

### 1.3 Create Base Configuration

**File**: `aot_framework/core/config.py`

```python
"""Unified configuration for AOT compilers"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Callable

@dataclass
class AOTConfig:
    """Configuration for AOT compilation"""
    
    # Input/Output
    input_files: List[Path]
    output_dir: Path
    output_formats: List[str] = field(default_factory=lambda: ['c_header', 'c_implementation'])
    
    # Compiler selection
    compiler_type: str = 'auto'  # 'owl', 'shacl', 'combined', 'auto'
    
    # Compilation options
    optimization_level: int = 2
    enable_inference: bool = True
    strict_mode: bool = False
    
    # Template options
    template_dir: Optional[Path] = None
    custom_templates: Dict[str, str] = field(default_factory=dict)
    custom_filters: Dict[str, Callable] = field(default_factory=dict)
    
    # C generation options
    c_standard: str = "c11"
    generate_comments: bool = True
    include_guards: bool = True
    
    # Eightfold Path
    enable_eightfold: bool = True
    
    # Performance
    enable_caching: bool = True
    parallel_compilation: bool = True
    max_workers: int = 4
    
    # Validation
    validate_output: bool = True
    run_tests: bool = False
```

## Phase 2: Template Unification (Week 2)

### 2.1 Create Unified Template Manager

**File**: `aot_framework/core/template_manager.py`

```python
"""Unified template management system"""

import os
from pathlib import Path
from typing import Dict, Optional, Any
from jinja2 import Environment, FileSystemLoader, Template
from .filters import COMMON_FILTERS

class UnifiedTemplateManager:
    """Manages templates for all AOT compilers"""
    
    def __init__(self, config: AOTConfig):
        self.config = config
        self.env = self._create_environment()
        self._cache: Dict[str, Template] = {}
        
    def _create_environment(self) -> Environment:
        """Create Jinja2 environment with common configuration"""
        # Set up template search paths
        search_paths = []
        
        # User-provided template directory
        if self.config.template_dir:
            search_paths.append(self.config.template_dir)
            
        # Built-in templates
        builtin_dir = Path(__file__).parent.parent / 'templates'
        search_paths.append(builtin_dir)
        
        # Create environment
        env = Environment(
            loader=FileSystemLoader(search_paths),
            trim_blocks=True,
            lstrip_blocks=True,
            keep_trailing_newline=True
        )
        
        # Register common filters
        env.filters.update(COMMON_FILTERS)
        
        # Register custom filters
        if self.config.custom_filters:
            env.filters.update(self.config.custom_filters)
            
        return env
        
    def get_template(self, name: str) -> Template:
        """Get a template by name with caching"""
        if name not in self._cache:
            # Check for custom template first
            if name in self.config.custom_templates:
                self._cache[name] = self.env.from_string(
                    self.config.custom_templates[name]
                )
            else:
                self._cache[name] = self.env.get_template(f"{name}.j2")
                
        return self._cache[name]
        
    def render(self, template_name: str, context: Dict[str, Any]) -> str:
        """Render a template with context"""
        template = self.get_template(template_name)
        return template.render(**context)
```

### 2.2 Migrate Templates

Create unified templates that can be used by both compilers:

**File**: `aot_framework/templates/c_header.j2`

```jinja2
/*
 * Generated {{ generator_type }} C Header
 * Timestamp: {{ timestamp }}
 * Compiler: AOT Framework {{ version }}
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
#define {{ prefix }}_VERSION "{{ metadata.version }}"
#define {{ prefix }}_TIMESTAMP "{{ metadata.timestamp }}"

{% if has_owl %}
/* OWL Definitions */
{% include "partials/owl_header.j2" %}
{% endif %}

{% if has_shacl %}
/* SHACL Definitions */
{% include "partials/shacl_header.j2" %}
{% endif %}

#ifdef __cplusplus
}
#endif

#endif /* {{ header_guard }} */
```

## Phase 3: Base Compiler Implementation (Week 3)

### 3.1 Create Abstract Base Compiler

**File**: `aot_framework/core/base_compiler.py`

```python
"""Base class for all AOT compilers"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Dict, Any, List, Optional
import time
from rdflib import Graph

from .config import AOTConfig
from .template_manager import UnifiedTemplateManager
from .statistics import CompilationStatistics

class BaseAOTCompiler(ABC):
    """Abstract base class for AOT compilers"""
    
    def __init__(self, config: AOTConfig):
        self.config = config
        self.template_manager = UnifiedTemplateManager(config)
        self.statistics = CompilationStatistics()
        self.graph = Graph()
        
    @abstractmethod
    def get_compiler_type(self) -> str:
        """Return the compiler type identifier"""
        pass
        
    @abstractmethod
    def parse_input(self, input_path: Path) -> None:
        """Parse input file into internal representation"""
        pass
        
    @abstractmethod
    def extract_elements(self) -> Dict[str, Any]:
        """Extract semantic elements from parsed input"""
        pass
        
    @abstractmethod
    def transform_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply compiler-specific transformations"""
        pass
        
    def generate_code(self, data: Dict[str, Any]) -> Dict[str, str]:
        """Generate code from transformed data"""
        outputs = {}
        
        # Add common context
        context = {
            'generator_type': self.get_compiler_type(),
            'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
            'version': '1.0.0',
            'metadata': self.statistics.to_dict(),
            **data
        }
        
        # Generate each output format
        for format in self.config.output_formats:
            template_name = f"{self.get_compiler_type()}_{format}"
            outputs[format] = self.template_manager.render(template_name, context)
            
        return outputs
        
    def compile(self) -> Dict[str, Any]:
        """Execute the complete compilation pipeline"""
        start_time = time.time()
        
        try:
            # Parse all input files
            for input_file in self.config.input_files:
                self.parse_input(input_file)
                
            # Extract semantic elements
            data = self.extract_elements()
            
            # Transform data
            data = self.transform_data(data)
            
            # Generate code
            outputs = self.generate_code(data)
            
            # Write outputs
            written_files = self._write_outputs(outputs)
            
            # Update statistics
            self.statistics.total_time_ms = (time.time() - start_time) * 1000
            self.statistics.compilation_success = True
            
            return {
                'success': True,
                'outputs': written_files,
                'statistics': self.statistics.to_dict()
            }
            
        except Exception as e:
            self.statistics.compilation_success = False
            return {
                'success': False,
                'error': str(e),
                'statistics': self.statistics.to_dict()
            }
```

## Phase 4: Compiler Migration (Week 4)

### 4.1 Migrate OWL Compiler

**File**: `aot_framework/compilers/owl.py`

```python
"""OWL compiler using the unified framework"""

from pathlib import Path
from typing import Dict, Any
from rdflib import RDF, RDFS, OWL

from ..core.base_compiler import BaseAOTCompiler
from ..core.config import AOTConfig

class OWLCompiler(BaseAOTCompiler):
    """Compiles OWL ontologies to C code"""
    
    def __init__(self, config: AOTConfig):
        super().__init__(config)
        self.classes = {}
        self.properties = {}
        self.rules = []
        
    def get_compiler_type(self) -> str:
        return "owl"
        
    def parse_input(self, input_path: Path) -> None:
        """Parse OWL/TTL file"""
        format_map = {
            '.ttl': 'turtle',
            '.owl': 'xml',
            '.rdf': 'xml',
            '.n3': 'n3',
        }
        
        file_format = format_map.get(input_path.suffix.lower(), 'turtle')
        self.graph.parse(input_path, format=file_format)
        
        self.statistics.total_triples = len(self.graph)
        
    def extract_elements(self) -> Dict[str, Any]:
        """Extract OWL classes and properties"""
        # Extract classes
        for class_uri in self.graph.subjects(RDF.type, OWL.Class):
            self.classes[str(class_uri)] = self._extract_class(class_uri)
            
        # Extract properties  
        for prop_uri in self.graph.subjects(RDF.type, OWL.ObjectProperty):
            self.properties[str(prop_uri)] = self._extract_property(prop_uri)
            
        self.statistics.total_classes = len(self.classes)
        self.statistics.total_properties = len(self.properties)
        
        return {
            'classes': list(self.classes.values()),
            'properties': list(self.properties.values()),
            'rules': self.rules,
            'has_owl': True,
            'has_shacl': False
        }
```

### 4.2 Migrate SHACL Compiler

Similar pattern for SHACL compiler migration.

## Phase 5: Lifecycle Unification (Week 5)

### 5.1 Create Unified Lifecycle Manager

**File**: `aot_framework/core/lifecycle.py`

```python
"""Unified lifecycle management for all AOT compilers"""

import asyncio
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Callable, Any
import subprocess

from .config import AOTConfig
from .base_compiler import BaseAOTCompiler
from ..compilers.owl import OWLCompiler
from ..compilers.shacl import SHACLCompiler

class CompilerFactory:
    """Factory for creating appropriate compiler instances"""
    
    @staticmethod
    def create_compiler(config: AOTConfig) -> BaseAOTCompiler:
        """Create compiler based on configuration"""
        
        if config.compiler_type == 'owl':
            return OWLCompiler(config)
        elif config.compiler_type == 'shacl':
            return SHACLCompiler(config)
        elif config.compiler_type == 'combined':
            return CombinedCompiler(config)
        elif config.compiler_type == 'auto':
            # Auto-detect based on file extensions
            return CompilerFactory._auto_detect(config)
        else:
            raise ValueError(f"Unknown compiler type: {config.compiler_type}")

class UnifiedLifecycle:
    """Manages the complete AOT compilation lifecycle"""
    
    def __init__(self, config: AOTConfig):
        self.config = config
        self.compiler = CompilerFactory.create_compiler(config)
        
    async def compile(self) -> Dict[str, Any]:
        """Execute the compilation pipeline"""
        # Run compiler
        result = self.compiler.compile()
        
        if not result['success']:
            return result
            
        # Compile C code if requested
        if self.config.validate_output:
            c_files = [f for f in result['outputs'].values() if f.endswith('.c')]
            for c_file in c_files:
                await self._compile_c_code(c_file)
                
        return result
```

## Phase 6: Testing and Migration (Week 6)

### 6.1 Create Migration Scripts

**File**: `migrate_to_unified.py`

```python
#!/usr/bin/env python3
"""Migrate existing code to use unified framework"""

import os
import sys
from pathlib import Path

def migrate_owl_compiler():
    """Update owl_compiler.py to use framework"""
    print("Migrating owl_compiler.py...")
    
    # Create compatibility wrapper
    wrapper = '''
"""Compatibility wrapper for owl_compiler.py"""
from aot_framework.compilers.owl import OWLCompiler as _OWLCompiler
from aot_framework.core.config import AOTConfig

class OWLCompiler(_OWLCompiler):
    """Backward compatible OWL compiler"""
    
    def __init__(self, config=None, template_dir=None):
        # Convert old config format
        aot_config = AOTConfig(
            input_files=[],
            output_dir=Path('.'),
            compiler_type='owl'
        )
        super().__init__(aot_config)
        
    def compile(self, spec_path, output_dir=None):
        """Backward compatible compile method"""
        self.config.input_files = [Path(spec_path)]
        if output_dir:
            self.config.output_dir = Path(output_dir)
        return super().compile()
'''
    
    # Write wrapper
    with open('owl_compiler_compat.py', 'w') as f:
        f.write(wrapper)

def migrate_shacl_compiler():
    """Update shacl_compiler.py to use framework"""
    # Similar pattern
    pass

if __name__ == '__main__':
    migrate_owl_compiler()
    migrate_shacl_compiler()
    print("Migration complete!")
```

### 6.2 Create Test Suite

**File**: `aot_framework/tests/test_integration.py`

```python
"""Integration tests for unified framework"""

import pytest
from pathlib import Path
from aot_framework.core.config import AOTConfig
from aot_framework.core.lifecycle import UnifiedLifecycle

def test_owl_compilation():
    """Test OWL compilation through unified framework"""
    config = AOTConfig(
        input_files=[Path('test_data/ontology.ttl')],
        output_dir=Path('test_output'),
        compiler_type='owl'
    )
    
    lifecycle = UnifiedLifecycle(config)
    result = lifecycle.compile()
    
    assert result['success']
    assert 'c_header' in result['outputs']
    assert 'c_implementation' in result['outputs']

def test_combined_compilation():
    """Test combined OWL+SHACL compilation"""
    config = AOTConfig(
        input_files=[
            Path('test_data/ontology.ttl'),
            Path('test_data/shapes.ttl')
        ],
        output_dir=Path('test_output'),
        compiler_type='combined'
    )
    
    lifecycle = UnifiedLifecycle(config)
    result = lifecycle.compile()
    
    assert result['success']
    # Should have both OWL classes and SHACL validation
```

## Rollout Strategy

### Stage 1: Alpha Testing (Week 1-2)
- Deploy framework alongside existing code
- Run parallel tests comparing outputs
- Ensure 100% compatibility

### Stage 2: Beta Migration (Week 3-4)
- Update internal projects to use framework
- Gather performance metrics
- Fix any issues

### Stage 3: Full Migration (Week 5-6)
- Replace old implementations with wrappers
- Update all documentation
- Archive old code

### Stage 4: Deprecation (Week 7-8)
- Mark old APIs as deprecated
- Provide migration guide
- Plan removal timeline

## Success Metrics

1. **Code Reduction**: Target 40% reduction in total lines
2. **Performance**: Maintain or improve compilation speed
3. **Compatibility**: 100% backward compatibility
4. **Test Coverage**: >90% code coverage
5. **Documentation**: Complete API documentation

## Risk Mitigation

1. **Backward Compatibility**
   - Provide wrapper classes
   - Extensive testing
   - Gradual migration

2. **Performance Regression**
   - Benchmark at each phase
   - Profile critical paths
   - Optimize as needed

3. **Feature Parity**
   - Checklist of all features
   - Test each feature
   - No feature left behind

## Conclusion

This phased approach ensures a smooth transition to the unified AOT framework while maintaining compatibility and improving the overall system architecture.