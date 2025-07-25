# CNS Forge Jinja Extraction Analysis

## Overview

This document analyzes all files in the CNS Forge project that reference Jinja templates, identifying which files need to extract, use, or integrate with the Jinja template system.

## Files That Reference Jinja Templates

### 1. Python Files (Core Template Engine)

#### Primary Template Engines

**`cns_forge_generator.py`** - Main CNS Forge Generator
- **Purpose**: Leverages existing Jinja templates to generate production CNS Forge implementation
- **Jinja Usage**: 
  - Imports: `from jinja2 import Environment, FileSystemLoader`
  - Template References: 12+ template files
  - Custom Filters: `c_identifier`, `upper_case`, `snake_case`
- **Templates Used**:
  - `bitactor/bitactor_erlang.j2`
  - `ash_reactor_bitactor.j2`
  - `bitactor/bitactor_c.j2`
  - `bitactor/bitactor_python.j2`
  - `k8s_deployment.yaml.j2`
  - `k8s_service.yaml.j2`
  - `k8s_configmap.yaml.j2`
  - `terraform_aegis.tf.j2`
  - `bitactor/bitactor_test_c.j2`
  - `bitactor/bitactor_benchmark_c.j2`
  - `makefile.j2`

**`jinja_aot_compiler.py`** - AOT Compilation Engine
- **Purpose**: Ahead-of-time Jinja2 compiler for performance optimization
- **Jinja Usage**:
  - Imports: `import jinja2`, `from jinja2 import Environment, DictLoader, Template, meta`
  - AOT Compilation: Template pre-compilation and caching
  - Performance: 10-50x improvement over runtime compilation
- **Key Features**:
  - Template caching with bytecode storage
  - Custom filter registration
  - Performance benchmarking
  - Template metadata tracking

**`dfls_semantic_codegen.py`** - DFLS Semantic Code Generator
- **Purpose**: TTL/OWL/SHACL/SPARQL → Jinja Typer → Erlang/OTP Workflow
- **Jinja Usage**:
  - Imports: `import jinja2`, `from jinja2 import Environment, FileSystemLoader, select_autoescape`
  - Integration: Uses existing `JinjaAOTCompiler`
  - Custom Filters: Erlang-specific filters (`erlang_atom`, `erlang_string`, `erlang_module_name`)
- **Templates Used**:
  - GenServer templates
  - Supervisor templates
  - Erlang module templates

#### Secondary Template Engines

**`cns_forge_implementation.py`** - CNS Forge Implementation
- **Purpose**: Implements CNS Forge with Jinja2 template generation
- **Jinja Usage**:
  - Imports: `from jinja2 import Environment, FileSystemLoader`
  - Custom Filters: `c_identifier`
- **Templates Used**:
  - `bitactor/bitactor_c.j2`
  - `terraform_aegis.tf.j2`
  - `k8s_deployment.yaml.j2`

**`aegis_ttl_template_engine.py`** - Aegis TTL Template Engine
- **Purpose**: Generates all system components from TTL ontology using Jinja2 templates
- **Jinja Usage**:
  - Imports: `from jinja2 import Environment, FileSystemLoader, Template`
  - Template Creation: Creates Jinja2 templates for BitActor C code

**`ttl_to_nuxt_generator.py`** - TTL to Nuxt Generator
- **Purpose**: Uses Jinja2 templates for 80/20 optimized code generation
- **Jinja Usage**:
  - Imports: `from jinja2 import Environment, FileSystemLoader`
  - Frontend Generation: Nuxt.js components from TTL ontologies

**`shacl_compiler.py`** - SHACL Compiler
- **Purpose**: Compiles SHACL shapes into optimized C validation code using Jinja2
- **Jinja Usage**:
  - Imports: `import jinja2`, `from jinja2 import Environment, FileSystemLoader, Template`
  - Template Environment: `Environment(loader=jinja2.DictLoader({}))`
  - Custom Filters: SHACL-specific filters

#### Testing and Benchmarking

**`jinja_aot_benchmark.py`** - Jinja AOT Benchmark
- **Purpose**: Comprehensive performance testing for Jinja AOT optimization
- **Jinja Usage**:
  - Imports: `from jinja2 import Environment, DictLoader`
  - Integration: Uses `JinjaAOTCompiler`, `OWLTemplateCompiler`, `SHACLTemplateCompiler`
  - Benchmarking: Performance comparison between runtime and AOT compilation

**`test_owl_compiler_coverage.py`** - OWL Compiler Test Coverage
- **Purpose**: Tests custom Jinja filters and template functionality
- **Jinja Usage**:
  - Imports: `from jinja2 import Environment, DictLoader`
  - Test Functions: `test_custom_jinja_filters`, `test_adding_custom_jinja_filter`

### 2. Elixir Files (Integration Layer)

#### Production Reactor

**`cns_forge_production_reactor.ex`** - Production Reactor Workflow
- **Purpose**: Production CNS Forge reactor with Jinja template integration
- **Jinja Usage**:
  - Template Path: `jinja_templates: "/Users/sac/cns/templates"`
  - Integration: Calls Python generator via `CNSForge.PythonBridge`
  - Code Generation: Generates BitActor code using existing Jinja template infrastructure
- **Key Integration Points**:
  ```elixir
  argument :jinja_templates, :string, allow_nil?: false
  
  generation_result = CNSForge.PythonBridge.call_generator(%{
    parameters: input.decoded_params,
    performance_targets: input.performance_targets,
    template_path: input.jinja_templates,
    ttl_budget: input.ttl_remaining
  })
  ```

#### Generated Test Files

**`generated/cns_forge_tdd_generated_tests.exs`** - Generated Test Suite
- **Purpose**: TDD-generated tests including Jinja template testing
- **Jinja Usage**:
  - Test Function: `test_jinja_template_code_generation_tdd`
  - Template Validation: Tests Jinja template existence at `templates/ash_reactor_bitactor.j2`

### 3. Generated Files (Output)

#### Generated C Files
All generated C files include Jinja references in headers:
- **Pattern**: `* Generated by CNS Jinja AOT Compiler` or `* Compiler: OWL AOT Compiler with Jinja 1.0.0`
- **Files**: 50+ generated C files across multiple directories
- **Examples**:
  - `generated/bytecode/cnsforge.c`
  - `generated/cns_forge_8020/cns_forge_bitactor.c`
  - `generated/bitactor/semantic_test.c`
  - `live_system/uhft_core.c`

#### Generated Erlang Files
All generated Erlang files include Jinja references in module headers:
- **Pattern**: `%%% @copyright 2025 CNS - Generated by Jinja AOT`
- **Files**: 10+ generated Erlang files
- **Examples**:
  - `generated/bitactor/semantic_bitactor.erl`
  - `generated/cns_forge_8020/cns_forge_bitactor.erl`
  - `generated/coverage_test/coverage_bitactor.erl`

### 4. Configuration and Setup Files

**`setup.py`** - Project Setup
- **Purpose**: Project setup and dependency management
- **Jinja Usage**:
  - Dependency Check: `import rdflib, jinja2; print("Core deps OK")`
  - Ensures Jinja2 is available as a core dependency

## Files That Need Jinja Template Extraction

### 1. High Priority (Core Functionality)

#### `cns_forge_generator.py`
**Current State**: Uses 12+ hardcoded template paths
**Extraction Needs**:
- Template discovery and dynamic loading
- Template validation and error handling
- Template versioning and compatibility checking
- Template dependency resolution

#### `jinja_aot_compiler.py`
**Current State**: AOT compilation with caching
**Extraction Needs**:
- Template source extraction from various locations
- Template dependency graph construction
- Template compilation pipeline optimization
- Template cache invalidation strategies

#### `dfls_semantic_codegen.py`
**Current State**: Uses existing JinjaAOTCompiler
**Extraction Needs**:
- Template integration with semantic workflows
- Template context generation from TTL ontologies
- Template output validation and quality gates
- Template performance monitoring

### 2. Medium Priority (Integration Layer)

#### `cns_forge_production_reactor.ex`
**Current State**: Hardcoded template path
**Extraction Needs**:
- Dynamic template path resolution
- Template availability validation
- Template version compatibility checking
- Template fallback mechanisms

#### `cns_forge_implementation.py`
**Current State**: Uses specific template files
**Extraction Needs**:
- Template selection based on implementation type
- Template parameter validation
- Template output post-processing
- Template error recovery

### 3. Low Priority (Testing and Validation)

#### Test Files
**Current State**: Test template existence
**Extraction Needs**:
- Template test data generation
- Template output validation
- Template performance benchmarking
- Template regression testing

## Template Extraction Requirements

### 1. Template Discovery

```python
def discover_templates(self) -> List[Path]:
    """Discover all Jinja templates in the project"""
    templates = []
    for template_file in self.templates_path.rglob("*.j2"):
        templates.append(template_file)
    return templates
```

### 2. Template Validation

```python
def validate_template(self, template_path: Path) -> bool:
    """Validate template syntax and dependencies"""
    try:
        with open(template_path, 'r') as f:
            source = f.read()
        self.env.parse(source)
        return True
    except Exception as e:
        logger.error(f"Template validation failed for {template_path}: {e}")
        return False
```

### 3. Template Context Generation

```python
def generate_template_context(self, ontology: Dict) -> Dict[str, Any]:
    """Generate context for template rendering"""
    return {
        "ontology_name": ontology["name"],
        "timestamp": datetime.now().isoformat(),
        "signals": self._extract_signals(ontology),
        "handlers": self._extract_handlers(ontology),
        "config": self._extract_config(ontology)
    }
```

### 4. Template Dependency Resolution

```python
def resolve_template_dependencies(self, template_path: Path) -> List[Path]:
    """Resolve template dependencies"""
    dependencies = []
    with open(template_path, 'r') as f:
        source = f.read()
    
    # Extract include/extend statements
    for line in source.split('\n'):
        if line.strip().startswith('{% include') or line.strip().startswith('{% extends'):
            dep_path = self._extract_template_path(line)
            if dep_path:
                dependencies.append(dep_path)
    
    return dependencies
```

## Integration Points

### 1. Elixir-Python Bridge

```elixir
defmodule CNSForge.PythonBridge do
  @moduledoc """
  Bridge to Python Jinja template engine
  """
  
  def call_generator(params) do
    # Call Python generator with template parameters
    System.cmd("python3", [
      "cns_forge_generator.py",
      "--params", Jason.encode!(params.parameters),
      "--templates", params.template_path,
      "--ttl", Integer.to_string(params.ttl_budget)
    ])
  end
end
```

### 2. Template Engine Integration

```python
class CNSForgeTemplateEngine:
    """Unified template engine for CNS Forge"""
    
    def __init__(self, template_root: Path):
        self.template_root = template_root
        self.jinja_env = self._setup_jinja_environment()
        self.aot_compiler = JinjaAOTCompiler()
        self.templates = self._discover_templates()
    
    def generate_project(self, ontology: Dict, project_type: str) -> Dict[str, str]:
        """Generate complete project from ontology"""
        context = self._generate_context(ontology)
        templates = self._select_templates(project_type)
        
        files = {}
        for template_name in templates:
            content = self._render_template(template_name, context)
            output_path = self._determine_output_path(template_name, project_type)
            files[output_path] = content
        
        return files
```

## Performance Considerations

### 1. Template Caching

```python
def _cache_template(self, template_name: str, template_source: str):
    """Cache compiled template for performance"""
    compiled = self.aot_compiler.compile_template(template_name, template_source)
    self.template_cache[template_name] = compiled
```

### 2. Parallel Processing

```python
def generate_parallel(self, templates: List[str], context: Dict) -> Dict[str, str]:
    """Generate templates in parallel"""
    with ThreadPoolExecutor(max_workers=4) as executor:
        future_to_template = {
            executor.submit(self._render_template, template, context): template
            for template in templates
        }
        
        results = {}
        for future in as_completed(future_to_template):
            template = future_to_template[future]
            results[template] = future.result()
        
        return results
```

## Error Handling and Recovery

### 1. Template Not Found

```python
def _handle_template_not_found(self, template_name: str) -> str:
    """Handle missing template with fallback"""
    logger.warning(f"Template {template_name} not found, using fallback")
    return self._generate_fallback_template(template_name)
```

### 2. Template Rendering Errors

```python
def _handle_rendering_error(self, template_name: str, error: Exception) -> str:
    """Handle template rendering errors"""
    logger.error(f"Template rendering failed for {template_name}: {error}")
    return self._generate_error_template(template_name, error)
```

## Future Enhancements

### 1. Template Versioning

```python
def _check_template_version(self, template_path: Path) -> str:
    """Check template version and compatibility"""
    # Extract version from template metadata
    # Validate compatibility with current system
    # Return version string
```

### 2. Template Composition

```python
def _compose_templates(self, base_template: str, components: List[str]) -> str:
    """Compose templates from components"""
    # Combine multiple templates into single output
    # Handle template inheritance and composition
    # Return composed template
```

---

*This analysis provides a comprehensive overview of all files that reference Jinja templates in the CNS Forge project, identifying extraction needs and integration requirements for the template system.* 