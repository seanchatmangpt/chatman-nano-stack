#!/usr/bin/env python3
"""
OWL AOT Compiler - Complete Lifecycle Implementation
Manages the entire compilation pipeline from ontology parsing to C code generation
"""

import os
import sys
import logging
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Union
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
import json
import yaml
import concurrent.futures
from contextlib import contextmanager

# Import the main compiler components
from owl_compiler import OWLCompiler, TemplateManager

class CompilationStage(Enum):
    """Compilation pipeline stages"""
    INITIALIZATION = "initialization"
    PARSING = "parsing"
    EXTRACTION = "extraction"
    INFERENCE = "inference"
    OPTIMIZATION = "optimization"
    CODE_GENERATION = "code_generation"
    COMPILATION = "compilation"
    LINKING = "linking"
    TESTING = "testing"
    PACKAGING = "packaging"
    DEPLOYMENT = "deployment"

class LogLevel(Enum):
    """Logging levels"""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"

@dataclass
class CompilationConfig:
    """Complete configuration for the compilation lifecycle"""
    # Input configuration
    input_files: List[Path] = field(default_factory=list)
    input_format: str = "auto"  # auto, turtle, xml, json-ld
    
    # Output configuration
    output_dir: Path = Path("./output")
    output_formats: List[str] = field(default_factory=lambda: ["c_header", "c_implementation", "json"])
    target_architecture: str = "x86_64"
    
    # Compilation options
    strict_mode: bool = True
    inference_enabled: bool = True
    reasoning_depth: int = 3
    extract_shacl: bool = True
    eightfold_integration: bool = True
    optimization_level: int = 2  # 0=none, 1=basic, 2=aggressive, 3=experimental
    
    # C compilation options
    c_compiler: str = "gcc"
    c_flags: List[str] = field(default_factory=lambda: ["-std=c11", "-Wall", "-Wextra", "-O2"])
    include_paths: List[Path] = field(default_factory=list)
    library_paths: List[Path] = field(default_factory=list)
    libraries: List[str] = field(default_factory=list)
    
    # Template configuration
    template_dir: Optional[Path] = None
    custom_templates: Dict[str, str] = field(default_factory=dict)
    
    # Testing configuration
    enable_tests: bool = True
    test_framework: str = "unity"  # unity, ctest, custom
    
    # Packaging configuration
    create_package: bool = False
    package_format: str = "tar.gz"  # tar.gz, zip, deb, rpm
    
    # Parallel processing
    max_workers: int = 4
    
    # Logging
    log_level: LogLevel = LogLevel.INFO
    log_file: Optional[Path] = None

@dataclass
class CompilationResult:
    """Results from compilation lifecycle"""
    success: bool = False
    stage_results: Dict[CompilationStage, Dict[str, Any]] = field(default_factory=dict)
    output_files: List[Path] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    metrics: Dict[str, Any] = field(default_factory=dict)
    start_time: datetime = field(default_factory=datetime.now)
    end_time: Optional[datetime] = None
    
    @property
    def duration(self) -> float:
        """Get compilation duration in seconds"""
        if self.end_time:
            return (self.end_time - self.start_time).total_seconds()
        return 0.0

class CompilationError(Exception):
    """Custom exception for compilation errors"""
    def __init__(self, stage: CompilationStage, message: str, details: Optional[Dict[str, Any]] = None):
        self.stage = stage
        self.message = message
        self.details = details or {}
        super().__init__(f"{stage.value}: {message}")

class OWLCompilerLifecycle:
    """Manages the complete OWL compilation lifecycle"""
    
    def __init__(self, config: CompilationConfig):
        self.config = config
        self.result = CompilationResult()
        self.logger = self._setup_logging()
        self.temp_dirs: List[Path] = []
        self.compiler: Optional[OWLCompiler] = None
        self.current_stage = CompilationStage.INITIALIZATION
        
    def _setup_logging(self) -> logging.Logger:
        """Setup logging configuration"""
        logger = logging.getLogger("owl_compiler_lifecycle")
        logger.setLevel(getattr(logging, self.config.log_level.value.upper()))
        
        # Clear existing handlers
        for handler in logger.handlers[:]:
            logger.removeHandler(handler)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_format = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        console_handler.setFormatter(console_format)
        logger.addHandler(console_handler)
        
        # File handler if specified
        if self.config.log_file:
            self.config.log_file.parent.mkdir(parents=True, exist_ok=True)
            file_handler = logging.FileHandler(self.config.log_file)
            file_handler.setFormatter(console_format)
            logger.addHandler(file_handler)
        
        return logger
    
    @contextmanager
    def _stage_context(self, stage: CompilationStage):
        """Context manager for compilation stages"""
        self.current_stage = stage
        stage_start = datetime.now()
        self.logger.info(f"Starting stage: {stage.value}")
        
        stage_result = {
            'stage': stage,
            'start_time': stage_start,
            'success': False,
            'metrics': {},
            'outputs': [],
            'errors': [],
            'warnings': []
        }
        
        try:
            yield stage_result
            stage_result['success'] = True
            self.logger.info(f"Completed stage: {stage.value}")
            
        except Exception as e:
            stage_result['success'] = False
            stage_result['errors'].append(str(e))
            self.logger.error(f"Failed stage {stage.value}: {e}")
            raise
            
        finally:
            stage_result['end_time'] = datetime.now()
            stage_result['duration'] = (stage_result['end_time'] - stage_start).total_seconds()
            self.result.stage_results[stage] = stage_result
    
    def run(self) -> CompilationResult:
        """Run the complete compilation lifecycle"""
        try:
            self._run_initialization()
            self._run_parsing()
            self._run_extraction()
            
            if self.config.inference_enabled:
                self._run_inference()
            
            self._run_optimization()
            self._run_code_generation()
            self._run_compilation()
            self._run_linking()
            
            if self.config.enable_tests:
                self._run_testing()
            
            if self.config.create_package:
                self._run_packaging()
            
            self.result.success = True
            self.logger.info("Compilation lifecycle completed successfully")
            
        except CompilationError as e:
            self.result.success = False
            self.result.errors.append(str(e))
            self.logger.error(f"Compilation failed at stage {e.stage.value}: {e.message}")
            
        except Exception as e:
            self.result.success = False
            self.result.errors.append(f"Unexpected error: {str(e)}")
            self.logger.critical(f"Unexpected error in stage {self.current_stage.value}: {e}")
            
        finally:
            self.result.end_time = datetime.now()
            self._cleanup()
            
        return self.result
    
    def _run_initialization(self) -> None:
        """Initialize the compilation environment"""
        with self._stage_context(CompilationStage.INITIALIZATION) as stage:
            self.logger.info("Initializing compilation environment")
            
            # Validate input files
            for input_file in self.config.input_files:
                if not input_file.exists():
                    raise CompilationError(
                        CompilationStage.INITIALIZATION,
                        f"Input file not found: {input_file}"
                    )
            
            stage['metrics']['input_file_count'] = len(self.config.input_files)
            
            # Create output directory
            self.config.output_dir.mkdir(parents=True, exist_ok=True)
            
            # Initialize compiler
            compiler_config = {
                'strict_mode': self.config.strict_mode,
                'inference_enabled': self.config.inference_enabled,
                'reasoning_depth': self.config.reasoning_depth,
                'extract_shacl': self.config.extract_shacl,
                'eightfold_integration': self.config.eightfold_integration,
                'optimization_hints': self.config.optimization_level > 0,
                'output_formats': self.config.output_formats
            }
            
            self.compiler = OWLCompiler(compiler_config, self.config.template_dir)
            
            # Add custom templates
            for name, template_content in self.config.custom_templates.items():
                self.compiler.template_manager.env.loader.mapping[name] = template_content
            
            stage['outputs'].append(str(self.config.output_dir))
    
    def _run_parsing(self) -> None:
        """Parse input ontology files"""
        with self._stage_context(CompilationStage.PARSING) as stage:
            self.logger.info("Parsing ontology files")
            
            total_triples = 0
            
            for input_file in self.config.input_files:
                self.logger.info(f"Parsing file: {input_file}")
                
                # Parse each file
                self.compiler._parse_specification(input_file)
                
                # Track metrics
                file_triples = len(self.compiler.graph)
                total_triples += file_triples
                
                stage['metrics'][f'triples_{input_file.stem}'] = file_triples
            
            stage['metrics']['total_triples'] = total_triples
            stage['metrics']['namespaces'] = len(self.compiler.prefixes)
    
    def _run_extraction(self) -> None:
        """Extract ontology components"""
        with self._stage_context(CompilationStage.EXTRACTION) as stage:
            self.logger.info("Extracting ontology components")
            
            # Extract classes
            self.compiler._extract_classes()
            stage['metrics']['classes_extracted'] = len(self.compiler.classes)
            
            # Extract properties
            self.compiler._extract_properties()
            stage['metrics']['properties_extracted'] = len(self.compiler.properties)
            
            # Extract rules
            self.compiler._extract_rules()
            stage['metrics']['rules_extracted'] = len(self.compiler.rules)
            
            # Extract Eightfold mappings
            if self.config.eightfold_integration:
                self.compiler._extract_eightfold_mappings()
                mapped_classes = sum(1 for cls in self.compiler.classes.values() 
                                   if cls.eightfold_mapping is not None)
                stage['metrics']['eightfold_mapped_classes'] = mapped_classes
    
    def _run_inference(self) -> None:
        """Apply inference and reasoning"""
        with self._stage_context(CompilationStage.INFERENCE) as stage:
            self.logger.info("Applying inference and reasoning")
            
            initial_triples = len(self.compiler.graph)
            
            # Apply inference
            self.compiler._apply_inference()
            
            final_triples = len(self.compiler.graph)
            inferred_triples = final_triples - initial_triples
            
            stage['metrics']['initial_triples'] = initial_triples
            stage['metrics']['final_triples'] = final_triples
            stage['metrics']['inferred_triples'] = inferred_triples
            stage['metrics']['inference_ratio'] = inferred_triples / initial_triples if initial_triples > 0 else 0
    
    def _run_optimization(self) -> None:
        """Apply optimizations"""
        with self._stage_context(CompilationStage.OPTIMIZATION) as stage:
            self.logger.info(f"Applying optimizations (level {self.config.optimization_level})")
            
            if self.config.optimization_level > 0:
                self.compiler._generate_optimization_hints()
                
                # Apply optimization-level specific transformations
                if self.config.optimization_level >= 2:
                    self._apply_aggressive_optimizations()
                
                if self.config.optimization_level >= 3:
                    self._apply_experimental_optimizations()
            
            # Count optimization hints
            hint_count = 0
            for cls in self.compiler.classes.values():
                hints = cls.annotations.get('optimization_hints', [])
                hint_count += len(hints)
            
            stage['metrics']['optimization_hints'] = hint_count
            stage['metrics']['optimization_level'] = self.config.optimization_level
    
    def _apply_aggressive_optimizations(self) -> None:
        """Apply aggressive optimizations"""
        self.logger.info("Applying aggressive optimizations")
        
        # Inline small classes
        for class_uri, owl_class in self.compiler.classes.items():
            if len(owl_class.properties) <= 2 and not owl_class.parent_classes:
                owl_class.annotations.setdefault('optimization_hints', []).append({
                    'type': 'inline_class',
                    'reason': 'small_class',
                    'suggestion': 'inline_in_parent'
                })
        
        # Cache frequently used properties
        property_usage = {}
        for owl_class in self.compiler.classes.values():
            for prop in owl_class.properties:
                prop_uri = prop['uri']
                property_usage[prop_uri] = property_usage.get(prop_uri, 0) + 1
        
        for prop_uri, usage_count in property_usage.items():
            if usage_count > 5:  # Threshold for caching
                if prop_uri in self.compiler.properties:
                    prop = self.compiler.properties[prop_uri]
                    prop.annotations.setdefault('optimization_hints', []).append({
                        'type': 'cache_property',
                        'reason': f'high_usage_{usage_count}',
                        'suggestion': 'create_property_cache'
                    })
    
    def _apply_experimental_optimizations(self) -> None:
        """Apply experimental optimizations"""
        self.logger.info("Applying experimental optimizations")
        
        # SIMD vectorization hints for array operations
        for owl_class in self.compiler.classes.values():
            array_properties = [p for p in owl_class.properties 
                              if 'array' in p.get('label', '').lower()]
            
            if array_properties:
                owl_class.annotations.setdefault('optimization_hints', []).append({
                    'type': 'simd_vectorization',
                    'reason': 'array_operations',
                    'suggestion': 'use_simd_intrinsics'
                })
        
        # Memory pool allocation for related objects
        for owl_class in self.compiler.classes.values():
            if owl_class.eightfold_mapping:
                stage = owl_class.eightfold_mapping.get('stage')
                owl_class.annotations.setdefault('optimization_hints', []).append({
                    'type': 'memory_pool',
                    'reason': f'eightfold_stage_{stage}',
                    'suggestion': 'use_stage_specific_allocator'
                })
    
    def _run_code_generation(self) -> None:
        """Generate C code from ontology"""
        with self._stage_context(CompilationStage.CODE_GENERATION) as stage:
            self.logger.info("Generating C code")
            
            # Compile statistics
            self.compiler._compile_statistics()
            
            # Create compilation result
            compilation_result = self.compiler._create_compilation_result()
            
            # Generate output files
            base_name = "owl_ontology"
            self.compiler._generate_output_files(compilation_result, self.config.output_dir, base_name)
            
            # Track generated files
            generated_files = []
            for output_format in self.config.output_formats:
                if output_format == "c_header":
                    file_path = self.config.output_dir / f"{base_name}.h"
                elif output_format == "c_implementation":
                    file_path = self.config.output_dir / f"{base_name}.c"
                elif output_format == "json":
                    file_path = self.config.output_dir / f"{base_name}.json"
                elif output_format == "makefile":
                    file_path = self.config.output_dir / "Makefile"
                else:
                    continue
                
                if file_path.exists():
                    generated_files.append(file_path)
                    stage['outputs'].append(str(file_path))
            
            stage['metrics']['generated_files'] = len(generated_files)
            stage['metrics']['code_size'] = sum(f.stat().st_size for f in generated_files)
            
            self.result.output_files.extend(generated_files)
    
    def _run_compilation(self) -> None:
        """Compile generated C code"""
        with self._stage_context(CompilationStage.COMPILATION) as stage:
            self.logger.info("Compiling generated C code")
            
            c_files = [f for f in self.result.output_files if f.suffix == '.c']
            h_files = [f for f in self.result.output_files if f.suffix == '.h']
            
            if not c_files:
                self.logger.warning("No C source files to compile")
                return
            
            # Prepare compilation command
            compile_cmd = [self.config.c_compiler] + self.config.c_flags
            
            # Add include paths
            for include_path in self.config.include_paths:
                compile_cmd.extend(["-I", str(include_path)])
            
            # Add source files
            compile_cmd.extend([str(f) for f in c_files])
            
            # Output object files
            object_files = []
            for c_file in c_files:
                obj_file = c_file.with_suffix('.o')
                object_files.append(obj_file)
                
                # Compile each source file
                file_cmd = compile_cmd + ["-c", str(c_file), "-o", str(obj_file)]
                
                self.logger.debug(f"Compiling: {' '.join(file_cmd)}")
                result = subprocess.run(file_cmd, capture_output=True, text=True)
                
                if result.returncode != 0:
                    raise CompilationError(
                        CompilationStage.COMPILATION,
                        f"C compilation failed for {c_file}",
                        {'stderr': result.stderr, 'stdout': result.stdout}
                    )
                
                stage['outputs'].append(str(obj_file))
                self.result.output_files.append(obj_file)
            
            stage['metrics']['compiled_files'] = len(object_files)
            stage['metrics']['object_size'] = sum(f.stat().st_size for f in object_files)
    
    def _run_linking(self) -> None:
        """Link compiled objects into final executable/library"""
        with self._stage_context(CompilationStage.LINKING) as stage:
            self.logger.info("Linking compiled objects")
            
            object_files = [f for f in self.result.output_files if f.suffix == '.o']
            
            if not object_files:
                self.logger.warning("No object files to link")
                return
            
            # Create shared library
            output_lib = self.config.output_dir / "libowl_ontology.so"
            
            link_cmd = [self.config.c_compiler, "-shared"] + [str(f) for f in object_files]
            
            # Add library paths
            for lib_path in self.config.library_paths:
                link_cmd.extend(["-L", str(lib_path)])
            
            # Add libraries
            for lib in self.config.libraries:
                link_cmd.extend(["-l", lib])
            
            link_cmd.extend(["-o", str(output_lib)])
            
            self.logger.debug(f"Linking: {' '.join(link_cmd)}")
            result = subprocess.run(link_cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                raise CompilationError(
                    CompilationStage.LINKING,
                    "Linking failed",
                    {'stderr': result.stderr, 'stdout': result.stdout}
                )
            
            stage['outputs'].append(str(output_lib))
            self.result.output_files.append(output_lib)
            
            stage['metrics']['library_size'] = output_lib.stat().st_size
    
    def _run_testing(self) -> None:
        """Run tests on compiled code"""
        with self._stage_context(CompilationStage.TESTING) as stage:
            self.logger.info("Running tests")
            
            # Generate test files
            test_files = self._generate_test_files()
            
            # Compile test files
            test_executables = self._compile_test_files(test_files)
            
            # Run tests
            test_results = self._run_test_executables(test_executables)
            
            stage['metrics']['tests_generated'] = len(test_files)
            stage['metrics']['tests_compiled'] = len(test_executables)
            stage['metrics']['tests_passed'] = sum(1 for r in test_results if r['passed'])
            stage['metrics']['tests_failed'] = sum(1 for r in test_results if not r['passed'])
            stage['metrics']['test_coverage'] = self._calculate_test_coverage(test_results)
            
            # Check if all tests passed
            if stage['metrics']['tests_failed'] > 0:
                failed_tests = [r['name'] for r in test_results if not r['passed']]
                raise CompilationError(
                    CompilationStage.TESTING,
                    f"Tests failed: {', '.join(failed_tests)}"
                )
    
    def _generate_test_files(self) -> List[Path]:
        """Generate test files for the compiled ontology"""
        test_files = []
        
        # Basic functionality tests
        basic_test = self._generate_basic_test()
        basic_test_file = self.config.output_dir / "test_basic.c"
        basic_test_file.write_text(basic_test)
        test_files.append(basic_test_file)
        
        # Eightfold Path tests
        if self.config.eightfold_integration:
            eightfold_test = self._generate_eightfold_test()
            eightfold_test_file = self.config.output_dir / "test_eightfold.c"
            eightfold_test_file.write_text(eightfold_test)
            test_files.append(eightfold_test_file)
        
        # Reasoning tests
        if self.config.inference_enabled:
            reasoning_test = self._generate_reasoning_test()
            reasoning_test_file = self.config.output_dir / "test_reasoning.c"
            reasoning_test_file.write_text(reasoning_test)
            test_files.append(reasoning_test_file)
        
        return test_files
    
    def _generate_basic_test(self) -> str:
        """Generate basic functionality test"""
        template = '''
#include "owl_ontology.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

int test_basic_creation() {
    printf("Testing basic object creation...\\n");
    
    // Test each class can be created
    {% for class in compiler.classes.values() %}
    {
        {{ class.label|snake_case }}_t* obj = {{ class.label|snake_case }}_create();
        assert(obj != NULL);
        assert({{ class.label|snake_case }}_validate(obj));
        {{ class.label|snake_case }}_destroy(obj);
        printf("✓ {{ class.label }} creation test passed\\n");
    }
    {% endfor %}
    
    return 1;
}

int test_api_functions() {
    printf("Testing API functions...\\n");
    
    // Test class registry
    {% for class in compiler.classes.values() %}
    {
        const class_descriptor_t* desc = owl_get_class("{{ class.uri }}");
        assert(desc != NULL);
        assert(strcmp(desc->uri, "{{ class.uri }}") == 0);
        printf("✓ {{ class.label }} registry test passed\\n");
    }
    {% endfor %}
    
    return 1;
}

int main() {
    printf("Running basic functionality tests...\\n");
    
    if (!test_basic_creation()) {
        printf("❌ Basic creation tests failed\\n");
        return 1;
    }
    
    if (!test_api_functions()) {
        printf("❌ API function tests failed\\n");
        return 1;
    }
    
    printf("✅ All basic tests passed\\n");
    return 0;
}
'''
        return self.compiler.template_manager.render_from_string(
            template, {'compiler': self.compiler}
        )
    
    def _generate_eightfold_test(self) -> str:
        """Generate Eightfold Path test"""
        template = '''
#include "owl_ontology.h"
#include <stdio.h>
#include <assert.h>

int test_eightfold_context() {
    printf("Testing Eightfold Path context...\\n");
    
    eightfold_context_t* ctx = eightfold_create_context();
    assert(ctx != NULL);
    
    // Test adding instances for each stage
    {% for class in compiler.classes.values() %}
    {% if class.eightfold_mapping %}
    {
        {{ class.label|snake_case }}_t* obj = {{ class.label|snake_case }}_create();
        assert(obj != NULL);
        assert(eightfold_add_instance(ctx, (owl_object_t*)obj));
        printf("✓ Added {{ class.label }} to stage {{ class.eightfold_mapping.stage }}\\n");
    }
    {% endif %}
    {% endfor %}
    
    // Test stage execution
    for (int stage = 0; stage < EIGHTFOLD_STAGE_COUNT; stage++) {
        if (eightfold_execute_stage(ctx, (eightfold_stage_t)stage)) {
            printf("✓ Stage %d execution passed\\n", stage);
        }
    }
    
    eightfold_destroy_context(ctx);
    return 1;
}

int main() {
    printf("Running Eightfold Path tests...\\n");
    
    if (!test_eightfold_context()) {
        printf("❌ Eightfold context tests failed\\n");
        return 1;
    }
    
    printf("✅ All Eightfold tests passed\\n");
    return 0;
}
'''
        return self.compiler.template_manager.render_from_string(
            template, {'compiler': self.compiler}
        )
    
    def _generate_reasoning_test(self) -> str:
        """Generate reasoning test"""
        template = '''
#include "owl_ontology.h"
#include <stdio.h>
#include <assert.h>

int test_reasoning_rules() {
    printf("Testing reasoning rules...\\n");
    
    // Test that reasoning rules are loaded
    printf("Number of reasoning rules: %zu\\n", sizeof(g_reasoning_rules)/sizeof(g_reasoning_rules[0]));
    
    // Test reasoning application
    {% for class in compiler.classes.values() %}
    {
        {{ class.label|snake_case }}_t* obj = {{ class.label|snake_case }}_create();
        assert(obj != NULL);
        
        // Apply reasoning - should not fail
        owl_apply_reasoning((owl_object_t*)obj);
        
        // Object should still be valid after reasoning
        assert({{ class.label|snake_case }}_validate(obj));
        
        {{ class.label|snake_case }}_destroy(obj);
        printf("✓ {{ class.label }} reasoning test passed\\n");
    }
    {% endfor %}
    
    return 1;
}

int main() {
    printf("Running reasoning tests...\\n");
    
    if (!test_reasoning_rules()) {
        printf("❌ Reasoning tests failed\\n");
        return 1;
    }
    
    printf("✅ All reasoning tests passed\\n");
    return 0;
}
'''
        return self.compiler.template_manager.render_from_string(
            template, {'compiler': self.compiler}
        )
    
    def _compile_test_files(self, test_files: List[Path]) -> List[Path]:
        """Compile test files"""
        test_executables = []
        
        for test_file in test_files:
            executable = test_file.with_suffix('')
            
            # Find the library
            lib_file = next((f for f in self.result.output_files if f.name.startswith('lib')), None)
            header_file = next((f for f in self.result.output_files if f.suffix == '.h'), None)
            
            compile_cmd = [
                self.config.c_compiler,
                str(test_file),
                "-I", str(self.config.output_dir),
                "-o", str(executable)
            ]
            
            if lib_file:
                compile_cmd.extend(["-L", str(lib_file.parent), "-l", lib_file.stem[3:]])  # Remove 'lib' prefix
            
            result = subprocess.run(compile_cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                test_executables.append(executable)
            else:
                self.logger.warning(f"Failed to compile test {test_file}: {result.stderr}")
        
        return test_executables
    
    def _run_test_executables(self, test_executables: List[Path]) -> List[Dict[str, Any]]:
        """Run test executables"""
        results = []
        
        for executable in test_executables:
            result = subprocess.run([str(executable)], capture_output=True, text=True)
            
            test_result = {
                'name': executable.stem,
                'passed': result.returncode == 0,
                'stdout': result.stdout,
                'stderr': result.stderr,
                'return_code': result.returncode
            }
            
            results.append(test_result)
            
            if test_result['passed']:
                self.logger.info(f"✅ Test {executable.stem} passed")
            else:
                self.logger.error(f"❌ Test {executable.stem} failed")
        
        return results
    
    def _calculate_test_coverage(self, test_results: List[Dict[str, Any]]) -> float:
        """Calculate test coverage percentage"""
        if not test_results:
            return 0.0
        
        passed_tests = sum(1 for r in test_results if r['passed'])
        return (passed_tests / len(test_results)) * 100
    
    def _run_packaging(self) -> None:
        """Create deployment package"""
        with self._stage_context(CompilationStage.PACKAGING) as stage:
            self.logger.info(f"Creating {self.config.package_format} package")
            
            package_name = f"owl_ontology_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            
            if self.config.package_format == "tar.gz":
                package_file = self.config.output_dir / f"{package_name}.tar.gz"
                self._create_tar_package(package_file)
            elif self.config.package_format == "zip":
                package_file = self.config.output_dir / f"{package_name}.zip"
                self._create_zip_package(package_file)
            else:
                raise CompilationError(
                    CompilationStage.PACKAGING,
                    f"Unsupported package format: {self.config.package_format}"
                )
            
            stage['outputs'].append(str(package_file))
            self.result.output_files.append(package_file)
            stage['metrics']['package_size'] = package_file.stat().st_size
    
    def _create_tar_package(self, package_file: Path) -> None:
        """Create tar.gz package"""
        import tarfile
        
        with tarfile.open(package_file, "w:gz") as tar:
            for output_file in self.result.output_files:
                if output_file != package_file:  # Don't include the package itself
                    tar.add(output_file, arcname=output_file.name)
    
    def _create_zip_package(self, package_file: Path) -> None:
        """Create zip package"""
        import zipfile
        
        with zipfile.ZipFile(package_file, 'w', zipfile.ZIP_DEFLATED) as zip_file:
            for output_file in self.result.output_files:
                if output_file != package_file:  # Don't include the package itself
                    zip_file.write(output_file, output_file.name)
    
    def _cleanup(self) -> None:
        """Clean up temporary resources"""
        self.logger.info("Cleaning up temporary resources")
        
        for temp_dir in self.temp_dirs:
            if temp_dir.exists():
                shutil.rmtree(temp_dir)
        
        # Final metrics
        self.result.metrics.update({
            'total_duration': self.result.duration,
            'total_output_files': len(self.result.output_files),
            'total_output_size': sum(f.stat().st_size for f in self.result.output_files if f.exists()),
            'successful_stages': sum(1 for stage_result in self.result.stage_results.values() 
                                   if stage_result['success']),
            'total_stages': len(self.result.stage_results)
        })

def create_config_from_file(config_file: Path) -> CompilationConfig:
    """Create configuration from YAML/JSON file"""
    if config_file.suffix.lower() in ['.yaml', '.yml']:
        import yaml
        with open(config_file) as f:
            data = yaml.safe_load(f)
    else:
        with open(config_file) as f:
            data = json.load(f)
    
    # Convert paths
    if 'input_files' in data:
        data['input_files'] = [Path(f) for f in data['input_files']]
    if 'output_dir' in data:
        data['output_dir'] = Path(data['output_dir'])
    if 'template_dir' in data:
        data['template_dir'] = Path(data['template_dir'])
    if 'log_file' in data:
        data['log_file'] = Path(data['log_file'])
    if 'include_paths' in data:
        data['include_paths'] = [Path(p) for p in data['include_paths']]
    if 'library_paths' in data:
        data['library_paths'] = [Path(p) for p in data['library_paths']]
    
    # Convert enums
    if 'log_level' in data:
        data['log_level'] = LogLevel(data['log_level'])
    
    return CompilationConfig(**data)

def main():
    """Main entry point for the lifecycle compiler"""
    import argparse
    
    parser = argparse.ArgumentParser(description="OWL AOT Compiler Lifecycle")
    parser.add_argument("input_files", nargs="+", type=Path, help="Input OWL/TTL files")
    parser.add_argument("--config", type=Path, help="Configuration file (YAML/JSON)")
    parser.add_argument("--output-dir", type=Path, default=Path("./output"), help="Output directory")
    parser.add_argument("--optimization-level", type=int, choices=[0, 1, 2, 3], default=2, help="Optimization level")
    parser.add_argument("--no-inference", action="store_true", help="Disable inference")
    parser.add_argument("--no-tests", action="store_true", help="Disable testing")
    parser.add_argument("--package", choices=["tar.gz", "zip"], help="Create package")
    parser.add_argument("--parallel", type=int, default=4, help="Number of parallel workers")
    parser.add_argument("--log-level", choices=["debug", "info", "warning", "error"], default="info")
    parser.add_argument("--log-file", type=Path, help="Log file path")
    
    args = parser.parse_args()
    
    # Create configuration
    if args.config:
        config = create_config_from_file(args.config)
        # Override with command line arguments
        config.input_files = args.input_files
        if args.output_dir != Path("./output"):
            config.output_dir = args.output_dir
    else:
        config = CompilationConfig(
            input_files=args.input_files,
            output_dir=args.output_dir,
            optimization_level=args.optimization_level,
            inference_enabled=not args.no_inference,
            enable_tests=not args.no_tests,
            create_package=bool(args.package),
            package_format=args.package or "tar.gz",
            max_workers=args.parallel,
            log_level=LogLevel(args.log_level),
            log_file=args.log_file
        )
    
    # Run compilation lifecycle
    lifecycle = OWLCompilerLifecycle(config)
    result = lifecycle.run()
    
    # Print results
    print(f"\n{'='*60}")
    print(f"OWL AOT Compilation {'SUCCESS' if result.success else 'FAILED'}")
    print(f"{'='*60}")
    print(f"Duration: {result.duration:.2f} seconds")
    print(f"Output files: {len(result.output_files)}")
    print(f"Total size: {result.metrics.get('total_output_size', 0):,} bytes")
    
    if result.errors:
        print(f"\nErrors ({len(result.errors)}):")
        for error in result.errors:
            print(f"  ❌ {error}")
    
    if result.warnings:
        print(f"\nWarnings ({len(result.warnings)}):")
        for warning in result.warnings:
            print(f"  ⚠️  {warning}")
    
    print(f"\nStage Results:")
    for stage, stage_result in result.stage_results.items():
        status = "✅" if stage_result['success'] else "❌"
        duration = stage_result.get('duration', 0)
        print(f"  {status} {stage.value}: {duration:.2f}s")
    
    if result.output_files:
        print(f"\nOutput Files:")
        for output_file in result.output_files:
            if output_file.exists():
                size = output_file.stat().st_size
                print(f"  📄 {output_file} ({size:,} bytes)")
    
    return 0 if result.success else 1

if __name__ == "__main__":
    sys.exit(main())
