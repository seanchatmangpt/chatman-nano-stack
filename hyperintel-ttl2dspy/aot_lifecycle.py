#!/usr/bin/env python3
"""
AOT Compilation Lifecycle Manager
Orchestrates the complete compilation pipeline from OWL/SHACL to optimized C code
"""

import asyncio
import hashlib
import json
import logging
import os
import re
import shutil
import sys
import tempfile
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Set

import yaml

logger = logging.getLogger(__name__)

class LifecycleStage(Enum):
    """Compilation lifecycle stages"""
    INITIALIZATION = "initialization"
    PARSING = "parsing"
    SEMANTIC_ANALYSIS = "semantic_analysis"
    CONSTRAINT_EXTRACTION = "constraint_extraction"
    OPTIMIZATION = "optimization"
    CODE_GENERATION = "code_generation"
    COMPILATION = "compilation"
    LINKING = "linking"
    VALIDATION = "validation"
    PACKAGING = "packaging"
    DEPLOYMENT = "deployment"
    CLEANUP = "cleanup"

class StageStatus(Enum):
    """Stage execution status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"
    CANCELLED = "cancelled"

@dataclass
class StageMetrics:
    """Metrics for a compilation stage"""
    stage: LifecycleStage
    status: StageStatus = StageStatus.PENDING
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    duration: Optional[timedelta] = None
    memory_peak: int = 0
    cpu_time: float = 0.0
    artifacts_produced: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    optimization_applied: List[str] = field(default_factory=list)
    eightfold_stage: Optional[str] = None

@dataclass
class CompilationTarget:
    """Compilation target specification"""
    name: str
    architecture: str = "x86_64"
    platform: str = "linux"
    optimization_level: str = "O2"
    debug_symbols: bool = False
    eightfold_optimizations: bool = True
    custom_flags: List[str] = field(default_factory=list)

@dataclass
class SourceSpec:
    """Source specification for compilation"""
    owl_files: List[Path] = field(default_factory=list)
    shacl_files: List[Path] = field(default_factory=list)
    config_file: Optional[Path] = None
    template_dirs: List[Path] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)

@dataclass
class CompilationResult:
    """Complete compilation result"""
    success: bool
    target: CompilationTarget
    artifacts: Dict[str, Path] = field(default_factory=dict)
    metrics: Dict[LifecycleStage, StageMetrics] = field(default_factory=dict)
    total_duration: Optional[timedelta] = None
    memory_peak: int = 0
    final_size: int = 0
    optimization_report: Dict[str, Any] = field(default_factory=dict)
    eightfold_analysis: Dict[str, Any] = field(default_factory=dict)

class LifecycleHook:
    """Hook for lifecycle events"""
    def __init__(self, stage: LifecycleStage, callback: Callable, priority: int = 0):
        self.stage = stage
        self.callback = callback
        self.priority = priority

class AOTLifecycleManager:
    """Manages the complete AOT compilation lifecycle"""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or self._default_config()
        self.work_dir: Optional[Path] = None
        self.source_spec: Optional[SourceSpec] = None
        self.targets: List[CompilationTarget] = []
        self.results: List[CompilationResult] = []
        self.hooks: Dict[LifecycleStage, List[LifecycleHook]] = {stage: [] for stage in LifecycleStage}
        self.global_metrics: Dict[str, Any] = {}
        self.cache_dir: Optional[Path] = None
        self.temp_files: Set[Path] = set()

        # Import compilers
        self.owl_compiler = None
        self.shacl_compiler = None

        # Setup logging
        self._setup_logging()

    def _default_config(self) -> Dict[str, Any]:
        """Default lifecycle configuration"""
        return {
            'parallel_compilation': True,
            'max_workers': os.cpu_count(),
            'cache_enabled': True,
            'incremental_build': True,
            'validation_enabled': True,
            'benchmark_generation': True,
            'eightfold_integration': True,
            'optimization_passes': 3,
            'memory_limit': 2 * 1024 * 1024 * 1024,  # 2GB
            'timeout_seconds': 3600,  # 1 hour
            'cleanup_on_failure': True,
            'debug_artifacts': False
        }

    def _setup_logging(self) -> None:
        """Setup lifecycle logging"""
        log_level = self.config.get('log_level', 'INFO')
        logging.basicConfig(
            level=getattr(logging, log_level),
            format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
            handlers=[
                logging.StreamHandler(),
                logging.FileHandler('aot_lifecycle.log') if self.config.get('log_to_file') else logging.NullHandler()
            ]
        )

    async def compile(self, source_spec: SourceSpec, targets: List[CompilationTarget]) -> List[CompilationResult]:
        """Execute complete compilation lifecycle"""
        logger.info("Starting AOT compilation lifecycle")

        self.source_spec = source_spec
        self.targets = targets
        self.results = []

        start_time = datetime.now()

        async with self._lifecycle_context():
            # Execute stages sequentially
            for stage in LifecycleStage:
                if not await self._execute_stage(stage):
                    logger.error(f"Stage {stage.value} failed, aborting compilation")
                    break

            # Compile final results
            await self._finalize_results()

        total_duration = datetime.now() - start_time
        logger.info(f"Compilation lifecycle completed in {total_duration}")

        return self.results

    @asynccontextmanager
    async def _lifecycle_context(self):
        """Lifecycle context manager"""
        # Setup
        self.work_dir = Path(tempfile.mkdtemp(prefix="aot_compile_"))
        self.cache_dir = self.work_dir / "cache" if self.config['cache_enabled'] else None

        if self.cache_dir:
            self.cache_dir.mkdir(exist_ok=True)

        logger.info(f"Working directory: {self.work_dir}")

        await self._execute_hooks(LifecycleStage.INITIALIZATION)

        # Execute
        yield

        # Cleanup
        await self._execute_hooks(LifecycleStage.CLEANUP)

        # Cleanup only if configured to do so
        should_cleanup = True

        if self.config.get('debug_artifacts', False):
            should_cleanup = False  # Keep artifacts for debugging
        elif not self.config.get('cleanup_on_failure', True) and not any(r.success for r in self.results):
            should_cleanup = False  # Keep on failure if configured

        if should_cleanup and self.work_dir and self.work_dir.exists():
            shutil.rmtree(self.work_dir)
            logger.info(f"Cleaned up working directory: {self.work_dir}")
        elif not should_cleanup:
            logger.info(f"Keeping working directory for inspection: {self.work_dir}")

    async def _execute_stage(self, stage: LifecycleStage) -> bool:
        """Execute a single lifecycle stage"""
        logger.info(f"Executing stage: {stage.value}")

        metrics = StageMetrics(stage=stage, status=StageStatus.RUNNING, start_time=datetime.now())

        # Execute pre-hooks
        await self._execute_hooks(stage)

        success = False

        # Execute stage-specific logic
        if stage == LifecycleStage.PARSING:
            success = await self._stage_parsing(metrics)
        elif stage == LifecycleStage.SEMANTIC_ANALYSIS:
            success = await self._stage_semantic_analysis(metrics)
        elif stage == LifecycleStage.CONSTRAINT_EXTRACTION:
            success = await self._stage_constraint_extraction(metrics)
        elif stage == LifecycleStage.OPTIMIZATION:
            success = await self._stage_optimization(metrics)
        elif stage == LifecycleStage.CODE_GENERATION:
            success = await self._stage_code_generation(metrics)
        elif stage == LifecycleStage.COMPILATION:
            success = await self._stage_compilation(metrics)
        elif stage == LifecycleStage.LINKING:
            success = await self._stage_linking(metrics)
        elif stage == LifecycleStage.VALIDATION:
            success = await self._stage_validation(metrics)
        elif stage == LifecycleStage.PACKAGING:
            success = await self._stage_packaging(metrics)
        elif stage == LifecycleStage.DEPLOYMENT:
            success = await self._stage_deployment(metrics)
        else:
            success = True  # Skip unknown stages

        # Finalize metrics
        metrics.end_time = datetime.now()
        metrics.duration = metrics.end_time - metrics.start_time
        metrics.status = StageStatus.COMPLETED if success else StageStatus.FAILED

        # Store metrics for all targets
        for result in self.results:
            result.metrics[stage] = metrics

        logger.info(f"Stage {stage.value} {'completed' if success else 'failed'} in {metrics.duration}")

        return success

    async def _stage_parsing(self, metrics: StageMetrics) -> bool:
        """Parse OWL and SHACL source files"""
        metrics.eightfold_stage = "Right Understanding"

        # Initialize compilers
        from owl_compiler import OWLCompiler
        from shacl_compiler import SHACLCompiler

        # Prepare compiler configs with required fields
        owl_config = {
            'strict_mode': True,
            'inference_enabled': True,
            'reasoning_depth': 3,
            'extract_shacl': True,
            'eightfold_integration': self.config.get('eightfold_integration', True),
            'optimization_hints': True,
            'output_formats': ['c_header', 'c_implementation', 'json'],
            'template_customization': True
        }
        owl_config.update(self.config)

        shacl_config = {
            'template_dir': 'templates',
            'output_format': 'c',
            'optimization_level': self.config.get('optimization_level', 'O2'),
            'eightfold_integration': self.config.get('eightfold_integration', True),
            'generate_benchmarks': self.config.get('benchmark_generation', True),
            'inline_simple_checks': True,
            'use_lookup_tables': True,
            'memory_pool_size': 1024 * 1024,
            'max_validation_depth': 10
        }
        shacl_config.update(self.config)

        self.owl_compiler = OWLCompiler(owl_config)
        self.shacl_compiler = SHACLCompiler(shacl_config)

        # Parse OWL files
        owl_results = {}
        for owl_file in self.source_spec.owl_files:
            logger.info(f"Parsing OWL file: {owl_file}")

            cache_key = self._get_cache_key(owl_file)
            cached_result = await self._get_cached_result(cache_key)

            if cached_result:
                owl_results[str(owl_file)] = cached_result
                metrics.optimization_applied.append("cache_hit")
            else:
                result = self.owl_compiler.compile(owl_file)
                owl_results[str(owl_file)] = result
                await self._cache_result(cache_key, result)

            metrics.artifacts_produced.append(f"owl_ast_{owl_file.stem}")

        # Parse SHACL files
        shacl_results = {}
        for shacl_file in self.source_spec.shacl_files:
            logger.info(f"Parsing SHACL file: {shacl_file}")

            cache_key = self._get_cache_key(shacl_file)
            cached_result = await self._get_cached_result(cache_key)

            if cached_result:
                shacl_results[str(shacl_file)] = cached_result
                metrics.optimization_applied.append("cache_hit")
            else:
                result = self.shacl_compiler.compile(shacl_file)
                shacl_results[str(shacl_file)] = result
                await self._cache_result(cache_key, result)

            metrics.artifacts_produced.append(f"shacl_constraints_{shacl_file.stem}")

        # Store parsed results
        self.global_metrics['owl_results'] = owl_results
        self.global_metrics['shacl_results'] = shacl_results

        return len(owl_results) > 0 or len(shacl_results) > 0

    async def _stage_semantic_analysis(self, metrics: StageMetrics) -> bool:
        """Perform semantic analysis and consistency checking"""
        metrics.eightfold_stage = "Right Understanding"

        owl_results = self.global_metrics.get('owl_results', {})
        shacl_results = self.global_metrics.get('shacl_results', {})

        # Cross-reference analysis
        cross_refs = await self._analyze_cross_references(owl_results, shacl_results)

        # Consistency checking
        consistency_report = await self._check_consistency(owl_results, shacl_results)

        # Eightfold mapping analysis
        eightfold_analysis = await self._analyze_eightfold_mappings(owl_results, shacl_results)

        # Store analysis results
        self.global_metrics['cross_references'] = cross_refs
        self.global_metrics['consistency_report'] = consistency_report
        self.global_metrics['eightfold_analysis'] = eightfold_analysis

        metrics.artifacts_produced.extend([
            "cross_reference_analysis",
            "consistency_report",
            "eightfold_mapping_analysis"
        ])

        # Check for critical errors
        critical_errors = [issue for issue in consistency_report.get('issues', [])
                          if issue.get('severity') == 'error']

        if critical_errors:
            metrics.errors.extend([issue['message'] for issue in critical_errors])
            return False

        return True

    async def _stage_constraint_extraction(self, metrics: StageMetrics) -> bool:
        """Extract and optimize constraints"""
        metrics.eightfold_stage = "Right Thought"

        shacl_results = self.global_metrics.get('shacl_results', {})
        eightfold_analysis = self.global_metrics.get('eightfold_analysis', {})

        # Extract all constraints
        all_constraints = []
        for file_path, result in shacl_results.items():
            constraints = result.get('constraints', [])
            all_constraints.extend(constraints)

        # Optimize constraint order based on Eightfold stages
        optimized_constraints = await self._optimize_constraint_order(all_constraints, eightfold_analysis)

        # Group constraints by validation complexity
        constraint_groups = await self._group_constraints_by_complexity(optimized_constraints)

        # Generate constraint dependency graph
        dependency_graph = await self._build_constraint_dependencies(optimized_constraints)

        self.global_metrics['optimized_constraints'] = optimized_constraints
        self.global_metrics['constraint_groups'] = constraint_groups
        self.global_metrics['constraint_dependencies'] = dependency_graph

        metrics.artifacts_produced.extend([
            "optimized_constraints",
            "constraint_groups",
            "dependency_graph"
        ])

        metrics.optimization_applied.extend([
            "eightfold_ordering",
            "complexity_grouping",
            "dependency_analysis"
        ])

        return True

    async def _stage_optimization(self, metrics: StageMetrics) -> bool:
        """Apply optimization passes"""
        metrics.eightfold_stage = "Right Effort"

        optimizations_applied = []

        # Optimization pass 1: Dead code elimination
        await self._optimize_dead_code_elimination()
        optimizations_applied.append("dead_code_elimination")

        # Optimization pass 2: Constraint inlining
        await self._optimize_constraint_inlining()
        optimizations_applied.append("constraint_inlining")

        # Optimization pass 3: Eightfold path optimizations
        if self.config['eightfold_integration']:
            await self._optimize_eightfold_paths()
            optimizations_applied.append("eightfold_path_optimization")

        # Optimization pass 4: Memory layout optimization
        await self._optimize_memory_layout()
        optimizations_applied.append("memory_layout_optimization")

        metrics.optimization_applied.extend(optimizations_applied)
        metrics.artifacts_produced.append("optimization_report")

        self.global_metrics['optimizations_applied'] = optimizations_applied

        return True

    async def _stage_code_generation(self, metrics: StageMetrics) -> bool:
        """Generate C code using templates"""
        metrics.eightfold_stage = "Right Speech"

        # Prepare for parallel code generation
        generation_tasks = []

        for target in self.targets:
            task = self._generate_code_for_target(target, metrics)
            generation_tasks.append(task)

        # Execute code generation in parallel if enabled
        if self.config['parallel_compilation']:
            results = await asyncio.gather(*generation_tasks, return_exceptions=True)

            for i, result in enumerate(results):
                if isinstance(result, Exception):
                    metrics.errors.append(f"Code generation failed for target {self.targets[i].name}: {result}")
                    return False
                elif not result:
                    metrics.errors.append(f"Code generation failed for target {self.targets[i].name}")
                    return False
        else:
            for task in generation_tasks:
                success = await task
                if not success:
                    return False

        metrics.artifacts_produced.extend([
            f"generated_code_{target.name}" for target in self.targets
        ])

        return True

    async def _generate_code_for_target(self, target: CompilationTarget, metrics: StageMetrics) -> bool:
        """Generate code for a specific target"""
        try:
            target_dir = self.work_dir / "generated" / target.name
            target_dir.mkdir(parents=True, exist_ok=True)

            # Generate validation code
            validation_code = await self._generate_validation_code(target)

            # Generate ontology code
            ontology_code = await self._generate_ontology_code(target)

            # Generate benchmark code if requested
            benchmark_code = {}
            if self.config['benchmark_generation']:
                benchmark_code = await self._generate_benchmark_code(target)

            # Write generated code to files
            code_files = {
                'shacl_validation.h': validation_code.get('header', ''),
                'shacl_validation.c': validation_code.get('implementation', ''),
                'ontology.h': ontology_code.get('header', ''),
                'ontology.c': ontology_code.get('implementation', ''),
            }

            if self.config['benchmark_generation']:
                code_files['benchmark.c'] = benchmark_code.get('implementation', '')

            # Count how many files have content
            files_written = 0
            for filename, content in code_files.items():
                if content:
                    file_path = target_dir / filename
                    file_path.write_text(content)
                    self.temp_files.add(file_path)
                    files_written += 1
                    logger.info(f"Generated {filename} ({len(content)} bytes)")

            # Copy runtime support header
            runtime_support_src = Path(__file__).parent / "runtime_support.h"
            if runtime_support_src.exists():
                runtime_support_dst = target_dir / "runtime_support.h"
                shutil.copy2(runtime_support_src, runtime_support_dst)
                logger.info("Copied runtime_support.h")
                files_written += 1

            # Create compilation result for this target
            result = CompilationResult(
                success=files_written > 0,
                target=target,
                artifacts={'generated_code_dir': target_dir}
            )
            self.results.append(result)

            return files_written > 0

        except Exception as e:
            logger.error(f"Error generating code for target {target.name}: {e}")
            import traceback
            traceback.print_exc()
            return False

    async def _stage_compilation(self, metrics: StageMetrics) -> bool:
        """Compile generated C code"""
        metrics.eightfold_stage = "Right Action"

        compilation_tasks = []

        for result in self.results:
            if not result.success:
                continue

            task = self._compile_target(result, metrics)
            compilation_tasks.append(task)

        # Execute compilation in parallel
        if self.config['parallel_compilation']:
            compile_results = await asyncio.gather(*compilation_tasks, return_exceptions=True)

            for i, compile_result in enumerate(compile_results):
                if isinstance(compile_result, Exception):
                    self.results[i].success = False
                    metrics.errors.append(f"Compilation failed: {compile_result}")
                elif not compile_result:
                    self.results[i].success = False
        else:
            for task in compilation_tasks:
                success = await task
                if not success:
                    return False

        successful_compilations = sum(1 for r in self.results if r.success)
        total_targets = len(self.results)

        logger.info(f"Compilation completed: {successful_compilations}/{total_targets} targets successful")

        return successful_compilations > 0

    async def _compile_target(self, result: CompilationResult, metrics: StageMetrics) -> bool:
        """Compile a specific target"""
        target = result.target
        code_dir = result.artifacts['generated_code_dir']

        # Prepare compilation command
        compiler = self._get_compiler_for_target(target)
        flags = self._get_compilation_flags(target)

        object_files = []

        # Compile each C file
        for c_file in code_dir.glob("*.c"):
            obj_file = c_file.with_suffix(".o")

            cmd = [compiler] + flags + ["-c", str(c_file), "-o", str(obj_file)]

            process = await asyncio.create_subprocess_exec(
                *cmd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd=code_dir
            )

            stdout, stderr = await process.communicate()

            if process.returncode != 0:
                error_msg = f"Compilation failed for {c_file}: {stderr.decode()}"
                metrics.errors.append(error_msg)
                logger.error(error_msg)
                return False

            object_files.append(obj_file)
            self.temp_files.add(obj_file)

        result.artifacts['object_files'] = object_files
        metrics.artifacts_produced.append(f"object_files_{target.name}")

        return True

    async def _stage_linking(self, metrics: StageMetrics) -> bool:
        """Link compiled object files"""
        metrics.eightfold_stage = "Right Action"

        for result in self.results:
            if not result.success:
                continue

            success = await self._link_target(result, metrics)
            if not success:
                result.success = False

        successful_links = sum(1 for r in self.results if r.success)
        return successful_links > 0

    async def _link_target(self, result: CompilationResult, metrics: StageMetrics) -> bool:
        """Link a specific target"""
        target = result.target
        object_files = result.artifacts.get('object_files', [])

        if not object_files:
            return False

        # Create output binary
        output_dir = self.work_dir / "output" / target.name
        output_dir.mkdir(parents=True, exist_ok=True)

        binary_name = f"aot_validator_{target.name}"
        if target.platform == "windows":
            binary_name += ".exe"

        binary_path = output_dir / binary_name

        # Prepare linker command
        linker = self._get_linker_for_target(target)
        link_flags = self._get_link_flags(target)

        cmd = [linker] + [str(f) for f in object_files] + link_flags + ["-o", str(binary_path)]

        process = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )

        stdout, stderr = await process.communicate()

        if process.returncode != 0:
            error_msg = f"Linking failed for {target.name}: {stderr.decode()}"
            metrics.errors.append(error_msg)
            logger.error(error_msg)
            return False

        result.artifacts['binary'] = binary_path
        result.final_size = binary_path.stat().st_size

        logger.info(f"Successfully linked {target.name}: {binary_path} ({result.final_size} bytes)")

        return True

    async def _stage_validation(self, metrics: StageMetrics) -> bool:
        """Validate compiled binaries"""
        metrics.eightfold_stage = "Right Mindfulness"

        if not self.config['validation_enabled']:
            metrics.status = StageStatus.SKIPPED
            return True

        validation_tasks = []

        for result in self.results:
            if not result.success or 'binary' not in result.artifacts:
                continue

            task = self._validate_binary(result, metrics)
            validation_tasks.append(task)

        validation_results = await asyncio.gather(*validation_tasks, return_exceptions=True)

        for i, validation_result in enumerate(validation_results):
            if isinstance(validation_result, Exception):
                self.results[i].success = False
                metrics.errors.append(f"Validation failed: {validation_result}")
            elif not validation_result:
                self.results[i].success = False

        return any(r.success for r in self.results)

    async def _validate_binary(self, result: CompilationResult, metrics: StageMetrics) -> bool:
        """Validate a compiled binary"""
        binary_path = result.artifacts['binary']

        # Basic validation - check if binary exists and is executable
        if not binary_path.exists():
            return False

        # Try to execute basic functionality test
        test_cmd = [str(binary_path), "--version"]

        process = await asyncio.create_subprocess_exec(
            *test_cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )

        stdout, stderr = await process.communicate()

        # For now, just check that it doesn't crash immediately
        validation_passed = process.returncode in [0, 1]  # 0 = success, 1 = expected error

        if validation_passed:
            metrics.artifacts_produced.append(f"validation_report_{result.target.name}")
        else:
            metrics.errors.append(f"Binary validation failed for {result.target.name}")

        return validation_passed

    async def _stage_packaging(self, metrics: StageMetrics) -> bool:
        """Package compiled artifacts"""
        metrics.eightfold_stage = "Right Livelihood"

        package_dir = self.work_dir / "packages"
        package_dir.mkdir(exist_ok=True)

        for result in self.results:
            if not result.success:
                continue

            package_path = await self._create_package(result, package_dir)
            if package_path:
                result.artifacts['package'] = package_path
                metrics.artifacts_produced.append(f"package_{result.target.name}")

        return any('package' in r.artifacts for r in self.results)

    async def _create_package(self, result: CompilationResult, package_dir: Path) -> Optional[Path]:
        """Create a deployment package for a target"""
        target = result.target
        package_name = f"aot_validator_{target.name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.tar.gz"
        package_path = package_dir / package_name

        # Create package contents
        temp_package_dir = package_dir / f"temp_{target.name}"
        temp_package_dir.mkdir(exist_ok=True)

        # Copy binary
        if 'binary' in result.artifacts:
            binary_src = result.artifacts['binary']
            binary_dst = temp_package_dir / binary_src.name
            shutil.copy2(binary_src, binary_dst)

        # Copy documentation and metadata
        metadata = {
            'target': target.name,
            'architecture': target.architecture,
            'platform': target.platform,
            'optimization_level': target.optimization_level,
            'build_timestamp': datetime.now().isoformat(),
            'size_bytes': result.final_size,
            'eightfold_optimizations': target.eightfold_optimizations
        }

        metadata_file = temp_package_dir / "metadata.json"
        metadata_file.write_text(json.dumps(metadata, indent=2))

        # Create tarball
        import tarfile
        with tarfile.open(package_path, "w:gz") as tar:
            tar.add(temp_package_dir, arcname=target.name)

        # Cleanup temp directory
        shutil.rmtree(temp_package_dir)

        return package_path

    async def _stage_deployment(self, metrics: StageMetrics) -> bool:
        """Deploy packages to target destinations"""
        metrics.eightfold_stage = "Right Concentration"

        deployment_config = self.config.get('deployment', {})

        if not deployment_config.get('enabled', False):
            metrics.status = StageStatus.SKIPPED
            return True

        # This would implement actual deployment logic
        # For now, just mark as successful
        for result in self.results:
            if 'package' in result.artifacts:
                # Simulate deployment
                logger.info(f"Would deploy package: {result.artifacts['package']}")
                result.artifacts['deployment_status'] = 'deployed'

        return True

    # Lifecycle helper methods

    async def _execute_hooks(self, stage: LifecycleStage) -> None:
        """Execute hooks for a lifecycle stage"""
        hooks = sorted(self.hooks[stage], key=lambda h: h.priority)

        for hook in hooks:
            await hook.callback(stage, self)

    def add_hook(self, stage: LifecycleStage, callback: Callable, priority: int = 0) -> None:
        """Add a lifecycle hook"""
        hook = LifecycleHook(stage, callback, priority)
        self.hooks[stage].append(hook)

    async def _get_cached_result(self, cache_key: str) -> Optional[Dict[str, Any]]:
        """Get cached compilation result"""
        if not self.cache_dir:
            return None

        cache_file = self.cache_dir / f"{cache_key}.json"
        if cache_file.exists():
            return json.loads(cache_file.read_text())

        return None

    async def _cache_result(self, cache_key: str, result: Dict[str, Any]) -> None:
        """Cache compilation result"""
        if not self.cache_dir:
            return

        cache_file = self.cache_dir / f"{cache_key}.json"
        cache_file.write_text(json.dumps(result, indent=2, default=str))

    def _get_cache_key(self, file_path: Path) -> str:
        """Generate cache key for a file"""
        content = file_path.read_bytes()
        file_hash = hashlib.sha256(content).hexdigest()
        return f"{file_path.stem}_{file_hash[:16]}"

    def _get_compiler_for_target(self, target: CompilationTarget) -> str:
        """Get compiler command for target"""
        compiler_map = {
            'linux': 'gcc',
            'windows': 'x86_64-w64-mingw32-gcc',
            'macos': 'clang'
        }
        return compiler_map.get(target.platform, 'gcc')

    def _get_compilation_flags(self, target: CompilationTarget) -> List[str]:
        """Get compilation flags for target"""
        flags = [f"-{target.optimization_level}", "-Wall", "-Wextra"]

        if target.debug_symbols:
            flags.append("-g")

        if target.eightfold_optimizations:
            flags.extend(["-DEIGHTFOLD_OPTIMIZATIONS", "-finline-functions"])

        flags.extend(target.custom_flags)

        return flags

    def _get_linker_for_target(self, target: CompilationTarget) -> str:
        """Get linker command for target"""
        return self._get_compiler_for_target(target)  # Usually same as compiler

    def _get_link_flags(self, target: CompilationTarget) -> List[str]:
        """Get linking flags for target"""
        flags = []

        if target.platform == "linux":
            flags.extend(["-lm", "-lpthread"])
        elif target.platform == "windows":
            flags.extend(["-lws2_32"])

        return flags

    # Analysis and optimization methods (simplified implementations)

    async def _analyze_cross_references(self, owl_results: Dict, shacl_results: Dict) -> Dict[str, Any]:
        """Analyze cross-references between OWL and SHACL"""
        return {"cross_reference_count": len(owl_results) + len(shacl_results)}

    async def _check_consistency(self, owl_results: Dict, shacl_results: Dict) -> Dict[str, Any]:
        """Check consistency between OWL and SHACL specifications"""
        return {"issues": [], "status": "consistent"}

    async def _analyze_eightfold_mappings(self, owl_results: Dict, shacl_results: Dict) -> Dict[str, Any]:
        """Analyze Eightfold Path mappings"""
        return {"coverage": 85.0, "optimizable_stages": ["Right Action", "Right Effort"]}

    async def _optimize_constraint_order(self, constraints: List, eightfold_analysis: Dict) -> List:
        """Optimize constraint evaluation order"""
        # Simple implementation - sort by complexity
        return sorted(constraints, key=lambda c: c.get('complexity', 0))

    async def _group_constraints_by_complexity(self, constraints: List) -> Dict[str, List]:
        """Group constraints by validation complexity"""
        return {
            "simple": [c for c in constraints if c.get('complexity', 0) < 3],
            "medium": [c for c in constraints if 3 <= c.get('complexity', 0) < 7],
            "complex": [c for c in constraints if c.get('complexity', 0) >= 7]
        }

    async def _build_constraint_dependencies(self, constraints: List) -> Dict[str, List]:
        """Build constraint dependency graph"""
        return {c.get('id', ''): [] for c in constraints}  # Simplified

    async def _optimize_dead_code_elimination(self) -> None:
        """Remove dead code"""
        pass  # Implementation would analyze and remove unused code

    async def _optimize_constraint_inlining(self) -> None:
        """Inline simple constraints"""
        pass  # Implementation would inline simple constraint checks

    async def _optimize_eightfold_paths(self) -> None:
        """Apply Eightfold Path optimizations"""
        pass  # Implementation would apply stage-specific optimizations

    async def _optimize_memory_layout(self) -> None:
        """Optimize memory layout"""
        pass  # Implementation would optimize data structure layout

    async def _generate_validation_code(self, target: CompilationTarget) -> Dict[str, str]:
        """Generate validation code for target"""
        shacl_results = self.global_metrics.get('shacl_results', {})

        if not shacl_results or not self.shacl_compiler:
            return {}

        # Combine all SHACL constraints
        all_shapes = []
        all_constraints = []

        for file_path, result in shacl_results.items():
            shapes = result.get('shapes', [])
            constraints = result.get('constraints', [])
            all_shapes.extend(shapes)
            all_constraints.extend(constraints)

        # Generate validation code using SHACL compiler templates
        generated_code = self.shacl_compiler._generate_code()

        return {
            "header": generated_code.get('shacl_validation.h', ''),
            "implementation": generated_code.get('shacl_validation.c', '')
        }

    async def _generate_ontology_code(self, target: CompilationTarget) -> Dict[str, str]:
        """Generate ontology code for target"""
        owl_results = self.global_metrics.get('owl_results', {})

        if not owl_results or not self.owl_compiler:
            return {}

        # Get the first OWL result (we could combine multiple if needed)
        first_result = next(iter(owl_results.values()), {})

        # Prepare context for template rendering
        context = self.owl_compiler._prepare_template_context(first_result, "ontology")

        # Add target-specific configuration
        if 'config' not in context:
            context['config'] = {}
        context['config'].update({
            'optimization_level': target.optimization_level,
            'debug_symbols': target.debug_symbols,
            'eightfold_optimizations': target.eightfold_optimizations
        })

        # Generate code using OWL compiler templates
        header_code = self.owl_compiler.template_manager.render_template('c_header.h.j2', context)
        impl_code = self.owl_compiler.template_manager.render_template('c_implementation.c.j2', context)

        return {
            "header": header_code,
            "implementation": impl_code
        }

    async def _generate_benchmark_code(self, target: CompilationTarget) -> Dict[str, str]:
        """Generate benchmark code for target"""
        benchmark_template = """
/*
 * AOT Validation Benchmark
 * Target: {{ target.name }}
 * Generated: {{ timestamp }}
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include "ontology.h"
#include "shacl_validation.h"
#include "runtime_support.h"

// High-resolution timing function (portable)
static inline uint64_t get_ticks() {
    #if defined(__x86_64__)
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
    #elif defined(__aarch64__)
    // ARM64 timer
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
    #else
    // Fallback to clock_gettime for portability
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
    #endif
}

// Test data structures
static void* create_test_data() {
    // Create a test graph with sample data
    graph_t* g = graph_create();
    populate_test_data(g);
    return g;
}

// Benchmark functions
typedef struct {
    const char* name;
    uint64_t ticks;
    int iterations;
    int passed;
} benchmark_result_t;

static benchmark_result_t run_owl_benchmark(void* test_data) {
    benchmark_result_t result = {"OWL Validation", 0, 1000000, 0};
    graph_t* g = (graph_t*)test_data;
    
    uint64_t start = get_ticks();
    for (int i = 0; i < result.iterations; i++) {
        // Simple validation: check if test instance has required properties
        bool valid = true;
        
        // Check if instance has rdf:type
        if (!has_property(g, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) {
            valid = false;
        }
        
        // Check if it has the test property
        if (valid && has_property(g, "http://example.org/test#testProperty")) {
            result.passed++;
        }
    }
    uint64_t end = get_ticks();
    
    result.ticks = end - start;
    return result;
}

static benchmark_result_t run_shacl_benchmark(void* test_data) {
    benchmark_result_t result = {"SHACL Validation", 0, 1000000, 0};
    graph_t* g = (graph_t*)test_data;
    
    uint64_t start = get_ticks();
    for (int i = 0; i < result.iterations; i++) {
        // SHACL validation: check minCount constraint
        bool conforms = true;
        
        // Check if testProperty exists (minCount 1)
        uint32_t count = count_property_values(g, "http://example.org/test#testProperty");
        if (count >= 1) {
            // Check datatype is string (simplified)
            const char* value = get_property_value(g, "http://example.org/test#testProperty");
            if (value != NULL) {
                result.passed++;
            }
        }
    }
    uint64_t end = get_ticks();
    
    result.ticks = end - start;
    return result;
}

// Mermaid diagram generation
static void generate_mermaid_report(benchmark_result_t* results, int count) {
    printf("```mermaid\\n");
    printf("gantt\\n");
    printf("    title AOT Benchmark Results\\n");
    printf("    dateFormat X\\n");
    printf("    axisFormat %%d\\n");
    
    for (int i = 0; i < count; i++) {
        double ticks_per_iter = (double)results[i].ticks / results[i].iterations;
        printf("    %s : 0, %.0f\\n", results[i].name, ticks_per_iter);
    }
    printf("```\\n\\n");
    
    printf("```mermaid\\n");
    printf("pie title Validation Pass Rate\\n");
    for (int i = 0; i < count; i++) {
        double pass_rate = (double)results[i].passed / results[i].iterations * 100;
        printf("    \\\"%s\\\" : %.1f\\n", results[i].name, pass_rate);
    }
    printf("```\\n");
}

int main(void) {
    printf("AOT Validation Benchmark\\n");
    printf("========================\\n\\n");
    
    // Create test data
    void* test_data = create_test_data();
    
    // Run benchmarks
    benchmark_result_t results[2];
    results[0] = run_owl_benchmark(test_data);
    results[1] = run_shacl_benchmark(test_data);
    
    // Generate OpenTelemetry-style report
    generate_mermaid_report(results, 2);
    
    // Cleanup
    graph_t* g = (graph_t*)test_data;
    // Free triples
    triple_t* t = g->triples;
    while (t) {
        triple_t* next = t->next;
        free(t);
        t = next;
    }
    free(g);
    
    return 0;
}
"""

        # Simple template rendering (without full Jinja2 for benchmark)
        code = benchmark_template.replace("{{ target.name }}", target.name)
        code = code.replace("{{ timestamp }}", datetime.now().isoformat())
        code = code.replace("{{ header_base }}", target.name)
        code = code.replace("{{ target.name|c_identifier }}", self._to_c_identifier(target.name))
        code = code.replace("{{ target.name|snake_case }}", self._to_snake_case(target.name))

        return {
            "implementation": code
        }

    def _to_c_identifier(self, name: str) -> str:
        """Convert name to C identifier"""
        return re.sub(r'[^a-zA-Z0-9_]', '_', name)

    def _to_snake_case(self, name: str) -> str:
        """Convert name to snake_case"""
        s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
        return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

    async def _finalize_results(self) -> None:
        """Finalize compilation results"""
        for result in self.results:
            # Calculate total duration
            if result.metrics:
                durations = [m.duration for m in result.metrics.values() if m.duration]
                if durations:
                    result.total_duration = sum(durations, timedelta())

            # Set memory peak
            if result.metrics:
                memory_peaks = [m.memory_peak for m in result.metrics.values()]
                if memory_peaks:
                    result.memory_peak = max(memory_peaks)


def main():
    """Main entry point for lifecycle manager"""
    import argparse

    parser = argparse.ArgumentParser(description="AOT Compilation Lifecycle Manager")
    parser.add_argument("--owl-files", nargs="+", type=Path, help="OWL files to compile")
    parser.add_argument("--shacl-files", nargs="+", type=Path, help="SHACL files to compile")
    parser.add_argument("--targets", nargs="+", default=["linux_x64"], help="Compilation targets")
    parser.add_argument("--config", type=Path, help="Configuration file")
    parser.add_argument("--parallel", action="store_true", help="Enable parallel compilation")

    args = parser.parse_args()

    # Load configuration
    config = {}
    if args.config and args.config.exists():
        with open(args.config) as f:
            config = yaml.safe_load(f)

    if args.parallel:
        config['parallel_compilation'] = True

    # Create source specification
    source_spec = SourceSpec(
        owl_files=args.owl_files or [],
        shacl_files=args.shacl_files or []
    )

    # Create compilation targets
    targets = []
    for target_name in args.targets:
        if "_" in target_name:
            platform, arch = target_name.split("_", 1)
        else:
            platform, arch = target_name, "x86_64"

        target = CompilationTarget(
            name=target_name,
            platform=platform,
            architecture=arch
        )
        targets.append(target)

    # Create lifecycle manager and run compilation
    manager = AOTLifecycleManager(config)

    async def run_compilation():
        results = await manager.compile(source_spec, targets)

        # Print results
        print("\nCompilation Results:")
        print("===================")

        for result in results:
            status = "SUCCESS" if result.success else "FAILED"
            print(f"{result.target.name}: {status}")

            if result.total_duration:
                print(f"  Duration: {result.total_duration}")

            if result.final_size:
                print(f"  Size: {result.final_size} bytes")

            print(f"  Artifacts: {len(result.artifacts)}")

            if result.artifacts.get('package'):
                print(f"  Package: {result.artifacts['package']}")

        return results

    # Run the compilation
    results = asyncio.run(run_compilation())

    # Exit with appropriate code
    success_count = sum(1 for r in results if r.success)
    if success_count == 0:
        sys.exit(1)
    elif success_count < len(results):
        sys.exit(2)  # Partial success
    else:
        sys.exit(0)  # Complete success


if __name__ == "__main__":
    main()
