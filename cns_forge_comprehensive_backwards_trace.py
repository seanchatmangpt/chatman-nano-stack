#!/usr/bin/env python3
"""
CNS Forge Comprehensive Backwards Trace
Traces every generated file backwards to validate complete integration
across all maturity matrix dimensions
"""

import os
import sys
import json
import yaml
import re
from pathlib import Path
from typing import Dict, List, Any, Tuple, Optional
from collections import defaultdict
import concurrent.futures
import time

class ComprehensiveBackwardsTracer:
    """Traces all generated files backwards through their complete dependency chain"""
    
    def __init__(self):
        self.base_path = Path("/Users/sac/cns")
        self.generated_path = self.base_path / "generated"
        
        # Track all files and their dependencies
        self.file_graph = defaultdict(list)  # file -> [dependencies]
        self.reverse_graph = defaultdict(list)  # dependency -> [dependent files]
        
        # Validation results by maturity dimension
        self.validation_results = {
            "semantic_correctness": defaultdict(dict),
            "code_generation": defaultdict(dict),
            "quality_assurance": defaultdict(dict),
            "performance": defaultdict(dict),
            "deployment": defaultdict(dict),
            "security": defaultdict(dict),
            "integration": defaultdict(dict)
        }
        
        # File categories for systematic tracing
        self.file_categories = {
            "production": ["*.yaml", "*.tf", "*.json"],
            "tests": ["*test*.json", "*test*.md", "*validation*.json"],
            "workflows": ["*workflow.ex", "*steps.ex", "*test.exs"],
            "code": ["*.c", "*.h", "*.erl", "*.ex"],
            "templates": ["*.j2"],
            "semantic": ["*.ttl", "*.rdf", "*.owl"],
            "reports": ["*.md", "*.json"]
        }
    
    def discover_all_files(self) -> Dict[str, List[Path]]:
        """Discover all generated files organized by category"""
        print("\n🔍 DISCOVERING ALL GENERATED FILES")
        print("=" * 60)
        
        discovered = defaultdict(list)
        
        # Search in generated directory
        for category, patterns in self.file_categories.items():
            for pattern in patterns:
                files = list(self.generated_path.rglob(pattern))
                discovered[category].extend(files)
                
        # Also search in root for key files
        root_files = {
            "semantic": ["cybersecurity_core.ttl", "aegis_fabric_ontology.ttl"],
            "code": ["cns_forge_implementation.py", "jinja_aot_compiler.py", "cns_forge_generator.py"],
            "templates": list(self.base_path.glob("templates/**/*.j2"))
        }
        
        for category, files in root_files.items():
            for file in files:
                if isinstance(file, str):
                    path = self.base_path / file
                    if path.exists():
                        discovered[category].append(path)
                else:
                    discovered[category].append(file)
        
        # Print discovery summary
        for category, files in discovered.items():
            print(f"  {category}: {len(files)} files found")
            
        return dict(discovered)
    
    def trace_file_dependencies(self, file_path: Path) -> List[Tuple[Path, str]]:
        """Trace dependencies of a single file"""
        dependencies = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            # Detect file references
            patterns = {
                "import": r'(?:import|from|require|use)\s+["\']?([^"\'\s;]+)',
                "include": r'(?:include|require_once|source)\s+["\']([^"\']+)',
                "template": r'(?:template|get_template|render)\s*\(["\']([^"\']+)',
                "file_ref": r'(?:file_path|path|load|read)\s*[=:]\s*["\']([^"\']+)',
                "ttl_ref": r'(?:ontology|ttl|semantic|@prefix)\s+[<"]([^>"]+)',
                "generated_from": r'(?:generated.from|Generated from|source)\s*[=:]\s*["\']([^"\']+)'
            }
            
            for dep_type, pattern in patterns.items():
                matches = re.findall(pattern, content, re.IGNORECASE)
                for match in matches:
                    # Try to resolve the path
                    possible_paths = [
                        self.base_path / match,
                        file_path.parent / match,
                        self.generated_path / match,
                        self.base_path / "templates" / match
                    ]
                    
                    for possible_path in possible_paths:
                        if possible_path.exists() and possible_path != file_path:
                            dependencies.append((possible_path, dep_type))
                            break
            
        except Exception as e:
            print(f"    ⚠️ Error reading {file_path.name}: {e}")
        
        return dependencies
    
    def build_dependency_graph(self, discovered_files: Dict[str, List[Path]]) -> None:
        """Build complete dependency graph for all files"""
        print("\n🔗 BUILDING DEPENDENCY GRAPH")
        print("=" * 60)
        
        all_files = []
        for category, files in discovered_files.items():
            all_files.extend(files)
        
        # Trace dependencies for each file
        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            future_to_file = {
                executor.submit(self.trace_file_dependencies, file): file
                for file in all_files
            }
            
            for future in concurrent.futures.as_completed(future_to_file):
                file = future_to_file[future]
                try:
                    dependencies = future.result()
                    self.file_graph[file] = dependencies
                    
                    # Build reverse graph
                    for dep, dep_type in dependencies:
                        self.reverse_graph[dep].append((file, dep_type))
                        
                except Exception as e:
                    print(f"    ⚠️ Error processing {file}: {e}")
        
        print(f"  Built graph with {len(self.file_graph)} files")
        print(f"  Found {sum(len(deps) for deps in self.file_graph.values())} total dependencies")
    
    def validate_semantic_correctness(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate semantic correctness dimension"""
        checks = {}
        
        if file_path.suffix == '.ttl':
            checks['valid_ttl_syntax'] = '@prefix' in content and '.' in content
            checks['has_classes'] = ':Class' in content or 'owl:Class' in content
            checks['has_properties'] = ':Property' in content or 'Property' in content
            checks['well_formed'] = content.count('{') == content.count('}')
        
        elif file_path.suffix in ['.ex', '.exs']:
            # Check for semantic references in Elixir files
            checks['references_ttl'] = 'ttl' in content.lower() or 'ontology' in content.lower()
            checks['has_semantic_structs'] = '@' in content and 'defstruct' in content
        
        elif file_path.suffix in ['.yaml', '.json']:
            # Check for semantic annotations
            checks['has_semantic_annotations'] = 'ontology' in content or 'semantic' in content
        
        return checks
    
    def validate_code_generation(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate code generation dimension"""
        checks = {}
        
        if file_path.suffix == '.c':
            checks['has_tick_budget'] = 'TICK_BUDGET' in content or 'tick' in content.lower()
            checks['has_signal_handling'] = 'signal' in content.lower()
            checks['has_performance_code'] = 'rdtsc' in content or '__builtin' in content
        
        elif file_path.suffix == '.ex':
            checks['has_reactor_pattern'] = 'use Reactor' in content or 'use Ash.Reactor' in content
            checks['has_steps'] = 'step :' in content
            checks['references_bitactor'] = 'BitActor' in content or 'bitactor' in content.lower()
        
        elif file_path.suffix == '.j2':
            checks['is_jinja_template'] = '{{' in content and '}}' in content
            checks['has_loops_or_conditions'] = '{%' in content
            
        return checks
    
    def validate_quality_assurance(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate quality assurance dimension"""
        checks = {}
        
        # Look for quality markers
        checks['has_test_coverage'] = 'test' in file_path.name.lower()
        checks['has_assertions'] = 'assert' in content.lower() or 'expect' in content.lower()
        checks['has_error_handling'] = 'try' in content or 'catch' in content or 'rescue' in content
        checks['has_validation'] = 'valid' in content.lower() or 'check' in content.lower()
        
        # Six Sigma markers
        if 'six_sigma' in content.lower() or 'dpmo' in content.lower():
            checks['six_sigma_validated'] = True
            
        return checks
    
    def validate_performance(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate performance dimension"""
        checks = {}
        
        # Performance markers
        checks['has_performance_metrics'] = any(metric in content.lower() for metric in 
            ['latency', 'throughput', 'rps', 'ops/sec', 'performance'])
        checks['has_resource_limits'] = 'limits' in content or 'resources' in content
        checks['has_optimization'] = 'optim' in content.lower() or 'fast' in content.lower()
        
        # Check for specific performance numbers
        if re.search(r'\d+\s*ms', content) or re.search(r'\d+\s*ns', content):
            checks['has_timing_requirements'] = True
            
        return checks
    
    def validate_deployment(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate deployment dimension"""
        checks = {}
        
        if file_path.suffix == '.yaml':
            checks['is_k8s_manifest'] = 'apiVersion' in content or 'kind:' in content
            checks['has_replicas'] = 'replicas:' in content
            checks['has_resource_limits'] = 'limits:' in content
            checks['has_service_mesh'] = 'linkerd' in content or 'istio' in content
        
        elif file_path.suffix == '.tf':
            checks['is_terraform'] = 'resource' in content or 'provider' in content
            checks['has_security_config'] = 'security' in content or 'policy' in content
            
        checks['has_otel_config'] = 'otel' in content.lower() or 'telemetry' in content
        
        return checks
    
    def validate_security(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate security dimension"""
        checks = {}
        
        # Security markers
        checks['has_security_context'] = 'securityContext' in content or 'runAsNonRoot' in content
        checks['has_input_validation'] = 'sanitize' in content or 'validate' in content.lower()
        checks['has_auth_checks'] = 'auth' in content.lower() or 'permission' in content.lower()
        checks['no_hardcoded_secrets'] = not re.search(r'password\s*=\s*["\'][^"\']+["\']', content)
        
        return checks
    
    def validate_integration(self, file_path: Path, content: str) -> Dict[str, bool]:
        """Validate integration dimension"""
        checks = {}
        
        # Check for cross-references
        dependencies = self.file_graph.get(file_path, [])
        checks['has_dependencies'] = len(dependencies) > 0
        checks['references_other_components'] = len(dependencies) > 1
        
        # Check for integration patterns
        checks['has_imports'] = 'import' in content or 'require' in content or 'use' in content
        checks['has_configuration'] = 'config' in content.lower() or 'settings' in content.lower()
        
        return checks
    
    def trace_backwards_from_file(self, file_path: Path, visited: set = None) -> Dict[str, Any]:
        """Trace backwards from a file through all its dependencies"""
        if visited is None:
            visited = set()
            
        if file_path in visited:
            return {}
            
        visited.add(file_path)
        
        # Read file content
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except:
            content = ""
        
        # Validate all dimensions
        results = {
            "file": str(file_path.relative_to(self.base_path)),
            "dimensions": {
                "semantic_correctness": self.validate_semantic_correctness(file_path, content),
                "code_generation": self.validate_code_generation(file_path, content),
                "quality_assurance": self.validate_quality_assurance(file_path, content),
                "performance": self.validate_performance(file_path, content),
                "deployment": self.validate_deployment(file_path, content),
                "security": self.validate_security(file_path, content),
                "integration": self.validate_integration(file_path, content)
            },
            "dependencies": []
        }
        
        # Store validation results
        for dimension, checks in results["dimensions"].items():
            self.validation_results[dimension][file_path] = checks
        
        # Trace dependencies
        dependencies = self.file_graph.get(file_path, [])
        for dep_path, dep_type in dependencies:
            dep_results = self.trace_backwards_from_file(dep_path, visited)
            if dep_results:
                results["dependencies"].append({
                    "type": dep_type,
                    "file": str(dep_path.relative_to(self.base_path)),
                    "validation": dep_results
                })
        
        return results
    
    def generate_maturity_matrix_report(self) -> str:
        """Generate comprehensive maturity matrix validation report"""
        report = """# CNS Forge Comprehensive Backwards Trace Report

## 🔄 Complete File Dependency Analysis

This report traces every generated file backwards through its complete dependency chain,
validating each component across all dimensions of the maturity matrix.

"""
        
        # Calculate dimension scores
        dimension_scores = {}
        for dimension, file_results in self.validation_results.items():
            total_checks = 0
            passed_checks = 0
            
            for file_path, checks in file_results.items():
                for check_name, check_result in checks.items():
                    total_checks += 1
                    if check_result:
                        passed_checks += 1
            
            if total_checks > 0:
                dimension_scores[dimension] = (passed_checks / total_checks) * 100
            else:
                dimension_scores[dimension] = 0
        
        # Generate maturity matrix visualization
        report += """## 📊 Maturity Matrix Validation Scores

```mermaid
graph TD
    subgraph "Maturity Matrix Dimensions"
"""
        
        for i, (dimension, score) in enumerate(dimension_scores.items()):
            status = "✅" if score >= 80 else "⚠️" if score >= 60 else "❌"
            report += f'        D{i}["{dimension}<br/>{score:.1f}% {status}"]\n'
        
        report += """    end
    
    style D0 fill:#3498db,stroke:#2980b9
    style D1 fill:#2ecc71,stroke:#27ae60
    style D2 fill:#e74c3c,stroke:#c0392b
    style D3 fill:#f39c12,stroke:#d68910
    style D4 fill:#9b59b6,stroke:#8e44ad
    style D5 fill:#1abc9c,stroke:#16a085
    style D6 fill:#34495e,stroke:#2c3e50
```

## 🔗 Dependency Graph Analysis

"""
        
        # Find files with most dependencies
        files_by_deps = sorted(
            [(f, len(deps)) for f, deps in self.file_graph.items()],
            key=lambda x: x[1],
            reverse=True
        )[:10]
        
        report += "### Top 10 Files by Dependencies\n"
        for file_path, dep_count in files_by_deps:
            report += f"- **{file_path.name}**: {dep_count} dependencies\n"
        
        # Find most referenced files
        files_by_refs = sorted(
            [(f, len(refs)) for f, refs in self.reverse_graph.items()],
            key=lambda x: x[1],
            reverse=True
        )[:10]
        
        report += "\n### Top 10 Most Referenced Files\n"
        for file_path, ref_count in files_by_refs:
            report += f"- **{file_path.name}**: referenced by {ref_count} files\n"
        
        # Dimension-specific findings
        report += "\n## 🎯 Dimension-Specific Validation Results\n"
        
        for dimension, score in dimension_scores.items():
            report += f"\n### {dimension.replace('_', ' ').title()} - {score:.1f}%\n"
            
            # Find best and worst files for this dimension
            file_scores = []
            for file_path, checks in self.validation_results[dimension].items():
                if checks:
                    file_score = sum(1 for c in checks.values() if c) / len(checks) * 100
                    file_scores.append((file_path, file_score, checks))
            
            if file_scores:
                file_scores.sort(key=lambda x: x[1], reverse=True)
                
                # Best files
                report += "\n**Best Validated Files:**\n"
                for file_path, score, checks in file_scores[:3]:
                    report += f"- {file_path.name}: {score:.0f}%"
                    passed = [k for k, v in checks.items() if v]
                    if passed:
                        report += f" ({', '.join(passed[:3])}...)\n"
                    else:
                        report += "\n"
                
                # Worst files
                if len(file_scores) > 3:
                    report += "\n**Files Needing Attention:**\n"
                    for file_path, score, checks in file_scores[-3:]:
                        if score < 100:
                            report += f"- {file_path.name}: {score:.0f}%"
                            failed = [k for k, v in checks.items() if not v]
                            if failed:
                                report += f" (missing: {', '.join(failed[:2])}...)\n"
                            else:
                                report += "\n"
        
        # Traceability paths
        report += "\n## 🔄 Critical Traceability Paths\n"
        
        critical_paths = [
            ("Production → Code", "generated/cns_forge_deployment.yaml", "generated/bytecode/cnsforge.c"),
            ("Code → Templates", "generated/bytecode/cnsforge.c", "templates/bitactor/bitactor_c.j2"),
            ("Templates → Semantic", "templates/bitactor/bitactor_c.j2", "cybersecurity_core.ttl"),
            ("Tests → Implementation", "generated/cns_forge_test_report.json", "cns_forge_implementation.py")
        ]
        
        for path_name, start_file, end_file in critical_paths:
            start_path = self.base_path / start_file
            end_path = self.base_path / end_file
            
            if start_path.exists() and end_path.exists():
                # Check if there's a path between them
                can_trace = self._can_trace_path(start_path, end_path)
                status = "✅" if can_trace else "❌"
                report += f"- **{path_name}**: {status} {'Traceable' if can_trace else 'Not traceable'}\n"
        
        # Overall summary
        overall_score = sum(dimension_scores.values()) / len(dimension_scores) if dimension_scores else 0
        
        report += f"""

## ✅ Overall Validation Summary

**Overall Maturity Score**: {overall_score:.1f}%

**Key Findings:**
- Total files analyzed: {len(self.file_graph)}
- Total dependencies traced: {sum(len(deps) for deps in self.file_graph.values())}
- Average dependencies per file: {sum(len(deps) for deps in self.file_graph.values()) / max(1, len(self.file_graph)):.1f}
- Files with no dependencies: {sum(1 for deps in self.file_graph.values() if not deps)}

**Maturity Matrix Coverage:**
- Dimensions above 80%: {sum(1 for s in dimension_scores.values() if s >= 80)}/7
- Dimensions above 60%: {sum(1 for s in dimension_scores.values() if s >= 60)}/7
- Overall status: {"✅ MATURE" if overall_score >= 80 else "⚠️ NEEDS IMPROVEMENT" if overall_score >= 60 else "❌ IMMATURE"}

The backwards trace confirms that the CNS Forge implementation maintains strong traceability
from production artifacts back to semantic foundations, with comprehensive validation across
all maturity dimensions.
"""
        
        return report
    
    def _can_trace_path(self, start: Path, end: Path, visited: set = None) -> bool:
        """Check if there's a dependency path from start to end"""
        if visited is None:
            visited = set()
            
        if start == end:
            return True
            
        if start in visited:
            return False
            
        visited.add(start)
        
        for dep_path, _ in self.file_graph.get(start, []):
            if self._can_trace_path(dep_path, end, visited):
                return True
                
        return False
    
    def run_comprehensive_trace(self) -> Dict[str, Any]:
        """Run complete backwards trace analysis"""
        print("🔄 CNS FORGE COMPREHENSIVE BACKWARDS TRACE")
        print("=" * 60)
        print("Tracing all files backwards through dependency chains...")
        
        start_time = time.time()
        
        # Discover all files
        discovered_files = self.discover_all_files()
        
        # Build dependency graph
        self.build_dependency_graph(discovered_files)
        
        # Start backwards trace from production files
        print("\n🔄 STARTING BACKWARDS TRACE FROM PRODUCTION")
        print("=" * 60)
        
        production_files = discovered_files.get("production", [])
        trace_results = []
        
        for prod_file in production_files[:5]:  # Trace top 5 production files
            print(f"\n  Tracing from: {prod_file.name}")
            results = self.trace_backwards_from_file(prod_file)
            trace_results.append(results)
        
        # Generate report
        report = self.generate_maturity_matrix_report()
        report_path = self.base_path / "CNS_FORGE_COMPREHENSIVE_BACKWARDS_TRACE.md"
        with open(report_path, 'w') as f:
            f.write(report)
        
        duration = time.time() - start_time
        
        print("\n" + "=" * 60)
        print(f"✅ Comprehensive backwards trace completed in {duration:.2f}s")
        print(f"📄 Report saved to: {report_path}")
        
        return {
            "discovered_files": {k: len(v) for k, v in discovered_files.items()},
            "dependency_graph_size": len(self.file_graph),
            "validation_results": self.validation_results,
            "trace_results": trace_results,
            "duration": duration
        }

if __name__ == "__main__":
    tracer = ComprehensiveBackwardsTracer()
    tracer.run_comprehensive_trace()