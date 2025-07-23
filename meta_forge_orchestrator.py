#!/usr/bin/env python3
"""
Meta Forge Orchestrator - Automated ontology generation pipeline
Orchestrates the complete flow from domain requirements to optimized C code
"""

import typer
from pathlib import Path
from rich.console import Console
from rich.tree import Tree
from rich.table import Table
import json
import subprocess
import yaml
from typing import List, Dict, Optional
from datetime import datetime

from ontology_meta_forge import OntologyMetaForge, DomainType, MetaOntologySpec
from owl_compiler import OWLCompiler
from shacl_compiler import SHACLCompiler

app = typer.Typer(
    name="meta-forge",
    help="CNS Meta Forge - Automated domain ontology generation"
)
console = Console()

class MetaForgeOrchestrator:
    """Orchestrates the complete ontology generation pipeline"""
    
    def __init__(self, config_path: Optional[Path] = None):
        self.meta_forge = OntologyMetaForge()
        self.owl_compiler = OWLCompiler()
        self.config = self._load_config(config_path)
        
    def _load_config(self, config_path: Optional[Path]) -> dict:
        """Load meta forge configuration"""
        if not config_path:
            config_path = Path("meta_forge_config.yaml")
            
        if config_path.exists():
            with open(config_path) as f:
                return yaml.safe_load(f)
        return {}
    
    def generate_domain_suite(self, 
                            domain: str,
                            requirements: str,
                            patterns: Optional[List[str]] = None) -> Dict[str, any]:
        """Generate complete ontology suite for a domain"""
        
        console.print(f"[bold blue]Meta Forge Orchestrator[/bold blue]")
        console.print(f"Domain: {domain}")
        console.print(f"Requirements: {requirements[:100]}...")
        console.print()
        
        # Step 1: Generate meta specification
        with console.status("Generating meta specification..."):
            meta_spec = self.meta_forge.generate_meta_spec(domain, requirements)
            
            # Add pattern compositions if specified
            if patterns and self.config.get('patterns'):
                for pattern_name in patterns:
                    if pattern_name in self.config['patterns']:
                        pattern_data = self.config['patterns'][pattern_name]
                        # Extend meta spec with pattern data
                        meta_spec.modules.append(pattern_name)
        
        # Step 2: Generate ontology files
        console.print("[yellow]Generating ontology files...[/yellow]")
        output_dir = Path("ontologies/meta_generated")
        generated_files = self.meta_forge.forge_domain_ontologies(meta_spec, output_dir)
        
        # Step 3: Compile to C
        console.print("[yellow]Compiling to optimized C...[/yellow]")
        c_files = self._compile_to_c(generated_files, meta_spec)
        
        # Step 4: Generate benchmarks
        console.print("[yellow]Generating benchmark suite...[/yellow]")
        benchmark_file = self._generate_benchmarks(meta_spec, c_files)
        
        # Step 5: Create visualization
        visualization = self._create_visualization(meta_spec, generated_files)
        
        # Results summary
        results = {
            "domain": domain,
            "meta_spec": meta_spec,
            "generated_files": generated_files,
            "c_files": c_files,
            "benchmark": benchmark_file,
            "visualization": visualization,
            "timestamp": datetime.now().isoformat()
        }
        
        # Save results
        results_file = output_dir / meta_spec.domain.value / "generation_results.json"
        with open(results_file, 'w') as f:
            json.dump({
                "domain": domain,
                "modules": list(generated_files.keys()),
                "c_files": [str(f) for f in c_files],
                "benchmark": str(benchmark_file) if benchmark_file else None,
                "timestamp": results["timestamp"]
            }, f, indent=2)
        
        return results
    
    def _compile_to_c(self, ontology_files: Dict[str, Path], meta_spec: MetaOntologySpec) -> List[Path]:
        """Compile all ontology files to C"""
        c_files = []
        c_output_dir = Path("generated_c/meta") / meta_spec.domain.value
        
        for module, ttl_path in ontology_files.items():
            if ttl_path.suffix == '.ttl':
                output_dir = c_output_dir / module
                output_dir.mkdir(parents=True, exist_ok=True)
                
                try:
                    # Get optimization flags from config
                    perf_profile = "ultra_low_latency" if "8tick" in str(meta_spec.performance_requirements) else "low_latency"
                    
                    # Compile with appropriate optimizations
                    self.owl_compiler.compile_file(str(ttl_path), str(output_dir))
                    
                    c_file = output_dir / f"{module}.c"
                    if c_file.exists():
                        c_files.append(c_file)
                        
                except Exception as e:
                    console.print(f"[red]Error compiling {module}: {e}[/red]")
                    
        return c_files
    
    def _generate_benchmarks(self, meta_spec: MetaOntologySpec, c_files: List[Path]) -> Optional[Path]:
        """Generate domain-specific benchmarks"""
        
        benchmark_template = f"""#include <stdio.h>
#include "src/benchmark/otel_benchmark.h"

// Auto-generated benchmarks for {meta_spec.name}

void benchmark_{meta_spec.domain.value}_operations(otel_context_t* ctx) {{
    const int iterations = 1000000;
    
"""
        
        # Add benchmarks based on performance targets
        if hasattr(meta_spec, 'performance_requirements'):
            for op, target in meta_spec.performance_requirements.items():
                if isinstance(target, int):
                    benchmark_template += f"""
    otel_start_timing(ctx, "{op} [{target}tick]");
    for (int i = 0; i < iterations; i++) {{
        volatile int x = i; // Placeholder operation
    }}
    otel_end_timing(ctx, "{op} [{target}tick]", iterations);
"""
        
        benchmark_template += """
}

int main() {
    otel_context_t ctx;
    otel_init(&ctx);
    
    printf("Meta-Generated Benchmark: """ + meta_spec.name + """\\n");
    printf("==========================================\\n\\n");
    
    benchmark_""" + meta_spec.domain.value + """_operations(&ctx);
    
    otel_report_mermaid(&ctx);
    return 0;
}
"""
        
        benchmark_file = Path(f"benchmarks/{meta_spec.domain.value}_benchmark.c")
        benchmark_file.parent.mkdir(exist_ok=True)
        
        with open(benchmark_file, 'w') as f:
            f.write(benchmark_template)
            
        return benchmark_file
    
    def _create_visualization(self, meta_spec: MetaOntologySpec, files: Dict[str, Path]) -> str:
        """Create mermaid visualization of generated ontology structure"""
        
        mermaid = f"""```mermaid
graph TB
    subgraph "{meta_spec.name}"
        META[Meta Specification]
"""
        
        # Add modules
        for i, module in enumerate(meta_spec.modules):
            mermaid += f"""
        MODULE{i}[{module}]
        META --> MODULE{i}"""
        
        # Add performance requirements
        if meta_spec.performance_requirements:
            mermaid += """
        
        subgraph "Performance Requirements"
"""
            for req, val in meta_spec.performance_requirements.items():
                mermaid += f"""
            PERF_{req}[{req}: {val}]"""
        
        # Add compliance standards
        if meta_spec.compliance_standards:
            mermaid += """
        
        subgraph "Compliance Standards"
"""
            for std in meta_spec.compliance_standards:
                mermaid += f"""
            STD_{std}[{std}]"""
        
        mermaid += """
    end
```"""
        
        return mermaid

@app.command()
def generate(
    domain: str = typer.Argument(..., help="Domain type (trading, healthcare, iot, automotive)"),
    requirements: str = typer.Argument(..., help="Domain requirements description"),
    patterns: Optional[List[str]] = typer.Option(None, "--pattern", "-p", help="Additional patterns to include"),
    compile: bool = typer.Option(True, "--compile", help="Compile to C"),
    benchmark: bool = typer.Option(True, "--benchmark", help="Generate benchmarks")
):
    """Generate complete ontology suite for a domain"""
    
    orchestrator = MetaForgeOrchestrator()
    results = orchestrator.generate_domain_suite(domain, requirements, patterns)
    
    # Display results tree
    tree = Tree(f"[bold blue]{results['domain'].upper()} Ontology Suite[/bold blue]")
    
    # Ontology files
    ontology_branch = tree.add("[green]Ontology Files[/green]")
    for module, path in results['generated_files'].items():
        ontology_branch.add(f"{module}: {path.name}")
    
    # C files
    if results['c_files']:
        c_branch = tree.add("[yellow]C Files[/yellow]")
        for c_file in results['c_files']:
            c_branch.add(c_file.name)
    
    # Benchmark
    if results['benchmark']:
        tree.add(f"[cyan]Benchmark: {results['benchmark'].name}[/cyan]")
    
    console.print(tree)
    
    # Show visualization
    console.print("\n[bold]Architecture Visualization:[/bold]")
    console.print(results['visualization'])

@app.command()
def batch_generate(
    config_file: Path = typer.Argument(..., help="YAML file with batch generation specs")
):
    """Generate multiple domain ontologies from config file"""
    
    with open(config_file) as f:
        batch_config = yaml.safe_load(f)
    
    orchestrator = MetaForgeOrchestrator()
    
    results_table = Table(title="Batch Generation Results")
    results_table.add_column("Domain", style="cyan")
    results_table.add_column("Modules", style="yellow")
    results_table.add_column("C Files", style="green")
    results_table.add_column("Status", style="bold")
    
    for domain_spec in batch_config['domains']:
        try:
            results = orchestrator.generate_domain_suite(
                domain_spec['type'],
                domain_spec['requirements'],
                domain_spec.get('patterns')
            )
            
            results_table.add_row(
                domain_spec['type'],
                str(len(results['generated_files'])),
                str(len(results['c_files'])),
                "[green]✓ Success[/green]"
            )
        except Exception as e:
            results_table.add_row(
                domain_spec['type'],
                "0",
                "0",
                f"[red]✗ Failed: {str(e)[:30]}...[/red]"
            )
    
    console.print(results_table)

@app.command()
def analyze(
    domain_dir: Path = typer.Argument(..., help="Directory with generated domain ontologies")
):
    """Analyze generated ontology metrics"""
    
    manifest_file = domain_dir / "manifest.json"
    if not manifest_file.exists():
        console.print("[red]No manifest.json found in directory[/red]")
        return
    
    with open(manifest_file) as f:
        manifest = json.load(f)
    
    # Display analysis
    console.print(f"[bold blue]Domain Analysis: {manifest['name']}[/bold blue]")
    console.print(f"Generated: {manifest['generated']}")
    console.print()
    
    # Modules table
    modules_table = Table(title="Ontology Modules")
    modules_table.add_column("Module", style="cyan")
    modules_table.add_column("File", style="white")
    
    for module, filepath in manifest['files'].items():
        modules_table.add_row(module, Path(filepath).name)
    
    console.print(modules_table)
    
    # Performance requirements
    if manifest.get('performance_requirements'):
        console.print("\n[yellow]Performance Requirements:[/yellow]")
        for req, val in manifest['performance_requirements'].items():
            console.print(f"  • {req}: {val}")
    
    # Compliance standards
    if manifest.get('compliance_standards'):
        console.print("\n[green]Compliance Standards:[/green]")
        for std in manifest['compliance_standards']:
            console.print(f"  • {std}")

if __name__ == "__main__":
    app()