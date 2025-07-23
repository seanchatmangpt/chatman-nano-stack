#!/usr/bin/env python3
"""
CNS Ontology Forge CLI - One-command semantic artifact generator
Implements the Typer-based interface described in the press release
Uses Ollama with qwen3:latest model by default for local LLM inference
"""

import typer
from typing import Optional, List
from pathlib import Path
import dspy
from dspy.teleprompt import BootstrapFewShot
from rich.console import Console
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.table import Table
import json
import os
import time
from datetime import datetime
import hashlib
import subprocess

from owl_compiler import OWLCompiler
from shacl_compiler import SHACLCompiler
from aot_lifecycle import AOTLifecycleManager

app = typer.Typer(
    name="ontology-forge",
    help="CNS Ontology Forge - Transform product visions into semantic artifacts and C code"
)
console = Console()

class OntologyForge:
    """Main forge class that orchestrates artifact generation"""
    
    def __init__(self, llm_model: str = "qwen3:latest", ollama_base_url: str = "http://localhost:11434"):
        """Initialize forge with DSPy language model using Ollama"""
        # Use Ollama with qwen3:latest by default
        self.llm = dspy.OllamaLocal(
            model=llm_model,
            base_url=ollama_base_url,
            max_tokens=2000,
            temperature=0.7
        )
        dspy.settings.configure(lm=self.llm)
        self.archetype_path = Path("archetypes")
        self.output_path = Path("ontologies/generated")
        
    def generate_artifacts(self, 
                         prompt: str,
                         arena_count: int = 100,
                         ringbus_count: int = 40,
                         fiber_count: int = 128,
                         bitactor_count: int = 64) -> dict:
        """Generate semantic artifacts from prompt using DSPy"""
        
        # Create output directory
        self.output_path.mkdir(parents=True, exist_ok=True)
        
        results = {
            "generated_files": [],
            "statistics": {
                "arena": arena_count,
                "ringbus": ringbus_count,
                "fiber": fiber_count,
                "bitactor": bitactor_count,
                "total_artifacts": 0
            }
        }
        
        # Generate unique project hash from prompt
        prompt_hash = hashlib.sha256(prompt.encode()).hexdigest()[:8]
        
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=console
        ) as progress:
            
            # Phase 1: Archetype Loop
            task = progress.add_task("Running Archetype Loop...", total=4)
            
            for module_type, count in [
                ("arena", arena_count),
                ("ringbus", ringbus_count),
                ("fiber", fiber_count),
                ("bitactor", bitactor_count)
            ]:
                # DSPy prompt for archetype specialization
                archetype_prompt = f"""
                Given the product vision: {prompt}
                
                Generate a specialized {module_type} semantic definition.
                Include OWL classes, SHACL constraints, and SPARQL queries.
                Make it specific to the vision while maintaining CNS architecture patterns.
                """
                
                # Generate base archetype
                response = self.llm(archetype_prompt)
                
                # Save archetype
                archetype_file = self.output_path / f"{module_type}_archetype_{prompt_hash}.ttl"
                with open(archetype_file, 'w') as f:
                    f.write(self._format_as_turtle(module_type, response, prompt_hash))
                
                results["generated_files"].append(str(archetype_file))
                progress.advance(task)
            
            # Phase 2: Instance Loop
            task2 = progress.add_task("Running Instance Loop...", total=sum([arena_count, ringbus_count, fiber_count, bitactor_count]))
            
            for module_type, count in [
                ("arena", arena_count),
                ("ringbus", ringbus_count),
                ("fiber", fiber_count),
                ("bitactor", bitactor_count)
            ]:
                for i in range(count):
                    # Generate unique instance with incremental suffix
                    instance_name = f"{module_type}_{i:03d}"
                    
                    # Add project-specific annotations
                    instance_ttl = self._generate_instance(module_type, i, prompt_hash, prompt)
                    
                    instance_file = self.output_path / f"{instance_name}_{prompt_hash}.ttl"
                    with open(instance_file, 'w') as f:
                        f.write(instance_ttl)
                    
                    results["generated_files"].append(str(instance_file))
                    results["statistics"]["total_artifacts"] += 1
                    progress.advance(task2)
        
        return results
    
    def _format_as_turtle(self, module_type: str, content: str, hash: str) -> str:
        """Format DSPy output as valid Turtle/RDF"""
        return f"""@prefix cns: <http://cns.io/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix shacl: <http://www.w3.org/ns/shacl#> .

# Generated by CNS Ontology Forge
# Module Type: {module_type}
# Hash: {hash}
# Timestamp: {datetime.now().isoformat()}

cns:{module_type}_archetype a owl:Class ;
    rdfs:label "{module_type} Archetype" ;
    rdfs:comment "{content}" .
"""
    
    def _generate_instance(self, module_type: str, index: int, hash: str, prompt: str) -> str:
        """Generate a specific instance with unique IRI"""
        return f"""@prefix cns: <http://cns.io/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Instance {index:03d} of {module_type}
# Project Hash: {hash}

cns:{module_type}_{index:03d} a cns:{module_type}_archetype ;
    rdfs:label "{module_type} Instance {index:03d}" ;
    cns:projectHash "{hash}" ;
    cns:generatedFrom "{prompt[:100]}..." ;
    cns:instanceIndex {index} .
"""

@app.command()
def generate(
    prompt: str = typer.Argument(..., help="Product vision description"),
    arena: int = typer.Option(100, "--arena", help="Number of arena modules"),
    ringbus: int = typer.Option(40, "--ringbus", help="Number of ringbus modules"),
    fiber: int = typer.Option(128, "--fiber", help="Number of fiber modules"),
    bitactor: int = typer.Option(64, "--bitactor", help="Number of bitactor modules"),
    model: str = typer.Option("qwen3:latest", "--model", help="Ollama model to use (default: qwen3:latest)"),
    ollama_url: str = typer.Option("http://localhost:11434", "--ollama-url", help="Ollama API URL"),
    compile_to_c: bool = typer.Option(True, "--compile", help="Compile to C using AOT")
):
    """Generate semantic artifacts from a product vision"""
    
    console.print(f"[bold blue]CNS Ontology Forge[/bold blue]")
    console.print(f"Vision: {prompt[:80]}...")
    console.print(f"Model: {model} (via Ollama)")
    console.print(f"Ollama URL: {ollama_url}")
    console.print()
    
    # Initialize forge with Ollama
    forge = OntologyForge(llm_model=model, ollama_base_url=ollama_url)
    
    # Generate artifacts
    start_time = time.time()
    results = forge.generate_artifacts(prompt, arena, ringbus, fiber, bitactor)
    generation_time = time.time() - start_time
    
    # Display results
    table = Table(title="Generation Results")
    table.add_column("Metric", style="cyan")
    table.add_column("Value", style="green")
    
    table.add_row("Total Artifacts", str(results["statistics"]["total_artifacts"]))
    table.add_row("Generation Time", f"{generation_time:.2f}s")
    table.add_row("Artifacts/Second", f"{results['statistics']['total_artifacts']/generation_time:.1f}")
    
    console.print(table)
    
    # Compile to C if requested
    if compile_to_c:
        console.print("\n[bold yellow]Compiling to C...[/bold yellow]")
        
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=console
        ) as progress:
            task = progress.add_task("Running AOT compilation...", total=len(results["generated_files"]))
            
            lifecycle = AOTLifecycleManager()
            c_files = []
            
            for ttl_file in results["generated_files"]:
                if os.path.exists(ttl_file):
                    # Compile each TTL file
                    output_dir = Path("generated_c") / Path(ttl_file).stem
                    output_dir.mkdir(parents=True, exist_ok=True)
                    
                    try:
                        compiler = OWLCompiler()
                        compiler.compile_file(ttl_file, str(output_dir))
                        c_files.append(str(output_dir / f"{Path(ttl_file).stem}.c"))
                    except Exception as e:
                        console.print(f"[red]Error compiling {ttl_file}: {e}[/red]")
                    
                    progress.advance(task)
        
        console.print(f"\n[green]✓ Generated {len(c_files)} C files[/green]")
        console.print(f"[dim]Output directory: generated_c/[/dim]")
    
    # Save metadata
    metadata_file = forge.output_path / f"generation_metadata_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(metadata_file, 'w') as f:
        json.dump({
            "prompt": prompt,
            "model": model,
            "statistics": results["statistics"],
            "generation_time": generation_time,
            "timestamp": datetime.now().isoformat(),
            "files_generated": len(results["generated_files"])
        }, f, indent=2)
    
    console.print(f"\n[bold green]Success![/bold green] Generated {results['statistics']['total_artifacts']} artifacts in {generation_time:.1f} seconds")

@app.command()
def benchmark(
    directory: Path = typer.Argument("generated_c", help="Directory with C files to benchmark"),
    output_format: str = typer.Option("mermaid", "--format", help="Output format (mermaid, json)")
):
    """Benchmark generated C files for 8-tick compliance"""
    
    console.print(f"[bold blue]Running C benchmarks...[/bold blue]")
    
    # Import benchmark runner
    import subprocess
    
    # Compile and run benchmark
    benchmark_results = []
    
    c_files = list(Path(directory).rglob("*.c"))
    console.print(f"Found {len(c_files)} C files to benchmark")
    
    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console
    ) as progress:
        task = progress.add_task("Benchmarking...", total=len(c_files))
        
        for c_file in c_files:
            # Compile with benchmark harness
            output_binary = c_file.with_suffix("")
            compile_cmd = [
                "gcc", "-O3", "-march=native", 
                str(c_file),
                "/Users/sac/cns/src/benchmark/otel_benchmark.c",
                "-I/Users/sac/cns/src/benchmark",
                "-o", str(output_binary)
            ]
            
            try:
                subprocess.run(compile_cmd, check=True, capture_output=True)
                
                # Run benchmark
                result = subprocess.run([str(output_binary)], capture_output=True, text=True)
                benchmark_results.append({
                    "file": str(c_file),
                    "output": result.stdout,
                    "success": result.returncode == 0
                })
            except Exception as e:
                benchmark_results.append({
                    "file": str(c_file),
                    "error": str(e),
                    "success": False
                })
            
            progress.advance(task)
    
    # Generate report
    if output_format == "mermaid":
        console.print("\n[bold green]Benchmark Results (Mermaid):[/bold green]")
        for result in benchmark_results:
            if result.get("success") and "mermaid" in result.get("output", "").lower():
                console.print(result["output"])
    else:
        # JSON output
        output_file = Path("benchmark_results.json")
        with open(output_file, 'w') as f:
            json.dump(benchmark_results, f, indent=2)
        console.print(f"\n[green]Results saved to {output_file}[/green]")

@app.command()
def list_archetypes():
    """List available archetype templates"""
    console.print("[bold blue]Available Archetypes:[/bold blue]\n")
    
    archetypes = [
        ("arena", "High-performance memory arena for zero-copy operations"),
        ("ringbus", "Lock-free ring buffer for inter-thread communication"),
        ("fiber", "Lightweight coroutine for concurrent execution"),
        ("bitactor", "Bit-level actor for ultra-compact state machines"),
    ]
    
    table = Table(title="CNS Module Archetypes")
    table.add_column("Type", style="cyan")
    table.add_column("Description", style="white")
    
    for archetype, desc in archetypes:
        table.add_row(archetype, desc)
    
    console.print(table)

@app.command()
def check_ollama(
    ollama_url: str = typer.Option("http://localhost:11434", "--ollama-url", help="Ollama API URL")
):
    """Check Ollama status and available models"""
    import requests
    
    console.print(f"[bold blue]Checking Ollama at {ollama_url}...[/bold blue]\n")
    
    try:
        # Check if Ollama is running
        response = requests.get(f"{ollama_url}/api/tags")
        if response.status_code == 200:
            console.print("[green]✓ Ollama is running[/green]\n")
            
            models = response.json().get("models", [])
            if models:
                table = Table(title="Available Models")
                table.add_column("Model", style="cyan")
                table.add_column("Size", style="yellow")
                table.add_column("Modified", style="white")
                
                for model in models:
                    size_gb = model.get("size", 0) / (1024**3)
                    table.add_row(
                        model.get("name", "Unknown"),
                        f"{size_gb:.1f} GB",
                        model.get("modified_at", "Unknown")[:10]
                    )
                
                console.print(table)
                
                # Check if qwen3:latest is available
                model_names = [m.get("name", "") for m in models]
                if "qwen3:latest" in model_names:
                    console.print("\n[green]✓ qwen3:latest is available[/green]")
                else:
                    console.print("\n[yellow]⚠ qwen3:latest not found. Install with:[/yellow]")
                    console.print("[dim]ollama pull qwen3:latest[/dim]")
            else:
                console.print("[yellow]No models found. Install qwen3:latest with:[/yellow]")
                console.print("[dim]ollama pull qwen3:latest[/dim]")
        else:
            console.print(f"[red]✗ Ollama returned status {response.status_code}[/red]")
    except requests.exceptions.ConnectionError:
        console.print("[red]✗ Cannot connect to Ollama[/red]")
        console.print("\nMake sure Ollama is running:")
        console.print("[dim]1. Install from https://ollama.com[/dim]")
        console.print("[dim]2. Run: ollama serve[/dim]")
        console.print("[dim]3. Pull model: ollama pull qwen3:latest[/dim]")
    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")

if __name__ == "__main__":
    app()