#!/usr/bin/env python3
"""
BitActor CLI - TTL-based Code Generation with Self-Check
Generates BitActor implementations in C, Python, and Erlang from TTL ontologies
Includes comprehensive self-checking across all languages
"""

import os
import sys
import subprocess
import json
import time
from pathlib import Path
from typing import Optional, List, Dict, Any
import typer
from rich import print
from rich.console import Console
from rich.table import Table
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.panel import Panel
import rdflib

# Import the generator
from bitactor_ttl_generator import BitActorTTLGenerator

app = typer.Typer(
    name="bitactor",
    help="🚀 BitActor CLI - Generate ultra-fast signal processing from TTL ontologies",
    add_completion=True
)
console = Console()

class BitActorCLI:
    """Main CLI class for BitActor operations"""
    
    def __init__(self):
        self.generator = BitActorTTLGenerator()
        self.test_results: Dict[str, Dict[str, Any]] = {}
    
    def validate_ttl(self, ttl_file: Path) -> bool:
        """Validate TTL file syntax"""
        try:
            g = rdflib.Graph()
            g.parse(ttl_file, format='turtle')
            return True
        except Exception as e:
            console.print(f"[red]❌ Invalid TTL: {e}[/red]")
            return False
    
    def run_c_tests(self, output_dir: Path) -> Dict[str, Any]:
        """Run C tests and benchmarks"""
        result = {
            "language": "C",
            "build": False,
            "tests": False,
            "benchmark": False,
            "performance": {}
        }
        
        with console.status("[bold green]Running C tests...") as status:
            # Build
            status.update("Building C code...")
            build_result = subprocess.run(
                ["make", "clean", "all"],
                cwd=output_dir,
                capture_output=True,
                text=True
            )
            result["build"] = build_result.returncode == 0
            
            if result["build"]:
                # Run tests
                status.update("Running unit tests...")
                test_result = subprocess.run(
                    [f"./{list(output_dir.glob('*_test'))[0].name}"],
                    cwd=output_dir,
                    capture_output=True,
                    text=True
                )
                result["tests"] = test_result.returncode == 0
                result["test_output"] = test_result.stdout
                
                # Run benchmark
                status.update("Running benchmarks...")
                bench_result = subprocess.run(
                    [f"./{list(output_dir.glob('*_benchmark'))[0].name}"],
                    cwd=output_dir,
                    capture_output=True,
                    text=True,
                    timeout=15  # 15 second timeout for 10s stress test
                )
                result["benchmark"] = bench_result.returncode == 0
                
                # Parse performance metrics
                if result["benchmark"]:
                    output = bench_result.stdout
                    if "Average throughput:" in output:
                        for line in output.split('\n'):
                            if "Average throughput:" in line:
                                throughput = line.split(':')[1].strip()
                                result["performance"]["throughput"] = throughput
                            elif "Average latency:" in line:
                                latency = line.split(':')[1].strip()
                                result["performance"]["latency"] = latency
        
        return result
    
    def run_python_tests(self, output_dir: Path) -> Dict[str, Any]:
        """Run Python tests"""
        result = {
            "language": "Python",
            "import": False,
            "instantiation": False,
            "signal_processing": False,
            "performance": {}
        }
        
        with console.status("[bold green]Running Python tests...") as status:
            status.update("Testing Python module...")
            
            # Create test script
            test_script = output_dir / "test_python.py"
            test_code = '''
import sys
import time
sys.path.insert(0, '.')
# Get the module name dynamically
import glob
module_files = glob.glob('*_bitactor.py')
if module_files:
    module_name = module_files[0].replace('.py', '')
    exec(f'from {module_name} import *')
    # Get the class names dynamically
    for name in dir():
        if 'BitActor' in name and name != 'BitActor':
            BitActorClass = eval(name)
        elif 'Signal' in name and 'Type' not in name and name != 'Signal':
            SignalClass = eval(name)
        elif 'SignalType' in name:
            SignalTypeClass = eval(name)

# Test instantiation
ba = BitActorClass()
print("✅ BitActor instantiated")

# Test signal processing
signal = SignalClass(
    type=1,  # First signal type
    flags=0,
    timestamp=int(time.time() * 1e9),
    payload=0xDEADBEEF
)

start = time.perf_counter()
for i in range(100000):
    ba.process_signal(signal)
elapsed = time.perf_counter() - start

stats = ba.get_stats()
print(f"✅ Processed {stats['signals_processed']} signals")
print(f"Throughput: {stats['signals_processed']/elapsed:.2f} signals/sec")
print(f"Avg time: {elapsed/stats['signals_processed']*1e6:.2f} µs/signal")
'''
            test_script.write_text(test_code)
            
            # Run test
            test_result = subprocess.run(
                [sys.executable, "test_python.py"],
                cwd=output_dir,
                capture_output=True,
                text=True
            )
            
            result["import"] = "BitActor instantiated" in test_result.stdout
            result["signal_processing"] = "Processed" in test_result.stdout
            result["instantiation"] = result["import"]
            
            # Parse performance
            if result["signal_processing"]:
                output = test_result.stdout
                for line in output.split('\n'):
                    if "Throughput:" in line:
                        result["performance"]["throughput"] = line.split(':')[1].strip()
                    elif "Avg time:" in line:
                        result["performance"]["latency"] = line.split(':')[1].strip()
            
            # Cleanup
            test_script.unlink()
        
        return result
    
    def run_erlang_tests(self, output_dir: Path) -> Dict[str, Any]:
        """Run Erlang tests"""
        result = {
            "language": "Erlang",
            "compile": False,
            "start": False,
            "signal_processing": False,
            "performance": {}
        }
        
        with console.status("[bold green]Running Erlang tests...") as status:
            status.update("Testing Erlang module...")
            
            # Find the Erlang module file
            erl_files = list(output_dir.glob("*_bitactor.erl"))
            if not erl_files:
                result["compile"] = False
                return result
            
            module_name = erl_files[0].stem  # Get filename without extension
            
            # Create test script
            test_script = output_dir / "test_erlang.escript"
            test_code = f'''#!/usr/bin/env escript
%%! -pa .

main(_) ->
    % Compile the module
    case compile:file("{module_name}", [binary, return_errors]) of
        {{ok, _, Binary}} ->
            code:load_binary({module_name}, "{module_name}.beam", Binary),
            io:format("✅ Module compiled~n"),
            
            % Start the server
            {{ok, Pid}} = {module_name}:start_link(),
            io:format("✅ Server started: ~p~n", [Pid]),
            
            % Send signals
            Start = erlang:monotonic_time(),
            send_signals(100000),
            End = erlang:monotonic_time(),
            
            % Get stats
            Stats = {module_name}:get_stats(),
            Count = maps:get(signals_processed, Stats, 0),
            ElapsedMs = erlang:convert_time_unit(End - Start, native, millisecond),
            
            io:format("✅ Processed ~p signals~n", [Count]),
            io:format("Time: ~p ms~n", [ElapsedMs]),
            io:format("Throughput: ~.2f signals/sec~n", [Count * 1000 / ElapsedMs]),
            
            {module_name}:stop();
        {{error, Errors, _}} ->
            io:format("❌ Compilation failed: ~p~n", [Errors])
    end.

send_signals(0) -> ok;
send_signals(N) ->
    {module_name}:send_signal(semanticsignal, N),
    send_signals(N - 1).
'''
            test_script.write_text(test_code)
            test_script.chmod(0o755)
            
            # Check if escript is available
            escript_check = subprocess.run(
                ["which", "escript"],
                capture_output=True,
                text=True
            )
            
            if escript_check.returncode != 0:
                console.print("[yellow]⚠️ Erlang escript not found, skipping Erlang tests[/yellow]")
                result["compile"] = False
                result["error"] = "escript not available"
                test_script.unlink()
                return result
            
            # Run test
            test_result = subprocess.run(
                ["escript", "test_erlang.escript"],
                cwd=output_dir,
                capture_output=True,
                text=True,
                timeout=10  # 10 second timeout
            )
            
            output = test_result.stdout
            result["compile"] = "Module compiled" in output
            result["start"] = "Server started" in output
            result["signal_processing"] = "Processed" in output and "signals" in output
            
            # Parse performance
            if result["signal_processing"]:
                for line in output.split('\n'):
                    if "Throughput:" in line:
                        result["performance"]["throughput"] = line.split(':')[1].strip()
            
            # Cleanup
            test_script.unlink()
        
        return result
    
    def display_results(self, results: Dict[str, Dict[str, Any]]):
        """Display test results in a nice table"""
        table = Table(title="🔍 BitActor Self-Check Results")
        
        table.add_column("Language", style="cyan", no_wrap=True)
        table.add_column("Build/Compile", style="green")
        table.add_column("Tests", style="green")
        table.add_column("Performance", style="yellow")
        table.add_column("Status", style="bold")
        
        for lang, result in results.items():
            if lang == "C":
                build = "✅" if result.get("build") else "❌"
                tests = "✅" if result.get("tests") else "❌"
            elif lang == "Python":
                build = "✅" if result.get("import") else "❌"
                tests = "✅" if result.get("signal_processing") else "❌"
            elif lang == "Erlang":
                build = "✅" if result.get("compile") else "❌"
                tests = "✅" if result.get("signal_processing") else "❌"
            
            perf = result.get("performance", {})
            perf_str = ""
            if "throughput" in perf:
                perf_str = f"{perf['throughput']}"
            if "latency" in perf:
                perf_str += f"\n{perf['latency']}"
            
            all_good = all([
                result.get("build", result.get("compile", result.get("import"))),
                result.get("tests", result.get("signal_processing"))
            ])
            status = "[green]✅ PASS[/green]" if all_good else "[red]❌ FAIL[/red]"
            
            table.add_row(lang, build, tests, perf_str or "N/A", status)
        
        console.print(table)

cli = BitActorCLI()

@app.command()
def generate(
    ttl_file: Path = typer.Argument(
        ...,
        exists=True,
        help="TTL ontology file to generate from"
    ),
    output_dir: Path = typer.Argument(
        ...,
        help="Output directory for generated code"
    ),
    prefix: str = typer.Argument(
        ...,
        help="Prefix for generated code (e.g., 'semantic')"
    ),
    validate: bool = typer.Option(
        True,
        "--validate/--no-validate",
        help="Validate TTL before generation"
    )
):
    """🚀 Generate BitActor implementations from TTL ontology"""
    
    console.print(Panel.fit(
        f"[bold cyan]BitActor Code Generation[/bold cyan]\n"
        f"TTL: {ttl_file}\n"
        f"Output: {output_dir}\n"
        f"Prefix: {prefix}",
        title="🔧 Configuration"
    ))
    
    # Validate TTL
    if validate:
        with console.status("[bold green]Validating TTL..."):
            if not cli.validate_ttl(ttl_file):
                raise typer.Exit(1)
        console.print("[green]✅ TTL validation passed[/green]")
    
    # Generate code
    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console
    ) as progress:
        task = progress.add_task("Generating code...", total=None)
        
        try:
            cli.generator.generate_all(str(ttl_file), str(output_dir), prefix)
            progress.update(task, completed=True)
            console.print(f"[green]✅ Code generation complete![/green]")
            console.print(f"[dim]Output directory: {output_dir}[/dim]")
        except Exception as e:
            console.print(f"[red]❌ Generation failed: {e}[/red]")
            raise typer.Exit(1)

@app.command()
def self_check(
    output_dir: Path = typer.Argument(
        ...,
        exists=True,
        help="Directory containing generated BitActor code"
    ),
    languages: Optional[List[str]] = typer.Option(
        None,
        "--lang", "-l",
        help="Languages to test (default: all)"
    )
):
    """🔍 Run self-check on generated BitActor code"""
    
    if not languages:
        languages = ["c", "python", "erlang"]
    
    console.print(Panel.fit(
        f"[bold cyan]BitActor Self-Check[/bold cyan]\n"
        f"Directory: {output_dir}\n"
        f"Languages: {', '.join(languages)}",
        title="🔍 Self-Check Configuration"
    ))
    
    results = {}
    
    # Run tests for each language
    if "c" in languages:
        results["C"] = cli.run_c_tests(output_dir)
    
    if "python" in languages:
        results["Python"] = cli.run_python_tests(output_dir)
    
    if "erlang" in languages:
        results["Erlang"] = cli.run_erlang_tests(output_dir)
    
    # Display results
    cli.display_results(results)
    
    # Save results
    results_file = output_dir / "self_check_results.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    console.print(f"\n[dim]Results saved to: {results_file}[/dim]")
    
    # Exit code based on results
    all_passed = all(
        all([
            r.get("build", r.get("compile", r.get("import"))),
            r.get("tests", r.get("signal_processing"))
        ])
        for r in results.values()
    )
    
    if not all_passed:
        raise typer.Exit(1)

@app.command()
def validate(
    ttl_file: Path = typer.Argument(
        ...,
        exists=True,
        help="TTL file to validate"
    )
):
    """✅ Validate a TTL ontology file"""
    
    console.print(f"[bold]Validating:[/bold] {ttl_file}")
    
    if cli.validate_ttl(ttl_file):
        # Load and show some stats
        g = rdflib.Graph()
        g.parse(ttl_file, format='turtle')
        
        console.print(f"[green]✅ Valid TTL file[/green]")
        console.print(f"[dim]Triples: {len(g)}[/dim]")
        
        # Count signal types
        BA = rdflib.Namespace("http://bitactor.org/ontology#")
        signals = list(g.subjects(rdflib.RDFS.subClassOf, BA.Signal))
        console.print(f"[dim]Signals: {len(signals)}[/dim]")
    else:
        raise typer.Exit(1)

@app.command()
def list_signals(
    ttl_file: Path = typer.Argument(
        ...,
        exists=True,
        help="TTL file to analyze"
    )
):
    """📋 List signals defined in TTL ontology"""
    
    g = rdflib.Graph()
    g.parse(ttl_file, format='turtle')
    
    BA = rdflib.Namespace("http://bitactor.org/ontology#")
    
    table = Table(title="📡 BitActor Signals")
    table.add_column("Signal", style="cyan")
    table.add_column("Label", style="green")
    table.add_column("Description", style="dim")
    
    for signal in g.subjects(rdflib.RDFS.subClassOf, BA.Signal):
        name = str(signal).split('#')[-1]
        label = g.value(signal, rdflib.RDFS.label, default=name)
        desc = g.value(signal, rdflib.RDFS.comment, default="")
        table.add_row(name, str(label), str(desc))
    
    console.print(table)

@app.command()
def full_cycle(
    ttl_file: Path = typer.Argument(
        ...,
        exists=True,
        help="TTL ontology file"
    ),
    output_dir: Path = typer.Argument(
        ...,
        help="Output directory"
    ),
    prefix: str = typer.Argument(
        ...,
        help="Code prefix"
    )
):
    """🔄 Run full cycle: generate + self-check"""
    
    console.print(Panel.fit(
        "[bold cyan]BitActor Full Cycle[/bold cyan]\n"
        "1. Generate code from TTL\n"
        "2. Run self-check on all languages",
        title="🔄 Full Cycle"
    ))
    
    # Generate
    generate(ttl_file, output_dir, prefix, validate=True)
    
    console.print("\n" + "="*50 + "\n")
    
    # Self-check
    self_check(output_dir, languages=None)

def version_callback(value: bool):
    if value:
        console.print("[bold cyan]BitActor CLI[/bold cyan] version 1.0.0")
        console.print("🚀 Ultra-fast signal processing from semantic ontologies")
        raise typer.Exit()

@app.callback()
def main(
    version: Optional[bool] = typer.Option(
        None,
        "--version", "-v",
        callback=version_callback,
        is_eager=True,
        help="Show version"
    )
):
    """
    🚀 BitActor CLI - Generate ultra-fast signal processing from TTL ontologies
    
    Examples:
        bitactor generate ontology.ttl output/ myprefix
        bitactor self-check output/
        bitactor full-cycle ontology.ttl output/ myprefix
    """
    pass

if __name__ == "__main__":
    app()