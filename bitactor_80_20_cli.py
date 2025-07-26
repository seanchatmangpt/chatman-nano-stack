#!/usr/bin/env python3
"""
⚡ BitActor 80/20 Management CLI
Specialized CLI for high-performance BitActor component management
"""

import typer
import subprocess
import json
import os
import time
from pathlib import Path
from typing import Optional, List
from rich.console import Console
from rich.table import Table
from rich.panel import Panel

app = typer.Typer(name="bitactor", help="⚡ BitActor 80/20 CLI")
console = Console()

COMPILE_PROFILES = {
    "debug": {"cflags": "-O0 -g -DDEBUG", "description": "Debug build"},
    "release": {"cflags": "-O3 -DNDEBUG -march=native", "description": "Release build"},
    "ultra": {"cflags": "-O3 -DNDEBUG -march=native -flto", "description": "Ultra-optimized"}
}

@app.command("build")
def build(
    profile: str = typer.Option("release", help="Build profile: debug/release/ultra"),
    target: str = typer.Option("all", help="Target: all/core/forex"),
    jobs: int = typer.Option(-1, help="Parallel jobs")
):
    """🔨 Build BitActor components"""
    if profile not in COMPILE_PROFILES:
        console.print(f"❌ Unknown profile: {profile}", style="bold red")
        raise typer.Exit(1)
    
    console.print(Panel(f"🔨 Building {target} ({profile})", style="bold green"))
    
    if jobs == -1:
        jobs = os.cpu_count() or 4
    
    env = {**os.environ, "CFLAGS": COMPILE_PROFILES[profile]["cflags"]}
    cmd = ["make", f"-j{jobs}"]
    
    if target != "all":
        cmd.append(target)
    
    with console.status(f"[bold green]Building..."):
        result = subprocess.run(cmd, env=env)
    
    if result.returncode == 0:
        console.print("✅ Build completed", style="bold green")
    else:
        console.print("❌ Build failed", style="bold red")
        raise typer.Exit(1)

@app.command("test")
def test(
    suite: str = typer.Option("critical", help="Suite: critical/all/unit"),
    coverage: bool = typer.Option(True, help="Generate coverage")
):
    """🧪 Run tests"""
    console.print(Panel(f"🧪 Running {suite} tests", style="bold blue"))
    
    test_patterns = {
        "critical": ["test_bitactor_core*.c", "test_sparql*.c"],
        "unit": ["test_*_unit.c"],
        "all": ["test_*.c"]
    }
    
    patterns = test_patterns.get(suite, ["test_*.c"])
    passed = failed = 0
    
    for pattern in patterns:
        test_files = list(Path("tests").glob(pattern))
        for test_file in test_files:
            if not test_file.exists():
                continue
            
            binary = test_file.stem
            compile_result = subprocess.run([
                "gcc", str(test_file), "-o", binary, "-I.", "-lm"
            ], cwd="tests", capture_output=True)
            
            if compile_result.returncode != 0:
                failed += 1
                continue
            
            run_result = subprocess.run([f"./{binary}"], cwd="tests", capture_output=True)
            if run_result.returncode == 0:
                passed += 1
            else:
                failed += 1
    
    console.print(f"✅ Passed: {passed}, ❌ Failed: {failed}")
    if failed > 0:
        raise typer.Exit(1)

@app.command("benchmark")
def benchmark(
    type: str = typer.Option("performance", help="Type: performance/latency"),
    iterations: int = typer.Option(1000, help="Iterations")
):
    """📊 Run benchmarks"""
    console.print(Panel(f"📊 {type} benchmark", style="bold yellow"))
    
    binary_map = {
        "performance": "benchmark_v8_system",
        "latency": "test_tick_parallel_bdd"
    }
    
    binary = binary_map.get(type, "benchmark_v8_system")
    
    if not Path(binary).exists():
        console.print(f"❌ Binary not found: {binary}")
        raise typer.Exit(1)
    
    with console.status("[bold yellow]Running benchmark..."):
        start = time.time()
        result = subprocess.run([f"./{binary}", str(iterations)])
        duration = time.time() - start
    
    if result.returncode == 0:
        console.print(f"✅ Completed in {duration:.2f}s")
    else:
        console.print("❌ Benchmark failed", style="bold red")

@app.command("status")
def status():
    """🎯 Component status"""
    console.print(Panel("🎯 BitActor Status", style="bold blue"))
    
    components = {
        "Core": Path("src/bitactor_core.c").exists(),
        "Forex": Path("forex/forex_engine.c").exists(),
        "Tests": len(list(Path("tests").glob("test_*.c"))) > 0 if Path("tests").exists() else False
    }
    
    table = Table()
    table.add_column("Component", style="cyan")
    table.add_column("Status", style="green")
    
    for name, ready in components.items():
        status = "✅ Ready" if ready else "❌ Missing"
        table.add_row(name, status)
    
    console.print(table)

if __name__ == "__main__":
    app()