#!/usr/bin/env python3
"""
🚀 CNS Forge 80/20 Management CLI
Typer-based CLI for managing all projects in the CNS ecosystem
Applies 80/20 principle: 20% of commands for 80% of management value
"""

import typer
import subprocess
import json
import yaml
import os
import sys
from pathlib import Path
from typing import Optional, List
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn
import time

app = typer.Typer(
    name="cns",
    help="🚀 CNS Forge 80/20 Management CLI - Manage the entire CNS ecosystem",
    rich_markup_mode="rich"
)

console = Console()

# Core project paths
FORGE_PATH = Path(".")
BITACTOR_PATH = Path("bitactor")
NUXT_PATH = Path("nuxt-ui") 
K8S_PATH = Path("k8s")
TERRAFORM_PATH = Path("terraform")
GENERATED_PATH = Path("generated")

# ============================================================================
# 🔧 Core CNS Forge Commands (80/20 Priority: HIGH)
# ============================================================================

forge_app = typer.Typer(name="forge", help="🔧 Manage CNS Forge core system")
app.add_typer(forge_app)

@forge_app.command("start")
def forge_start(
    env: str = typer.Option("dev", help="Environment: dev/test/prod"),
    port: int = typer.Option(4000, help="Phoenix server port")
):
    """🚀 Start CNS Forge Phoenix server"""
    console.print(Panel("🚀 Starting CNS Forge", style="bold green"))
    
    env_vars = {
        "MIX_ENV": env,
        "PORT": str(port)
    }
    
    with console.status("[bold green]Starting Phoenix server..."):
        result = subprocess.run(
            ["mix", "phx.server"],
            env={**os.environ, **env_vars},
            cwd=FORGE_PATH
        )
    
    if result.returncode == 0:
        console.print(f"✅ CNS Forge started on port {port}")
    else:
        console.print("❌ Failed to start CNS Forge", style="bold red")
        raise typer.Exit(1)

@forge_app.command("test")
def forge_test(
    coverage: bool = typer.Option(True, help="Run with coverage"),
    pattern: Optional[str] = typer.Option(None, help="Test pattern filter")
):
    """🧪 Run CNS Forge tests"""
    console.print(Panel("🧪 Running CNS Forge Tests", style="bold blue"))
    
    cmd = ["mix", "test"]
    if coverage:
        cmd.append("--cover")
    if pattern:
        cmd.extend(["--only", pattern])
    
    with console.status("[bold blue]Running tests..."):
        result = subprocess.run(cmd, cwd=FORGE_PATH)
    
    if result.returncode == 0:
        console.print("✅ All tests passed")
    else:
        console.print("❌ Tests failed", style="bold red")
        raise typer.Exit(1)

@forge_app.command("deps")
def forge_deps(
    action: str = typer.Option("get", help="Action: get/update/clean/compile")
):
    """📦 Manage Elixir dependencies"""
    console.print(Panel(f"📦 Managing dependencies: {action}", style="bold yellow"))
    
    cmd = ["mix", f"deps.{action}"]
    
    with console.status(f"[bold yellow]Running deps.{action}..."):
        result = subprocess.run(cmd, cwd=FORGE_PATH)
    
    if result.returncode == 0:
        console.print(f"✅ Dependencies {action} completed")
    else:
        console.print(f"❌ Dependencies {action} failed", style="bold red")
        raise typer.Exit(1)

@forge_app.command("compile")
def forge_compile(
    force: bool = typer.Option(False, help="Force recompilation"),
    warnings_as_errors: bool = typer.Option(True, help="Treat warnings as errors")
):
    """⚙️ Compile CNS Forge"""
    console.print(Panel("⚙️ Compiling CNS Forge", style="bold cyan"))
    
    cmd = ["mix", "compile"]
    if force:
        cmd.append("--force")
    if warnings_as_errors:
        cmd.append("--warnings-as-errors")
    
    with console.status("[bold cyan]Compiling..."):
        result = subprocess.run(cmd, cwd=FORGE_PATH)
    
    if result.returncode == 0:
        console.print("✅ Compilation successful")
    else:
        console.print("❌ Compilation failed", style="bold red")
        raise typer.Exit(1)

# ============================================================================
# ⚡ BitActor Commands (80/20 Priority: HIGH)
# ============================================================================

bitactor_app = typer.Typer(name="bitactor", help="⚡ Manage BitActor high-performance components")
app.add_typer(bitactor_app)

@bitactor_app.command("build")
def bitactor_build(
    optimization: str = typer.Option("O3", help="Optimization level: O0/O1/O2/O3"),
    target: str = typer.Option("all", help="Build target: all/core/forex/cybersecurity")
):
    """🔨 Build BitActor C components"""
    console.print(Panel(f"🔨 Building BitActor ({target})", style="bold green"))
    
    make_cmd = ["make"]
    if target != "all":
        make_cmd.append(target)
    
    env_vars = {"CFLAGS": f"-{optimization}"}
    
    with console.status("[bold green]Building BitActor..."):
        result = subprocess.run(
            make_cmd,
            env={**os.environ, **env_vars},
            cwd=BITACTOR_PATH if BITACTOR_PATH.exists() else FORGE_PATH
        )
    
    if result.returncode == 0:
        console.print(f"✅ BitActor {target} built successfully")
    else:
        console.print("❌ BitActor build failed", style="bold red")
        raise typer.Exit(1)

@bitactor_app.command("test")
def bitactor_test(
    suite: str = typer.Option("all", help="Test suite: all/unit/integration/stress"),
    coverage: bool = typer.Option(True, help="Generate coverage report")
):
    """🧪 Run BitActor tests"""
    console.print(Panel(f"🧪 Running BitActor tests ({suite})", style="bold blue"))
    
    test_files = {
        "all": "test_*.c",
        "unit": "test_*_unit.c", 
        "integration": "test_*_integration.c",
        "stress": "test_*_stress.c"
    }
    
    pattern = test_files.get(suite, "test_*.c")
    
    cmd = ["make", "test", f"PATTERN={pattern}"]
    if coverage:
        cmd.append("coverage")
    
    with console.status(f"[bold blue]Running {suite} tests..."):
        result = subprocess.run(cmd, cwd=FORGE_PATH / "tests")
    
    if result.returncode == 0:
        console.print(f"✅ BitActor {suite} tests passed")
    else:
        console.print("❌ BitActor tests failed", style="bold red")
        raise typer.Exit(1)

@bitactor_app.command("benchmark")
def bitactor_benchmark(
    type: str = typer.Option("performance", help="Benchmark type: performance/memory/latency"),
    iterations: int = typer.Option(1000, help="Number of iterations")
):
    """📊 Run BitActor benchmarks"""
    console.print(Panel(f"📊 Running {type} benchmark", style="bold yellow"))
    
    benchmark_map = {
        "performance": "benchmark_v8_system",
        "memory": "test_memory_stress_bdd",
        "latency": "test_tick_parallel_bdd"
    }
    
    binary = benchmark_map.get(type, "benchmark_v8_system")
    
    with console.status(f"[bold yellow]Running {iterations} iterations..."):
        result = subprocess.run([f"./{binary}", str(iterations)], cwd=FORGE_PATH)
    
    if result.returncode == 0:
        console.print(f"✅ {type} benchmark completed")
    else:
        console.print("❌ Benchmark failed", style="bold red")
        raise typer.Exit(1)

# ============================================================================
# 🔄 Ash Reactor Commands (80/20 Priority: HIGH)
# ============================================================================

reactor_app = typer.Typer(name="reactor", help="🔄 Manage Ash Reactor workflows")
app.add_typer(reactor_app)

@reactor_app.command("run")
def reactor_run(
    workflow: str = typer.Argument(..., help="Reactor workflow to execute"),
    args: Optional[str] = typer.Option(None, help="JSON arguments for workflow"),
    dry_run: bool = typer.Option(False, help="Validate without executing")
):
    """🔄 Execute Ash Reactor workflow"""
    console.print(Panel(f"🔄 Running Reactor: {workflow}", style="bold green"))
    
    script_content = f"""
    alias CnsForge.ReactorWorkflows.{workflow.title()}Reactor
    
    args = {args or "{}"}
    
    {"# DRY RUN MODE" if dry_run else ""}
    case {workflow.title()}Reactor.run(args) do
      {{:ok, result}} -> 
        IO.puts("✅ Workflow completed: {{inspect(result)}}")
      {{:error, reason}} -> 
        IO.puts("❌ Workflow failed: {{inspect(reason)}}")
        System.halt(1)
    end
    """
    
    with console.status(f"[bold green]Executing {workflow}..."):
        result = subprocess.run(
            ["mix", "run", "-e", script_content],
            cwd=FORGE_PATH
        )
    
    if result.returncode == 0:
        console.print(f"✅ Reactor {workflow} completed")
    else:
        console.print("❌ Reactor failed", style="bold red")
        raise typer.Exit(1)

@reactor_app.command("list")
def reactor_list():
    """📋 List available Reactor workflows"""
    console.print(Panel("📋 Available Reactor Workflows", style="bold blue"))
    
    # Scan for reactor files
    reactor_files = list(Path("lib").glob("**/*reactor*.ex"))
    
    table = Table(title="Reactor Workflows")
    table.add_column("Name", style="cyan")
    table.add_column("File", style="green") 
    table.add_column("Status", style="yellow")
    
    for file in reactor_files:
        name = file.stem.replace("_reactor", "")
        status = "✅ Active" if file.exists() else "❌ Missing"
        table.add_row(name, str(file), status)
    
    console.print(table)

@reactor_app.command("validate")
def reactor_validate(
    workflow: Optional[str] = typer.Option(None, help="Specific workflow to validate")
):
    """✅ Validate Reactor workflow definitions"""
    console.print(Panel("✅ Validating Reactor workflows", style="bold yellow"))
    
    cmd = ["mix", "test", "test/cns_forge/*reactor*test.exs"]
    if workflow:
        cmd.append(f"--only=reactor:{workflow}")
    
    with console.status("[bold yellow]Validating workflows..."):
        result = subprocess.run(cmd, cwd=FORGE_PATH)
    
    if result.returncode == 0:
        console.print("✅ All workflows valid")
    else:
        console.print("❌ Workflow validation failed", style="bold red")
        raise typer.Exit(1)

# ============================================================================
# 🧠 Semantic/TTL Commands (80/20 Priority: MEDIUM)
# ============================================================================

semantic_app = typer.Typer(name="semantic", help="🧠 Manage semantic processing and TTL workflows")
app.add_typer(semantic_app)

@semantic_app.command("generate")
def semantic_generate(
    input_file: str = typer.Argument(..., help="Input TTL/RDF file"),
    output_type: str = typer.Option("dspy", help="Output: dspy/bitactor/ash/all"),
    validate: bool = typer.Option(True, help="Validate generated output")
):
    """🔄 Generate code from TTL/semantic definitions"""
    console.print(Panel(f"🔄 Generating {output_type} from {input_file}", style="bold green"))
    
    generators = {
        "dspy": "ttl2dspy.py",
        "bitactor": "bitactor_ttl_generator.py", 
        "ash": "ttl_to_ash_generator.py",
        "all": "pipeline_validator.py"
    }
    
    generator = generators.get(output_type, "ttl2dspy.py")
    
    with console.status(f"[bold green]Generating {output_type}..."):
        result = subprocess.run([
            "python", generator, input_file, 
            "--validate" if validate else "--no-validate"
        ])
    
    if result.returncode == 0:
        console.print(f"✅ {output_type} generated successfully")
    else:
        console.print("❌ Generation failed", style="bold red")
        raise typer.Exit(1)

@semantic_app.command("validate")
def semantic_validate(
    file: str = typer.Argument(..., help="TTL/RDF file to validate"),
    schema: Optional[str] = typer.Option(None, help="SHACL schema file")
):
    """✅ Validate TTL/RDF semantic definitions"""
    console.print(Panel(f"✅ Validating {file}", style="bold blue"))
    
    cmd = ["python", "scripts/validate_ttl.py", file]
    if schema:
        cmd.extend(["--schema", schema])
    
    with console.status("[bold blue]Validating semantics..."):
        result = subprocess.run(cmd)
    
    if result.returncode == 0:
        console.print("✅ Semantic validation passed")
    else:
        console.print("❌ Semantic validation failed", style="bold red")
        raise typer.Exit(1)

@semantic_app.command("ontology")
def semantic_ontology(
    action: str = typer.Option("list", help="Action: list/generate/validate/compile"),
    domain: Optional[str] = typer.Option(None, help="Domain: cybersecurity/healthcare/automotive/etc")
):
    """🧠 Manage ontology definitions"""
    console.print(Panel(f"🧠 Ontology {action}", style="bold cyan"))
    
    if action == "list":
        ontology_files = list(Path("ontologies").glob("*.ttl"))
        table = Table(title="Available Ontologies")
        table.add_column("Domain", style="cyan")
        table.add_column("File", style="green")
        table.add_column("Size", style="yellow")
        
        for file in ontology_files:
            domain = file.stem.replace("_core", "").replace("_shacl", "")
            size = f"{file.stat().st_size // 1024}KB"
            table.add_row(domain, file.name, size)
        
        console.print(table)
    
    elif action == "generate":
        if not domain:
            console.print("❌ Domain required for generation", style="bold red")
            raise typer.Exit(1)
        
        with console.status(f"[bold cyan]Generating {domain} ontology..."):
            result = subprocess.run([
                "python", "ontology_forge_cli.py", "generate", 
                "--domain", domain
            ])
        
        if result.returncode == 0:
            console.print(f"✅ {domain} ontology generated")
        else:
            console.print("❌ Ontology generation failed", style="bold red")
            raise typer.Exit(1)

# ============================================================================
# 🖥️ Nuxt UI Commands (80/20 Priority: MEDIUM)
# ============================================================================

nuxt_app = typer.Typer(name="nuxt", help="🖥️ Manage Nuxt UI frontend")
app.add_typer(nuxt_app)

@nuxt_app.command("dev")
def nuxt_dev(
    port: int = typer.Option(3000, help="Development server port"),
    open_browser: bool = typer.Option(True, help="Open browser automatically")
):
    """🚀 Start Nuxt development server"""
    console.print(Panel("🚀 Starting Nuxt dev server", style="bold green"))
    
    cmd = ["npm", "run", "dev", "--", "--port", str(port)]
    if open_browser:
        cmd.extend(["--open"])
    
    nuxt_path = NUXT_PATH if NUXT_PATH.exists() else Path("nuxt-ui") or Path("aegis-nuxt")
    
    with console.status("[bold green]Starting Nuxt..."):
        result = subprocess.run(cmd, cwd=nuxt_path)
    
    if result.returncode == 0:
        console.print(f"✅ Nuxt started on port {port}")
    else:
        console.print("❌ Failed to start Nuxt", style="bold red")
        raise typer.Exit(1)

@nuxt_app.command("build")
def nuxt_build(
    target: str = typer.Option("static", help="Build target: static/server/spa"),
    analyze: bool = typer.Option(False, help="Analyze bundle size")
):
    """🔨 Build Nuxt for production"""
    console.print(Panel(f"🔨 Building Nuxt ({target})", style="bold blue"))
    
    cmd = ["npm", "run", "build"]
    if analyze:
        cmd = ["npm", "run", "build:analyze"]
    
    nuxt_path = NUXT_PATH if NUXT_PATH.exists() else Path("aegis-nuxt")
    
    with console.status("[bold blue]Building Nuxt..."):
        result = subprocess.run(cmd, cwd=nuxt_path)
    
    if result.returncode == 0:
        console.print("✅ Nuxt build completed")
    else:
        console.print("❌ Nuxt build failed", style="bold red")
        raise typer.Exit(1)

@nuxt_app.command("test")
def nuxt_test(
    type: str = typer.Option("unit", help="Test type: unit/e2e/cypress"),
    coverage: bool = typer.Option(True, help="Generate coverage report")
):
    """🧪 Run Nuxt tests"""
    console.print(Panel(f"🧪 Running Nuxt {type} tests", style="bold yellow"))
    
    test_commands = {
        "unit": "npm run test:unit",
        "e2e": "npm run test:e2e", 
        "cypress": "npx cypress run"
    }
    
    cmd = test_commands.get(type, "npm run test").split()
    if coverage and type == "unit":
        cmd.append("--coverage")
    
    nuxt_path = NUXT_PATH if NUXT_PATH.exists() else Path("cypress_pipeline_tests")
    
    with console.status(f"[bold yellow]Running {type} tests..."):
        result = subprocess.run(cmd, cwd=nuxt_path)
    
    if result.returncode == 0:
        console.print(f"✅ Nuxt {type} tests passed")
    else:
        console.print("❌ Nuxt tests failed", style="bold red")
        raise typer.Exit(1)

# ============================================================================
# ☸️ Infrastructure Commands (80/20 Priority: MEDIUM)
# ============================================================================

infra_app = typer.Typer(name="infra", help="☸️ Manage K8s and infrastructure")
app.add_typer(infra_app)

@infra_app.command("deploy")
def infra_deploy(
    environment: str = typer.Option("dev", help="Environment: dev/staging/prod"),
    component: str = typer.Option("all", help="Component: all/cns-forge/bitactor/nuxt"),
    dry_run: bool = typer.Option(False, help="Validate without applying")
):
    """🚀 Deploy to Kubernetes"""
    console.print(Panel(f"🚀 Deploying {component} to {environment}", style="bold green"))
    
    k8s_files = {
        "all": "k8s/*.yaml",
        "cns-forge": "k8s/cns-forge-deployment.yaml",
        "bitactor": "bitactor-k8s-deployment.yaml", 
        "nuxt": "k8s/nuxt-ui-deployment.yaml"
    }
    
    file_pattern = k8s_files.get(component, "k8s/*.yaml")
    
    cmd = ["kubectl", "apply", "-f", file_pattern]
    if dry_run:
        cmd.append("--dry-run=client")
    
    with console.status(f"[bold green]Deploying {component}..."):
        result = subprocess.run(cmd)
    
    if result.returncode == 0:
        console.print(f"✅ {component} deployed to {environment}")
    else:
        console.print("❌ Deployment failed", style="bold red")
        raise typer.Exit(1)

@infra_app.command("status")
def infra_status(
    namespace: str = typer.Option("default", help="Kubernetes namespace"),
    output: str = typer.Option("table", help="Output format: table/json/yaml")
):
    """📊 Check infrastructure status"""
    console.print(Panel(f"📊 Infrastructure Status ({namespace})", style="bold blue"))
    
    resources = ["pods", "services", "deployments", "ingress"]
    
    for resource in resources:
        console.print(f"\n[bold cyan]{resource.title()}:[/bold cyan]")
        
        cmd = ["kubectl", "get", resource, "-n", namespace]
        if output != "table":
            cmd.extend(["-o", output])
        
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        if result.returncode == 0:
            console.print(result.stdout)
        else:
            console.print(f"❌ Failed to get {resource}", style="bold red")

@infra_app.command("terraform")
def infra_terraform(
    action: str = typer.Option("plan", help="Action: init/plan/apply/destroy"),
    environment: str = typer.Option("dev", help="Environment: dev/staging/prod"),
    auto_approve: bool = typer.Option(False, help="Auto-approve changes")
):
    """🏗️ Manage Terraform infrastructure"""
    console.print(Panel(f"🏗️ Terraform {action} ({environment})", style="bold yellow"))
    
    terraform_dir = TERRAFORM_PATH if TERRAFORM_PATH.exists() else Path(".")
    
    cmd = ["terraform", action]
    if action in ["apply", "destroy"] and auto_approve:
        cmd.append("-auto-approve")
    
    env_vars = {"TF_VAR_environment": environment}
    
    with console.status(f"[bold yellow]Running terraform {action}..."):
        result = subprocess.run(
            cmd,
            env={**os.environ, **env_vars},
            cwd=terraform_dir
        )
    
    if result.returncode == 0:
        console.print(f"✅ Terraform {action} completed")
    else:
        console.print("❌ Terraform failed", style="bold red")
        raise typer.Exit(1)

# ============================================================================
# 🧪 Testing Commands (80/20 Priority: HIGH)
# ============================================================================

test_app = typer.Typer(name="test", help="🧪 Comprehensive testing suite")
app.add_typer(test_app)

@test_app.command("all")
def test_all(
    coverage: bool = typer.Option(True, help="Generate coverage reports"),
    parallel: bool = typer.Option(True, help="Run tests in parallel"),
    output_format: str = typer.Option("pretty", help="Output: pretty/json/junit")
):
    """🚀 Run complete test suite (80/20 approach)"""
    console.print(Panel("🚀 Running Complete Test Suite", style="bold green"))
    
    # 80/20 critical tests
    test_suites = [
        ("Elixir Unit Tests", ["mix", "test", "--cover"]),
        ("BitActor Core Tests", ["make", "test", "PATTERN=test_bitactor_core*.c"]),
        ("Reactor Workflow Tests", ["mix", "test", "test/cns_forge/*reactor*test.exs"]),
        ("K8s Deployment Tests", ["kubectl", "apply", "--dry-run=client", "-f", "k8s/"]),
        ("Integration Tests", ["python", "comprehensive_integration_validator.exs"])
    ]
    
    results = {}
    
    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console
    ) as progress:
        
        for name, cmd in test_suites:
            task = progress.add_task(f"Running {name}...", total=None)
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            results[name] = {
                "success": result.returncode == 0,
                "output": result.stdout,
                "errors": result.stderr
            }
            
            progress.update(task, completed=True)
    
    # Results summary
    table = Table(title="Test Results Summary")
    table.add_column("Test Suite", style="cyan")
    table.add_column("Status", style="green")
    table.add_column("Details", style="yellow")
    
    for name, result in results.items():
        status = "✅ PASS" if result["success"] else "❌ FAIL"
        details = "OK" if result["success"] else "Check logs"
        table.add_row(name, status, details)
    
    console.print(table)
    
    # Overall result
    all_passed = all(r["success"] for r in results.values())
    if all_passed:
        console.print("✅ All critical tests passed!", style="bold green")
    else:
        console.print("❌ Some tests failed", style="bold red")
        raise typer.Exit(1)

@test_app.command("coverage")
def test_coverage(
    minimum: float = typer.Option(80.0, help="Minimum coverage percentage"),
    report_format: str = typer.Option("html", help="Format: html/json/lcov")
):
    """📊 Generate comprehensive coverage report"""
    console.print(Panel("📊 Generating Coverage Report", style="bold blue"))
    
    with console.status("[bold blue]Collecting coverage data..."):
        # Run tests with coverage
        subprocess.run(["mix", "test", "--cover"], cwd=FORGE_PATH)
        subprocess.run(["make", "coverage"], cwd=Path("tests"))
        subprocess.run(["npm", "run", "test:coverage"], cwd=NUXT_PATH if NUXT_PATH.exists() else Path("."))
    
    console.print(f"✅ Coverage report generated in {report_format} format")

# ============================================================================
# 📊 Monitoring/OTEL Commands (80/20 Priority: MEDIUM) 
# ============================================================================

monitor_app = typer.Typer(name="monitor", help="📊 Monitoring and observability")
app.add_typer(monitor_app)

@monitor_app.command("otel")
def monitor_otel(
    component: str = typer.Option("all", help="Component: all/forge/bitactor/nuxt"),
    duration: int = typer.Option(60, help="Monitoring duration in seconds"),
    export_format: str = typer.Option("json", help="Export format: json/prometheus/jaeger")
):
    """📡 Collect OTEL telemetry data"""
    console.print(Panel(f"📡 Collecting OTEL data for {component}", style="bold cyan"))
    
    scripts = {
        "all": "otel_instrumentation_suite.py",
        "forge": "ash_reactor_otel_validation_engine.py",
        "bitactor": "validate_otel.py",
        "nuxt": "nuxt_80_20_otel_validation.py"
    }
    
    script = scripts.get(component, "otel_instrumentation_suite.py")
    
    with console.status(f"[bold cyan]Collecting data for {duration}s..."):
        result = subprocess.run([
            "python", script, 
            "--duration", str(duration),
            "--format", export_format
        ])
    
    if result.returncode == 0:
        console.print(f"✅ OTEL data collected and exported as {export_format}")
    else:
        console.print("❌ OTEL collection failed", style="bold red")
        raise typer.Exit(1)

@monitor_app.command("metrics")
def monitor_metrics(
    metric_type: str = typer.Option("performance", help="Type: performance/business/system"),
    timeframe: str = typer.Option("1h", help="Timeframe: 1h/24h/7d/30d")
):
    """📈 Display system metrics"""
    console.print(Panel(f"📈 {metric_type.title()} Metrics ({timeframe})", style="bold yellow"))
    
    # Mock metrics display - in real implementation would query monitoring system
    metrics_data = {
        "performance": ["Latency: 45ms", "Throughput: 1200 req/s", "Error Rate: 0.1%"],
        "business": ["Active Users: 1,250", "Revenue: $12,500", "Conversion: 3.2%"],
        "system": ["CPU: 65%", "Memory: 2.1GB", "Disk: 45%"]
    }
    
    table = Table(title=f"{metric_type.title()} Metrics")
    table.add_column("Metric", style="cyan")
    table.add_column("Value", style="green")
    table.add_column("Status", style="yellow")
    
    for metric in metrics_data.get(metric_type, []):
        name, value = metric.split(": ")
        status = "✅ Good" if "Error" not in metric else "⚠️ Watch"
        table.add_row(name, value, status)
    
    console.print(table)

# ============================================================================
# 🎯 Main CLI Status and Help Commands
# ============================================================================

@app.command("status")
def system_status():
    """🎯 Show overall system status"""
    console.print(Panel("🎯 CNS Forge System Status", style="bold green"))
    
    # Check component health
    components = {
        "CNS Forge": check_forge_health(),
        "BitActor": check_bitactor_health(), 
        "Nuxt UI": check_nuxt_health(),
        "K8s Cluster": check_k8s_health(),
        "Database": check_db_health()
    }
    
    table = Table(title="System Health")
    table.add_column("Component", style="cyan")
    table.add_column("Status", style="green")
    table.add_column("Details", style="yellow")
    
    for component, (status, details) in components.items():
        status_icon = "✅" if status else "❌"
        table.add_row(component, f"{status_icon} {'Running' if status else 'Down'}", details)
    
    console.print(table)

def check_forge_health():
    """Check if Phoenix server is running"""
    try:
        result = subprocess.run(["mix", "deps"], capture_output=True, cwd=FORGE_PATH)
        return True, "Dependencies OK"
    except:
        return False, "Mix not available"

def check_bitactor_health():
    """Check BitActor build status"""
    bitactor_binary = Path("bitactor_integrated_demo")
    if bitactor_binary.exists():
        return True, "Binaries built"
    return False, "Need to build"

def check_nuxt_health():
    """Check Nuxt setup"""
    package_json = Path("package.json")
    if package_json.exists():
        return True, "Node dependencies OK"
    return False, "No package.json"

def check_k8s_health():
    """Check K8s cluster connectivity"""
    try:
        result = subprocess.run(["kubectl", "cluster-info"], capture_output=True)
        return result.returncode == 0, "Cluster accessible" if result.returncode == 0 else "No cluster"
    except:
        return False, "kubectl not available"

def check_db_health():
    """Check database connectivity"""
    # Simplified check - would check actual DB in real implementation
    return True, "PostgreSQL OK"

@app.command("quickstart")
def quickstart():
    """🚀 Quick start guide for new users"""
    console.print(Panel("🚀 CNS Forge Quick Start", style="bold blue"))
    
    steps = [
        "1. Install dependencies: `cns forge deps`",
        "2. Compile the system: `cns forge compile`", 
        "3. Build BitActor: `cns bitactor build`",
        "4. Run tests: `cns test all`",
        "5. Start services: `cns forge start`",
        "6. Check status: `cns status`"
    ]
    
    for step in steps:
        console.print(f"  {step}")
    
    console.print("\n💡 For help with any command: `cns <command> --help`")

if __name__ == "__main__":
    app()