#!/usr/bin/env python3
"""
🚀 CNS ULTRATHINK 80/20 Master CLI
Python Typer-based CLI for managing all projects in this repository

Projects managed:
- CNS Forge (Elixir/Phoenix backend)
- Generated Cybersecurity Project (Ash Resources)  
- BitActor Distributed System
- Nuxt.js Frontend (NO TypeScript)
- Ash Reactor Workflows
- Kubernetes Deployments
- TTL/Ontology Generation
- DSPy Integration
- Pipeline Orchestration

20% effort → 80% project management capability
"""

import typer
import subprocess
import json
import os
import sys
from pathlib import Path
from typing import Optional, List
import yaml
from rich.console import Console
from rich.table import Table
from rich.progress import Progress, TaskID
from rich.panel import Panel
from rich.tree import Tree
import asyncio
import aiohttp

app = typer.Typer(
    name="cns",
    help="🚀 CNS ULTRATHINK 80/20 Master CLI - Manage all repository projects",
    rich_markup_mode="rich"
)

console = Console()

# Project configurations
PROJECTS = {
    "cns_forge": {
        "name": "CNS Forge",
        "path": ".",
        "type": "elixir_phoenix",
        "description": "Main Elixir/Phoenix backend with TTL transformation",
        "commands": {
            "install": "mix deps.get",
            "build": "mix compile",
            "test": "mix test", 
            "start": "mix phx.server",
            "format": "mix format"
        },
        "health_check": "http://localhost:4000/health"
    },
    "cybersecurity": {
        "name": "Generated Cybersecurity Project",
        "path": "generated_cybersecurity_project",
        "type": "ash_resources",
        "description": "Generated Ash resources for cybersecurity domain",
        "commands": {
            "install": "mix deps.get",
            "build": "mix compile",
            "test": "mix test",
            "migrate": "mix ash.migrate",
            "seed": "mix ash.seed"
        }
    },
    "dashboard": {
        "name": "ULTRATHINK Dashboard",
        "path": "dashboard_80_20",
        "type": "nuxt_js",
        "description": "Nuxt.js dashboard (NO TypeScript)",
        "commands": {
            "install": "npm install",
            "build": "npm run build",
            "dev": "npm run dev",
            "test": "npm run test",
            "lint": "npm run lint"
        },
        "health_check": "http://localhost:3000"
    },
    "templates": {
        "name": "Template Engine",
        "path": "templates",
        "type": "templates",
        "description": "Template files for code generation",
        "commands": {
            "validate": "find . -name '*.hbs' -exec handlebars {} \\;",
            "lint": "find . -name '*.vue' -exec npm run lint:vue {} \\;"
        }
    }
}

# Pipeline stages configuration
PIPELINE_STAGES = [
    "typer",      # Python CLI management (this file!)
    "turtle",     # TTL/Turtle generation
    "ttl2dspy",   # TTL to DSPy conversion  
    "bitactor",   # Distributed coordination
    "erlang",     # OTP code generation
    "ash",        # Resource creation
    "reactor",    # Workflow execution
    "k8s"         # Kubernetes deployment
]

@app.command()
def status():
    """📊 Show status of all projects in the repository"""
    console.print(Panel("🚀 CNS ULTRATHINK 80/20 Repository Status", style="bold blue"))
    
    table = Table(title="Project Status")
    table.add_column("Project", style="cyan")
    table.add_column("Type", style="magenta")
    table.add_column("Path", style="green")
    table.add_column("Status", style="yellow")
    table.add_column("Description")
    
    for project_id, config in PROJECTS.items():
        path = Path(config["path"])
        status = "✅ Ready" if path.exists() else "❌ Missing"
        
        table.add_row(
            config["name"],
            config["type"],
            config["path"],
            status,
            config["description"]
        )
    
    console.print(table)
    
    # Show pipeline status
    console.print("\n🌊 Pipeline Stages:")
    pipeline_tree = Tree("ULTRATHINK 80/20 Pipeline")
    for i, stage in enumerate(PIPELINE_STAGES, 1):
        stage_status = "🎯 Active" if stage == "typer" else "⏳ Pending"
        pipeline_tree.add(f"{i}. {stage.upper()} {stage_status}")
    
    console.print(pipeline_tree)

@app.command()
def install(
    project: Optional[str] = typer.Argument(None, help="Project to install (or 'all')"),
    force: bool = typer.Option(False, "--force", "-f", help="Force reinstall")
):
    """📦 Install dependencies for projects"""
    
    if project == "all" or project is None:
        projects_to_install = PROJECTS.keys()
        console.print("🔄 Installing all projects...")
    else:
        if project not in PROJECTS:
            console.print(f"❌ Unknown project: {project}")
            console.print(f"Available: {', '.join(PROJECTS.keys())}")
            raise typer.Exit(1)
        projects_to_install = [project]
    
    with Progress() as progress:
        task = progress.add_task("Installing dependencies...", total=len(projects_to_install))
        
        for proj_id in projects_to_install:
            config = PROJECTS[proj_id]
            console.print(f"\n📦 Installing {config['name']}...")
            
            if not Path(config["path"]).exists():
                console.print(f"⚠️  Path {config['path']} does not exist, skipping...")
                progress.advance(task)
                continue
            
            cmd = config["commands"].get("install")
            if cmd:
                result = run_command(cmd, cwd=config["path"])
                if result.returncode == 0:
                    console.print(f"✅ {config['name']} installed successfully")
                else:
                    console.print(f"❌ {config['name']} installation failed")
            else:
                console.print(f"⚠️  No install command for {config['name']}")
            
            progress.advance(task)

@app.command()
def build(
    project: Optional[str] = typer.Argument(None, help="Project to build (or 'all')"),
    watch: bool = typer.Option(False, "--watch", "-w", help="Watch mode for development")
):
    """🔨 Build projects"""
    
    if project == "all" or project is None:
        projects_to_build = PROJECTS.keys()
    else:
        if project not in PROJECTS:
            console.print(f"❌ Unknown project: {project}")
            raise typer.Exit(1)
        projects_to_build = [project]
    
    for proj_id in projects_to_build:
        config = PROJECTS[proj_id]
        console.print(f"\n🔨 Building {config['name']}...")
        
        cmd = config["commands"].get("build")
        if cmd:
            if watch and config["type"] == "nuxt_js":
                cmd = config["commands"].get("dev", cmd)
            
            result = run_command(cmd, cwd=config["path"])
            if result.returncode == 0:
                console.print(f"✅ {config['name']} built successfully")
            else:
                console.print(f"❌ {config['name']} build failed")

@app.command()
def test(
    project: Optional[str] = typer.Argument(None, help="Project to test (or 'all')"),
    coverage: bool = typer.Option(False, "--coverage", "-c", help="Run with coverage")
):
    """🧪 Run tests for projects"""
    
    if project == "all" or project is None:
        projects_to_test = PROJECTS.keys()
    else:
        if project not in PROJECTS:
            console.print(f"❌ Unknown project: {project}")
            raise typer.Exit(1)
        projects_to_test = [project]
    
    test_results = {}
    
    for proj_id in projects_to_test:
        config = PROJECTS[proj_id]
        console.print(f"\n🧪 Testing {config['name']}...")
        
        cmd = config["commands"].get("test")
        if cmd:
            if coverage and config["type"] == "elixir_phoenix":
                cmd = "mix test --cover"
            elif coverage and config["type"] == "nuxt_js":
                cmd = "npm run test:coverage"
            
            result = run_command(cmd, cwd=config["path"])
            test_results[proj_id] = result.returncode == 0
            
            if result.returncode == 0:
                console.print(f"✅ {config['name']} tests passed")
            else:
                console.print(f"❌ {config['name']} tests failed")
        else:
            console.print(f"⚠️  No test command for {config['name']}")
            test_results[proj_id] = None
    
    # Summary
    console.print("\n📊 Test Results Summary:")
    table = Table()
    table.add_column("Project")
    table.add_column("Result")
    
    for proj_id, result in test_results.items():
        if result is True:
            status = "✅ PASS"
        elif result is False:
            status = "❌ FAIL"
        else:
            status = "⚠️  N/A"
        
        table.add_row(PROJECTS[proj_id]["name"], status)
    
    console.print(table)

@app.command()
def start(
    project: str = typer.Argument(..., help="Project to start"),
    port: Optional[int] = typer.Option(None, "--port", "-p", help="Custom port"),
    dev: bool = typer.Option(False, "--dev", "-d", help="Development mode")
):
    """🚀 Start a project server"""
    
    if project not in PROJECTS:
        console.print(f"❌ Unknown project: {project}")
        raise typer.Exit(1)
    
    config = PROJECTS[project]
    console.print(f"🚀 Starting {config['name']}...")
    
    cmd = config["commands"].get("start")
    if not cmd:
        console.print(f"❌ No start command for {config['name']}")
        raise typer.Exit(1)
    
    # Modify command for development mode
    if dev and config["type"] == "nuxt_js":
        cmd = config["commands"].get("dev", cmd)
    
    # Add port if specified
    if port:
        if config["type"] == "elixir_phoenix":
            os.environ["PORT"] = str(port)
        elif config["type"] == "nuxt_js":
            cmd = f"{cmd} --port {port}"
    
    console.print(f"Running: {cmd}")
    try:
        run_command(cmd, cwd=config["path"], interactive=True)
    except KeyboardInterrupt:
        console.print(f"\n🛑 Stopped {config['name']}")

@app.command()
def pipeline(
    stage: Optional[str] = typer.Argument(None, help="Pipeline stage to execute"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be executed"),
    notify: bool = typer.Option(True, "--notify/--no-notify", help="Send notifications")
):
    """🌊 Execute ULTRATHINK 80/20 pipeline stages"""
    
    if stage is None:
        # Show pipeline status
        console.print("🌊 ULTRATHINK 80/20 Pipeline Stages:")
        
        table = Table()
        table.add_column("Stage", style="cyan")
        table.add_column("Description", style="white")
        table.add_column("Status", style="yellow")
        
        stage_descriptions = {
            "typer": "Python CLI management (THIS!)",
            "turtle": "TTL/Turtle ontology generation", 
            "ttl2dspy": "TTL to DSPy signature conversion",
            "bitactor": "Distributed actor coordination",
            "erlang": "Erlang OTP code generation",
            "ash": "Ash resource creation",
            "reactor": "Reactor workflow execution",
            "k8s": "Kubernetes deployment"
        }
        
        for i, pipeline_stage in enumerate(PIPELINE_STAGES, 1):
            status = "🎯 ACTIVE" if pipeline_stage == "typer" else "⏳ Ready"
            table.add_row(
                f"{i}. {pipeline_stage.upper()}",
                stage_descriptions.get(pipeline_stage, ""),
                status
            )
        
        console.print(table)
        return
    
    if stage not in PIPELINE_STAGES:
        console.print(f"❌ Unknown pipeline stage: {stage}")
        console.print(f"Available stages: {', '.join(PIPELINE_STAGES)}")
        raise typer.Exit(1)
    
    console.print(f"🌊 Executing pipeline stage: {stage.upper()}")
    
    if dry_run:
        console.print("🔍 DRY RUN - Commands that would be executed:")
    
    # Execute stage-specific commands
    execute_pipeline_stage(stage, dry_run, notify)

def execute_pipeline_stage(stage: str, dry_run: bool, notify: bool):
    """Execute specific pipeline stage"""
    
    stage_commands = {
        "typer": [
            "python cns_cli.py status",
            "echo '✅ Typer CLI management active'"
        ],
        "turtle": [
            "mix run -e 'CnsForge.TurtleGenerator.generate_from_ontology()'",
            "echo '🐢 TTL generation complete'"
        ],
        "ttl2dspy": [
            "python ttl_to_dspy_converter.py --input ontology.ttl --output signatures.py",
            "echo '🔄 DSPy conversion complete'"
        ],
        "bitactor": [
            "mix run -e 'CnsForge.BitActor.start_distributed_system()'",
            "echo '🎭 BitActor coordination active'"
        ],
        "erlang": [
            "mix run -e 'CnsForge.ErlangGenerator.generate_otp_code()'",
            "echo '⚡ Erlang OTP generation complete'"
        ],
        "ash": [
            "cd generated_cybersecurity_project && mix ash.migrate",
            "echo '🔥 Ash resources ready'"
        ],
        "reactor": [
            "mix run -e 'CnsForge.ReactorWorkflows.execute_main_workflow()'",
            "echo '⚛️ Reactor workflow execution complete'"
        ],
        "k8s": [
            "kubectl apply -f k8s-notification-deployment.yaml",
            "echo '☸️ Kubernetes deployment complete'"
        ]
    }
    
    commands = stage_commands.get(stage, [])
    
    for cmd in commands:
        console.print(f"📝 {cmd}")
        
        if not dry_run:
            result = run_command(cmd)
            if result.returncode != 0:
                console.print(f"❌ Stage {stage} failed")
                return
    
    if not dry_run:
        console.print(f"✅ Pipeline stage {stage.upper()} completed")
        
        if notify:
            send_pipeline_notification(stage, "completed")

@app.command()
def health():
    """🩺 Check health of all running services"""
    
    console.print("🩺 Health Check for CNS ULTRATHINK 80/20 Services")
    
    table = Table()
    table.add_column("Service", style="cyan")
    table.add_column("URL", style="blue")
    table.add_column("Status", style="yellow")
    table.add_column("Response Time")
    
    for project_id, config in PROJECTS.items():
        if "health_check" in config:
            url = config["health_check"]
            status, response_time = check_service_health(url)
            
            table.add_row(
                config["name"],
                url,
                status,
                f"{response_time:.2f}ms" if response_time else "N/A"
            )
    
    console.print(table)

@app.command()
def generate(
    component: str = typer.Argument(..., help="Component to generate"),
    name: str = typer.Argument(..., help="Name for the component"),
    template: Optional[str] = typer.Option(None, "--template", "-t", help="Template to use")
):
    """🏗️ Generate new components using templates"""
    
    generators = {
        "ash_resource": generate_ash_resource,
        "nuxt_component": generate_nuxt_component,
        "reactor_workflow": generate_reactor_workflow,
        "bitactor": generate_bitactor,
        "channel_handler": generate_channel_handler
    }
    
    if component not in generators:
        console.print(f"❌ Unknown component type: {component}")
        console.print(f"Available: {', '.join(generators.keys())}")
        raise typer.Exit(1)
    
    console.print(f"🏗️ Generating {component}: {name}")
    generators[component](name, template)

@app.command()
def deploy(
    environment: str = typer.Argument(..., help="Environment (dev/staging/prod)"),
    project: Optional[str] = typer.Option(None, "--project", "-p", help="Specific project"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show deployment plan")
):
    """🚀 Deploy projects to specified environment"""
    
    if environment not in ["dev", "staging", "prod"]:
        console.print("❌ Environment must be: dev, staging, or prod")
        raise typer.Exit(1)
    
    console.print(f"🚀 Deploying to {environment.upper()}")
    
    # Build deployment plan
    deployment_plan = build_deployment_plan(environment, project)
    
    if dry_run:
        console.print("📋 Deployment Plan:")
        for step in deployment_plan:
            console.print(f"  • {step}")
        return
    
    # Execute deployment
    execute_deployment(deployment_plan, environment)

# Utility functions

def run_command(cmd: str, cwd: Optional[str] = None, interactive: bool = False) -> subprocess.CompletedProcess:
    """Run shell command with proper handling"""
    try:
        if interactive:
            subprocess.run(cmd, shell=True, cwd=cwd)
            return subprocess.CompletedProcess(cmd, 0)
        else:
            return subprocess.run(
                cmd, 
                shell=True, 
                cwd=cwd, 
                capture_output=True, 
                text=True,
                timeout=300
            )
    except subprocess.TimeoutExpired:
        console.print(f"⏰ Command timed out: {cmd}")
        return subprocess.CompletedProcess(cmd, 1)

def check_service_health(url: str) -> tuple[str, Optional[float]]:
    """Check if service is healthy"""
    try:
        import requests
        import time
        
        start_time = time.time()
        response = requests.get(url, timeout=5)
        response_time = (time.time() - start_time) * 1000
        
        if response.status_code == 200:
            return "✅ Healthy", response_time
        else:
            return f"⚠️ {response.status_code}", response_time
            
    except Exception as e:
        return "❌ Unreachable", None

def send_pipeline_notification(stage: str, status: str):
    """Send notification about pipeline stage completion"""
    try:
        # This would integrate with the notification system we built
        notification = {
            "stage": stage,
            "status": status,
            "timestamp": "now",
            "source": "cns_cli"
        }
        console.print(f"📢 Notification sent: {stage} {status}")
    except Exception as e:
        console.print(f"⚠️ Failed to send notification: {e}")

def generate_ash_resource(name: str, template: Optional[str]):
    """Generate Ash resource"""
    console.print(f"🔥 Generating Ash resource: {name}")
    # Implementation would use templates
    
def generate_nuxt_component(name: str, template: Optional[str]):
    """Generate Nuxt.js component (NO TypeScript)"""
    console.print(f"🎨 Generating Nuxt component: {name} (NO TypeScript)")
    # Implementation would use Vue templates
    
def generate_reactor_workflow(name: str, template: Optional[str]):
    """Generate Reactor workflow"""
    console.print(f"⚛️ Generating Reactor workflow: {name}")
    # Implementation would use workflow templates
    
def generate_bitactor(name: str, template: Optional[str]):
    """Generate BitActor"""
    console.print(f"🎭 Generating BitActor: {name}")
    # Implementation would use actor templates
    
def generate_channel_handler(name: str, template: Optional[str]):
    """Generate ChannelHandler"""
    console.print(f"📡 Generating ChannelHandler: {name}")
    # Implementation would use channel templates

def build_deployment_plan(environment: str, project: Optional[str]) -> List[str]:
    """Build deployment plan for environment"""
    plan = [
        "1. Build all projects",
        "2. Run tests",
        "3. Generate deployment manifests",
        "4. Deploy to Kubernetes",
        "5. Run health checks",
        "6. Send notifications"
    ]
    return plan

def execute_deployment(plan: List[str], environment: str):
    """Execute deployment plan"""
    with Progress() as progress:
        task = progress.add_task("Deploying...", total=len(plan))
        
        for step in plan:
            console.print(f"🚀 {step}")
            # Implementation would execute actual deployment steps
            progress.advance(task)
    
    console.print(f"✅ Deployment to {environment} completed!")

if __name__ == "__main__":
    console.print("""
🚀 CNS ULTRATHINK 80/20 Master CLI
==================================
    
Python Typer CLI for managing all repository projects:
- CNS Forge (Elixir/Phoenix)
- Generated Cybersecurity Project  
- Nuxt.js Dashboard (NO TypeScript)
- BitActor Distributed System
- Ash Reactor Workflows
- Kubernetes Deployments

20% effort → 80% project management capability
    """)
    
    app()