#!/usr/bin/env python3
"""
CNS Claude Flow CLI - DSPy-powered AI orchestration
Integrates with ollama qwen3:latest for ultra-intelligent development
"""

import typer
from typing import Optional, List
from pathlib import Path
import dspy
from rich.console import Console
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.table import Table
import json
import subprocess
import sys
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor, ConsoleSpanExporter

# Initialize OpenTelemetry
trace.set_tracer_provider(TracerProvider())
tracer = trace.get_tracer(__name__)
span_processor = BatchSpanProcessor(ConsoleSpanExporter())
trace.get_tracer_provider().add_span_processor(span_processor)

console = Console()
app = typer.Typer(
    name="cf",
    help="🚀 CNS Claude Flow - DSPy-powered AI orchestration with qwen3:latest",
    rich_markup_mode="rich"
)

class UltraThinkChain(dspy.Module):
    """DSPy chain for ultra-intelligence reasoning"""
    
    def __init__(self):
        super().__init__()
        self.think = dspy.ChainOfThought("context, task -> reasoning, implementation_plan, code_strategy")
        self.validate = dspy.ChainOfThought("plan, requirements -> validation, business_value, telemetry_points")
        
    def forward(self, context: str, task: str, requirements: str = ""):
        with tracer.start_as_current_span("ultrathink_reasoning"):
            # Ultra-intelligence reasoning phase
            thinking = self.think(context=context, task=task)
            
            # Validation with business value assessment
            validation = self.validate(
                plan=thinking.implementation_plan, 
                requirements=requirements or "80/20 principle, Design for Lean Six Sigma"
            )
            
            return dspy.Prediction(
                reasoning=thinking.reasoning,
                implementation_plan=thinking.implementation_plan,
                code_strategy=thinking.code_strategy,
                validation=validation.validation,
                business_value=validation.business_value,
                telemetry_points=validation.telemetry_points
            )

class CNSClaudeFlow:
    """Main Claude Flow orchestrator with DSPy integration"""
    
    def __init__(self):
        # Initialize DSPy with ollama qwen3:latest
        try:
            self.lm = dspy.OllamaLocal(model="qwen3:latest")
            dspy.settings.configure(lm=self.lm)
            self.ultrathink = UltraThinkChain()
            console.print("✅ [green]Connected to ollama qwen3:latest[/green]")
        except Exception as e:
            console.print(f"⚠️ [yellow]Warning: Could not connect to ollama: {e}[/yellow]")
            console.print("💡 [blue]Tip: Run 'ollama serve' and 'ollama pull qwen3:latest'[/blue]")
            self.lm = None
            self.ultrathink = None
    
    def get_project_context(self) -> str:
        """Get current project context for reasoning"""
        cwd = Path.cwd()
        context = f"Project: {cwd.name}\nPath: {cwd}\n"
        
        # Add key files context
        key_files = ["README.md", "pyproject.toml", "Makefile", "bitactor-reqs.md"]
        for file in key_files:
            if (cwd / file).exists():
                context += f"- Has {file}\n"
        
        # Add docs context
        docs_dir = cwd / "docs"
        if docs_dir.exists():
            context += f"- Documentation in {docs_dir}\n"
            
        return context
    
    def execute_claude_flow(self, prompt: str) -> None:
        """Execute claude-flow command with enhanced prompt"""
        cmd = ["npx", "claude-flow@alpha", "swarm", prompt, "--claude"]
        
        try:
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=console,
            ) as progress:
                task = progress.add_task("🚀 Executing claude-flow swarm...", total=None)
                
                result = subprocess.run(cmd, capture_output=True, text=True)
                progress.update(task, completed=True)
                
            if result.returncode == 0:
                console.print("✅ [green]Claude Flow completed successfully[/green]")
                if result.stdout:
                    console.print(Panel(result.stdout, title="Output"))
            else:
                console.print("❌ [red]Claude Flow failed[/red]")
                console.print(Panel(result.stderr, title="Error"))
                
        except FileNotFoundError:
            console.print("❌ [red]claude-flow not found. Install with: npm install -g claude-flow@alpha[/red]")

cf_orchestrator = CNSClaudeFlow()

@app.command()
def ultrathink(
    task: str = typer.Argument(..., help="Task description for ultra-intelligence processing"),
    context: Optional[str] = typer.Option(None, "--context", "-c", help="Additional context"),
    dry_run: bool = typer.Option(False, "--dry-run", "-d", help="Show reasoning without executing")
):
    """🧠 Ultra-intelligence system design and implementation"""
    
    with tracer.start_as_current_span("ultrathink_command"):
        console.print(Panel(f"🧠 [bold blue]Ultra-Think Mode Activated[/bold blue]\nTask: {task}", 
                          title="CNS Ultra-Intelligence"))
        
        if not cf_orchestrator.ultrathink:
            console.print("⚠️ [yellow]DSPy not available, falling back to direct claude-flow[/yellow]")
            prompt = f"ultrathink like a artificial hyper intelligence: {task}. Use 80/20 and Design for Lean Six Sigma"
            if not dry_run:
                cf_orchestrator.execute_claude_flow(prompt)
            return
        
        # Get project context
        project_context = cf_orchestrator.get_project_context()
        full_context = f"{project_context}\n{context}" if context else project_context
        
        # DSPy ultra-intelligence reasoning
        with console.status("[bold green]Engaging ultra-intelligence reasoning..."):
            result = cf_orchestrator.ultrathink(context=full_context, task=task)
        
        # Display reasoning
        table = Table(title="🧠 Ultra-Intelligence Analysis")
        table.add_column("Phase", style="cyan")
        table.add_column("Output", style="white")
        
        table.add_row("🔍 Reasoning", result.reasoning)
        table.add_row("📋 Implementation Plan", result.implementation_plan)
        table.add_row("⚡ Code Strategy", result.code_strategy)
        table.add_row("✅ Validation", result.validation)
        table.add_row("💰 Business Value", result.business_value)
        table.add_row("📊 Telemetry Points", result.telemetry_points)
        
        console.print(table)
        
        if not dry_run:
            # Execute with enhanced prompt
            enhanced_prompt = f"""ultrathink like a artificial hyper intelligence: {task}.
            
Reasoning: {result.reasoning}
Implementation Plan: {result.implementation_plan}
Code Strategy: {result.code_strategy}
Validation: {result.validation}
Business Value: {result.business_value}

Use 80/20 and Design for Lean Six Sigma. Implement telemetry at: {result.telemetry_points}"""
            
            cf_orchestrator.execute_claude_flow(enhanced_prompt)

@app.command()
def implement(
    target_path: str = typer.Argument(..., help="Directory or requirements file to implement"),
    focus: Optional[str] = typer.Option("functionality", help="Focus area: functionality, tests, or both"),
    ai_level: str = typer.Option("hyper", help="AI intelligence level: normal, high, hyper")
):
    """⚡ 80/20 implementation with DSPy reasoning (directories/requirements only)"""
    
    target = Path(target_path)
    
    # Prevent single file creation - only allow directories or requirement files
    if target.suffix and target.suffix not in ['.md', '.txt', '.rst']:
        console.print(f"❌ [red]CF CLI does not support single file creation. Use directories or requirement files (.md, .txt, .rst)[/red]")
        console.print(f"💡 [blue]Tip: Create a requirements file or specify a directory instead[/blue]")
        raise typer.Exit(1)
    
    # Allow non-existent paths for directories (will be created)
    if target.suffix and not target.exists():
        console.print(f"❌ [red]Requirements file not found: {target_path}[/red]")
        raise typer.Exit(1)
    
    ai_prompts = {
        "normal": "implement efficiently",
        "high": "implement with advanced techniques", 
        "hyper": "implement at a Artificial Hyper Intelligence level with capabilities that a human could not imagine but always provide business value"
    }
    
    focus_prompts = {
        "functionality": "write the functionality, another swarm is writing the tests",
        "tests": "write the tests, another swarm is writing functionality", 
        "both": "implement both functionality and comprehensive tests"
    }
    
    if target.is_dir() or not target.suffix:
        prompt = f"ultrathink then 80/20 implement the system/module in {target_path} {ai_prompts[ai_level]}, {focus_prompts[focus]}. Create proper directory structure and multiple files as needed"
    else:
        prompt = f"ultrathink then 80/20 implement the requirements in {target_path} {ai_prompts[ai_level]}, {focus_prompts[focus]}. Create proper directory structure and multiple files as needed"
    
    console.print(Panel(f"⚡ [bold green]80/20 Implementation[/bold green]\nTarget: {target_path}\nFocus: {focus}\nAI Level: {ai_level}"))
    cf_orchestrator.execute_claude_flow(prompt)

@app.command()
def finish(
    target_path: str = typer.Argument(..., help="Directory or project to finish"),
    merge_tests: bool = typer.Option(True, help="Merge functionality with tests")
):
    """🎯 Finish implementation and merge components"""
    
    action = "merge the functionality with the tests" if merge_tests else "complete the implementation"
    prompt = f"ultrathink then finish the reverse 80/20 to finish {target_path} at a Artificial Hyper Intelligence level, {action}, there is WIP, don't start from scratch. LOOK AT THE ENTIRE FILE TREE and create proper multi-file structure"
    
    console.print(Panel(f"🎯 [bold yellow]Finishing Implementation[/bold yellow]\nTarget: {target_path}"))
    cf_orchestrator.execute_claude_flow(prompt)

@app.command()
def benchmark(
    report: bool = typer.Option(True, help="Generate comprehensive report"),
    validate_telemetry: bool = typer.Option(True, help="Validate with OpenTelemetry")
):
    """📊 Run benchmarks with telemetry validation"""
    
    base_prompt = "ultrathink run all the benchmarks that work"
    if report:
        base_prompt += ", and then write a report about all the revolutionary aspects of the Chatman Nano Stack"
    if validate_telemetry:
        base_prompt += ". Validate results against benchmarks and open telemetry. No fake or simulation code"
    
    console.print(Panel("📊 [bold blue]Running Benchmarks with Telemetry[/bold blue]"))
    cf_orchestrator.execute_claude_flow(base_prompt)

@app.command()
def validate(
    target_path: str = typer.Argument(..., help="Directory or system to validate"),
    check_business_value: bool = typer.Option(True, help="Validate business value"),
    check_telemetry: bool = typer.Option(True, help="Validate telemetry integration")
):
    """✅ Validate implementation with business value and telemetry"""
    
    validations = []
    if check_business_value:
        validations.append("business value")
    if check_telemetry:
        validations.append("benchmarks, telemetry")
    
    validation_str = ", ".join(validations)
    prompt = f"ultrathink then validate the system in {target_path}, check {validation_str}, analyze the entire codebase structure before providing any summaries"
    
    console.print(Panel(f"✅ [bold green]Validating Implementation[/bold green]\nTarget: {target_path}"))
    cf_orchestrator.execute_claude_flow(prompt)

@app.command()
def clean(
    target: str = typer.Option("mock", help="What to clean: mock, errors, all"),
    replace_with: str = typer.Option("ollama-qwen3", help="Replacement system")
):
    """🧹 Clean up mock implementations and error handling"""
    
    clean_prompts = {
        "mock": "ultrathink to remove all mock dspy implementation and replace with ollama with qwen3:latest",
        "errors": "ultrathink to add unit tests to any code with try catch, verify it works then let it crash. I do not want anything that handles errors",
        "all": "ultrathink to remove all mock implementations, replace with ollama qwen3:latest, and add crash-first unit tests"
    }
    
    prompt = clean_prompts.get(target, clean_prompts["mock"])
    console.print(Panel(f"🧹 [bold red]Cleaning: {target}[/bold red]"))
    cf_orchestrator.execute_claude_flow(prompt)

@app.command()
def fix():
    """🔧 Fix claude-flow command issues"""
    console.print(Panel("🔧 [bold yellow]Fixing Claude Flow Issues[/bold yellow]"))
    cf_orchestrator.execute_claude_flow("ultrathink to make the claude-flow commands work")

@app.command()
def custom(
    prompt: str = typer.Argument(..., help="Custom ultrathink prompt"),
    use_dspy: bool = typer.Option(True, help="Use DSPy reasoning enhancement")
):
    """🎨 Custom ultrathink prompt with DSPy enhancement"""
    
    if use_dspy and cf_orchestrator.ultrathink:
        console.print("🧠 [blue]Enhancing prompt with DSPy reasoning...[/blue]")
        context = cf_orchestrator.get_project_context()
        result = cf_orchestrator.ultrathink(context=context, task=prompt)
        
        enhanced_prompt = f"ultrathink {prompt}. Reasoning: {result.reasoning}. Strategy: {result.code_strategy}"
        cf_orchestrator.execute_claude_flow(enhanced_prompt)
    else:
        cf_orchestrator.execute_claude_flow(f"ultrathink {prompt}")

@app.command()
def status():
    """📊 Show CNS Claude Flow status"""
    
    table = Table(title="📊 CNS Claude Flow Status")
    table.add_column("Component", style="cyan")
    table.add_column("Status", style="white")
    
    # Check ollama
    try:
        result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
        if "qwen3:latest" in result.stdout:
            table.add_row("🦙 Ollama qwen3:latest", "✅ Available")
        else:
            table.add_row("🦙 Ollama qwen3:latest", "❌ Not found - run 'ollama pull qwen3:latest'")
    except FileNotFoundError:
        table.add_row("🦙 Ollama", "❌ Not installed")
    
    # Check claude-flow
    try:
        result = subprocess.run(["npx", "claude-flow@alpha", "--version"], capture_output=True, text=True)
        table.add_row("🌊 Claude Flow", "✅ Available")
    except FileNotFoundError:
        table.add_row("🌊 Claude Flow", "❌ Not available")
    
    # Check DSPy
    if cf_orchestrator.ultrathink:
        table.add_row("🧠 DSPy + qwen3", "✅ Connected")
    else:
        table.add_row("🧠 DSPy + qwen3", "❌ Not connected")
    
    console.print(table)

if __name__ == "__main__":
    app()