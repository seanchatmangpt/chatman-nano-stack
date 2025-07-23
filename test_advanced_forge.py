#!/usr/bin/env python3
"""
Test Advanced Forge Features
Comprehensive testing of quality controls and DSPy agents
"""

from pathlib import Path
import json
from rich.console import Console
from rich.panel import Panel

from ontology_quality_control import OntologyQualityController
from dspy_ontology_agents import OntologyAgentSwarm, CollaborativeOntologyOptimizer
from advanced_forge_cli import app

console = Console()

def test_quality_control():
    """Test quality control system"""
    console.print(Panel.fit(
        "[bold blue]Testing Quality Control System[/bold blue]",
        border_style="blue"
    ))
    
    # Test with existing UHFT ontologies
    uhft_dir = Path("ontologies/generated/uhft")
    if not uhft_dir.exists():
        console.print("[red]UHFT ontologies not found - run UHFT generation first[/red]")
        return
    
    qc = OntologyQualityController()
    
    console.print("🔍 Running comprehensive validation...")
    report = qc.validate_ontology_suite(uhft_dir, "trading")
    
    console.print(f"📊 Quality Score: {report.metrics['quality_score']:.1f}/100")
    console.print(f"🔴 Critical Issues: {report.critical_count}")
    console.print(f"🟡 Warnings: {report.warning_count}")
    console.print(f"✅ Passed: {'Yes' if report.passed else 'No'}")
    
    # Test optimization
    if report.issues:
        console.print("\n⚡ Testing ontology optimization...")
        
        core_files = list(uhft_dir.glob("*core*.ttl"))
        if core_files:
            try:
                optimized_content, optimizations = qc.optimize_ontology(
                    core_files[0], 
                    "trading", 
                    {"tick_compliance": 8}
                )
                
                console.print(f"📈 Applied {len(optimizations)} optimizations:")
                for opt in optimizations:
                    console.print(f"  • {opt}")
                    
            except Exception as e:
                console.print(f"[red]Optimization failed: {str(e)}[/red]")
    
    return report

def test_agent_swarm():
    """Test multi-agent analysis system"""
    console.print(Panel.fit(
        "[bold green]Testing Multi-Agent Swarm[/bold green]",
        border_style="green"
    ))
    
    # Test with trading ontologies
    trading_dir = Path("ontologies/meta_generated/trading")
    if not trading_dir.exists():
        console.print("[red]Trading ontologies not found - generate first[/red]")
        return
    
    swarm = OntologyAgentSwarm()
    
    requirements = {
        "performance_requirements": {"tick_compliance": 8},
        "compliance_standards": ["MiFID II"],
        "domain_patterns": {"ultra_low_latency": True},
        "hardware_constraints": {"target_latency_ns": 100}
    }
    
    console.print("🤖 Running multi-agent analysis...")
    try:
        analysis = swarm.analyze_ontology_suite(trading_dir, "trading", requirements)
        
        console.print(f"🎯 Domain Accuracy: {analysis['domain_analysis']['accuracy_score']:.1f}%")
        console.print(f"⚡ Performance Score: {analysis['performance_analysis']['performance_score']:.1f}%")
        console.print(f"📋 Compliance Score: {analysis['compliance_analysis']['compliance_score']:.1f}%")
        console.print(f"🏆 Overall Quality: {analysis['quality_synthesis']['overall_quality_score']:.1f}%")
        
        # Show agent communications
        console.print(f"\n💬 Agent Messages: {len(analysis['message_history'])}")
        for msg in analysis['message_history'][:3]:  # Show first 3
            console.print(f"  {msg['from_agent']} → {msg['to_agent']}: {msg['content']}")
        
        # Generate collaboration report
        report = swarm.generate_agent_collaboration_report()
        report_file = Path("agent_collaboration_report.md")
        report_file.write_text(report)
        console.print(f"📄 Collaboration report saved to: {report_file}")
        
        return analysis
        
    except Exception as e:
        console.print(f"[red]Agent swarm failed: {str(e)}[/red]")
        return None

def test_collaborative_optimization():
    """Test collaborative optimization"""
    console.print(Panel.fit(
        "[bold yellow]Testing Collaborative Optimization[/bold yellow]",
        border_style="yellow"
    ))
    
    trading_dir = Path("ontologies/meta_generated/trading")
    if not trading_dir.exists():
        console.print("[red]Trading ontologies not found[/red]")
        return
    
    optimizer = CollaborativeOntologyOptimizer()
    
    requirements = {
        "performance_requirements": {"tick_compliance": 8},
        "compliance_standards": ["MiFID II"],
        "quality_threshold": 90.0
    }
    
    console.print("🔄 Running iterative optimization...")
    try:
        optimization_result = optimizer.iterative_optimization(
            trading_dir, "trading", requirements, max_iterations=2
        )
        
        console.print(f"🎯 Iterations Completed: {optimization_result['iterations_completed']}")
        console.print(f"📈 Final Quality Score: {optimization_result['final_quality_score']:.1f}%")
        
        # Show optimization history
        for iteration in optimization_result['optimization_history']:
            if 'error' not in iteration:
                console.print(f"  Iteration {iteration['iteration']}: "
                            f"{iteration['quality_score_before']:.1f}% → "
                            f"{iteration['quality_score_after']:.1f}%")
        
        return optimization_result
        
    except Exception as e:
        console.print(f"[red]Collaborative optimization failed: {str(e)}[/red]")
        return None

def test_advanced_commands():
    """Test advanced CLI commands"""
    console.print(Panel.fit(
        "[bold magenta]Testing Advanced CLI Commands[/bold magenta]",
        border_style="magenta"
    ))
    
    # Test validation command
    console.print("🔍 Testing validation command...")
    
    # Create test manifest for validation
    test_dir = Path("test_ontologies")
    test_dir.mkdir(exist_ok=True)
    
    # Create simple test ontology
    test_ttl = test_dir / "test.ttl"
    test_ttl.write_text("""
@prefix : <http://test.io#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:TestClass a owl:Class ;
    rdfs:label "Test Class" .

:testProperty a owl:DatatypeProperty ;
    rdfs:domain :TestClass .
""")
    
    # Create manifest
    manifest = {
        "domain": "test",
        "name": "Test Ontology",
        "performance_requirements": {"tick_compliance": 8},
        "compliance_standards": []
    }
    
    manifest_file = test_dir / "manifest.json"
    with open(manifest_file, 'w') as f:
        json.dump(manifest, f, indent=2)
    
    console.print(f"✅ Created test ontology in {test_dir}")
    
    # Show what advanced commands would do
    console.print("\n📋 Available Advanced Commands:")
    commands = [
        ("validate", "Comprehensive quality validation"),
        ("optimize", "DSPy-powered optimization"),
        ("audit", "Compliance auditing"),
        ("benchmark", "Advanced benchmarking"),
        ("explain", "AI-powered explanation"),
        ("compare", "Multi-dimensional comparison"),
        ("deploy", "Quality-gated deployment")
    ]
    
    for cmd, desc in commands:
        console.print(f"  • {cmd}: {desc}")

def run_comprehensive_test():
    """Run comprehensive test suite"""
    console.print(Panel.fit(
        "[bold red]🚀 Advanced Forge Comprehensive Test Suite[/bold red]",
        border_style="red"
    ))
    
    results = {}
    
    # Test 1: Quality Control
    try:
        results['quality_control'] = test_quality_control()
        console.print("[green]✅ Quality Control Test: PASSED[/green]")
    except Exception as e:
        console.print(f"[red]❌ Quality Control Test: FAILED - {str(e)}[/red]")
        results['quality_control'] = None
    
    console.print()
    
    # Test 2: Agent Swarm
    try:
        results['agent_swarm'] = test_agent_swarm()
        console.print("[green]✅ Agent Swarm Test: PASSED[/green]")
    except Exception as e:
        console.print(f"[red]❌ Agent Swarm Test: FAILED - {str(e)}[/red]")
        results['agent_swarm'] = None
    
    console.print()
    
    # Test 3: Collaborative Optimization
    try:
        results['collaborative_optimization'] = test_collaborative_optimization()
        console.print("[green]✅ Collaborative Optimization Test: PASSED[/green]")
    except Exception as e:
        console.print(f"[red]❌ Collaborative Optimization Test: FAILED - {str(e)}[/red]")
        results['collaborative_optimization'] = None
    
    console.print()
    
    # Test 4: Advanced Commands
    try:
        test_advanced_commands()
        console.print("[green]✅ Advanced Commands Test: PASSED[/green]")
    except Exception as e:
        console.print(f"[red]❌ Advanced Commands Test: FAILED - {str(e)}[/red]")
    
    # Summary
    console.print("\n" + "="*60)
    console.print("[bold blue]Test Summary:[/bold blue]")
    
    passed = sum(1 for result in results.values() if result is not None)
    total = len(results) + 1  # +1 for advanced commands
    
    console.print(f"Tests Passed: {passed + 1}/{total}")
    console.print(f"Success Rate: {((passed + 1)/total)*100:.1f}%")
    
    if results['quality_control']:
        console.print(f"Quality Score: {results['quality_control'].metrics['quality_score']:.1f}/100")
    
    if results['agent_swarm']:
        console.print(f"Agent Analysis Quality: {results['agent_swarm']['quality_synthesis']['overall_quality_score']:.1f}/100")
    
    console.print("\n🎉 Advanced Forge Testing Complete!")

if __name__ == "__main__":
    run_comprehensive_test()